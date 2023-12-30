use core::panic;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, parse_quote, punctuated::Punctuated};

#[proc_macro_attribute]
pub fn system(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut func = parse_macro_input!(item as syn::ItemFn);
    let interface_ident = func.sig.ident.clone();
    let func_ident = syn::Ident::new(
        &std::format!("_{}_impl", interface_ident),
        interface_ident.span(),
    );
    func.sig.ident = func_ident.clone();
    let vis = func.vis.clone();

    let func_args = &func.sig.inputs;
    let mut func_arg_types = Punctuated::<syn::Type, syn::Token![,]>::new();
    let func_ret_type = match &func.sig.output {
        syn::ReturnType::Default => parse_quote!(()),
        syn::ReturnType::Type(_, btp) => btp.as_ref().clone(),
    };

    let mut take_types = Punctuated::<syn::Type, syn::Token![,]>::new();
    let take_pat: syn::Pat = parse_quote!(());
    let mut take_pat = match take_pat {
        syn::Pat::Tuple(tup) => tup,
        _ => panic!(),
    };

    let mut take_index_idents = Punctuated::<syn::Ident, syn::Token![,]>::new();

    let mut take_var_count = 0;
    let mut take_index_generics = Punctuated::<syn::GenericParam, syn::Token![,]>::new();
    let mut call_operands = Punctuated::<syn::Expr, syn::Token![,]>::new();

    let mut put_index_idents = Punctuated::<syn::Ident, syn::Token![,]>::new();
    let mut put_types = Punctuated::<syn::Type, syn::Token![,]>::new();
    for tp in func_args {
        match tp {
            syn::FnArg::Typed(tp) => {
                let tp = tp.ty.as_ref();
                func_arg_types.push(tp.clone());
                take_index_idents.push(syn::Ident::new(
                    &std::format!("_TAKE_IDX_{}", take_index_idents.len()),
                    Span::mixed_site(),
                ));
                let take_var_ident = syn::Ident::new(
                    &std::format!("_TAKE_VAR_{}", take_var_count),
                    Span::mixed_site(),
                );
                take_var_count += 1;
                let id = take_index_idents.last();
                take_index_generics.push(parse_quote!(const #id: usize));
                match tp {
                    syn::Type::Reference(tp) => {
                        take_types.push(tp.elem.as_ref().clone());
                        let id = take_var_ident;
                        take_pat.elems.push(parse_quote!(mut #id));
                        put_types.push(tp.elem.as_ref().clone());
                        put_index_idents.push(id.clone());
                        call_operands.push(parse_quote!(&mut #id));
                    }
                    tp => {
                        take_types.push(parse_quote!(#tp));
                        let id = take_var_ident;
                        take_pat.elems.push(parse_quote!(mut #id));
                        call_operands.push(parse_quote!(#id));
                    }
                }
            }
            _ => panic!(),
        }
    }
    let mut res_var_idents = Punctuated::<syn::Ident, syn::Token![,]>::new();
    match &func.sig.output {
        syn::ReturnType::Default => {}
        syn::ReturnType::Type(_, tp) => match tp.as_ref() {
            syn::Type::Tuple(tup) => {
                for elem in &tup.elems {
                    put_types.push(elem.clone());
                    put_index_idents.push(syn::Ident::new(
                        &std::format!("_RES_VAR_{}", put_index_idents.len()),
                        Span::mixed_site(),
                    ));
                    res_var_idents.push(put_index_idents.last().unwrap().clone());
                }
            }
            elem => {
                put_types.push(elem.clone());
                put_index_idents.push(syn::Ident::new(
                    &std::format!("_RES_VAR_{}", put_index_idents.len()),
                    Span::mixed_site(),
                ));
                res_var_idents.push(put_index_idents.last().unwrap().clone());
            }
        },
    }
    let mut where_clause: syn::WhereClause = parse_quote! {
        where _F: Fn(#func_arg_types)->(#func_ret_type),
    };

    where_clause.predicates.clear();

    let where_preds = &mut where_clause.predicates;
    let mut tmp_type: syn::Type = parse_quote!(_T);
    for (tp, idx) in take_types.iter().zip(take_index_idents.iter()) {
        let _ref = &tmp_type;
        where_preds.push(parse_quote! {
            #_ref: ComponentGet<#tp, #idx, Item=#tp>
        });
        tmp_type = parse_quote! {
            <#_ref as ComponentGet<#tp, #idx>>::AfterTake
        };
    }
    let mut res_expr: syn::Expr = parse_quote!(_rem);
    for (tp, idx) in put_types.iter().zip(put_index_idents.iter()) {
        let _ref = &tmp_type;
        where_preds.push(parse_quote! {
            #_ref: ComponentPut<#tp, Item = #tp>
        });
        tmp_type = parse_quote! {
            <#_ref as ComponentPut<#tp>>::AfterPut
        };
        res_expr = parse_quote!(#res_expr.put(#idx));
    }

    let take_pat: syn::Pat = if take_pat.elems.len() != 1 {
        parse_quote!(#take_pat)
    } else {
        let elem = take_pat.elems.pop().unwrap();
        parse_quote!(#elem)
    };
    let let_stmt: syn::Stmt = if call_operands.len() > 0 {
        parse_quote! {
            let (#take_pat, _rem) = take!((#take_types), data);
        }
    } else {
        parse_quote! {
            let _rem = data;
        }
    };

    let asyncness = &func.sig.asyncness;

    let func_gens = &func.sig.generics.params;
    func.sig.generics.where_clause.as_ref().map(|preds| {
        for pred in preds.predicates.iter() {
            where_preds.push(pred.clone());
        }
    });

    let aw: Option<proc_macro2::TokenStream> = asyncness.map(|_| parse_quote!(.await));

    let mut generics = take_index_generics;
    generics.extend(func_gens.clone().into_iter());

    let expanded = quote! {

        #func

        #vis #asyncness fn #interface_ident<_T, #generics>(entity: _T) -> #tmp_type
        #where_clause
        {
            let data = entity;
            #let_stmt
            let (#res_var_idents) = #func_ident(#call_operands)#aw;
            #res_expr
        }
    };
    let res = TokenStream::from(expanded);
    res
}
