use core::panic;
use std::collections::HashMap;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    fold::Fold, parse::Parse, parse_macro_input, parse_quote, punctuated::Punctuated,
    spanned::Spanned, visit_mut::VisitMut, ExprTuple, Ident, ItemFn, Signature, Stmt, Token, Type,
    TypeParamBound, TypeTuple,
};

struct TransofrmImplInput {
    ids: Vec<syn::Ident>,
    bounds: Vec<Punctuated<TypeParamBound, Token![+]>>,
}
impl syn::fold::Fold for TransofrmImplInput {
    fn fold_type(&mut self, i: syn::Type) -> syn::Type {
        let i = syn::fold::fold_type(self, i);
        match i {
            syn::Type::ImplTrait(impl_trait) => {
                self.ids.push(syn::Ident::new(
                    &std::format!("__ImplType_{}", self.ids.len()),
                    Span::mixed_site(),
                ));
                self.bounds.push(impl_trait.bounds);
                let tp = self.ids.last();
                parse_quote!(#tp)
            }
            v => syn::fold::fold_type(self, v),
        }
    }
}

fn transoform_impl_fnarg_to_generics(mut sig: Signature) -> Signature {
    let mut t = TransofrmImplInput {
        ids: Default::default(),
        bounds: Default::default(),
    };
    sig.inputs.iter_mut().for_each(|x| {
        *x = t.fold_fn_arg(x.clone());
    });
    for (bounds, id) in t.bounds.iter().zip(t.ids) {
        sig.generics
            .params
            .push(parse_quote!(#[allow(non_camel_case_types)] #id: #bounds));
    }
    sig
}

struct TypeTransition {
    id: syn::Ident,
    _colon: Token![:],
    inputs: syn::TypeTuple,
    _c: Option<Token![->]>,
    outputs: Option<syn::TypeTuple>,
}

struct TypeTransitions(Punctuated<TypeTransition, Token![,]>);
impl Parse for TypeTransitions {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self(Punctuated::parse_terminated(input)?))
    }
}

impl Parse for TypeTransition {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let id = input.parse()?;
        let _colon = input.parse()?;
        let inputs = input.parse()?;
        let (_c, outputs) = if input.peek(Token![->]) {
            (Some(input.parse()?), Some(input.parse()?))
        } else {
            (None, None)
        };
        Ok(TypeTransition {
            id,
            _colon,
            inputs,
            _c,
            outputs,
        })
    }
}
macro_rules! count {
    ($p:literal, $counter:tt) => {{
        let value = $counter;
        $counter += 1;
        &std::format!($p, value) as &str
    }};
}

fn expand_one_generic_for_inputs(
    func: &mut ItemFn,
    inputs: &TypeTuple,
    generic_id: &Ident,
) -> Ident {
    let mut idx_counter = 0;
    let iter_value_input = inputs
        .elems
        .iter()
        .filter(|x| !matches!(x, Type::Reference(_)));
    let after = None;
    let (take_bounds, after) = iter_value_input.rev().fold(
        (None, after),
        |(item, after): (Option<Type>, Option<Ident>), tp| {
            let after = after.or_else(|| {
                let id = syn::Ident::new(
                    &std::format!("__EXPAND_{generic_id}_TAKE_ID"),
                    generic_id.span(),
                );
                func.sig
                    .generics
                    .params
                    .push(parse_quote!(#[allow(non_camel_case_types)] #id));
                parse_quote!(#id)
            });
            let item = item.unwrap_or_else(|| parse_quote!(#after));
            let idx = syn::Ident::new(
                count!("__EXPAND_{generic_id}_TAKE_IDX_{}", idx_counter),
                Span::mixed_site(),
            );
            func.sig
                .generics
                .params
                .push(parse_quote!(const #idx: usize));
            (
                Some(parse_quote!(impl ComponentGet<#tp, #idx, Item=#tp, AfterTake=#item>)),
                after,
            )
        },
    );
    let take_bounds = take_bounds.unwrap_or_else(|| parse_quote!(TupleMerge));
    let after_take_type_id = &after.unwrap_or(generic_id.clone());
    func.sig
        .generics
        .make_where_clause()
        .predicates
        .push(parse_quote!(#after_take_type_id: TupleMerge));
    fn find_param_bounds<'a>(func: &'a mut ItemFn, k: &Ident) -> &'a mut syn::TypeParam {
        match func
            .sig
            .generics
            .params
            .iter_mut()
            .find(|x| match x {
                syn::GenericParam::Type(tp) => &tp.ident == k,
                _ => false,
            })
            .unwrap()
        {
            syn::GenericParam::Type(tp) => tp,
            _ => panic!(),
        }
    }
    let k_bounds = &mut find_param_bounds(func, generic_id).bounds;

    match take_bounds {
        Type::ImplTrait(impl_trait) => {
            let bounds = impl_trait.bounds;
            k_bounds.extend(bounds);
        }
        v => {
            k_bounds.push(parse_quote!(#v));
        }
    };
    after_take_type_id.clone()
}

fn generic_one_for_macro(
    func: &mut ItemFn,
    generic_id: &Ident,
    inputs: &TypeTuple,
    _outputs: &TypeTuple,
    after_take_type_id: &Ident,
) {
    let after_take_id = Ident::new("_after_take", Span::mixed_site());
    let stmts = vec![];
    let mut input_operands: Vec<Option<Ident>> = vec![];
    input_operands.resize(inputs.elems.len(), None);

    // takes
    let stmts = inputs
        .elems
        .iter()
        .enumerate()
        .filter(|(_idx, x)| !matches!(x, Type::Reference(_)))
        .fold(stmts, |mut stmts: Vec<syn::Stmt>, (idx, ty)| {
            let ident = Ident::new(&std::format!("_{}", idx), Span::mixed_site());
            input_operands[idx] = Some(ident.clone());
            match ty {
                Type::Reference(_) => panic!(),
                _ => {
                    stmts
                        .push(parse_quote!( let (#ident, #after_take_id) = #after_take_id.take();));
                }
            }
            stmts
        });
    // refs
    let (mut stmts, _) = inputs
                .elems
                .iter()
                .enumerate()
                .filter_map(|(_idx, x)| match x {
                    Type::Reference(ty) => Some((_idx, ty.elem.to_owned())),
                    _ => None,
                })
                .fold(
                    (stmts, None),
                    |(mut stmts, tmp): (Vec<syn::Stmt>, Option<syn::Expr>), (idx, _ty)| {
                        let tmp = tmp.unwrap_or_else(|| {
                            let ptr_ident: Ident = parse_quote!(after_take_ptr);
                            stmts.push(parse_quote!(let mut #after_take_id = #after_take_id;));
                            stmts.push(parse_quote!(let #ptr_ident = &mut #after_take_id as *mut #after_take_type_id;));
                            parse_quote!(#ptr_ident)
                        });
                        let ident = Ident::new(&std::format!("_{}", idx), Span::mixed_site());
                        input_operands[idx] = Some(ident.clone());
                        stmts.push(parse_quote!( let #ident:&mut #_ty = unsafe {(*#tmp).get_mut().0 as &mut #_ty};));
                        (stmts, Some(tmp))
                    },
                );
    let input_operands =
        input_operands
            .iter()
            .fold(parse_quote!(()), |mut input_operands: syn::ExprTuple, x| {
                let x = x.as_ref().unwrap();
                input_operands.elems.push(parse_quote!(#x));
                input_operands
            });
    stmts.push(Stmt::Expr(
        parse_quote!({let _res = _closure(#input_operands); (#after_take_id, _res)}),
        None,
    ));
    let mut block: syn::ExprBlock = parse_quote!({});
    block.block.stmts.extend(stmts);
    func.block.stmts.insert(
        0,
        parse_quote!(
            #[allow(unused)]
            macro_rules! #generic_id {
                ($expr:expr => $closure:expr) => {
                    {
                        let #after_take_id = $expr;
                        let _closure = $closure;
                        let (after_take, res) = #block;
                        after_take.merge(res)
                    }
                };
                ($expr:expr => $closure:expr, Option) => {
                    {
                        let #after_take_id = $expr;
                        let _closure = $closure;
                        let (after_take, res) = #block;
                        if let Some(res) = res {
                            Ok(after_take.merge(res))
                        } else {
                            Err(after_take)
                        }
                    }
                };
            }
        ),
    )
}

fn replace_outof_macro(func: &mut ItemFn, map: &HashMap<Ident, Type>) {
    struct ModifyOutOf<'a> {
        modified: bool,
        map: &'a HashMap<Ident, Type>,
    }
    impl<'a> syn::visit_mut::VisitMut for ModifyOutOf<'a> {
        fn visit_type_mut(&mut self, i: &mut Type) {
            syn::visit_mut::visit_type_mut(self, i);
            if let Type::Macro(mc) = i {
                let macro_ident = &mc.mac.path.segments.last().unwrap().ident;
                let tokens = &mc.mac.tokens;
                let id: Ident = parse_quote!(#tokens);
                if macro_ident == "outof" {
                    if let Some(tp) = self.map.get(&id) {
                        self.modified = true;
                        *i = tp.clone();
                    }
                }
            }
        }
    }
    let mut m = ModifyOutOf {
        modified: true,
        map,
    };
    // todo: check circle ref
    while m.modified {
        m.modified = false;
        m.visit_item_fn_mut(func);
    }
}

fn expand_transition_type_generics(
    func: &mut ItemFn,
    map: &HashMap<syn::Ident, (TypeTuple, Option<TypeTuple>)>,
) {
    let mut outof_map: HashMap<_, _> = Default::default();
    for (k, (inputs, outputs)) in map {
        let outputs = outputs.clone().unwrap_or_else(|| parse_quote!(()));
        let iter_ref_input = inputs.elems.iter().filter_map(|x| match x {
            Type::Reference(v) => Some(v),
            _ => None,
        });
        fn find_param_bounds<'a>(func: &'a mut ItemFn, k: &Ident) -> &'a mut syn::TypeParam {
            match func
                .sig
                .generics
                .params
                .iter_mut()
                .find(|x| match x {
                    syn::GenericParam::Type(tp) => &tp.ident == k,
                    _ => false,
                })
                .unwrap()
            {
                syn::GenericParam::Type(tp) => tp,
                _ => panic!(),
            }
        }
        let after_take_type_id = &expand_one_generic_for_inputs(func, inputs, k);
        // bounds for get as ref
        let mut idx_counter = 0;
        let appends = iter_ref_input
            .clone()
            .map(|x| x.elem.as_ref())
            .map(|tp| -> TypeParamBound {
                let idx = &syn::Ident::new(
                    count!("__EXPAND_{after_take_type_id}_REF_IDX_{}", idx_counter),
                    Span::mixed_site(),
                );
                func.sig
                    .generics
                    .params
                    .push(parse_quote!(const #idx: usize));
                parse_quote!(ComponentGet<#tp, #idx>)
            })
            .collect::<Vec<TypeParamBound>>();
        find_param_bounds(func, after_take_type_id)
            .bounds
            .extend(appends);

        // generate macro
        generic_one_for_macro(func, k, inputs, &outputs, after_take_type_id);

        outof_map.insert(
            k.clone(),
            parse_quote!(<#after_take_type_id as TupleMerge>::AfterMerge<(#outputs)>),
        );
    }
    replace_outof_macro(func, &outof_map);
}

#[proc_macro_attribute]
pub fn system_wrap(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let type_transitions = parse_macro_input!(_attr as TypeTransitions).0;
    let map = type_transitions
        .into_iter()
        .map(|x| (x.id, (x.inputs, x.outputs)))
        .collect::<HashMap<_, _>>();
    let mut func = parse_macro_input!(item as syn::ItemFn);
    func.sig = transoform_impl_fnarg_to_generics(func.sig);

    expand_transition_type_generics(&mut func, &map);
    let _interface_name = func.sig.ident.clone();
    let expanded = quote! {
        #func
    };
    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn system(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_copy = item.clone();
    let mut func = parse_macro_input!(item as syn::ItemFn);
    let input_generic_id = Ident::new("_WRAPPER_ID", Span::mixed_site());
    let mut input_tuple_type: TypeTuple = parse_quote!(());
    func.sig = transoform_impl_fnarg_to_generics(func.sig);
    input_tuple_type
        .elems
        .extend(func.sig.inputs.iter().filter_map(|x| match x {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(tp) => Some(tp.ty.as_ref().to_owned()),
        }));
    let input_types = input_tuple_type;
    let mut sig = func.sig.clone();
    sig.inputs.clear();
    sig.inputs.push(parse_quote!(_value: #input_generic_id));
    sig.output = parse_quote!(-> outof![#input_generic_id]);
    let func_name = &func.sig.ident;
    let generic_params = &sig.generics.params;
    let vis = func.vis.clone();
    let where_clause = &sig.generics.where_clause;
    let mut tup_operands: ExprTuple = parse_quote!(());
    let (output_type, is_tuple): (TypeTuple, bool) = match &func.sig.output {
        syn::ReturnType::Default => (parse_quote!(()), true),
        syn::ReturnType::Type(_, tp) => match tp.as_ref() {
            Type::Tuple(tp) => (tp.clone(), true),
            tp => (parse_quote!((#tp,)), false),
        },
    };
    for i in 0..func.sig.inputs.len() {
        let nm = Ident::new(&std::format!("_{}", i), func.sig.inputs[i].span());
        tup_operands.elems.push(parse_quote!(#nm));
    }
    let ret_stmt: Stmt = if is_tuple {
        parse_quote!(return _res;)
    } else {
        parse_quote!(return (_res,);)
    };
    let operands = &tup_operands.elems;
    let item = system_wrap(_attr.clone(), item_copy);
    let func = parse_macro_input!(item as syn::ItemFn);
    let new_func = quote!(
        #vis fn #func_name <#input_generic_id, #generic_params>(_args: #input_generic_id) -> outof![#input_generic_id] #where_clause {
            #func
            #input_generic_id!(
                _args => | (#tup_operands) | {
                    let _res = #func_name(#operands);
                    #ret_stmt
                }
            )
        }
    );
    let append_attr = quote!(#input_generic_id: #input_types -> #output_type,);
    let mut attr = TokenStream::from(append_attr);
    attr.extend(_attr);
    system_wrap(attr, TokenStream::from(new_func))
}
