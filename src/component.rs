pub trait ComponentGet<T, const I: usize> {
    type Item;
    type AfterTake;
    type AfterGetMut<'a>
    where
        Self: 'a;
    fn get(&self) -> &T;
    fn get_mut(&mut self) -> (&mut T, Self::AfterGetMut<'_>);
    fn take(self) -> (T, Self::AfterTake);
}

pub trait ComponentPut<T> {
    type Item;
    type AfterPut;
    fn put(self, value: Self::Item) -> Self::AfterPut;
}

#[macro_export]
macro_rules! declare_component {
    {$($(#[$m:meta])? $vis:vis $name:ident  (impl $($tts:tt)+);)*} => {
        $($(#[$m])? $vis struct $name<_OBJ>(pub _OBJ) where _OBJ: $($tts)+;)*
    };
}

#[macro_export]
macro_rules! take {
    (($first_type:ty, $second_type:ty), $exp:expr) => {
        {
        let (first, rem) = take!($first_type, $exp);
        let (second, rem) = take!($second_type, rem);
        ((first, second), rem)
        }
    };
    (($first_type:ty, $($type:ty),*), $exp:expr) => {
        {
        let (res, rem) = take!(($($type),*), $exp);
        let (first, rem) = take!($first_type, rem);
        (res.put(first), rem)
        }
    };
    ($type:ty, $exp:expr) => {{
        let (_data, _rem): ($type, _) = $exp.take();
        (_data, _rem)
    }};
    (($type:ty), $exp:expr) => {{
        take!($type, $exp)
    }};
    ($exp:expr, $type:ty) => {
        take!($type, $exp)
    };
}

#[macro_export]
macro_rules! put {
    ($obj:expr,) => {
        $obj
    };
    ($obj:expr) => {
        $obj
    };
    ($obj:expr, $first:expr) => {
        $obj.put($first)
    };
    ($obj:expr, $first:expr, $($others:expr),*) => {
        put!($obj.put($first), $($others),*)
    };
}

#[cfg(test)]
mod test {
    #[test]
    fn test_put_take() {
        use super::ComponentGet;
        use super::ComponentPut;

        let data = put!((), 666u32, 233i32, "string", 789f64);
        let data = take!((u32, i32, &str, f64), data).0;

        let data_i32 = take!(i32, data.clone()).0;
        let data_u32 = take!(data.clone(), u32).0;
        let data_str = take!(&str, data.clone()).0;
        let data_f64 = take!(f64, data.clone()).0;

        assert_eq!(
            (data_i32, (data_u32, data_str, data_f64)),
            take!(i32, data.clone())
        );
        assert_eq!(
            ((data_i32, data_f64), (data_u32, data_str)),
            take!((i32, f64), data.clone())
        );
        assert_eq!(
            ((data_i32, data_f64, data_str), (data_u32,)),
            take!((i32, f64, &str), data.clone())
        );
        assert_eq!(
            ((data_i32, data_f64, data_str, data_u32), ()),
            take!((i32, f64, &str, u32), data.clone())
        );
    }
}

macro_rules! impl_gets_puts {
    ([$first:ident, $($rems:ident),*]) => {
        impl_gets_puts!(0, /*tps*/[$first $($rems)*], /*prefix*/[], /*item*/$first, /*suffix*/[$($rems)*]);
        impl_gets_puts!([$($rems),*]);
        #[allow(non_snake_case)]
        impl<T, $first, $($rems),*> ComponentPut<T> for ($first, $($rems),*) {
            type Item = T;
            type AfterPut = (Self::Item, $first, $($rems),*);
            fn put(self, value: Self::Item) -> Self::AfterPut {
                let ($first, $($rems),*) = self;
                (value, $first, $($rems),*)
            }
        }
    };
    ([$first:ident]) => {
        #[allow(non_snake_case)]
        #[allow(unused_parens)]
        #[allow(unused_variables)]
        impl<$first> ComponentGet<$first, 0> for ($first,) {
            type Item = $first;
            type AfterTake = ();
            type AfterGetMut<'a> = () where Self: 'a;
            fn get(&self) -> &Self::Item {
                &self.0
            }
            fn get_mut(&mut self) -> (&mut Self::Item, Self::AfterGetMut<'_>) {
                (&mut self.0, ())
            }
            fn take(self) -> (Self::Item, Self::AfterTake) {
                (self.0, ())
            }
        }
        impl <T> ComponentPut<T> for () {
            type Item = T;
            type AfterPut = (Self::Item,);
            fn put(self, value: Self::Item) -> Self::AfterPut {
                (value,)
            }
        }
        impl <T, A> ComponentPut<T> for (A,) {
            type Item = T;
            type AfterPut = (Self::Item, A);
            fn put(self, value: Self::Item) -> Self::AfterPut {
                (value, self.0)
            }
        }
    };

    ($idx:expr, /*tps*/[$($tps:ident)*], /*prefix*/[], /*item*/$item:ident, /*suffix*/[$first:ident $($suffixs:ident)*]) => {
        #[allow(non_snake_case)]
        #[allow(unused_parens)]
        #[allow(unused_variables)]
        impl<$($tps),*> ComponentGet<$item, {$idx}> for ($($tps),*) /*where $($tps: 'static),* */{
            type Item = $item;
            type AfterTake = ($first, $($suffixs),*);
            type AfterGetMut<'a> = (&'a mut $first, $(&'a mut $suffixs),*) where Self: 'a;
            fn get(&self) -> &Self::Item {
                let ($($tps),*) = &self;
                $item
            }
            fn get_mut(&mut self) -> (&mut Self::Item, Self::AfterGetMut<'_>) {
                let ($($tps),*) = self;
                ($item, ($first, $($suffixs),*))
            }
            fn take(self) -> (Self::Item, Self::AfterTake) {
                let ($($tps),*) = self;
                ($item, ($first, $($suffixs),*))
            }
        }
        impl_gets_puts!($idx + 1, [$($tps)*], [$item], $first, [$($suffixs)*]);
    };
    ($idx:expr, /*tps*/[$($tps:ident)*], /*prefix*/[$($prefixs:ident)*], /*item*/$item:ident, /*suffix*/[]) => {
        #[allow(non_snake_case)]
        #[allow(unused_parens)]
        #[allow(unused_variables)]
        impl<$($tps),*> ComponentGet<$item, {$idx}> for ($($tps),*) {
            type Item = $item;
            type AfterTake = ($($prefixs),*,);
            type AfterGetMut<'a> = ($(&'a mut $prefixs),*,) where Self: 'a;
            fn get(&self) -> &Self::Item {
                let ($($tps),*) = &self;
                $item
            }
            fn get_mut(&mut self) -> (&mut Self::Item, Self::AfterGetMut<'_>) {
                let ($($tps),*) = self;
                ($item, ($($prefixs),*,))
            }
            fn take(self) -> (Self::Item, Self::AfterTake) {
                let ($($tps),*) = self;
                ($item, ($($prefixs),*,))
            }
        }
    };
    ($idx:expr, /*tps*/[$($tps:ident)*], /*prefix*/[$($prefixs:ident)*], /*item*/$item:ident, /*suffix*/[$first:ident $($suffixs:ident)*]) => {
        #[allow(non_snake_case)]
        #[allow(unused_parens)]
        #[allow(unused_variables)]
        impl<$($tps),*> ComponentGet<$item, {$idx}> for ($($tps),*) {
            type Item = $item;
            type AfterTake = ($($prefixs),*, $first, $($suffixs),*);
            type AfterGetMut<'a> = ($(&'a mut $prefixs),*, &'a mut $first, $(&'a mut $suffixs),*) where Self: 'a;
            fn get(&self) -> &Self::Item {
                let ($($tps),*) = &self;
                $item
            }
            fn get_mut(&mut self) -> (&mut Self::Item, Self::AfterGetMut<'_>) {
                let ($($tps),*) = self;
                ($item, ($($prefixs),*,$first,$($suffixs),*))
            }
            fn take(self) -> (Self::Item, Self::AfterTake) {
                let ($($tps),*) = self;
                ($item, ($($prefixs),*,$first,$($suffixs),*))
            }
        }
        impl_gets_puts!($idx + 1, [$($tps)*], [$($prefixs)* $item], $first, [$($suffixs)*]);
    };
}

impl_gets_puts!([A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]);
