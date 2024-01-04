pub trait TupleMerge {
    type AfterMerge<U>
    where
        U: TupleExtend;
    fn merge<T>(self, v: T) -> Self::AfterMerge<T>
    where
        T: TupleExtend;
}
pub trait TupleExtend {
    type AfterExtend<U>: TupleExtend;
    fn extend<T>(self, v: T) -> Self::AfterExtend<T>;
}

#[macro_export]
macro_rules! apply_tuple {
    ($macro:path, ($first:ident $(,$ids:ident)*)) => {
        $macro!(($first, $($ids),*));
        apply_tuple!($macro, ($($ids),*));
    };
    ($macro:path, ()) => {
        $macro!(());
    }
}

#[macro_export]
macro_rules! impl_extend_merge {
    (($($ids:ident),* $(,)?)) => {
        #[allow(non_snake_case)]
        impl<$($ids,)*> $crate::TupleExtend for ($($ids,)*) {
            type AfterExtend<U> = ($($ids,)* U,);
            fn extend<U>(self, v: U) -> Self::AfterExtend<U> {
                let ($($ids,)*) = self;
                ($($ids,)* v,)
            }
        }
        #[allow(non_snake_case)]
        impl<$($ids,)*> $crate::TupleMerge for ($($ids,)*) {
            type AfterMerge<U> = impl_extend_merge!(expand_trait_type, U, ($($ids),*)) where U: TupleExtend;
            fn merge<T>(self, v: T) -> Self::AfterMerge<T>
            where
                T: TupleExtend,
            {
                let ($($ids,)*) = self;
                v$(.extend($ids))*
            }
        }
    };
    (expand_trait_type, $tp:ty, ($first:ident $(,$ids:ident)*)) => {
        impl_extend_merge!(expand_trait_type, <$tp as TupleExtend>::AfterExtend<$first>, ($($ids),*))
    };
    (expand_trait_type, $tp:ty, ()) => {
        $tp
    };
}

pub struct TupleExtendOverflow;
impl TupleExtend for TupleExtendOverflow {
    type AfterExtend<U> = Self;

    fn extend<T>(self, _: T) -> Self::AfterExtend<T> {
        self
    }
}

apply_tuple!(
    impl_extend_merge,
    (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
);

#[rustfmt::skip]
impl<T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16> TupleExtend
    for (T, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
{
    type AfterExtend<U> = TupleExtendOverflow;
    fn extend<U>(self, _: U) -> Self::AfterExtend<U> {
        panic!("Too many components in an entity!")
    }
}
