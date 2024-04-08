use crate::apply_tuple;
use crate::ComponentGet;
use crate::TupleMerge;
use futures::future::FutureExt;
use std::{future::Future, marker::PhantomData};

use procs::system_wrap;

use crate::TupleExtend;

pub trait IntoTupleProcessor<IType, OType> {
    type IType;
    type OType;
    type TupleProcessorType;
    fn into_tuple_processor(self) -> Self::TupleProcessorType;
}

#[macro_export]
macro_rules! cascade {
    ($val:expr => $expr:expr) => {
        $expr.into_tuple_processor().cascade($val)
    };
}

#[macro_export]
macro_rules! cascade_fn {
    ($expr:expr) => {
        |x| cascade!(x => $expr)
    };
}

#[macro_export]
macro_rules! cascade_option {
    ($val:expr => $expr:expr) => {
        $expr.into_tuple_processor().cascade_option($val)
    };
}

#[macro_export]
macro_rules! cascade_option_fn {
    ($expr:expr) => {
        move |x| cascade_option!(x => $expr)
    };
}

#[macro_export]
macro_rules! cascade_result {
    ($val:expr => $expr:expr) => {
        $expr.into_tuple_processor().cascade_result($val)
    };
}

#[macro_export]
macro_rules! cascade_result_fn {
    ($expr:expr) => {
        move |x| cascade_result!(x => $expr)
    };
}

#[macro_export]
macro_rules! cascade_async {
    ($val:expr => $expr:expr) => {
        $expr.into_tuple_processor().cascade_fut($val)
    };
}

#[macro_export]
macro_rules! cascade_async_fn {
    ($expr:expr) => {
        move |x| cascade_async!(x => $expr)
    };
}

#[macro_export]
macro_rules! cascade_obj_async {
    (($obj:expr, $val:expr) => $expr:expr) => {
        $expr.into_tuple_processor().cascade_obj_fut($obj, $val)
    };
}

#[macro_export]
macro_rules! cascade_obj_async_fn {
    ($expr:expr) => {
        move |obj, x| cascade_obj_async!((obj, x) => $expr)
    };
}

#[macro_export]
macro_rules! cascade_option_async {
    ($val:expr => $expr:expr) => {
        $expr.into_tuple_processor().cascade_option_fut($val)
    };
}

#[macro_export]
macro_rules! cascade_option_async_fn {
    ($expr:expr) => {
        move |x| cascade_option_async!(x => $expr)
    };
}

#[macro_export]
macro_rules! cascade_result_async {
    ($val:expr => $expr:expr) => {
        $expr.into_tuple_processor().cascade_result_fut($val)
    };
}

#[macro_export]
macro_rules! cascade_result_async_fn {
    ($expr:expr) => {
        move |x| cascade_result_async!(x => $expr)
    };
}

#[cfg(test)]
mod tests {
    use crate::*;
    use tokio::test;
    #[test]
    async fn test_basics() {
        // cascade
        fn test_cascade(a: i32) -> (i32,) {
            (a + 1,)
        }
        assert_eq!((234i32,), cascade!((233i32,) => test_cascade));
        // option
        fn test_option(a: i32) -> Option<(u32,)> {
            if a > 0 {
                Some((a as u32,))
            } else {
                None
            }
        }
        assert_eq!(
            Some((1u32, 0i8, 2i16)),
            cascade_option!((0i8, 1i32, 2i16) => test_option)
        );
        // result
        fn test_result(a: i32, b: u32) -> Result<(i32,), u32> {
            if a as u32 > b {
                Ok((a,))
            } else {
                Err(b)
            }
        }
        assert_eq!(
            Err(666u32),
            cascade_result!((666u32, 233i32, "string".to_string()) => test_result)
        );
        assert_eq!(
            Ok((666i32, "string".to_string())),
            cascade_result!((233u32, 666i32, "string".to_string()) => test_result)
        );
    }
    #[test]
    async fn test_fut() {
        // cascade
        async fn test_cascade(a: i32) -> (i32,) {
            (a + 1,)
        }
        assert_eq!((234i32,), cascade_async!((233i32,) => test_cascade).await);
        // option
        async fn test_option(a: i32) -> Option<(u32,)> {
            if a > 0 {
                Some((a as u32,))
            } else {
                None
            }
        }
        assert_eq!(
            Some((1u32, 0i8, 2i16)),
            cascade_option_async!((0i8, 1i32, 2i16) => test_option).await
        );
        // result
        async fn test_result(a: i32, b: u32) -> Result<(i32,), u32> {
            if a as u32 > b {
                Ok((a,))
            } else {
                Err(b)
            }
        }
        assert_eq!(
            Err(666u32),
            cascade_result_async!((666u32, 233i32, "string".to_string()) => test_result).await
        );
        assert_eq!(
            Ok((666i32, "string".to_string())),
            cascade_result_async!((233u32, 666i32, "string".to_string()) => test_result).await
        );
    }
}

pub struct TupleProcessFn<F, M>(F, PhantomData<M>);

macro_rules! impl_tuple_proc {
    (($($ids:ident),*$(,)?)) => {
        #[allow(non_snake_case)]
        impl<F, OType, $($ids,)*> IntoTupleProcessor<($($ids,)*), OType> for F
        where
            F: FnOnce($($ids,)*) -> OType,
        {
            type IType = ($($ids,)*);

            type OType = OType;
            type TupleProcessorType = TupleProcessFn<F, (($($ids,)*), OType)>;

            fn into_tuple_processor(self) -> Self::TupleProcessorType {
                TupleProcessFn(self, Default::default())
            }
        }
        #[allow(non_snake_case)]
        impl<_F, OType, $($ids,)*> TupleProcessFn<_F, (($($ids,)*), OType)>
        where
            _F: FnOnce($($ids,)*) -> OType,
            OType: TupleExtend,
        {
            #[system_wrap(
                _E: ($($ids,)*)
            )]
            pub fn operate<_E>(self, e: _E) -> (OType, outof![_E]) {
                let (r, b) = _E!(e => |($($ids,)*)| self.0($($ids,)*) => NoMerge);
                (r, b.merge(()))
            }
            #[system_wrap(
                _E: ($($ids,)*)
            )]
            pub fn cascade<_E>(self, e: _E) -> <outof![_E] as TupleMerge>::AfterMerge<OType>
            where
                outof![_E]: TupleMerge,
            {
                let (r, b) = _E!(e => |($($ids,)*)| self.0($($ids,)*) => NoMerge);
                b.merge(()).merge(r)
            }
        }
        #[allow(non_snake_case)]
        impl<F, OType, $($ids,)*> TupleProcessFn<F, (($($ids,)*), Option<OType>)>
        where
            F: FnOnce($($ids,)*) -> Option<OType>,
        {
            #[system_wrap(
                E: ($($ids,)*)
            )]
            pub fn cascade_option<E>(self, e: E) -> Option<<outof![E] as TupleMerge>::AfterMerge<OType>>
            where
                outof![E]: TupleMerge,
                OType: TupleExtend,
            {
                let (r, b) = E!(e => |($($ids,)*)| self.0($($ids,)*) => NoMerge);
                r.map(|r| b.merge(()).merge(r))
            }
        }
        #[allow(non_snake_case)]
        impl<F, ErrType, OType, $($ids,)*> TupleProcessFn<F, (($($ids,)*), Result<OType, ErrType>)>
        where
            F: FnOnce($($ids,)*) -> Result<OType, ErrType>,
        {
            #[system_wrap(
                E: ($($ids,)*)
            )]
            pub fn cascade_result<E>(
                self,
                e: E,
            ) -> Result<<outof![E] as TupleMerge>::AfterMerge<OType>, ErrType>
            where
                outof![E]: TupleMerge,
                OType: TupleExtend,
            {
                let (r, b) = E!(e => |($($ids,)*)| self.0($($ids,)*) => NoMerge);
                r.map(|r| b.merge(()).merge(r))
            }
        }
        #[allow(non_snake_case)]
        impl<F, OType, Fut, $($ids,)*> TupleProcessFn<F, (($($ids,)*), Fut)>
        where
            F: FnOnce($($ids,)*) -> Fut,
            Fut: Future<Output = OType>,
            OType: TupleExtend,
        {
            #[system_wrap(
                E: ($($ids,)*)
            )]
            pub async fn operate_fut<E>(self, e: E) -> (OType, outof![E]) {
                let (r, b) = E!(e => |($($ids,)*)| self.0($($ids,)*) => NoMerge);
                (r.await, b.merge(()))
            }
            #[system_wrap(
                E: ($($ids,)*)
            )]
            pub async fn cascade_fut<E>(self, e: E) -> <outof![E] as TupleMerge>::AfterMerge<OType>
            where
                outof![E]: TupleMerge,
            {
                let (r, b) = E!(e => |($($ids,)*)| self.0($($ids,)*) => NoMerge);
                b.merge(()).merge(r.await)
            }
        }
        #[allow(non_snake_case)]
        impl<F, OType, Fut, $($ids,)*> TupleProcessFn<F, (($($ids,)*), Fut)>
        where
            F: FnOnce($($ids,)*) -> Fut,
            Fut: Future<Output = Option<OType>>,
        {
            #[system_wrap(
                E: ($($ids,)*)
            )]
            pub fn cascade_option_fut<E>(
                self,
                e: E,
            ) -> impl Future<Output=Option<<outof![E] as TupleMerge>::AfterMerge<OType>>>
            where
                outof![E]: TupleMerge,
                OType: TupleExtend,
            {
                let (r, b) = E!(e => |($($ids,)*)| self.0($($ids,)*) => NoMerge);
                // r.await.map(|r| b.merge(()).merge(r))
                r.map(|x| x.map(|r| b.merge(()).merge(r)))
            }
        }
        #[allow(non_snake_case)]
        impl<F, ErrType, OType, Fut, $($ids,)*> TupleProcessFn<F, (($($ids,)*), Fut)>
        where
            F: FnOnce($($ids,)*) -> Fut,
            Fut: Future<Output = Result<OType, ErrType>>,
        {
            #[system_wrap(
                E: ($($ids,)*)
            )]
            pub async fn cascade_result_fut<E>(
                self,
                e: E,
            ) -> Result<<outof![E] as TupleMerge>::AfterMerge<OType>, ErrType>
            where
                outof![E]: TupleMerge,
                OType: TupleExtend,
            {
                let (r, b) = E!(e => |($($ids,)*)| self.0($($ids,)*) => NoMerge);
                r.await.map(|r| b.merge(()).merge(r))
            }
        }
        #[allow(non_snake_case)]
        impl<Obj, F, OType, Fut, $($ids,)*> TupleProcessFn<F, ((Obj, ($($ids,)*)), Fut)>
        where
            F: FnOnce(Obj, ($($ids,)*)) -> Fut,
            Fut: Future<Output = (Obj, Option<OType>)>,
        {
            #[system_wrap(
                E: ($($ids,)*)
            )]
            pub async fn cascade_obj_fut<E>(
                self,
                obj: Obj,
                e: E,
            ) -> (Obj, Option<<outof![E] as TupleMerge>::AfterMerge<OType>>)
            where
                outof![E]: TupleMerge,
                OType: TupleExtend,
            {
                let (r, b) = E!(e => |($($ids,)*)| self.0(obj, ($($ids,)*)) => NoMerge);
                let (obj, r) = r.await;
                (obj, r.map(|r| b.merge(()).merge(r)))
            }
        }
        };
}
apply_tuple!(
    impl_tuple_proc,
    (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
);
