use crate::apply_tuple;
use crate::ComponentGet;
use crate::TupleMerge;
use std::{future::Future, marker::PhantomData};

use procs::system_wrap;

use crate::TupleExtend;

pub trait IntoTupleProcessor<IType, OType, _M> {
    type IType;
    type OType;
    type TupleProcessorType;
    fn into_tuple_processor(self) -> Self::TupleProcessorType;
}

#[macro_export]
macro_rules! cascade {
    ($val:expr => $expr:expr) => {{
        use $crate::IntoTupleProcessor;
        $expr.into_tuple_processor().cascade($val)
    }};
}

#[macro_export]
macro_rules! cascade_fn {
    (once $expr:expr) => {
        {
            let __e = $expr;
            move |x| { $crate::cascade!(x => __e) }
        }
    };
    ($expr:expr) => {
        {
            let mut __e = $expr;
            move |x| { $crate::cascade!(x => &mut __e) }
        }
    };
}

#[macro_export]
macro_rules! inspect {
    ($val:expr => $expr:expr) => {{
        use $crate::IntoTupleProcessor;
        use $crate::TupleRefRef;
        $expr.into_tuple_processor().apply($val)
    }};
}

#[macro_export]
macro_rules! inspect_fn {
    (once $expr:expr) => {{
        let __e = $expr;
        move |x| { $crate::inspect!(x => __e) }
    }};
    ($expr:expr) => {{
        let mut __e = $expr;
        fn do_ref_tuple<'a, T: $crate::TupleRefRef<'a>>(a: T) -> T::RefType {
            a._ref_tuple()
        }
        move |x| { $crate::inspect!(do_ref_tuple(x) => &__e) }
    }};
}

#[macro_export]
macro_rules! mutate_fn {
    (once $expr:expr) => {
        let __e = $expr;
        |x| { $crate::tuple_proc::mutate!(x => __e) }
    };
    ($expr:expr) => {
        let mut __e = $expr;
        |x| { $crate::tuple_proc::mutate!(x => &mut __e) }
    };
}

#[macro_export]
macro_rules! mutate {
    ($val:expr => $expr:expr) => {{
        use $crate::entity::TupleRefRef;
        use $crate::IntoTupleProcessor;
        $expr.into_tuple_processor().apply($val.mut_tuple())
    }};
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
            cascade!((0i8, 1i32, 2i16) => test_option)
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
            cascade!((666u32, 233i32, "string".to_string()) => test_result)
        );
        assert_eq!(
            Ok((666i32, "string".to_string())),
            cascade!((233u32, 666i32, "string".to_string()) => test_result)
        );
    }
    #[test]
    async fn test_fut() {
        // cascade
        async fn test_cascade(a: i32) -> (i32,) {
            (a + 1,)
        }
        assert_eq!((234i32,), cascade!((233i32,) => test_cascade).await);
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
            cascade!((0i8, 1i32, 2i16) => test_option).await
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
            cascade!((666u32, 233i32, "string".to_string()) => test_result).await
        );
        assert_eq!(
            Ok((666i32, "string".to_string())),
            cascade!((233u32, 666i32, "string".to_string()) => test_result).await
        );
    }
}

pub struct TupleProcessFn<F, M>(F, PhantomData<M>);

pub trait CascadeInto<const I: usize, const J: usize> {
    type Output<U>
    where
        U: TupleMerge;
    fn cascade_merge<U>(self, u: U) -> Self::Output<U>
    where
        U: TupleMerge;
}

impl<T> CascadeInto<0, 0> for T
where
    T: TupleExtend,
{
    type Output<U> = <U as TupleMerge>::AfterMerge<T> where U: TupleMerge;
    fn cascade_merge<U>(self, u: U) -> Self::Output<U>
    where
        U: TupleMerge,
    {
        u.merge(self)
    }
}

impl<T> CascadeInto<0, 1> for Option<T>
where
    T: TupleExtend,
{
    type Output<U> = Option<<U as TupleMerge>::AfterMerge<T>> where U: TupleMerge;
    fn cascade_merge<U>(self, u: U) -> Self::Output<U>
    where
        U: TupleMerge,
    {
        self.map(|s| u.merge(s))
    }
}

impl<T, E> CascadeInto<0, 2> for Result<T, E>
where
    T: TupleExtend,
{
    type Output<U> = Result<<U as TupleMerge>::AfterMerge<T>, E> where U: TupleMerge;
    fn cascade_merge<U>(self, u: U) -> Self::Output<U>
    where
        U: TupleMerge,
    {
        self.map(|s| u.merge(s))
    }
}
#[pin_project::pin_project]
pub struct CascadeFut<Fut, U, const I: usize> {
    #[pin]
    fut: Fut,
    u: Option<U>,
}

impl<Fut, U> Future for CascadeFut<Fut, U, 0>
where
    Fut: Future,
    Fut::Output: TupleExtend,
    U: TupleMerge,
{
    type Output = <U as TupleMerge>::AfterMerge<Fut::Output>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.project();
        let r: Fut::Output = futures::ready!(this.fut.poll(cx));
        std::task::Poll::Ready(this.u.take().unwrap().merge(r))
    }
}
impl<Fut, U, Res> Future for CascadeFut<Fut, U, 1>
where
    Fut: Future<Output = Option<Res>>,
    Res: TupleExtend,
    U: TupleMerge,
{
    type Output = Option<<U as TupleMerge>::AfterMerge<Res>>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.project();
        let r: Fut::Output = futures::ready!(this.fut.poll(cx));
        std::task::Poll::Ready(r.map(|r| this.u.take().unwrap().merge(r)))
    }
}
impl<Fut, U, Res, E> Future for CascadeFut<Fut, U, 2>
where
    Fut: Future<Output = Result<Res, E>>,
    Res: TupleExtend,
    U: TupleMerge,
{
    type Output = Result<<U as TupleMerge>::AfterMerge<Res>, E>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.project();
        let r: Fut::Output = futures::ready!(this.fut.poll(cx));
        std::task::Poll::Ready(r.map(|r| this.u.take().unwrap().merge(r)))
    }
}

impl<F, T, const J: usize> CascadeInto<3, J> for F
where
    F: Future<Output = T>,
    T: CascadeInto<0, J>,
{
    type Output<U> = CascadeFut<F, U, J> where U: TupleMerge;
    fn cascade_merge<U>(self, u: U) -> Self::Output<U>
    where
        U: TupleMerge,
    {
        CascadeFut {
            fut: self,
            u: Some(u),
        }
    }
}

macro_rules! impl_tuple_proc {
    (($($ids:ident),*$(,)?)) => {
        #[allow(non_snake_case)]
        impl<F, OType, $($ids,)*> IntoTupleProcessor<($($ids,)*), OType, OType> for F
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
            pub fn cascade<_E, const I: usize, const J: usize>(self, e: _E) ->
            <OType as CascadeInto<I, J>>::Output<outof![_E]>
            where
                outof![_E]: TupleMerge,
                OType: CascadeInto<I, J>,
            {
                let (r, b) = _E!(e => |($($ids,)*)| self.0($($ids,)*) => NoMerge);
                r.cascade_merge(b.merge(()))
            }
            #[system_wrap(
                _E: ($($ids,)*)
            )]
            pub fn apply<_E>(self, e: _E) -> OType {
                let (r, _) = _E!(e => |($($ids,)*)| self.0($($ids,)*) => NoMerge);
                r
            }
        }
        };
}
apply_tuple!(
    impl_tuple_proc,
    (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
);
