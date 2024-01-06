extern crate procs;
pub use procs::system;

#[macro_export]
macro_rules! chain {
    [] => {
        |x| {x}
    };
    [$first:expr] => {
        |x| {$first(x)}
    };
    [$first:expr, $($exprs:expr),* $(,)?] => {
        |x| {(chain![$($exprs),*])($first(x))}
    };
}

#[cfg(test)]
mod test {
    use crate::{declare_component, prelude::*};

    #[test]
    fn test_one() {
        #[system]
        fn test_system_gen() -> i32 {
            233
        }

        #[system]
        fn test_system_take(_: i32) {}

        // 0,1
        let e = test_system_gen(());
        assert_eq!(&(233,), &e);
        assert_eq!((), test_system_take(e));
        // 1,1
        let e = test_system_gen((666u32,));
        assert_eq!(&233, e.get() as &i32);
        assert_eq!((666u32,), test_system_take(e));
        // 2,1
        let e = test_system_gen((666u32, 789f32));
        assert_eq!(&233, e.get() as &i32);
        assert_eq!((666u32, 789f32), test_system_take(e));
    }
    #[test]
    fn test_two() {
        #[system]
        fn test_system_gen() -> (i32, u32) {
            (233, 666)
        }
        #[system]
        fn test_system_take(_: u32, _: i32) {}

        // 0,2
        let e = test_system_gen(());
        assert_eq!(&233, e.get() as &i32);
        assert_eq!(&666, e.get() as &u32);
        assert_eq!((), test_system_take(e));
        // 1,2
        let e = test_system_gen((789f32,));
        assert_eq!(&233, e.get() as &i32);
        assert_eq!(&666, e.get() as &u32);
        assert_eq!((789f32,), test_system_take(e));
        // 2,2
        let e = test_system_gen((789f32, 987f64));
        assert_eq!(&233, e.get() as &i32);
        assert_eq!(&666, e.get() as &u32);
        assert_eq!((789f32, 987f64), test_system_take(e));
    }

    #[test]
    fn test_chain() {
        #[system]
        fn f0() -> i32 {
            233
        }
        #[system]
        fn f1() -> u32 {
            666
        }
        #[system]
        fn f2(v: &mut i32) -> String {
            let res = v.to_string();
            *v += 1;
            res
        }
        let e = chain![f0, f1, f2](());
        let (res, ()) = take!((i32, u32, String), e);
        assert_eq!(res, (234, 666, "233".into()));
    }

    #[test]
    fn test_generic() {
        declare_component! {
            CompA(impl TraitA + Sized);
            CompB(impl TraitB + Sized);
        }
        trait TraitA {
            fn trait_func_a(&self) -> i32;
        }
        trait TraitB {
            fn trait_func_b(&self) -> f32;
        }
        struct AA;
        struct BB;
        impl TraitA for AA {
            fn trait_func_a(&self) -> i32 {
                233
            }
        }
        impl TraitA for BB {
            fn trait_func_a(&self) -> i32 {
                666
            }
        }
        impl TraitB for AA {
            fn trait_func_b(&self) -> f32 {
                123f32
            }
        }
        impl TraitB for BB {
            fn trait_func_b(&self) -> f32 {
                456f32
            }
        }

        #[system]
        fn fa<T: TraitA>(CompA(v): CompA<T>) -> i32 {
            v.trait_func_a()
        }

        #[system]
        fn fb<A: TraitA, B: TraitB>(CompB(b): &CompB<B>, CompA(a): &CompA<A>) -> String {
            let a = a.trait_func_a();
            let b = b.trait_func_b();
            std::format!("a={a}, b={b}")
        }
        assert_eq!(&233, fa((CompA(AA), CompB(AA))).get() as &i32);
        assert_eq!(&666, fa((CompB(BB), CompA(BB))).get() as &i32);

        assert_eq!("a=233, b=123", fb((CompA(AA), CompB(AA))).get() as &String);
        assert_eq!("a=233, b=123", fb((CompB(AA), CompA(AA))).get() as &String);
        assert_eq!("a=233, b=456", fb((CompA(AA), CompB(BB))).get() as &String);
        assert_eq!("a=666, b=123", fb((CompB(AA), CompA(BB))).get() as &String);
    }

    #[test]
    fn test_impl_trait_in_return() {
        use std::fmt::Display;
        #[system]
        fn s0(
            a: Box<impl Display>,
            b: Option<impl Display>,
        ) -> (Box<impl Display>, Option<impl Display>) {
            let a: i32 = str::parse(&a.to_string()).unwrap();
            let b: i32 = str::parse(&b.unwrap().to_string()).unwrap();
            (Box::new(a + b), Some(a - b))
        }
        fn assert(a: &Box<impl Display>, b: &Option<impl Display>) {
            assert_eq!(233 + 666, str::parse(&a.to_string()).unwrap());
            assert_eq!(
                666 - 233,
                str::parse(&b.as_ref().unwrap().to_string()).unwrap()
            );
        }
        let res = s0((Some(233), Box::new(666)));
        assert(res.get(), res.get());
    }
}
