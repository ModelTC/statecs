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
    [$first:expr, $($exprs:expr),*] => {
        |x| {(chain![$($exprs),*])($first(x))}
    };
}

#[cfg(test)]
mod test {
    use crate::prelude::*;

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
    fn test_async() {
        #[system]
        async fn test_system_gen() -> (i32, u32) {
            (233, 666)
        }
        #[system]
        async fn test_system_take(a: u32, b: i32) -> String {
            std::format!("({a}, {b})")
        }
        async fn _test() {
            let res = test_system_take(test_system_gen(()).await).await;
            let _: (String,) = res;
        }
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
        struct StructA<T>(T)
        where
            T: TraitA;
        trait TraitA {
            fn trait_func_a(&self) -> i32;
        }
        struct StructB<T>(T)
        where
            T: TraitB;
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
        fn fa<T>(StructA(v): StructA<T>) -> i32
        where
            T: TraitA,
        {
            v.trait_func_a()
        }

        #[system]
        fn fb<A, B>(StructB(b): &StructB<B>, StructA(a): &StructA<A>) -> String
        where
            A: TraitA,
            B: TraitB,
        {
            let a = a.trait_func_a();
            let b = b.trait_func_b();
            std::format!("a={a}, b={b}")
        }
        assert_eq!(&233, fa((StructA(AA), StructB(AA))).get() as &i32);
        assert_eq!(&666, fa((StructB(BB), StructA(BB))).get() as &i32);

        assert_eq!(
            "a=233, b=123",
            fb((StructA(AA), StructB(AA))).get() as &String
        );
        assert_eq!(
            "a=233, b=123",
            fb((StructB(AA), StructA(AA))).get() as &String
        );
        assert_eq!(
            "a=233, b=456",
            fb((StructA(AA), StructB(BB))).get() as &String
        );
        assert_eq!(
            "a=666, b=123",
            fb((StructB(AA), StructA(BB))).get() as &String
        );
    }
}

