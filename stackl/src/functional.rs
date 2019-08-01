pub fn compose<X1, X2, Y>(g: impl Fn(X2) -> Y, f: impl Fn(X1) -> X2) -> impl Fn(X1) -> Y {
    move |x| g(f(x))
}