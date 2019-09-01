use proc_macro2::*;

macro_rules! assert_impl {
    ($ty:ident is $($marker:ident) and +) => {
        #[test]
        #[allow(non_snake_case)]
        fn $ty() {
            fn assert_implemented<T: $($marker +)+>() {}
            assert_implemented::<$ty>();
        }
    };

    ($ty:ident is not $($marker:ident) or +) => {
        #[test]
        #[allow(non_snake_case)]
        fn $ty() {
            $(
                {
                    // Implemented for types that implement $marker.
                    trait IsNotImplemented {
                        fn assert_not_implemented() {}
                    }
                    impl<T: $marker> IsNotImplemented for T {}

                    // Implemented for the type being tested.
                    trait IsImplemented {
                        fn assert_not_implemented() {}
                    }
                    impl IsImplemented for $ty {}

                    // If $ty does not implement $marker, there is no ambiguity
                    // in the following trait method call.
                    <$ty>::assert_not_implemented();
                }
            )+
        }
    };
}

assert_impl!(Delimiter is Send and Sync);
assert_impl!(Spacing is Send and Sync);

#[cfg(wrap_proc_macro)]
mod not_send_sync_if_proc_macro {
    use super::*;
    #[cfg(not(feature = "proc-macro"))]
    compile_error!();

    assert_impl!(Group is not Send or Sync);
    assert_impl!(Ident is not Send or Sync);
    assert_impl!(LexError is not Send or Sync);
    assert_impl!(Literal is not Send or Sync);
    assert_impl!(Punct is not Send or Sync);
    assert_impl!(Span is not Send or Sync);
    assert_impl!(TokenStream is not Send or Sync);
    assert_impl!(TokenTree is not Send or Sync);
}

#[cfg(not(wrap_proc_macro))]
mod send_sync_if_not_proc_macro {
    use super::*;

    assert_impl!(Group is Send and Sync);
    assert_impl!(Ident is Send and Sync);
    assert_impl!(LexError is Send and Sync);
    assert_impl!(Literal is Send and Sync);
    assert_impl!(Punct is Send and Sync);
    assert_impl!(Span is Send and Sync);
    assert_impl!(TokenStream is Send and Sync);
    assert_impl!(TokenTree is Send and Sync);
}

#[cfg(procmacro2_semver_exempt)]
mod semver_exempt {
    use super::*;

    assert_impl!(LineColumn is Send and Sync);

    assert_impl!(SourceFile is not Send or Sync);
}
