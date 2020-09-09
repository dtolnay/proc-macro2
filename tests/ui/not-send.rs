use proc_macro2::Span;

fn main() {
    fn requires_send<T: Send>() {}
    requires_send::<Span>();
}
