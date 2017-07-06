extern crate proc_macro2;

use proc_macro2::Term;

fn assert_send<T: Send>() {}

fn main() {
    assert_send::<Term>(); //~ the trait bound `*const (): std::marker::Send` is not satisfied in `proc_macro2::Term`
}
