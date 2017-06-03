extern crate proc_macro2;

use proc_macro2::Symbol;

fn assert_send<T: Send>() {}

fn main() {
    assert_send::<Symbol>(); //~ the trait bound `*const (): std::marker::Send` is not satisfied in `proc_macro2::Symbol`
}
