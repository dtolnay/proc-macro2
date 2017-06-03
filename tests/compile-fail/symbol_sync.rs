extern crate proc_macro2;

use proc_macro2::Symbol;

fn assert_sync<T: Sync>() {}

fn main() {
    assert_sync::<Symbol>(); //~ the trait bound `*const (): std::marker::Sync` is not satisfied in `proc_macro2::Symbol`
}
