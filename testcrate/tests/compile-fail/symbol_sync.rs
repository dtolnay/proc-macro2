extern crate proc_macro2;

use proc_macro2::Term;

fn assert_sync<T: Sync>() {}

fn main() {
    assert_sync::<Term>(); //~ the trait bound `*const (): std::marker::Sync` is not satisfied in `proc_macro2::Term`
}
