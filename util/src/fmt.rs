//! Utilities for writing [`std::fmt`] implementations.

use std::fmt::{Debug, Display, Formatter};
struct FmtClosure<T>(T);

impl<T> Debug for FmtClosure<T>
where
    T: Fn(&mut Formatter<'_>) -> std::fmt::Result,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0(f)
    }
}

impl<T> Display for FmtClosure<T>
where
    T: Fn(&mut Formatter<'_>) -> std::fmt::Result,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0(f)
    }
}

/// Turns a closure that writes to a [`Formatter`] into a type that implements [`Display`] and
/// [`Debug`] by calling that closure.
pub fn fmt_closure<T: Fn(&mut Formatter<'_>) -> std::fmt::Result>(
    closure: T,
) -> impl Display + Debug {
    FmtClosure(closure)
}

/// Takes an iterator returning closure and returns a value that formats the yielded items using
/// [`Formatter::debug_list`] when formatted.
pub fn fmt_list<T: IntoIterator<Item = impl Debug>>(
    get_iter: impl Fn() -> T,
) -> impl Display + Debug {
    fmt_closure(move |f| f.debug_list().entries(get_iter()).finish())
}
