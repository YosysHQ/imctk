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

/// Takes an iterator returning closure and returns a value that formats the yielded items using
/// [`Formatter::debug_set`] when formatted.
pub fn fmt_set<T: IntoIterator<Item = impl Debug>>(
    get_iter: impl Fn() -> T,
) -> impl Display + Debug {
    fmt_closure(move |f| f.debug_set().entries(get_iter()).finish())
}

/// Takes an iterator returning closure and returns a value that formats the yielded items using
/// [`Formatter::debug_set`] when formatted.
pub fn fmt_map<T: IntoIterator<Item = (impl Debug, impl Debug)>>(
    get_iter: impl Fn() -> T,
) -> impl Display + Debug {
    fmt_closure(move |f| f.debug_map().entries(get_iter()).finish())
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, BTreeSet};

    use super::*;

    #[test]
    fn fmt_helpers() {
        let vec = Vec::from_iter(0..10);
        assert_eq!(format!("{vec:?}"), format!("{:?}", fmt_list(|| 0..10)));

        let set = BTreeSet::from_iter(0..10);
        assert_eq!(format!("{set:?}"), format!("{:?}", fmt_set(|| 0..10)));

        let map = BTreeMap::from_iter((0..10).map(|v| (v, v * 2)));
        assert_eq!(
            format!("{map:?}"),
            format!("{:?}", fmt_map(|| (0..10).map(|v| (v, v * 2)))),
        );
    }
}
