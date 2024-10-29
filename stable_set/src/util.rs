use std::ops::{Range, RangeBounds};

pub fn simplify_range(range: impl RangeBounds<usize>, len: usize) -> Range<usize> {
    let lower = match range.start_bound() {
        std::ops::Bound::Unbounded => 0,
        std::ops::Bound::Included(&n) => n,
        std::ops::Bound::Excluded(&n) => n.checked_add(1).expect("start point of range too large"),
    };
    let upper = match range.end_bound() {
        std::ops::Bound::Unbounded => len,
        std::ops::Bound::Included(&n) => n.checked_add(1).expect("end point of range too large"),
        std::ops::Bound::Excluded(&n) => n,
    };
    assert!(
        lower < len,
        "start point {lower} of range is >= length {len}"
    );
    assert!(upper <= len, "end point {upper} of range is > length {len}");
    assert!(
        lower <= upper,
        "start point {lower} is larger than end point {upper}"
    );
    lower..upper
}

macro_rules! impl_iterator {
    () => {
        impl_iterator!(|x| x);
    };
    ($f: expr) => {
        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next().map($f)
        }
        fn size_hint(&self) -> (usize, Option<usize>) {
            self.inner.size_hint()
        }
        fn count(self) -> usize {
            self.inner.count()
        }
        fn nth(&mut self, n: usize) -> Option<Self::Item> {
            self.inner.nth(n).map($f)
        }
        fn last(self) -> Option<Self::Item> {
            self.inner.last().map($f)
        }
    };
}
pub(crate) use impl_iterator;
