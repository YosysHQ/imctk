#![allow(missing_docs)] // TODO document

use std::cmp::Ordering;

use imctk_ids::{id_vec::IdVec, Id, Id32, IdRange};

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct ClassId(Id32);

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct MemberId(Id32);

#[derive(Default)]
pub struct PartitionRefinement {
    class_from_member: IdVec<MemberId, ClassId>,
    member_from_class: IdVec<ClassId, MemberId>,
}

impl PartitionRefinement {
    pub fn new(len: usize) -> Self {
        let member_from_class = IdVec::from_vec(IdRange::from_index_range(0..len).iter().collect());
        let mut class_from_member = IdVec::from_vec(vec![ClassId::MIN_ID; len]);
        if len > 0 {
            class_from_member[MemberId::MIN_ID] = ClassId::from_id_index(len - 1);
        }

        Self {
            class_from_member,
            member_from_class,
        }
    }

    pub fn class_of_member(&self, member: MemberId) -> ClassId {
        self.canonical_class(self.class_from_member[member])
    }

    fn canonical_class(&self, class: ClassId) -> ClassId {
        let repr_member = self.member_from_class[class];
        let repr_class = self.class_from_member[repr_member];
        class.min(repr_class)
    }

    pub fn is_class(&self, class: ClassId) -> bool {
        self.canonical_class(class) == class
    }

    fn unchecked_class_len(&self, class: ClassId) -> usize {
        let end = self.class_from_member[self.member_from_class[class]];
        end.id_index() - class.id_index() + 1
    }

    pub fn class_len(&self, class: ClassId) -> usize {
        if !self.is_class(class) {
            return 0;
        }
        self.unchecked_class_len(class)
    }

    pub fn members_in_class(&self, class: ClassId) -> &[MemberId] {
        let len = self.class_len(class);
        &self.member_from_class.values()[class.id_index()..][..len]
    }

    pub fn partition_class(
        &mut self,
        class: ClassId,
        mut predicate: impl FnMut(MemberId) -> bool,
    ) -> Option<ClassId> {
        let len = self.class_len(class);
        if len < 2 {
            return None;
        }

        // Temporarily remove the length marker while we're permuting class members
        self.class_from_member[self.member_from_class[class]] = class;

        let start = class.id_index();
        let end = start + len;

        let mut left = start;
        let mut right = end - 1;

        let split_at = loop {
            while left <= right && predicate(self.member_from_class.values()[left]) {
                left += 1
            }
            while left < right && !predicate(self.member_from_class.values()[right]) {
                right -= 1
            }
            if left >= right {
                break left;
            }
            self.member_from_class.values_mut().swap(left, right);
        };

        let end_marker = ClassId::from_id_index(end - 1);

        if split_at == start || split_at == end {
            self.class_from_member[self.member_from_class[class]] = end_marker;
            return None;
        }

        let new_end_marker = ClassId::from_id_index(split_at - 1);
        let new_class = ClassId::from_id_index(split_at);

        self.class_from_member[self.member_from_class[class]] = new_end_marker;

        self.class_from_member[self.member_from_class.values()[split_at]] = end_marker;
        for i in split_at + 1..end {
            self.class_from_member[self.member_from_class.values()[i]] = new_class;
        }

        Some(new_class)
    }

    pub fn permute_class<R>(
        &mut self,
        class: ClassId,
        permute: impl FnOnce(&mut [MemberId]) -> R,
    ) -> R {
        let len = self.class_len(class);
        if len == 0 {
            return permute(&mut []);
        }

        let start = class.id_index();
        let end = start + len;

        let end_marker = ClassId::from_id_index(end - 1);

        // Temporarily remove the length marker while we're permuting class members
        self.class_from_member[self.member_from_class[class]] = class;

        let result = permute(&mut self.member_from_class.values_mut()[start..end]);

        self.class_from_member[self.member_from_class[class]] = end_marker;

        result
    }

    pub fn split_at(&mut self, class: ClassId, keep: usize) -> Option<ClassId> {
        if keep == 0 {
            return None;
        }
        let len = self.class_len(class);
        if keep >= len {
            return None;
        }

        let start = class.id_index();
        let end = start + len;

        let split_at = start + keep;

        let end_marker = ClassId::from_id_index(end - 1);
        let new_end_marker = ClassId::from_id_index(split_at - 1);
        let new_class = ClassId::from_id_index(split_at);

        self.class_from_member[self.member_from_class[class]] = new_end_marker;

        self.class_from_member[self.member_from_class.values()[split_at]] = end_marker;
        for i in split_at + 1..end {
            self.class_from_member[self.member_from_class.values()[i]] = new_class;
        }

        Some(new_class)
    }

    pub fn multiway_split_by(
        &mut self,
        class: ClassId,
        mut split_between: impl FnMut(MemberId, MemberId) -> bool,
        mut new_class: impl FnMut(&mut Self, ClassId),
    ) {
        let mut len = self.class_len(class);
        let start = class.id_index();

        while len >= 2 {
            let mut pos = len - 1;

            loop {
                if split_between(
                    self.member_from_class.values()[start + pos - 1],
                    self.member_from_class.values()[start + pos],
                ) {
                    let new = self.split_at(class, pos).unwrap();
                    new_class(self, new);
                    len = pos;
                    break;
                }
                pos -= 1;
                if pos == 0 {
                    return;
                }
            }
        }
    }

    pub fn multiway_unstable_sort_and_split_by(
        &mut self,
        class: ClassId,
        mut compare: impl FnMut(MemberId, MemberId) -> Ordering,
        new_class: impl FnMut(&mut Self, ClassId),
    ) {
        self.permute_class(class, |members| {
            members.sort_unstable_by(|&a, &b| compare(a, b));
        });
        self.multiway_split_by(class, |a, b| compare(a, b).is_ne(), new_class)
    }

    pub fn members(&mut self) -> &[MemberId] {
        self.member_from_class.values()
    }
}

#[cfg(test)]
mod tests {

    use std::mem::swap;

    use imctk_ids::id_index_set::IdIndexSet;

    use super::*;

    #[test]
    pub fn test_partition_class() {
        // This isn't a good way to compute primes, but not a bad way to test the partition
        // refinement data structure.
        let mut partition = PartitionRefinement::new(100);

        let mut current_class = ClassId::MIN_ID;

        let mut primes = vec![];

        current_class = partition
            .partition_class(current_class, |member| member.id_index() < 2)
            .expect("split class");

        assert!(partition
            .members_in_class(current_class)
            .iter()
            .all(|&member| member.id_index() >= 2));

        for _ in 0..40 {
            let target = *partition
                .members_in_class(current_class)
                .iter()
                .min()
                .expect("non-empty class");

            primes.push(target.id_index());

            let Some(refined_class) = partition.partition_class(current_class, |member| {
                member.id_index() % target.id_index() == 0
            }) else {
                break;
            };

            current_class = refined_class
        }

        assert_eq!(
            primes,
            [
                2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79,
                83, 89, 97
            ]
        );
    }

    #[test]
    pub fn test_multiway_partition_class() {
        let mut strings: IdIndexSet<MemberId, &'static str> = Default::default();
        let common = [
            "the", "be", "to", "of", "and", "a", "in", "that", "have", "I", "it", "for", "not",
            "on", "with", "he", "as", "you", "do", "at", "this", "but", "his", "by", "from",
            "they", "we", "say", "her", "she", "or", "an", "will", "my", "one", "all", "would",
            "there", "their", "what", "so", "up", "out", "if", "about", "who", "get", "which",
            "go", "me", "when", "make", "can", "like", "time", "no", "just", "him", "know", "take",
            "people", "into", "year", "your", "good", "some", "could", "them", "see", "other",
            "than", "then", "now", "look", "only", "come", "its", "over", "think", "also", "back",
            "after", "use", "two", "how", "our", "work", "first", "well", "way", "even", "new",
            "want", "because", "any", "these", "give", "day", "most", "us",
        ];

        for s in common {
            strings.insert(s);
        }

        let mut partition = PartitionRefinement::new(common.len());

        let mut classes = vec![ClassId::MIN_ID];
        let mut new_classes = vec![];

        let mut offset = 0;

        while !classes.is_empty() {
            for class in classes.drain(..) {
                partition.multiway_unstable_sort_and_split_by(
                    class,
                    |a, b| {
                        strings[a]
                            .as_bytes()
                            .get(offset)
                            .cmp(&strings[b].as_bytes().get(offset))
                    },
                    |partition, new_class| {
                        if partition.class_len(new_class) >= 2 {
                            new_classes.push(new_class);
                        }
                    },
                );
                if partition.class_len(class) >= 2 {
                    new_classes.push(class);
                }
            }

            offset += 1;

            swap(&mut classes, &mut new_classes);
        }

        for pair in partition.members().windows(2) {
            let [a, b] = *pair else { unreachable!() };
            assert!(strings[a] < strings[b]);
        }
    }
}
