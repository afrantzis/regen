// Copyright 2020 Alexandros Frantzis
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
// SPDX-License-Identifier: MPL-2.0

pub(crate) trait AppendingIterator : AppendingIteratorClone {
    fn reset(&mut self);
    fn append_next(&mut self, out: &mut Vec<u8>) -> Option<usize>;
    fn set_max_elements_hint(&mut self, _max_elements: usize) {}
    fn min_elements(&self) -> usize;
}

pub(crate) trait AppendingIteratorClone {
    fn clone_box(&self) -> Box<dyn AppendingIterator>;
}

impl<T: 'static + AppendingIterator + Clone> AppendingIteratorClone for T {
    fn clone_box(&self) -> Box<dyn AppendingIterator> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn AppendingIterator> {
    fn clone(&self) -> Box<dyn AppendingIterator> {
        self.clone_box()
    }
}

#[derive(Clone)]
pub(crate) struct Empty;

impl Empty {
    pub(crate) fn new() -> Self {
        Empty
    }
}

impl AppendingIterator for Empty {
    fn append_next(&mut self, _out: &mut Vec<u8>) -> Option<usize> {
        None
    }

    fn reset(&mut self) {
    }

    fn min_elements(&self) -> usize {
        0
    }
}

pub(crate) trait ElementCount {
    fn element_count(&self) -> usize;
}

impl ElementCount for String {
    fn element_count(&self) -> usize { self.chars().count() }
}

impl ElementCount for Vec<u8> {
    fn element_count(&self) -> usize { self.len() }
}

#[derive(Clone)]
pub(crate) struct Literal<T> {
    literal: T,
    done: bool,
    element_count: usize,
}

impl<T: ElementCount> Literal<T> {
    pub(crate) fn new(literal: impl Into<T>) -> Self {
        let literal = literal.into();
        let element_count = literal.element_count();
        Literal {
            literal,
            done: false,
            element_count,
        }
    }
}

impl<T: 'static + Clone + AsRef<[u8]>> AppendingIterator for Literal<T> {
    fn append_next(&mut self, out: &mut Vec<u8>) -> Option<usize> {
        if self.done {
            None
        } else {
            self.done = true;
            out.extend(self.literal.as_ref());
            Some(self.element_count)
        }
    }

    fn reset(&mut self) {
        self.done = false;
    }

    fn min_elements(&self) -> usize {
        self.element_count
    }
}

pub(crate) type CharLiteral = Literal<String>;
pub(crate) type ByteLiteral = Literal<Vec<u8>>;

#[derive(Clone)]
pub(crate) struct Concatenation {
    children: Vec<Box<dyn AppendingIterator>>,
    results: Vec<Vec<u8>>,
    element_counts: Vec<usize>,
    min_elements_cumulative: Vec<usize>,
    max_elements: usize,
}

impl Concatenation {
    pub(crate) fn new(children: Vec<Box<dyn AppendingIterator>>) -> Self {
        Concatenation{
            children,
            results: Vec::new(),
            element_counts: Vec::new(),
            min_elements_cumulative: Vec::new(),
            max_elements: usize::max_value(),
        }
    }

    fn populate_min_elements_cumulative(&mut self) {
        self.min_elements_cumulative = self.children.iter().rev()
            .scan(0, |state, child| {
                *state += child.min_elements();
                Some(*state)
            })
            .collect();
        self.min_elements_cumulative.reverse();
        self.min_elements_cumulative.push(0);
    }

    fn calc_max_elements(&self, c: usize) -> usize {
        let total =
            self.element_counts[..c].iter().sum::<usize>() +
            self.min_elements_cumulative[c+1];

        if total < self.max_elements {
            self.max_elements - total
        } else {
            0
        }
    }

    fn first_append_next(&mut self, out: &mut Vec<u8>) -> Option<usize> {
        for i in 0..self.children.len() {
            self.results.push(Vec::new());
            self.element_counts.push(
                self.children[i].append_next(&mut self.results[i]).unwrap_or(0)
            );
        }

        if self.max_elements != usize::max_value() {
            for i in 0..self.children.len() {
                let max_elements = self.calc_max_elements(i);
                self.children[i].set_max_elements_hint(max_elements);
            }
        }

        for r in &self.results {
            out.extend(r);
        }

        return Some(self.element_counts.iter().sum());
    }
}

impl AppendingIterator for Concatenation {
    fn append_next(&mut self, out: &mut Vec<u8>) -> Option<usize> {
        if self.results.is_empty() && !self.children.is_empty() {
            return self.first_append_next(out);
        }

        for i in (0..self.children.len()).rev() {
            self.results[i].clear();
            match self.children[i].append_next(&mut self.results[i]) {
                None => {
                    self.children[i].reset();
                    self.element_counts[i] =
                        self.children[i].append_next(&mut self.results[i]).unwrap_or(0);
                },
                Some(n) => {
                    self.element_counts[i] = n;

                    if self.max_elements != usize::max_value() {
                        for j in i+1..self.children.len() {
                            let max_elements = self.calc_max_elements(j);
                            self.children[j].set_max_elements_hint(max_elements);
                        }
                    }

                    for r in &self.results {
                        out.extend(r);
                    }

                    return Some(self.element_counts.iter().sum());
                },
            }
        }

        None
    }

    fn reset(&mut self) {
        for c in &mut self.children {
            c.reset();
        }
        self.results.clear();
        self.element_counts.clear();
    }

    fn set_max_elements_hint(&mut self, max_elements: usize) {
        self.max_elements = max_elements;
        if self.max_elements != usize::max_value() &&
           self.min_elements_cumulative.len() != self.children.len() {
            self.populate_min_elements_cumulative();
        }
    }

    fn min_elements(&self) -> usize {
        self.children.iter().map(|c| c.min_elements()).sum()
    }
}

#[derive(Clone)]
pub(crate) struct Alternation {
    children: Vec<Box<dyn AppendingIterator>>,
    active: usize,
    max_elements: usize,
    last_child: usize,
}

impl Alternation {
    pub(crate) fn new(mut children: Vec<Box<dyn AppendingIterator>>) -> Self {
        let last_child = children.len();
        children.sort_unstable_by_key(|x| x.min_elements());
        Alternation{children, active: 0, max_elements: usize::max_value(), last_child}
    }
}

impl AppendingIterator for Alternation {
    fn append_next(&mut self, out: &mut Vec<u8>) -> Option<usize> {
        for i in self.active..self.last_child {
            if let Some(n) = self.children[i].append_next(out) {
                return Some(n);
            }
            self.active += 1;
        }
        None
    }

    fn reset(&mut self) {
        for c in &mut self.children {
            c.reset();
        }
        self.active = 0;
    }

    fn set_max_elements_hint(&mut self, max_elements: usize) {
        if self.max_elements == max_elements {
            return;
        }

        for c in &mut self.children {
            c.set_max_elements_hint(max_elements);
        }

        self.max_elements = max_elements;
        self.last_child = self.children.iter().position(
            |x| x.min_elements() > max_elements).unwrap_or(self.children.len());
    }

    fn min_elements(&self) -> usize {
        self.children.iter().map(|c| c.min_elements()).min().unwrap_or(0)
    }
}

#[derive(Clone)]
pub(crate) struct CharRange {
    pub(crate) start: char,
    pub(crate) len: u32,
}

#[derive(Clone)]
pub(crate) struct CharClass {
    ranges: Vec<CharRange>,
    active_range: usize,
    active_index: u32,
}

impl CharClass {
    pub(crate) fn new(ranges: Vec<CharRange>) -> Self {
        CharClass{ranges, active_range: 0, active_index: 0}
    }

    #[cfg(test)]
    fn new_from_str(elements: &str) -> Self {
        let ranges = elements.chars().map(|c| CharRange{start: c, len: 1}).collect();
        CharClass{ranges, active_range: 0, active_index: 0}
    }

    fn push_char(ch: char, out: &mut Vec<u8>) {
        match ch.len_utf8() {
            1 => out.push(ch as u8),
            _ => out.extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes()),
        }
    }

}

impl AppendingIterator for CharClass {
    fn append_next(&mut self, out: &mut Vec<u8>) -> Option<usize> {
        while self.active_range < self.ranges.len() &&
           self.active_index < self.ranges[self.active_range].len {
            let ord = self.ranges[self.active_range].start as u32 + self.active_index;

            self.active_index += 1;
            if self.active_index == self.ranges[self.active_range].len {
                self.active_index = 0;
                self.active_range += 1;
            }

            // This could only be none for a code point in the range 0xd800-0xdfff
            match std::char::from_u32(ord) {
                Some(c) => CharClass::push_char(c, out),
                None => continue,
            }

            return Some(1);
        }

        None
    }

    fn reset(&mut self) {
        self.active_range = 0;
        self.active_index = 0;
    }

    fn min_elements(&self) -> usize {
        1
    }
}

#[derive(Clone)]
pub(crate) struct ByteRange {
    pub(crate) start: u8,
    pub(crate) len: u16,
}

#[derive(Clone)]
pub(crate) struct ByteClass {
    ranges: Vec<ByteRange>,
    active_range: usize,
    active_index: u16,
}

impl ByteClass {
    pub(crate) fn new(ranges: Vec<ByteRange>) -> Self {
        ByteClass{ranges, active_range: 0, active_index: 0}
    }

    #[cfg(test)]
    fn new_from_slice(elements: &[u8]) -> Self {
        let ranges = elements.iter().map(|c| ByteRange{start: *c, len: 1}).collect();
        ByteClass{ranges, active_range: 0, active_index: 0}
    }
}

impl AppendingIterator for ByteClass {
    fn append_next(&mut self, out: &mut Vec<u8>) -> Option<usize> {
        if self.active_range < self.ranges.len() &&
           self.active_index < self.ranges[self.active_range].len {
            let byte = self.ranges[self.active_range].start + self.active_index as u8;

            self.active_index += 1;
            if self.active_index == self.ranges[self.active_range].len {
                self.active_index = 0;
                self.active_range += 1;
            }

            out.push(byte);

            return Some(1);
        }

        None
    }

    fn reset(&mut self) {
        self.active_range = 0;
        self.active_index = 0;
    }

    fn min_elements(&self) -> usize {
        1
    }
}

#[derive(Clone)]
pub(crate) struct Repeat {
    element: Box<dyn AppendingIterator>,
    begin: u32,
    end: u32,
    current: u32,
    concat: Concatenation,
    max_elements: usize,
}

impl Repeat {
    pub(crate) fn new(element: Box<dyn AppendingIterator>, begin: u32, end: u32) -> Self {
        let concat = if begin != 0 {
            Concatenation::new((0..begin).map(|_| element.clone()).collect())
        } else {
            Concatenation::new(vec![Box::new(CharLiteral::new(""))])
        };
        Repeat{element, begin, end, current: begin, concat, max_elements: usize::max_value()}
    }

}

impl AppendingIterator for Repeat {
    fn append_next(&mut self, out: &mut Vec<u8>) -> Option<usize> {
        if let Some(n) = self.concat.append_next(out) {
            return Some(n);
        }

        if self.current == self.end ||
           (self.max_elements != usize::max_value() &&
            self.current >= (self.max_elements / self.element.min_elements()) as u32) {
            return None;
        }

        self.current += 1;
        self.concat = Concatenation::new((0..self.current).map(|_| self.element.clone()).collect());
        self.concat.set_max_elements_hint(self.max_elements);
        return self.concat.append_next(out);
    }

    fn reset(&mut self) {
        self.current = self.begin;
        self.concat = if self.begin != 0 {
            Concatenation::new((0..self.begin).map(|_| self.element.clone()).collect())
        } else {
            Concatenation::new(vec![Box::new(CharLiteral::new(""))])
        };
    }

    fn set_max_elements_hint(&mut self, max_elements: usize) {
        self.max_elements = max_elements;
        self.concat.set_max_elements_hint(max_elements);
    }

    fn min_elements(&self) -> usize {
        self.element.min_elements() * self.begin as usize
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_all(appending_iter: &mut dyn AppendingIterator) -> Vec<Vec<u8>> {
        let mut out = Vec::new();
        let mut s = Vec::new();
        while appending_iter.append_next(&mut s).is_some() {
            out.push(s.clone());
            s.clear();
        }
        out
    }

    fn collect_counts(appending_iter: &mut dyn AppendingIterator) -> Vec<usize> {
        let mut s = Vec::new();
        let mut counts = Vec::new();
        while let Some(n) = appending_iter.append_next(&mut s) {
            counts.push(n);
        }
        counts
    }

    #[test]
    fn literal_returns_one_value() {
        let mut literal = CharLiteral::new("αβγ");
        let all = collect_all(&mut literal);
        assert_eq!(all, vec!["αβγ".as_bytes()]);
    }

    #[test]
    fn literal_resets() {
        let mut literal = CharLiteral::new("αβγ");
        collect_all(&mut literal);
        literal.reset();
        let all = collect_all(&mut literal);
        assert_eq!(all, vec!["αβγ".as_bytes()]);
    }

    #[test]
    fn char_literal_returns_element_count_in_chars() {
        let mut literal = CharLiteral::new("αβγ");
        let counts = collect_counts(&mut literal);
        assert_eq!(counts, vec![3]);
    }

    #[test]
    fn char_literal_returns_min_elements_in_chars() {
        let literal = CharLiteral::new("αβγ");
        assert_eq!(literal.min_elements(), 3);
    }

    #[test]
    fn byte_literal_returns_element_count_in_bytes() {
        let mut literal = ByteLiteral::new("αβγ");
        let counts = collect_counts(&mut literal);
        assert_eq!(counts, vec![6]);
    }

    #[test]
    fn byte_literal_returns_min_elements_in_bytes() {
        let literal = ByteLiteral::new("αβγ");
        assert_eq!(literal.min_elements(), 6);
    }

    #[test]
    fn concatenation_uses_all_values() {
        let mut concat = Concatenation::new( vec![
            Box::new(CharLiteral::new("α")),
            Box::new(CharLiteral::new("β")),
            Box::new(CharLiteral::new("γ")),
        ]);
        let all = collect_all(&mut concat);
        assert_eq!(all, vec!["αβγ".as_bytes()]);
    }

    #[test]
    fn concatenation_resets() {
        let mut concat = Concatenation::new( vec![
            Box::new(CharLiteral::new("α")),
            Box::new(CharLiteral::new("β")),
            Box::new(CharLiteral::new("γ")),
        ]);
        collect_all(&mut concat);
        concat.reset();
        let all = collect_all(&mut concat);
        assert_eq!(all, vec!["αβγ".as_bytes()]);
    }

    #[test]
    fn empty_concatenation_returns_nothing() {
        let mut concat = Concatenation::new(Vec::new());
        let all = collect_all(&mut concat);
        assert_eq!(all.len(), 0);
    }

    #[test]
    fn concatenation_reports_total_element_count() {
        let mut concat = Concatenation::new( vec![
            Box::new(CharLiteral::new("α")),
            Box::new(CharLiteral::new("ββ")),
            Box::new(CharLiteral::new("γγγ")),
        ]);
        let counts = collect_counts(&mut concat);
        assert_eq!(counts, vec![6]);
    }

    #[test]
    fn concatenation_reports_total_min_elements() {
        let concat = Concatenation::new( vec![
            Box::new(CharLiteral::new("α")),
            Box::new(CharLiteral::new("ββ")),
            Box::new(CharLiteral::new("γγγ")),
        ]);
        assert_eq!(concat.min_elements(), 6);
    }

    #[test]
    fn alternation_uses_all_values() {
        let mut alt = Alternation::new( vec![
            Box::new(CharLiteral::new("α")),
            Box::new(CharLiteral::new("β")),
            Box::new(CharLiteral::new("γ")),
        ]);
        let all = collect_all(&mut alt);
        assert_eq!(all, vec!["α".as_bytes(), "β".as_bytes(), "γ".as_bytes()]);
    }

    #[test]
    fn alternation_resets() {
        let mut alt = Alternation::new( vec![
            Box::new(CharLiteral::new("α")),
            Box::new(CharLiteral::new("β")),
            Box::new(CharLiteral::new("γ")),
        ]);
        collect_all(&mut alt);
        alt.reset();
        let all = collect_all(&mut alt);
        assert_eq!(all, vec!["α".as_bytes(), "β".as_bytes(), "γ".as_bytes()]);
    }

    #[test]
    fn empty_alternation_returns_nothing() {
        let mut alt = Alternation::new(Vec::new());
        let all = collect_all(&mut alt);
        assert_eq!(all.len(), 0);
    }

    #[test]
    fn alternation_reports_alt_element_counts() {
        let mut alt = Alternation::new( vec![
            Box::new(CharLiteral::new("α")),
            Box::new(CharLiteral::new("β")),
            Box::new(CharLiteral::new("γ")),
        ]);
        let counts = collect_counts(&mut alt);
        assert_eq!(counts, vec![1, 1, 1]);
    }

    #[test]
    fn alternation_reports_alt_min_elements() {
        let alt = Alternation::new( vec![
            Box::new(CharLiteral::new("ααα")),
            Box::new(CharLiteral::new("ββ")),
            Box::new(CharLiteral::new("γγγγ")),
        ]);
        assert_eq!(alt.min_elements(), 2);
    }

    #[test]
    fn char_class_uses_all_values() {
        let mut class = CharClass::new_from_str("αβγ");
        let all = collect_all(&mut class);
        assert_eq!(all, vec!["α".as_bytes(), "β".as_bytes(), "γ".as_bytes()]);
    }

    #[test]
    fn char_class_resets() {
        let mut class = CharClass::new_from_str("αβγ");
        collect_all(&mut class);
        class.reset();
        let all = collect_all(&mut class);
        assert_eq!(all, vec!["α".as_bytes(), "β".as_bytes(), "γ".as_bytes()]);
    }

    #[test]
    fn empty_char_class_returns_nothing() {
        let mut class = CharClass::new(Vec::new());
        let all = collect_all(&mut class);
        assert_eq!(all.len(), 0);
    }

    #[test]
    fn char_class_reports_element_count_of_one() {
        let mut class = CharClass::new_from_str("αβγ");
        let counts = collect_counts(&mut class);
        assert_eq!(counts, vec![1, 1, 1]);
    }

    #[test]
    fn char_class_reports_min_elements_of_one() {
        let class = CharClass::new_from_str("αβγ");
        assert_eq!(class.min_elements(), 1);
    }

    #[test]
    fn byte_class_uses_all_values() {
        let mut class = ByteClass::new_from_slice(&[0x10, 0x20, 0x30]);
        let all = collect_all(&mut class);
        assert_eq!(all, vec![&[0x10], &[0x20], &[0x30]]);
    }

    #[test]
    fn byte_class_resets() {
        let mut class = ByteClass::new_from_slice(&[0x10, 0x20, 0x30]);
        collect_all(&mut class);
        class.reset();
        let all = collect_all(&mut class);
        assert_eq!(all, vec![&[0x10], &[0x20], &[0x30]]);
    }

    #[test]
    fn empty_byte_class_returns_nothing() {
        let mut class = ByteClass::new(Vec::new());
        let all = collect_all(&mut class);
        assert_eq!(all.len(), 0);
    }

    #[test]
    fn byte_class_reports_element_count_of_one() {
        let class = ByteClass::new_from_slice(&[0x10, 0x20, 0x30]);
        assert_eq!(class.min_elements(), 1);
    }

    #[test]
    fn concatenation_char_class() {
        let mut concat = Concatenation::new( vec![
            Box::new(CharClass::new_from_str("ab")),
            Box::new(CharClass::new_from_str("xy")),
        ]);
        let all = collect_all(&mut concat);
        assert_eq!(all, vec![
            "ax".as_bytes(), "ay".as_bytes(),
            "bx".as_bytes(), "by".as_bytes()
        ]);
        concat.reset();
        let counts = collect_counts(&mut concat);
        assert_eq!(counts, vec![2, 2, 2, 2]);
        assert_eq!(concat.min_elements(), 2);
    }

    #[test]
    fn concatenation_byte_class() {
        let mut concat = Concatenation::new( vec![
            Box::new(ByteClass::new_from_slice(&[b'a', 0xEE])),
            Box::new(ByteClass::new_from_slice(&[b'x', 0xFF])),
        ]);
        let all = collect_all(&mut concat);
        assert_eq!(all, vec![
            &[b'a', b'x'], &[b'a', 0xFF],
            &[0xEE, b'x'], &[0xEE, 0xFF],
        ]);
        concat.reset();
        let counts = collect_counts(&mut concat);
        assert_eq!(counts, vec![2, 2, 2, 2]);
        assert_eq!(concat.min_elements(), 2);
    }

    #[test]
    fn concatenation_alternation() {
        let mut concat = Concatenation::new( vec![
            Box::new(Alternation::new(vec![
                Box::new(CharLiteral::new("a")),
                Box::new(CharLiteral::new("bb")),
            ])),
            Box::new(Alternation::new(vec![
                Box::new(CharLiteral::new("χ")),
                Box::new(CharLiteral::new("ψ")),
            ])),
        ]);
        let all = collect_all(&mut concat);
        assert_eq!(all, vec![
            "aχ".as_bytes(), "aψ".as_bytes(),
            "bbχ".as_bytes(), "bbψ".as_bytes()
        ]);
        concat.reset();
        let counts = collect_counts(&mut concat);
        assert_eq!(counts, vec![2, 2, 3, 3]);
        assert_eq!(concat.min_elements(), 2);
    }

    #[test]
    fn alternation_concatenation() {
        let mut alt = Alternation::new(vec![
            Box::new(Concatenation::new(vec![
                Box::new(CharLiteral::new("α")),
                Box::new(CharLiteral::new("β")),
            ])),
            Box::new(Concatenation::new(vec![
                Box::new(CharLiteral::new("x")),
                Box::new(CharLiteral::new("y")),
            ])),
        ]);
        let all = collect_all(&mut alt);
        assert_eq!(all, vec!["αβ".as_bytes(), "xy".as_bytes()]);
        alt.reset();
        let counts = collect_counts(&mut alt);
        assert_eq!(counts, vec![2, 2]);
        assert_eq!(alt.min_elements(), 2);
    }

    #[test]
    fn alternation_char_class() {
        let mut alt = Alternation::new(vec![
            Box::new(CharClass::new_from_str("αβ")),
            Box::new(CharClass::new_from_str("xy")),
        ]);
        let all = collect_all(&mut alt);
        assert_eq!(all, vec![
            "α".as_bytes(), "β".as_bytes(),
            "x".as_bytes(), "y".as_bytes()
        ]);
        alt.reset();
        let counts = collect_counts(&mut alt);
        assert_eq!(counts, vec![1, 1, 1, 1]);
        assert_eq!(alt.min_elements(), 1);
    }

    #[test]
    fn alternation_byte_class() {
        let mut alt = Alternation::new(vec![
            Box::new(ByteClass::new_from_slice(&[0xAA, 0xBB])),
            Box::new(ByteClass::new_from_slice(&[0xEE, 0xFF])),
        ]);
        let all = collect_all(&mut alt);
        assert_eq!(all, vec![&[0xAA], &[0xBB], &[0xEE], &[0xFF]]);
        alt.reset();
        let counts = collect_counts(&mut alt);
        assert_eq!(counts, vec![1, 1, 1, 1]);
        assert_eq!(alt.min_elements(), 1);
    }

    #[test]
    fn repeat_uses_all_values() {
        let mut repeat = Repeat::new(
            Box::new(CharLiteral::new("a")),
            2, 4,
        );
        let all = collect_all(&mut repeat);
        assert_eq!(all, vec![
            "aa".as_bytes(), "aaa".as_bytes(),
            "aaaa".as_bytes()
        ]);
    }

    #[test]
    fn repeat_resets() {
        let mut repeat = Repeat::new(
            Box::new(CharLiteral::new("α")),
            2, 4,
        );
        collect_all(&mut repeat);
        repeat.reset();
        let all = collect_all(&mut repeat);
        assert_eq!(all, vec![
            "αα".as_bytes(), "ααα".as_bytes(),
            "αααα".as_bytes()
        ]);
    }

    #[test]
    fn empty_repeat_returns_empty_string() {
        let mut repeat = Repeat::new(
            Box::new(CharLiteral::new("α")),
            0, 0,
        );
        let all = collect_all(&mut repeat);
        assert_eq!(all, vec!["".as_bytes()]);
    }

    #[test]
    fn repeat_reports_element_count() {
        let mut repeat = Repeat::new(
            Box::new(CharLiteral::new("α")),
            2, 4,
        );
        let counts = collect_counts(&mut repeat);
        assert_eq!(counts, vec![2, 3, 4]);
    }

    #[test]
    fn repeat_reports_min_elements() {
        let repeat = Repeat::new(
            Box::new(CharLiteral::new("αα")),
            2, 4,
        );
        assert_eq!(repeat.min_elements(), 4);
    }

    #[test]
    fn repeat_with_zero_reports_zero_min_elements() {
        let repeat = Repeat::new(
            Box::new(CharLiteral::new("αα")),
            0, 4,
        );
        assert_eq!(repeat.min_elements(), 0);
    }

    #[test]
    fn repeat_alternation() {
        let mut repeat = Repeat::new(
            Box::new(Alternation::new(vec![
                Box::new(CharLiteral::new("α")),
                Box::new(CharLiteral::new("b")),
            ])),
            0, 2,
        );
        let all = collect_all(&mut repeat);
        assert_eq!(all, vec![
            "".as_bytes(),
            "α".as_bytes(), "b".as_bytes(),
            "αα".as_bytes(), "αb".as_bytes(),
            "bα".as_bytes(), "bb".as_bytes(),
        ]);
        repeat.reset();
        let counts = collect_counts(&mut repeat);
        assert_eq!(counts, vec![0, 1, 1, 2, 2, 2, 2]);
    }

    #[test]
    fn repeat_char_class() {
        let mut repeat = Repeat::new(
            Box::new(CharClass::new_from_str("άb")),
            0, 2,
        );
        let all = collect_all(&mut repeat);
        assert_eq!(all, vec![
            "".as_bytes(),
            "ά".as_bytes(), "b".as_bytes(),
            "άά".as_bytes(), "άb".as_bytes(),
            "bά".as_bytes(), "bb".as_bytes(),
        ]);
        repeat.reset();
        let counts = collect_counts(&mut repeat);
        assert_eq!(counts, vec![0, 1, 1, 2, 2, 2, 2]);
    }

    #[test]
    fn repeat_byte_class() {
        let mut repeat = Repeat::new(
            Box::new(ByteClass::new_from_slice(&[0xAA, 0xBB])),
            0, 2,
        );
        let all = collect_all(&mut repeat);
        assert_eq!(all, vec![
            vec![],
            vec![0xAA], vec![0xBB],
            vec![0xAA, 0xAA], vec![0xAA, 0xBB],
            vec![0xBB, 0xAA], vec![0xBB, 0xBB],
        ]);
        repeat.reset();
        let counts = collect_counts(&mut repeat);
        assert_eq!(counts, vec![0, 1, 1, 2, 2, 2, 2]);
    }

    #[test]
    fn repeat_respects_max_element_hint() {
        let mut repeat = Repeat::new(
            Box::new(CharLiteral::new("αβγ")),
            0, u32::max_value(),
        );
        repeat.set_max_elements_hint(8);
        let all = collect_all(&mut repeat);
        assert_eq!(all, vec![
            "".as_bytes(),
            "αβγ".as_bytes(),
            "αβγαβγ".as_bytes(),
        ]);
    }

    #[test]
    fn repeat_alternation_respects_max_element_hint() {
        let mut repeat = Repeat::new(
            Box::new(Alternation::new(vec![
                Box::new(CharLiteral::new("a")),
                Box::new(CharLiteral::new("bb")),
            ])),
            1, u32::max_value(),
        );
        repeat.set_max_elements_hint(3);
        let all = collect_all(&mut repeat);
        assert_eq!(all, vec![
            "a".as_bytes(),
            "bb".as_bytes(),
            "aa".as_bytes(),
            "abb".as_bytes(),
            "bba".as_bytes(),
            "aaa".as_bytes(),
        ]);
    }
}
