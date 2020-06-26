// Copyright 2020 Alexandros Frantzis
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
// SPDX-License-Identifier: MPL-2.0

//! The regen crate provides a library and an command-line tool for generating
//! all strings matching a regular expression.
//!
//! # Generator construction
//!
//! The [Generator struct](struct.Generator.html) is the basic abstraction of
//! the `regen` crate. Use the
//! [Generator::new](struct.Generator.html#method.new) method to create a
//! generator with default options.
//!
//! ```
//! use regen::Generator;
//! let mut gen = Generator::new("[a-z]{2}")?;
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! Use the
//! [Generator::new_with_options](struct.Generator.html#method.new_with_options)
//! method to create a generator with user defined option values provided in
//! the [GeneratorOptions struct](struct.GeneratorOptions.html).
//!
//! ```
//! use regen::{Generator, GeneratorOptions};
//! let opts = GeneratorOptions{max_length: 7};
//! let mut gen = Generator::new_with_options("[a-z]*", &opts)?;
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! # String generation matching a regex
//!
//! Use the [Generator::append_next](struct.Generator.html#method.append_next)
//! method to append the next generated string to an existing `Vec<u8>`.
//!
//! ```
//! use regen::Generator;
//! let mut out = Vec::new();
//! let mut gen = Generator::new("[a-z]{2}")?;
//! while gen.append_next(&mut out).is_some() {
//!     // Process 'out'...
//!     // and possibly out.clear() depending on the scenario.
//! }
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! For a more straightforward, but possibly less efficient way to generate
//! strings, the [Generator struct](struct.Generator.html) also implements the
//! `std::iter::Iterator` trait.
//!
//! ```
//! use regen::Generator;
//! let mut gen = Generator::new("[a-z]{2}")?;
//! for out in gen {
//!     // Process 'out'...
//! }
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```

use regex_syntax::ParserBuilder;
use regex_syntax::hir;
mod appending_iterators;

use crate::appending_iterators::*;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

struct HirVisitor {
    iters: Vec<Box<dyn AppendingIterator>>,
}

impl HirVisitor {
    fn new() -> Self {
        HirVisitor{iters: Vec::new()}
    }
}

impl hir::Visitor for HirVisitor {
    type Output = Box<dyn AppendingIterator>;
    type Err = Box<dyn std::error::Error>;

    fn finish(mut self) -> Result<Self::Output> {
        self.iters.pop().ok_or("invalid regex".into())
    }

    fn visit_post(&mut self, hir: &hir::Hir) -> Result<()> {
        match hir.kind() {
            hir::HirKind::Literal(hir::Literal::Unicode(c)) => {
                self.iters.push(Box::new(CharLiteral::new(c.to_string())));
            },
            hir::HirKind::Literal(hir::Literal::Byte(c)) => {
                self.iters.push(Box::new(ByteLiteral::new(vec![*c])));
            },
            hir::HirKind::Concat(v) => {
                let children = self.iters.drain(self.iters.len()-v.len()..).collect();
                self.iters.push(Box::new(Concatenation::new(children)));
            },
            hir::HirKind::Alternation(v) => {
                let children = self.iters.drain(self.iters.len()-v.len()..).collect();
                self.iters.push(Box::new(Alternation::new(children)));
            },
            hir::HirKind::Repetition(rep) => {
                let (begin, end) =
                    match rep.kind {
                        hir::RepetitionKind::ZeroOrOne => (0, 1),
                        hir::RepetitionKind::ZeroOrMore=> (0, u32::max_value()),
                        hir::RepetitionKind::OneOrMore=> (1, u32::max_value()),
                        hir::RepetitionKind::Range(hir::RepetitionRange::Exactly(n)) => (n, n),
                        hir::RepetitionKind::Range(hir::RepetitionRange::Bounded(b,e)) => (b, e),
                        hir::RepetitionKind::Range(hir::RepetitionRange::AtLeast(n)) => (n, u32::max_value()),
                    };
                let child = self.iters.pop().ok_or("invalid regex")?;
                self.iters.push(Box::new(Repeat::new(child, begin, end)));
            },
            hir::HirKind::Class(hir::Class::Unicode(class)) => {
                let char_ranges =
                    class.iter()
                         .map(|r| CharRange{start: r.start(), len: r.end() as u32 - r.start() as u32 + 1})
                         .collect();

                self.iters.push(Box::new(CharClass::new(char_ranges)));
            },
            hir::HirKind::Class(hir::Class::Bytes(class)) => {
                let byte_ranges =
                    class.iter()
                         .map(|r| ByteRange{start: r.start(), len: r.end() as u16 - r.start() as u16 + 1})
                         .collect();

                self.iters.push(Box::new(ByteClass::new(byte_ranges)));
            },
            hir::HirKind::Empty => {
                self.iters.push(Box::new(Empty::new()));
            },
            _ => {}
        }
        Ok(())
    }
}

/// The options with which to create a `Generator`.
#[derive(Clone)]
pub struct GeneratorOptions {
    /// The maximum number of elements in the generated strings. If the
    /// regex used for string generation is a pure unicode regex then the
    /// maximum length refers to the number of unicode characters
    /// in the generated strings. If the regex is not a pure unicode regex
    /// then the maximum length refers to the number of bytes in the
    /// generated strings.
    pub max_length: usize,
}

impl Default for GeneratorOptions {
    fn default() -> Self {
        GeneratorOptions{max_length: usize::max_value()}
    }
}

/// A generator for strings matching a regular expression
pub struct Generator {
    iterator: Box<dyn AppendingIterator>,
    opts: GeneratorOptions,
}

impl Generator {
    /// Creates a `Generator` with default options.
    ///
    /// # Example
    ///
    /// ```
    /// # use regen::Generator;
    /// let mut gen = Generator::new("[a-z]{2}")?;
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn new<T : AsRef<str> + ?Sized>(s: &T) -> Result<Self> {
        Generator::new_with_options(s.as_ref(), &GeneratorOptions::default())
    }

    /// Creates a `Generator` with the provided options.
    ///
    /// # Example
    ///
    /// ```
    /// # use regen::{Generator, GeneratorOptions};
    /// let opts = GeneratorOptions{max_length: 5};
    /// let mut gen = Generator::new_with_options("[a-c]*", &opts)?;
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn new_with_options<T: AsRef<str> + ?Sized>(
        s: &T, 
        opts: &GeneratorOptions
    ) -> Result<Self> {
        let opts = opts.clone();
        let hir =
            ParserBuilder::new()
                .allow_invalid_utf8(true)
                .build()
                .parse(s.as_ref())?;
        let mut iterator = hir::visit(&hir, HirVisitor::new())?;
        if opts.max_length != usize::max_value() {
            iterator.set_max_elements_hint(opts.max_length);
        }

        Ok(Generator{iterator, opts})
    }

    /// Appends the next generated string to the provided Vec<u8> and
    /// returns the number of elements written.
    ///
    /// Depending on the kind of regular expression, the returned number of
    /// elements may be either a character count (for a pure unicode regex)
    /// or a byte count for a non pure unicode regex.
    ///
    /// # Example
    ///
    /// ```
    /// # use regen::Generator;
    /// let mut out = Vec::new();
    /// let mut gen = Generator::new("[a-c]{2}")?;
    /// while gen.append_next(&mut out).is_some() {
    ///     // Process 'out'...
    ///     out.clear();
    /// }
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn append_next(&mut self, mut out: &mut Vec<u8>) -> Option<usize> {
        if self.opts.max_length == usize::max_value() {
            self.iterator.append_next(&mut out)
        } else {
            self.append_next_limited(&mut out)
        }
    }

    #[inline(never)]
    fn append_next_limited(&mut self, mut out: &mut Vec<u8>) -> Option<usize> {
        let len_before_bytes = out.len();

        while let Some(n) = self.iterator.append_next(&mut out) {
            if n <= self.opts.max_length {
                return Some(n);
            }
            out.resize(len_before_bytes, 0);
        }
        None
    }
}

impl std::iter::Iterator for Generator {
    type Item = Vec<u8>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut out = Vec::new();
        if self.append_next(&mut out).is_some() {
            Some(out)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal() {
        let out: Vec<_> = Generator::new("a").unwrap().collect();
        assert_eq!(out, vec!["a".as_bytes()]);
    }

    #[test]
    fn concat() {
        let out: Vec<_> = Generator::new("aa").unwrap().collect();
        assert_eq!(out, vec!["aa".as_bytes()]);
    }

    #[test]
    fn alternation() {
        let out: Vec<_> = Generator::new("a|b").unwrap().collect();
        assert_eq!(out, vec!["a".as_bytes(), "b".as_bytes()]);
    }

    #[test]
    fn repetition_zero_or_one() {
        let out: Vec<_> = Generator::new("a?").unwrap().collect();
        assert_eq!(out, vec!["".as_bytes(), "a".as_bytes()]);
    }

    #[test]
    fn repetition_range() {
        let out: Vec<_> = Generator::new("a{1,3}").unwrap().collect();
        assert_eq!(out, vec!["a".as_bytes(), "aa".as_bytes(), "aaa".as_bytes()]);
    }

    #[test]
    fn repetition_range_exactly() {
        let out: Vec<_> = Generator::new("a{3}").unwrap().collect();
        assert_eq!(out, vec!["aaa".as_bytes()]);
    }

    #[test]
    fn class() {
        let out: Vec<_> = Generator::new("[a-z]").unwrap().collect();
        let expected: Vec<_> = (b'a'..=b'z').map(|c| vec![c]).collect();
        assert_eq!(out, expected);
    }

    #[test]
    fn empty_returns_nothing() {
        let out: Vec<_> = Generator::new("").unwrap().collect();
        assert_eq!(out.len(), 0);
    }

    #[test]
    fn unfinished_alternation_branch_is_ignored() {
        let out: Vec<_> = Generator::new("(a|b|)").unwrap().collect();
        assert_eq!(out, vec!["a".as_bytes(), "b".as_bytes()]);
    }

    #[test]
    fn byte_dot_all() {
        let out: Vec<_> = Generator::new("(?s-u).").unwrap().collect();
        let expected: Vec<_> = (0..=255).map(|c| vec![c]).collect();
        assert_eq!(out, expected);
    }

    #[test]
    fn max_length() {
        let opts = GeneratorOptions{max_length: 5};
        let out: Vec<_> = Generator::new_with_options("a*b*", &opts).unwrap().collect();
        let mut expected = Vec::new();
        for a in 0..=5 {
            for b in 0..=(5-a) {
                let s = format!("{}{}", "a".repeat(a), "b".repeat(b));
                expected.push(s.as_bytes().to_vec());
            }
        }
        assert_eq!(out.len(), 21);
        assert_eq!(expected, out);
    }

    #[test]
    fn max_length_with_fixed_parts() {
        let opts = GeneratorOptions{max_length: 6};
        let out: Vec<_> = Generator::new_with_options("0a*1b+2", &opts).unwrap().collect();
        let mut expected = Vec::new();
        for a in 0..=2 {
            for b in 1..=(3-a) {
                let s = format!("0{}1{}2", "a".repeat(a), "b".repeat(b));
                expected.push(s.as_bytes().to_vec());
            }
        }
        assert_eq!(out.len(), 6);
        assert_eq!(out, expected);
    }

    #[test]
    fn max_length_with_alternation_branches_of_unequal_length() {
        let opts = GeneratorOptions{max_length: 5};
        let out: Vec<_> = Generator::new_with_options("0(a|bb)*1", &opts).unwrap().collect();
        assert_eq!(out, vec![
            "01".as_bytes(),
            "0a1".as_bytes(), "0bb1".as_bytes(),
            "0aa1".as_bytes(), "0abb1".as_bytes(), "0bba1".as_bytes(),
            "0aaa1".as_bytes()
        ]);
    }
}
