// Copyright 2020 Alexandros Frantzis
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
// SPDX-License-Identifier: MPL-2.0

use regen::{Generator, GeneratorOptions};
use std::io::{self, Write};
use getopts::Options;

struct Opts {
    max_length: Option<usize>,
    count: bool,
    pattern: String,
}

fn print_usage(opts: &Options) {
    let brief = "Usage: regen [options] <pattern>";
    print!("{}", opts.usage(&brief));
}

fn parse_opts() -> Opts {
    let args: Vec<String> = std::env::args().collect();
    let mut opts = Options::new();
    opts
        .optopt(
            "m",
            "max-length",
            "The maximum number of elements (characters or bytes) in the generated strings",
            "LENGTH"
        )
        .optflag(
            "c",
            "count",
            "Print out the count of generated strings instead of the strings themselves"
        );

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(_) => {
            print_usage(&opts);
            std::process::exit(1);
        }
    };

    if matches.free.len() != 1 {
        print_usage(&opts);
        std::process::exit(1);
    }

    Opts {
        max_length: matches.opt_get::<usize>("max-length").unwrap(),
        count: matches.opt_present("count"),
        pattern: matches.free[0].clone(),
    }
}

fn main() {
    let opts = parse_opts();

    let mut gen_opts = GeneratorOptions::default();
    if let Some(max_length) = opts.max_length {
        gen_opts.max_length = max_length;
    }

    let mut gen =
        match Generator::new_with_options(&opts.pattern, &gen_opts) {
            Ok(gen) => gen,
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        };

    let mut out = Vec::new();

    if opts.count {
        let mut i = 0;
        while gen.append_next(&mut out).is_some() {
            out.clear();
            i += 1;
        }
        println!("{}", i);
    } else {
        let stdout = io::stdout();
        let mut handle = stdout.lock();

        while gen.append_next(&mut out).is_some() {
            out.push(b'\n');
            if out.len() >= 8192 {
                handle.write_all(&out).unwrap();
                out.clear();
            }
        }

        handle.write_all(&out).unwrap();
        handle.flush().unwrap();
    }
}
