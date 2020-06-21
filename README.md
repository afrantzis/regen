regen
=====

regen is a Rust library and command-line tool for generating all strings
matching a regular expression.

![](https://github.com/afrantzis/regen/workflows/build/badge.svg)

### Documentation

The detailed module documentation, including code examples for all features,
can be found at [https://docs.rs/regen](https://docs.rs/regen).

### Command-line tool usage

```
Usage: regen [options] <pattern>

Options:
    -m, --max-length LENGTH
                        The maximum number of elements (characters or bytes)
                        in the generated strings
    -c, --count         Print out the count of generated strings instead of
                        the strings themselves
```

### Library usage

To use the regen library add the following to your `Cargo.toml`:

```toml
[dependencies]
regen = "0.1"
```

A quickstart example:

```rust
// Uncomment the following when using the older Rust 2015 edition:
// extern crate regen;

use regen::{Generator, Result};

fn main() -> Result<()> {
    let mut out = Vec::new();
    let mut gen = Generator::new("[a-z]{2}")?;
    while gen.append_next(&mut out).is_some() {
        // Process 'out'...
        // and possibly out.clear() depending on the scenario.
    }

    Ok(())
}

```

### License

This project is licensed under the Mozilla Public License Version 2.0
([LICENSE](LICENSE) or https://www.mozilla.org/en-US/MPL/2.0/).
