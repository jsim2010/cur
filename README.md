cur - Your unicode pattern hunting companion.

This module provides a [`Cur`], which can determine if a pattern known as an [`Odor`] matches a
unicode string known as the "search". [`Odor`]s can "mark" any number of parts of their pattern
with [`Name`]s. For each match that a [`Cur`] finds, it can return a [`Catch`] that has
location information known as a [`Find`] for each of the marks of the [`Odor`].

# Examples
```
use cur::*;

// Define an Odor.
odor!(HELLO_WORLD = ["Hello ", .. as name, '!']);

// Create a Cur that will hunt for the Odor.
let mut cur = Cur::new(&HELLO_WORLD);

// Set the search to be hunted.
cur.set_search("Hello Bob!");

// Execute the hunt by requesting information from the Cur.
assert!(!cur.is_clear());

// The Cur iterates on the Catch found.
if let Some(catch) = cur.next() {
    assert_eq!(catch.get("name").map(Find::as_str), Some("Bob"));
} else {
    panic!("Cur did not find catch");
}
```

License: MIT

[`Cur`]: https://docs.rs/cur/latest/cur/struct.Cur.html
[`Find`]: https://docs.rs/cur/latest/cur/struct.Find.html
[`Odor`]: https://docs.rs/cur/latest/cur/struct.Odor.html
