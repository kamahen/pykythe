# Python code browser

Work-in-progress code for browsing a code repository, using the JSON
encoding of Kythe facts.

It doesn't use any of Javascript's fancy frameworks, and currently works
with statically-generated files in /tmp/pykythe_test/browser.

## Testing

```
    make -C .. test    # Creates /tmp/pykythe_test/KYTHE files
    make -C ..make-js  # Creates /tmp/pykythe_test/browser files
    make -C SRC_BROWSE_PORT=9999 run-src-browser
```

This starts a server on port 9999. You can see it at
[http://localhost:9999](http://localhost:9999).  To terminate the
server, enter `halt.` or just ctrl-D (this will be changed,
eventually; but it's useful for testing).

## Examples

The file `examples/kythe_facts.pl` contains Kythe facts (in Prolog
form) that are generated from the pykythe sources (see Makefile rule
`make-json`). The Makefile rule `run-src-browser` uses this if nothing
has been generated in `/tmp/pykyth_test/browser/files` (e.g.,
by `make test make-json`).


## Miscellaneous

favicon shamelessly taken from https://lsc.saillouisville.org/cropped-favicon-png/#prettyPhoto/0/

## Tables vs div

Probably this could have been done with &lt;div&gt;s and suitable use of CSS
`position`, `float`, etc. But trusty old HTML tables still are
supported, so using them.

## String-ified corpus+root+path and vnames (corpus+root+path+language+signature)

We use a general convention that when a vname is string-ified, its
constituent parts are joined by "`</>`"s. This convention wasn't used
at the beginning: "`/`"s were used instead, with the understanding
that corpus and path shouldn't contain any "`/`"s. The code has been
partially fixed to use "`</>`", but not everywhere.

Just to confuse you, there's also a "semantic hilite" that's added to
URLs, which also uses this convention (except it contains (in addition
to the corpus, root, path for navigation) language and
signature. These names are contained entirely within src\_browser.pl
and are treated as opaque by src\_browser.js. (The various
anchor\_to\_semantics and semantic\_to\_anchors items use these
string-ified vnames.)


## TODO

Add marks on scrollbar that correspond to highlighted text.

Handle Kythe protobufs.
