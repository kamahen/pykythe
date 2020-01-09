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
`http://localhost:9999/static/src_browser.html`


## Miscelaneous

favicon shamelessly taken from https://lsc.saillouisville.org/cropped-favicon-png/#prettyPhoto/0/

## Tables vs div

Probably this could have been done with <div>s and suitable use of CSS
`position`, `float`, etc. But trusty old HTML tables still are
supported, so using them.

## TODO

Add marks on scrollbar that correspond to highlighted text.

Add a real server (to get around CORS issues).

Handle Kythe protobufs.

Get rid of `line_keys` - `lines` becomes a 0-origin array and access is by `lineno - 1`.
