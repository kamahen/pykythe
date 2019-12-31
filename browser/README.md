# Python code browser

Work-in-progress code for browsing a code repository, using the JSON
encoding of Kythe facts.

It doesn't use any of Javascript's fancy frameworks, and currently works
with statically-generated files in /tmp/pykythe_test/browser.

See ../Makefile rule `make-js`.

## Testing

This code currently only works with "static" files, and dynamically loading
them gets messed up by CORS (Cross-Origin Resource Sharing).

The easiest way to deal with this is:
* start Firefox (not Chrome)
* `about:config`
* navigate to `security.fileuri.strict_origin_policy`
* set to `false`

Don't forget to set this back to `true` when you're done testing.

It's possible that this works (but it didn't for me):
```
mkdir /tmp/chrome-cors
google-chrome --disable-web-security --allow-file-access-from-files --user-data-dir=/tmp/chrome-cors
```

"Soon", there'll be a server version of the browser, which will avoid the problem.
But one thing at a time.

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
