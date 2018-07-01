# Python indexer (for Kythe)

The pykythe package provides code for preprocessing Python source code
into a representation for easy entity cross-referencing, using
entities in the style of the [Kythe](http://kythe.io) indexing
[schema](http://kythe.io/schema).

## Warning, Avis, Achtung,ご注意

This code is *pre-alpha* and is intended for collaboration with other
people, to create an industrial-strength indexer for Python. The
author intends to make significant changes and improvements to the
code, so if you want to work on it, please contact
<peter.ludemann@gmail.com> first.

## Installation

There is no installation, because this code is pre-alpha. To try it
out (on Linux):

* `cd` to your top-level source directory (pykythe assumes that all
  sources are in this, including those from other projects such as `kythe`
  and `typeshed`).

* `git clone https://github.com/google/kythe.git`

* Follow the instructions for building `kythe` (including installing Bazel).

* Following the instructions at
  [Kythe - getting started](https://github.com/google/kythe#getting-started )
  to download the latest tarball from the
  [Kythe repository](https://github.com/google/kythe/releases) and copy the binaries
  into `/opt/kythe`.

* Install `python3.7`

* Install [SWI-Prolog](http://www.swi-prolog.org/Download.html). You
  need at least version 7.7.13 (earlier versions have a bug in how
  they handle command line arguments), so as of 2018-06-08, this means
  using the "devel" download or PPA.

  After installing:

  * [edcg](https://github.com/mndrix/edcg):
    ```
    echo 'pack_install(edcg).' | swipl
    ```

* Install (using `pip`) `dataclasses`. (This is a backport
  of `dataclasses` from Python 3.7.)

* Install `mypy` and `pytype` (using `pip`, or by cloning the git
  repository, `cd`-ing into it, then running `sudo -H pip3 install
  --upgrade .`) (`pytype` is special -- see its installation
  instructions).
  * You might need to symlink `mypy_extensions` into
    `/usr/local/lib/python3.7/dist-packages`.

* Optional:

	* `git clone https://github.com/python/typeshed.git`
	* `git clone https://github.com/google/pytype`
	* `git clone https://github.com/python/mypy.git`
	* `git clone https://github.com/google/yapf.git`

* `make -C <pkgdir> all_tests`

* You can see the generated facts in `/tmp/pykythe_test/py3_test_grammar.json-decoded`

## Code formatting

All the Python code is formatted using `yapf` configured with `.style.yapf`.
You can either install it from [github](https://github.com/google/yapf)
or using [pip](https://pypi.python.org/pypi/yapf).
The `Makefile` has a rule `pyformat` that formats everything.

Prolog code is formatted according to the recommendations in
[plcoding.pdf](http://www.covingtoninnovations.com/mc/plcoding.pdf)
with an extension for EDCGs that shows the accumulators.

## Type declarations

The code is processed with `mypy` (using the `Makefile` rule `mypy`) and
`pylint`. It is intended to also be processed by `pytype`.


## Known issues

* Does not process the generated facts into a form that Kythe's `http_server`
  can properly display, so `http_server` cannot be used to interactively
  validate the generated facts. (The documentation at kythe.io seems to be
  out of date on how to post-process the facts for use by `http_server`.)

* Does not handle Python `import` statements.

* Doesn't know anything about builtins (`typeshed/stdlib` should be
  processed as a kind of preamble).

* Analysis and output is limited to `ref` and `defines/binding` facts
  for local and global variables.

* Needs more documentation.

* Needs *many* more test cases.

* Needs proper code review.

* Only works with UTF-8 files (actually, only ASCII), with Unix
  newline conventions.

* Only tested with Python 3 source (probably works with Python 2, with
  a bit of fiddling for things like `print` statements and (when
  implemented) some details of name scope, such as for list
  comprehensions).

* Requires Python 3.7

* Outputs JSON and expects `entrystream --read_json` to convert to the
  form that `write_tables` expects (it would be more efficient to
  output protobufs directly).

* Packaging of pykythe is incomplete and possibly wrong.
