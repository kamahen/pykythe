# Python indexer (for Kythe)

The pykythe package provides code for preprocessing Python source code
into a representation for easy entity cross-referencing, using
entities in the style of the [Kythe](http://kythe.io) indexing
[schema](http://kythe.io/schema).

## License
[Apache 2.0](LICENSE)

## Warning, Avis, Achtung,ご注意

This code is *pre-alpha* and is intended for collaboration with other
people, to create an industrial-strength indexer for Python. The
author intends to make significant changes and improvements to the
code, so if you want to work on it, please contact
<peter.ludemann@gmail.com> first.

## Browsing

Pykythe works with [underhood](https://github.com/TreeTide/underhood)
to allow source code browsing. A rough outline of how to set up the
servers in the Makefile rule `run-underhood-all` (after
running `add-index-pykythe` to set up the serving tables).

## Competition

[Pytype](https://github.com/google/pytype) has an experimental
indexer, which runs at least 10x slower and crashes on some inputs.
(Pykythe does about 100-200 files per minute on a 4 CPU workstation
and can process the entire Python3 library without crashing.) 
Pytype also requires a system such as [Bazel](https://bazel.build)
with associated BUILD files, or ninja, to run in parallel.

## Installation

There is no installation script, because this code is pre-alpha. To
try it out (on Linux):

* `cd` to your top-level source directory (pykythe assumes that all
  sources are in this, including those from other projects such as
  `kythe` and `typeshed`).

* `git clone --recursive https://github.com/google/kythe.git`

* `git submodule update --init --recursive`
  * You should do this each time you do `git pull` on the `pykythe`
    respository. (You can make this automatic by:
    `git config --global alias.update '!git pull && git submodule update --init --recursive'`)

* Follow the instructions at [Kythe - getting
  started](https://github.com/google/kythe#getting-started ) to
  download the latest tarball from the [Kythe
  repository](https://github.com/google/kythe/releases) and follow the
  instructions to copy the binaries into `/opt/kythe`.
  * The `build-kythe` rule in the `pykythe` Makefile will build Kythe from scratch,
    but the rule often breaks with new releases.

* Install `python3.7`

  This needs the *latest* version of Python 3.7. On Ubuntu, you might
  need to first run `sudo add-apt-repository ppa:deadsnakes/ppa`, then
  `sudo apt install python3.7`. More choices are given in
  [this tutorial](https://linuxize.com/post/how-to-install-python-3-7-on-ubuntu-18-04/).

  If you get an error in `DISPATCH[node.type]`, then it probably means
  that there's a conflict with Ubuntu package `python3-lib2to3`. The
  easiest way to fix this is to clone `cprolog` from github and then
  `sudo cp -r --preserve=mode,timestamps cpython/Lib/lib2to3/*
  /usr/lib/python3.7/lib2to3/`. Alternatively, `sudo apt-get install
  2to3 python3-lib2to3 python3-toolz`.

* Install `lib2to3` for Python.

  This might not be needed, depending on the exact state of Python3.x tools

  `sudo apt install python3.7-lib2to3`
  or
  `sudo apt-get install 2to3 python3-lib2to3 python3-toolz`


* Install [SWI-Prolog](http://www.swi-prolog.org/Download.html). You
  need at least version 8.1.4, so as of 2019-03-27, this means
  using the "devel" download or PPA.

  After installing, add the packages
  [edcg](https://github.com/mndrix/edcg) and
  [rdet](https://github.com/rla/rdet) by these commands:

  `echo 'pack_install(edcg, [interactive(false), upgrade(true)]).' | swipl`<br>
  `echo 'pack_install(rdet, [interactive(false), upgrade(true)]).' | swipl`

  Check that you have the correct versions (`echo 'forall(pack_property(N, version(V)), writeln(N:V)).' | swipl`):
  * rdet 1.0.0
  * edcg 0.9.0

* Optional (htis is now a submodule of `ptype`):
  `git clone https://github.com/python/typeshed.git`

* Optional:

  * Install `mypy` and `pytype` (using `pip`, or by cloning the git
    repository, `cd`-ing into it, then running `sudo -H pip3 install
    --upgrade .`) (`pytype` is special -- see its installation
    instructions).
    * You might need to symlink `mypy_extensions` into
      `/usr/local/lib/python3.7/dist-packages`.

* Optional:

	* `git clone https://github.com/google/pytype`
	* `git clone https://github.com/python/mypy.git`
	* `git clone https://github.com/google/yapf.git`

* `make -C <pkgdir> clean_lite test`

* You can see the generated facts in
  `/tmp/pykythe_test/KYTHE/pykythe/test_data/*.json-decoded`
  (requres running `scripts/decode_json.py` -- see `Makefile` rule
  `json-decoded-all`).

* Run the source browser by:

  * `make -C <pkgdir> make-json`
  * `make -C <pkgdir> SRC_BROWSER_PORT=9999 run-src-browser`
  * http://localhost:9999 to browse the Kythe facts created by the `test` rule and
  which were processed into a database for the browser by `make-json`.

* --If you wish to do a quick look at the source browser with
  already defined data (in `browser/examples/kythe_facts.pl`):--
  (Removed because the file became over 100MB.)

  * --`mkdir /tmp/pykythe_test`--
  * --`make -C <pkgdir> SRC_BROWSER_PORT=9999 run-src-browser`--
  * --http://localhost:9999 as above--

* Run the Kythe browser by:

* `make -C <pkgdir> add-index-pykythe
  This creates `/tmp/pykythe_test/tables`, which can be used by
  the Kythe browser:

  `make -C <pkgdir> run-server`

  You can look run the browser: http://localhost:8080

* Run the `underhood` browser by cloning `https://github.com/TreeTide/underhood.git`
  and running the commands in `run-underhood-all` (

* You can run `scripts/test3.sh` to see how the system works both with
  processing from source or by reusing the cache (or a combination).
  Note that this has some extra pre-processing steps that aren't needed
  for ordinary use.

## Coding conventions

### Code formatting

All the Python code is formatted using `yapf` configured with
`.style.yapf`.  You can either install it from
[github](https://github.com/google/yapf) or using
[pip](https://pypi.python.org/pypi/yapf).  The `Makefile` has a rule
`pyformat` that formats everything.

Prolog code is formatted according to the recommendations in
[plcoding.pdf](http://www.covingtoninnovations.com/mc/plcoding.pdf)
with an extension for EDCGs that shows the accumulators.

### Type declarations

The code is processed with `mypy` (using the `Makefile` rule `mypy`)
and `pylint`. It is intended to also be processed by `pytype`.

  * Currently this doesn't work because `mypy` doesn't support Python
    3.7 `dataclasses`

## Implementation notes

### Processing a single source file

A source file is processed in the following steps:

* `pykythe.pl` invokes a Python program to create an AST with fully
  qualified names:

  * `ast_raw.parse` generates an AST in `lib2to3.pytree`'s
    form. (Actually, this is more like a concrete syntax tree, which
    is why the next step is done.)

  * `ast_raw.cvt_parse_tree` converts the "raw" AST to a more
    convenient "cooked" form.

  * `ast_cooked.Base.add_fqns` traverses the "cooked" AST to fill in
    as many FQNs as possible (most names can be resolved, but those
    from builtins or `from … import *` cannot be known at this
    point (builtins are in effect `from builtins import *`).

  * The resulting AST (with FQNs) is serialized and passed back to
    `pykythe.pl`.

* A pass is made over the AST, generating Kythe anchors and a list of
  "expressions" in the AST.

* Each "import" is recursively processed (if there is a circular import,
  this is detected and the recursive import is skipped).
  * This includes outputting the `.kythe.json`, `.kythe.entries`, and
    `.pykythe.symtab` files.  (The `.pykythe.symtab` file can be
    reused as a "cache" to avoid reprocessing the source file.)

* The expressions are symbolically evaluated to fill in the
  symtab. This is, in effect, simple type inferencing or abstract
  interpretation (for example, resolving a function call to a class
  constructor, then applying the "dot" operator to determine the
  attribute's type).

  * When a symtab entry is updated with additional information, it is
    recorded in "rejected" list.

  * If the "rejected" list is non-empty after symbolically evaluating
    the expressions, the process is repeated. Generally, no more than
    3 passes are needed to incorporate all the information.

  * While symbolically evaluating the expressions, additional Kythe
    facts can be generated; for example, attributes (after a `.`
    operator) can be resolved to the appropriate class
    attribute/method or module variable/function.

* The symtab and Kythe facts are output to `pykythe.symtab`,
  `.kythe.json` file, and `.kythe.entries` files.  The test cases can
  be checked with `kythe/cxx/verifier/verifier`.


### <a name="symtab">Symtab</a>

A fully qualified name is the absolute path to an abstract entity,
using '.'s to separate the path items.

The symbol table ("symtab") is a mapping of fully qualified names
(FQNs) to their "types". (The word "type" is used a bit loosely;
essentially it is a term such as `class_type(FQN, Bases)` or
`module_type(module_and_token(Module,Path,Token))` This is different
from the usual definition of symtab, which maps a single ID to to a
type and where there is a "stack" of IDs of the various scopes (the
FQN resolution is done in two passes in the Python program that parses
the source and outputs an AST).

For example, in file `foo/bar.py`:

    import os
    sep = os.sep

the symtab would contain entries like the following (note that `os`
results in both a definition in the namespace of module `foo.bar` and
a reference to the imported module in typeshed).

    'foo.bar.os': module_type(module_alone(
        '${FQN_TYPESHED}.os',
        '${FQN_TYPESHED}/stdlib/3/os/__init__.pyi'))
    '.foo.bar.sep': class_type('${FQN_TYPESHED}.stdlib.2and3.builtins.str', [])
    '${FQN_TYPESHED}.os': module_type(module_alone(
        '${FQN_TYPESHED}.os',
        '${FQN_TYPESHED}/stdlib/3/os/__init__.pyi'))
    '${FQN_TYPESHED}.os.sep': class_type('${FQN_TYPESHED}.stdlib.2and3.builtins.str', [])

A symtab is self-contained: there is no need to import the symtabs of
modules that it imports. However, the imports' symtabs need to be
checked (recursively) to ensure that they are up-to-date; if any of
the imports's symtabs is out of date, the entire chain of symtabs
need to be recomputed.

When an "import *" is processed, the symtab entries that start with
the module name are added to the importing module's symtab (e.g., if
`foo.py` has `from bar import *`, then all symtab entries that start
with `path.to.bar` are added to the symtab, with the `path.to.bar`
replaced by `path.to.foo`).

Builtins are added to the "scope" of each program, as if there were a
`from builtins import *` added to each program.  This is not how
Python actually resolves builtin name lookup, but the effect is
essentially the same (with a few corner cass that probably don't
matter for real programs; after all pykythe can't figure out
everything anyway, because of Python's dynamic nature). In this way,
we avoid having a stack of symtabs.

### Imports, cache, and batches

When a source file is processed, all the imports must be processed
first. Pykythe doesn't depend on information about dependencies from a
build system (which typically aren't available for Python, in
comparison toC++ or similar, which have dependency information in a
build system or compiler (e.g.,
[gcc-M](https://www.gnu.org/software/make/manual/html_node/Automatic-Prerequisites.html)),
although [importlab](https://github.com/google/importlab) could be
used. Additionally, there can be circular imports, which prevent a
strict ordering of dependencies. (Python doesn't directly allow
circular imports; but they can be simulated by putting `import`
statements inside functions, so that they are evaluated at run time
rather than when the module is first compiled.)

Pykythe processes each `import` statement as it is encountered.
Circular imports are handled by keeping track of all imports that are
"in process" and skipping them when they are encountered a second time
(more details on this are in the [Symtab](#symtab) section).

When processing a codebase, reprocessing the imports cann easily get
<i>O(N<sup>3</sup>)</i> behavior (where <i>N</i> is the number of
lines of code), so a cache is used to avoid this. For each `.py` or
`.pyi` file, a corresponding `.kythe.json` file (and `.kythe.entries`)
is created, containing all the Kythe facts; the the pykythe symtab and
a hash of the source file go into a `.pykythe.symtab` file. When an
`import` is encountered, the cache file is used if possible:

* The cache file must have been created using the same source file.

* All imports (recursively) must have useable cache files.

If any of the imported cache files is not useable, all source files
that depend on it must be reprocssed to generate new cache files (that
is, if `a.py` imports `b.py`, `b.py` imports `c.py`, and `c.py`
imports `d.py`; and `d.pykythe.symtab` is useable but `c.pykythe.symtab` is
not (because `c.py` has changed since `c.pykythe.symtab` was created),
then `c.py`, `b.py`, and `a.py` must be reprocessed (`d.py` can be
reused as-is).

This kind of caching reduces the algorithm to <i>O(M<sup>2</sup>)</i>
(where <i>M</i> is the number of files), which is still a problem. A
further refinement reduces the cost to <i>O(N)</i>. Assuming that we
can snapshot the code base, then a single pass over all the source
files suffices, if we can determine an appropriate ordering. We don't
need to determine that order if instead we can mark each cache file as
being valid. We also want to allow processing in parallel. To handle
this, each processing run is given a unique ID (e.g., a nano-second
time stamp plus a random number)

To sum up, pykythe uses the `.pykythe.symtab` files as caches.  To
void recursively checking cache files, it uses
`.pykythe-batch-$BATCH_ID` files (using the `--batch_suffix`
command-line option).

Multiple pykythe processes can run at the same time; all output is
"atomic", as long as it's all on the same file system. (That is, if
the same file is being processed by two processes at the same time,
their outputs won't interfere with each other, because they should
both have the same content and they are written atomically.)

(A detail: to allow for read-only source trees, the cache files are in
a separate directory, as specified by `--kytheout`. This could in
future be generalized by specifying patterns for transforming an input
file to an output file, e.g.
`kytheout_pattern='s!/stuff/to/remove/(.*/)([^/]*)\..*}!/path/to/dir/\\1\\2.kythe.json!'`
... this probably should allow multiple patterns, to accomodate files
in multiple repositories.)

To give an idea of performance improvements, when processing the 7071
files in the Python library (this is total CPU time; wall time was roughly
1/3 on a 4 CPU machine (with SSD, so the affects of cache are less
than with HDD) running up to 8 pykythe processes in parallel):

* Initial run: 69 minutes (using "batch" caching for newly created
  files -- without this caching, performance would have been much worse).

* With regular caching and without batch files from the first run: 33
  minutes.

* With batch files from the first run: 5 minutes.


### Base64

For the JSON version of Kythe facts, base64 encoding is used, to
handle the some data being pure binary and not UTF8 (e.g., if the
`/kythe/text` fact is in Latin-1 encoding, which is not legal
UTF-8). Unfortunately, Prolog's base64 support isn't super fast, nor
is its UTF-8 encoding/decoding (the latter is needed because base64
works on ASCII only, so encoding/decoding from Unicode is needed).

For performance reasons, the Kythe facts are all crated as regular
strings (or atoms), and converted to base64 only on output. This is
because each pass over the AST creates Kythe facts, and there's a
significant saving by only doing the base64 translation once. (Some
micro-optimizations are possible by distinguishing between strings
that can only be ASCII and those that can be any Unicode; these
probably aren't worth doing and things are simpler by just assuming
UTF8 everywhere).

## Known issues

* See also https://github.com/kamahen/pykythe/issues

* Needs more documentation.

* Needs *many* more test cases.

* Needs proper code review.

* Only works with UTF-8 files (actually, only ASCII), with Unix
  newline conventions.

  * 8859-1 (Latin1) may cause bogus error messages.

* Performance: don't put symtab into JSON (`/pykythe/symtab`), but
  instead write directly to a separate file (search for comments in
  `pykythe.pl` about using `fast_read/2` etc).

* Only tested with Python 3 source (probably works with Python 2, with
  a bit of fiddling for things like `print` statements and (when
  implemented) some details of name scope, such as for list
  comprehensions).

  * Doesn't yet handle Python 3.8 "walrus" operator.

* Requires Python 3.7
   * On Ubuntu: `sudo apt-get install python3.7`
   * You also might have to do something like this: `cd /usr/lib/python3/dist-packages &&
sudo ln -s apt_pkg.cpython-36m-x86_64-linux-gnu.so apt_pkg.cpython-37m-x86_64-linux-gnu.so`

* Requires Python 3.7 `2to3`
   * (See above with "Install `lib2to3` for Python", although that
     might not be needed any more.)

* Requires `mypy_extensions`:
   * `python3.7 -m pip install mypy_extensions`

* Outputs JSON and uses `entrystream --read_format=json` to convert
  to the form that `write_tables` expects (it would be more efficient
  to output protobufs directly).

* Packaging of pykythe is incomplete and possibly wrong.
