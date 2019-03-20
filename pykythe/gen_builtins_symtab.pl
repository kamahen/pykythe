% -*- mode: Prolog -*-

%% Post-process the JSON file generated for builtins, to get just the
%% the symtab, and output it.

%% TODO:
%% This needs some "manual" adjustment.
%%   True, False are missing ("keywords" in Python 3, but not in Grammar.txt
%%               nor in builtins.pyi
%%   None is wrong (it gets defined implicitly from builtins.pyi)
%% Extra and missing, depending on Python version
%% -- see https://github.com/python/typeshed/issues/2727
%%    and https://github.com/python/typeshed/issues/2726
%% These are captured in missing/2 and extra/1 at the end of this file.


:- module(gen_builtins_symtab, [gen_builtins_symtab_main/0]).

:- use_module(library(base64), [base64/2]).
:- use_module(library(optparse), [opt_arguments/3]).
:- use_module(module_path).
:- use_module(must_once).
:- use_module(pykythe_utils).

:- ensure_loaded(bootstrap_builtins_symtab).

:- initialization(gen_builtins_symtab_main, main).

%% TODO:
%% These are missing from builtins:
%%   __debug__: bool
%%   __doc__: Optional[str]
%%   __name__: str
%%   __package__: Optional[str]
%% (and ignore __build_class__, __import__, __loader__, __spec__)

gen_builtins_symtab_main :-
    %% set_prolog_flag(color_term, false),        % TODO: remove (to ~/.plrc)
    %% allow --versions but don't use it (for now)
    OptsSpec =
        [[opt(version), type(atom), default(''), longflags(['version']),
          help('Pykythe version, used to validate cache entries')],
         [opt(pythonpath), type(atom), default(''), longflags(['pythonpath']),
          help('Similar to $PYTHONPATH for resolving imports (":"-separated paths)')]],
    opt_arguments(OptsSpec, Opts0, PositionalArgs),
    must_once(split_path_string_and_canonicalize(pythonpath, Opts0, Opts)),
    must_once_msg(PositionalArgs = [KytheInputPath, SymtabOutputPath],
                  'Missing/extra positional args'),
    open(KytheInputPath, read, KytheInputStream),
    read_symtab_from_cache(KytheInputStream, Symtab),
    read_package_from_cache(KytheInputStream, Package),
    atom_concat(Package, '.', PackageDot),
    convdict_pairs(strip_sym(PackageDot), Symtab, BuiltinsPairs),
    convdict(is_module, Symtab, SymtabModules),
    log_if(false, 'Package: ~q~n', [Package]),
    write_atomic_stream(gen_builtins_symtab:write_symtab_fact(
                            Opts, Symtab, BuiltinsPairs, SymtabModules),
                        SymtabOutputPath),
    halt.

%! strip_sym(+PackageDot:atom, +SymType:(atom-atom), -SymStrippedType:(atom-atom))) is det.
%% Strip PackageDot from beginning of Sym (if there), succeed if there
%% are no '.'s in the result or it isn't in extra/1.
strip_sym(PackageDot, Sym-Type, SymStripped-Type) :-
    %% SymStripped = Sym without leading PackageDot (or fail)
    atom_concat(PackageDot, SymStripped, Sym),
    \+ extra(SymStripped),
    %% \+ '.' in SymStripped:
    %%     Can also be written: split_string(SymStripped, '.', '', [_]),
    \+ sub_atom(SymStripped, _Before, _Len, _After, '.').
    %% TODO: maybe remove "local" names (one leading "_" and not "__...___")
    %%   ( \+ atom_concat('_', _, SymStripped);
    %%     ( atom_concat('__', _, SymStripped), atom_concat(SymStripped, '__d', _) ) ).

is_module(Sym-Type, Sym-Type) :-
    memberchk(module_type(_), Type).

write_symtab_fact(Opts, Symtab, BuiltinsPairs, SymtabModules, Stream) :-
    memberchk(version(Version), Opts),
    dict_pairs(BuiltinsDict, symtab, BuiltinsPairs),
    format(Stream, '~k.~n', [builtins_version(Version)]),
    format(Stream, '~k.~n', [builtins_symtab(Symtab)]),
    format(Stream, '~k.~n', [builtins_pairs(BuiltinsPairs)]),
    format(Stream, '~k.~n', [builtins_symtab_modules(SymtabModules)]),
    memberchk(pythonpath(PythonPaths), Opts),
    must_once(full_path([], '$PYTHONPATH/typing', PythonPaths, '', TypingModule0, _)),
    must_once(module_part(TypingModule0, TypingModule)),
    must_once(\+ token_part(TypingModule0, _)),
    must_once(is_resolved_path(TypingModule0)),
    %% TODO: delete the following, which are for eventually adding
    %%       support for mypy-style type declarations
    log_if(false, 'TYPING module: ~q (from ~q)', [TypingModule, TypingModule0]), % TODO: delete
    atomic_list_concat([TypingModule, 'Dict'], '.', TypingDict),
    log_if(false, 'TYPING-Dict: ~q', [Symtab.TypingDict]), % TODO: delete
    do_if(false,
          forall(member(K-V, BuiltinsPairs), % TODO: delete 
                 format('BUILTIN ~q: ~q~n', [K, V]))),
    %% TODO: 'None', 'NoneType', bool, bytes, function
    forall((builtins_symtab_primitive(Primitive, _Type),
            get_dict(Primitive, BuiltinsDict, Builtin)),
           format(Stream, '~k.~n', [builtins_symtab_primitive(Primitive, Builtin)])).

%% The following is a cut-down version of pykythe:read_cache_and_validate.
%% TODO: separate read_cache_and_validate
read_symtab_from_cache(KytheInputStream, SymtabFromCache) :-
    json_read_dict_validate(KytheInputStream, '/pykythe/version', _JsonVersion),
    json_read_dict_validate(KytheInputStream, '/pykythe/text/sha1', _JsonSha1),
    json_read_dict_validate(KytheInputStream, '/kythe/node/kind', JsonPath),
    ensure_dict_fact_base64(JsonPath, fact_value, 'file'),
        json_read_dict_validate(KytheInputStream, '/kythe/text/encoding', _JsonEncoding),
    json_read_dict_validate(KytheInputStream, '/pykythe/symtab', JsonSymtab),
    ensure_dict_fact(JsonSymtab, fact_value, SymtabFromCacheStr),
    %% base64_term(SymtabFromCacheBase64, SymtabFromCache). - Too slow.
    term_string(SymtabFromCache, SymtabFromCacheStr).

read_package_from_cache(KytheInputStream, Package) :-
    my_json_read_dict(KytheInputStream, JsonDict),
    (  JsonDict.fact_name == '/kythe/node/kind',
       base64(package, JsonDict.fact_value)
    -> Package = JsonDict.source.signature,
       ensure_no_more_package_facts(KytheInputStream, Package)
    ;  read_package_from_cache(KytheInputStream, Package)
    ).

ensure_no_more_package_facts(KytheInputStream, Package) :-
    my_json_read_dict(KytheInputStream, JsonDict),
    (  JsonDict == end_of_file
    -> true
    ;  JsonDict.fact_name == '/kythe/node/kind',
       base64(package, JsonDict.fact_value)
    -> throw(error(multiple_package(Package, JsonDict.source.signature), _))
    ;  ensure_no_more_package_facts(KytheInputStream, Package)
    ).

convdict_pairs(Pred, Dict0, Pairs) :-
    dict_pairs(Dict0, _Tag, Pairs0),
    convlist(Pred, Pairs0, Pairs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Edit symtab_builtins to produce a list in symtab_builtins, then
%% run the following Python code:
%%    print('Missing', [b for b in dir(__builtins__) if b not in symtab_builtins])
%%    print('Extra', [b for b in symtab_builtins if b not in dir(__builtins__)])
%% The following are for python3.7:

%% TODO: the following list doesn't take into account the conditional
%%       declarations in builtins.pyi

%% TODO: 'ellipsis' seems a bit strange (see below) ... it doesn't
%%       appear as a builtin name, but <class 'ellipsis'> exists.

%% TODO: 'None': ${TYPESHED_FQN}.stdlib.2.types.NoneType
%%               (and what about Python 3, which doesn't have the 'types' module?)
%%       (Currently, 'None' has type [])

missing('False', [[fqn_type('${TYPESHED_FQN}.stdlib.2and3.builtins.bool')]]).
missing('True',  [[fqn_type('${TYPESHED_FQN}.stdlib.2and3.builtins.bool')]]).
missing('__build_class__', []). % TODO
missing('__debug__', [[fqn_type('${TYPESHED_FQN}.stdlib.2and3.builtins.bool')]]).
missing('__doc__', []). % TODO: Optional[str]
missing('__import__', []). % TODO: Function
missing('__loader__', []). % TODO: Function
missing('__name__',  [[fqn_type('${TYPESHED_FQN}.stdlib.2and3.builtins.str')]]).
missing('__package__', [[fqn_type('${TYPESHED_FQN}.stdlib.2and3.builtins.str')]]).
missing('__spec__', []). % TODO: <class '_frozen_importlib.ModuleSpec'>

extra('ellipsis').  % TODO: this was generated manually

extra('ABCMeta').
extra('AbstractSet').
extra('Any').
extra('AnyStr').
extra('BinaryIO').
extra('ByteString').
extra('Callable').
extra('CodeType').
extra('Container').
extra('Dict').
extra('FrozenSet').
extra('Generic').
extra('IO').
extra('ItemsView').
extra('Iterable').
extra('Iterator').
extra('KeysView').
extra('List').
extra('Mapping').
extra('MutableMapping').
extra('MutableSequence').
extra('MutableSet').
extra('NoReturn').
extra('Optional').
extra('Reversible').
extra('Sequence').
extra('Set').
extra('Sized').
extra('StandardError').
extra('SupportsAbs').
extra('SupportsBytes').
extra('SupportsComplex').
extra('SupportsFloat').
extra('SupportsInt').
extra('SupportsRound').
extra('Text').
extra('TracebackType').
extra('Tuple').
extra('Type').
extra('TypeVar').
extra('Union').
extra('ValuesView').
extra('WindowsError').
extra('abstractmethod').
extra('apply').
extra('basestring').
extra('buffer').
extra('cmp').
extra('coerce').
extra('ellipsis').
extra('execfile').
extra('file').
extra('function').
extra('intern').
extra('long').
extra('mod').
extra('overload').
extra('raw_input').
extra('reduce').
extra('reload').
extra('sys').
extra('unichr').
extra('unicode').
extra('xrange').

