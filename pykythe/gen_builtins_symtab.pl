% -*- mode: Prolog -*-

%% Post-process the JSON file generated for builtins, to get just the
%% the symtab, and output it.

%% If the inputs were general .py files, then we'd need to re-process
%% this to get to a fixed point. But we can cheat a bit because we're
%% handling .pyi files with no expressions, as long as the
%% bootstrap_builtins_symtab.pl file has the right stuff in it.
%% Unfortunately, this is a bit complicated, because of things
%% like "class list(MutableSequence[_T], Generic[_T])" and
%% "class str(Sequence[str], _str_base)" (for Python 3.x, _str_base
%% is object).
%% TODO: compute fixed-point of builtins.

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

:- use_module(library(apply), [convlist/3]).
:- use_module(library(base64), [base64/2 as base64_ascii]).
:- use_module(library(optparse), [opt_arguments/3]).
:- use_module(module_path).
:- use_module(must_once).
:- use_module(pykythe_utils).

:- load_files([bootstrap_builtins_symtab], [silent(true), % TODO: should be a module
                                            imports([builtins_symtab_primitive/2])]).

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

    read_symtab_from_cache(KytheInputStream, Symtab0),
    log_if(true, '~q', [done-read_symtab_from_cache(KytheInputPath)]),

    read_package_from_cache(KytheInputStream, Package),
    log_if(true, '~q', [done-read_package_from_cache(KytheInputPath,Package)]),

    atom_concat(Package, '.', PackageDot),
    convdict_pairs(strip_sym(PackageDot), Symtab0, BuiltinsPairs0),

    %% object is special: it needs the 'object' type whereas for all
    %% other classes, 'object' is implied.
    atom_concat(PackageDot, 'object', ObjectFqn),
    ObjectType = [class_type(ObjectFqn, [])],
    maplist(clean_symtab_pair(ObjectType), BuiltinsPairs0, BuiltinsPairs1),
    replace_key_value(BuiltinsPairs1, 'object', ObjectType, BuiltinsPairs),
    convdict(clean_symtab_pair(ObjectType), Symtab0, Symtab1),
    put_dict('object', Symtab1, ObjectType, Symtab),
    memberchk('object'-ObjectType, BuiltinsPairs),
    log_if(true, 'ObjectType: ~q', [ObjectType]),
    must_once(ObjectType == Symtab.'object'),
    dict_pairs(BuiltinsDict, symtab, BuiltinsPairs),
    convdict(is_module, Symtab, SymtabModules),
    log_if(true, 'Package: ~q', [Package]),
    write_atomic_stream(gen_builtins_symtab:write_symtab_fact(
                            Opts, Symtab, BuiltinsDict, BuiltinsPairs, SymtabModules),
                        SymtabOutputPath),
    log_if(true, 'Finished gen_builtins_symtab'),
    halt.

replace_key_value(List0, Key, Value, List) :-
    ( append(Before, [Key-_|After], List0) -> true ; fail ),
    append(Before, [Key-Value|After], List).

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

write_symtab_fact(Opts, Symtab, BuiltinsDict, BuiltinsPairs, SymtabModules, Stream) :-
    memberchk(version(Version), Opts),
    format(Stream, '~k.~n', [builtins_version(Version)]),
    format(Stream, '~k.~n', [builtins_symtab(Symtab)]),
    format(Stream, '~k.~n', [builtins_pairs(BuiltinsPairs)]),
    format(Stream, '~k.~n', [builtins_symtab_modules(SymtabModules)]),
    memberchk(pythonpath(PythonPaths), Opts),
    must_once(full_path([], 'typing', PythonPaths, '', TypingModule0, _)),
    must_once(module_part(TypingModule0, TypingModule)),
    must_once(\+ token_part(TypingModule0, _)),
    log_if(false, 'TYPING module: ~q (from ~q)', [TypingModule, TypingModule0]), % TODO: delete
    module_file_exists(TypingModule0),
    %% TODO: delete the following, which are for eventually adding
    %%       support for mypy-style type declarations
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
    ensure_dict_fact_base64_ascii(JsonPath, fact_value, 'file'),
        json_read_dict_validate(KytheInputStream, '/kythe/text/encoding', _JsonEncoding),
    json_read_dict_validate(KytheInputStream, '/pykythe/symtab', JsonSymtab),
    ensure_dict_fact(JsonSymtab, fact_value, SymtabFromCacheStr),
    %% base64_term(SymtabFromCacheBase64, SymtabFromCache). - Too slow.
    term_string(SymtabFromCache, SymtabFromCacheStr).

read_package_from_cache(KytheInputStream, Package) :-
    my_json_read_dict(KytheInputStream, JsonDict),
    (  JsonDict.fact_name == '/kythe/node/kind',
       base64_utf8(package, JsonDict.fact_value)
    -> Package = JsonDict.source.signature,
       ensure_no_more_package_facts(KytheInputStream, Package)
    ;  read_package_from_cache(KytheInputStream, Package)
    ).

ensure_no_more_package_facts(KytheInputStream, Package) :-
    my_json_read_dict(KytheInputStream, JsonDict),
    (  JsonDict == @(end)
    -> true
    ;  JsonDict.fact_name == '/kythe/node/kind',
       base64_ascii(package, JsonDict.fact_value)
    -> throw(error(multiple_package(Package, JsonDict.source.signature), _))
    ;  ensure_no_more_package_facts(KytheInputStream, Package)
    ).

clean_symtab_pair(ObjectType, Name-Type, Name-CleanedType) :-
    maplist(clean_type(ObjectType), Type, CleanedType).

clean_type(ObjectType, class_type(Fqn,Bases), class_type(Fqn,CleanedBases)) :- !,
    %% similar to pykythe:clean_class/3 (no need to remove cycles),
    %% and doesn't use global builtins_symtab_primitive/2.
    exclude(is_object_type(ObjectType), Bases, CleanedBases).
clean_type(_ObjectType, Type, Type).

is_object_type(_ObjectType, []) :- !.
is_object_type(ObjectType, ObjectType) :- !.
is_object_type(ObjectType, [ObjectType]) :- !.

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
%%               (and what about Python 3, which doesn't have the 'types' module,
%%               except typeshed does have stdlib/3/types.pyi
%%               See also https://github.com/python/typeshed/issues/2
%%       (Currently, 'None' is defined as an object, which isn't quite right
%%       -- it should be NoneType (which has __bool__ in addition to the methods
%%          that 'object' provides), but that's not defined anywhere.

missing('None', [[fqn_type('${TYPESHED_FQN}.stdlib.2and3.builtins.object')]]).
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

