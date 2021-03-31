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
%% This needs some "manual" adjustment (see also builtins_extra.pyi).
%%   True, False are missing ("keywords" in Python 3, but not in Grammar.txt
%%               nor in builtins.pyi
%% Extra and missing, depending on Python version
%% -- see https://github.com/python/typeshed/issues/2727
%%    and https://github.com/python/typeshed/issues/2726
%% These are captured in missing/2 and extra/1 at the end of this file.


:- module(gen_builtins_symtab, [gen_builtins_symtab_main/0]).
:- encoding(utf8).
:- set_prolog_flag(autoload, false).
:- set_prolog_flag(optimise, true).

:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(apply), [convlist/3, exclude/3, maplist/3]).
:- use_module(library(base64), [base64/2 as base64_ascii]).
:- use_module(library(optparse), [opt_arguments/3]).

:- use_module(module_path).
:- use_module(must_once).
:- use_module(pykythe_utils).
:- use_module(pykythe_symtab).

:- load_files([bootstrap_builtins_symtab], [silent(true), % TODO: should be a module
                                            imports([builtins_symtab_primitive/2,
                                                     builtins_module/1 % TODO: needed?
                                                     ])]).

:- initialization(gen_builtins_symtab_main, main).

% TODO:
% These are missing from builtins:
%   __debug__: bool
%   __doc__: Optional[str]
%   __name__: str
%   __package__: Optional[str]
% (and ignore __build_class__, __import__, __loader__, __spec__)

gen_builtins_symtab_main :-
    % set_prolog_flag(color_term, false),        % TODO: remove (to ~/.plrc)
    % allow --versions but don't use it (for now)
    OptsSpec =
        [[opt(version), type(atom), default(''), longflags(['version']),
          help('Pykythe version, used to validate cache entries')],
         [opt(pythonpath), type(atom), default(''), longflags(['pythonpath']),
          help('Similar to $PYTHONPATH for resolving imports (":"-separated paths)')]],
    opt_arguments(OptsSpec, Opts0, PositionalArgs),
    split_atom(Opts0.pythonpath, ':', '', PythonpathList0),
    convlist(maybe_absolute_dir, PythonpathList0, PythonpathList),
    put_dict(pythonpath, Opts0, PythonpathList, Opts),
    must_once_msg(PositionalArgs = [PykytheSymtabInputPath, KytheJsonInputPath, SymtabOutputPath],
                  'Missing/extra positional args'),

    must_once(read_symtab_from_cache_no_check(PykytheSymtabInputPath, Symtab0)),
    log_if(true, '~q', [done-read_symtab_from_cache(PykytheSymtabInputPath)]),

    open(KytheJsonInputPath, read, KytheJsonInputStream, [type(binary)]),
    must_once(read_package_from_cache(KytheJsonInputStream, Package)),
    log_if(true, '~q', [done-read_package_from_cache(KytheJsonInputPath, Package)]),

    atom_concat(Package, '.', PackageDot),
    must_once(conv_symtab_pairs(strip_sym(PackageDot), Symtab0, BuiltinsPairs0)),

    % object is special: it needs the 'object' type whereas for all
    % other classes, 'object' is implied.
    atom_concat(PackageDot, 'object', ObjectFqn),
    ObjectType = [class_type(ObjectFqn, [])],
    maplist(clean_symtab_pair(ObjectType), BuiltinsPairs0, BuiltinsPairs1),
    must_once(replace_key_value(BuiltinsPairs1, 'object', ObjectType, BuiltinsPairs)),
    conv_symtab(clean_symtab_pair(ObjectType), Symtab0, Symtab1),
    symtab_insert('object', Symtab1, ObjectType, Symtab),
    memberchk('object'-ObjectType, BuiltinsPairs),
    log_if(true, 'ObjectType: ~q', [ObjectType]),
    must_once(symtab_lookup('object', Symtab, ObjectType)),
    list_to_symtab(BuiltinsPairs, BuiltinsSymtab),
    conv_symtab(is_module_sym_type, Symtab, SymtabModules),
    log_if(true, 'Package: ~q', [Package]),
    builtins_module(BuiltinsModule),
    write_atomic_stream(write_symtab_fact(
                            Opts, BuiltinsModule, Symtab, BuiltinsSymtab, BuiltinsPairs, SymtabModules),
                        SymtabOutputPath),
    log_if(true, 'Finished gen_builtins_symtab'),
    halt.

%! replace_key_value(+List0, +Key, +Value, -List) is nondet.
%  Succceeds if Key-_ in List0 and replaced with Key-Value; backtracks
%  for more solutions.
replace_key_value(List0, Key, Value, List) :-
    append(Before, [Key-_|After], List0),
    append(Before, [Key-Value|After], List).

%! strip_sym(+PackageDot:atom, +SymType:(atom-atom), -SymStrippedType:(atom-atom))) is det.
% Strip PackageDot from beginning of Sym (if there), succeed if there
% are no '.'s in the result or it isn't in extra/1.
strip_sym(PackageDot, Sym-Type, SymStrippedType) =>
    SymStrippedType = SymStripped-Type,
    % SymStripped = Sym without leading PackageDot (or fail)
    atom_concat(PackageDot, SymStripped, Sym),
    \+ extra(SymStripped),
    % \+ '.' in SymStripped:
    %     Can also be written: split_string(SymStripped, '.', '', [_]),
    \+ sub_atom(SymStripped, _Before, _Len, _After, '.').
    % TODO: maybe remove "local" names (one leading "_" and not "__...___")
    %   ( \+ atom_concat('_', _, SymStripped);
    %     ( atom_concat('__', _, SymStripped), atom_concat(SymStripped, '__d', _) ) ).

%! is_module_sym_type(+SymType, -SymType) is semidet
is_module_sym_type(Sym-Type, SymType) =>
    SymType = Sym-Type,
    memberchk(module_type(_), Type).

write_symtab_fact(Opts, BuiltinsModule, Symtab, BuiltinsSymtab, BuiltinsPairs, SymtabModules, Stream) :-
    format(Stream, '~k.~n', [builtins_version(Opts.version)]),
    format(Stream, '~k.~n', [builtins_module(BuiltinsModule)]), % TODO: needed?
    format(Stream, '~k.~n', [builtins_symtab(Symtab)]),
    format(Stream, '~k.~n', [builtins_pairs(BuiltinsPairs)]),
    format(Stream, '~k.~n', [builtins_symtab_modules(SymtabModules)]),
    must_once(full_path([], 'typing', Opts.pythonpath, '', TypingModule0, _)),
    must_once(module_part(TypingModule0, TypingModule)),
    must_once(\+ token_part(TypingModule0, _)),
    log_if(false, 'TYPING module: ~q (from ~q)', [TypingModule, TypingModule0]), % TODO: delete
    module_file_exists(TypingModule0),
    % TODO: delete the following, which are for eventually adding
    %       support for mypy-style type declarations
    atomic_list_concat([TypingModule, 'Dict'], '.', TypingDict),
    symtab_lookup(TypingDict, Symtab, TypingDictType),
    log_if(true, 'TYPING-Dict: ~q', [TypingDictType]), % TODO: delete
    do_if(false,
          forall(member(K-V, BuiltinsPairs), % TODO: delete
                 format('BUILTIN ~q: ~q~n', [K, V]))),
    % TODO: 'None', 'NoneType', bool, bytes, function
    % TODO: Dict, Map, etc.
    forall((builtins_symtab_primitive(Primitive, _Type),
            symtab_lookup(Primitive, BuiltinsSymtab, Builtin)),
           format(Stream, '~k.~n', [builtins_symtab_primitive(Primitive, Builtin)])).

read_package_from_cache(KytheJsonInputStream, Package) :-
    pykythe_json_read_dict(KytheJsonInputStream, JsonDict),
    (   JsonDict.fact_name == '/kythe/node/kind',
        base64_utf8(package, JsonDict.fact_value)
    ->  Package = JsonDict.source.signature,
        ensure_no_more_package_facts(KytheJsonInputStream, Package)
    ;   read_package_from_cache(KytheJsonInputStream, Package)
    ).

ensure_no_more_package_facts(KytheInputStream, Package) :-
    pykythe_json_read_dict(KytheInputStream, JsonDict),
    (   JsonDict == @(end)
    ->  true
    ;   JsonDict.fact_name == '/kythe/node/kind',
        base64_ascii(package, JsonDict.fact_value)
    ->  throw(error(multiple_package(Package, JsonDict.source.signature), _))
    ;   ensure_no_more_package_facts(KytheInputStream, Package)
    ).

clean_symtab_pair(ObjectType, Name-Type, NameCleanedType) =>
    NameCleanedType = Name-CleanedType,
    maplist(clean_type(ObjectType), Type, CleanedType).

clean_type(ObjectType, class_type(Fqn,Bases), Type) =>
    Type = class_type(Fqn,CleanedBases),
    % similar to pykythe:clean_class/3 (no need to remove cycles),
    % and doesn't use global builtins_symtab_primitive/2.
    exclude(is_object_type(ObjectType), Bases, CleanedBases).
clean_type(_ObjectType, Type0, Type) => Type0 = Type.

is_object_type(_ObjectType, []) => true.
is_object_type(ObjectType, ObjectType) => true.
is_object_type(ObjectType, [ObjectType]) => true.
is_object_type(_, _) => fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Edit symtab_builtins to produce a list in symtab_builtins, then
%% run the following Python code:
%%    print('Missing', [b for b in dir(__builtins__) if b not in symtab_builtins])
%%    print('Extra', [b for b in symtab_builtins if b not in dir(__builtins__)])
%% The following are for python3.7:

% TODO: the following list doesn't take into account the conditional
%       declarations in builtins.pyi

% TODO: 'ellipsis' seems a bit strange (see below) ... it doesn't
%       appear as a builtin name, but <class 'ellipsis'> exists.

% TODO: Some of these are wrong (e.g., None should be NoneType)
%       But right now aren't used anyway.
:- if(false).
missing('None', [[fqn_type('${TYPESHED_FQN}.stdlib.builtins.object')]]).
missing('False', [[fqn_type('${TYPESHED_FQN}.stdlib.builtins.bool')]]).
missing('True',  [[fqn_type('${TYPESHED_FQN}.stdlib.builtins.bool')]]).
missing('__build_class__', []). % TODO
missing('__debug__', [[fqn_type('${TYPESHED_FQN}.stdlib.builtins.bool')]]).
missing('__doc__', []).         % TODO: Optional[str]
missing('__import__', []).      % TODO: Function
missing('__loader__', []).      % TODO: Function
missing('__name__',  [[fqn_type('${TYPESHED_FQN}.stdlib.builtins.str')]]).
missing('__package__', [[fqn_type('${TYPESHED_FQN}.stdlib.builtins.str')]]).
missing('__spec__', []). % TODO: <class '_frozen_importlib.ModuleSpec'>
:- endif.

extra('ellipsis').              % TODO: this was generated manually

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

end_of_file.
