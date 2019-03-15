% -*- mode: Prolog -*-

%% Module/path manipulation routines.

%% '.' in a directory name causes problems
%%    ... e.g. /usr/lib/python3.7/test/support/__init__.py
%%    doesn't roundtrip:
%%             /usr/lib/python3/7/test/support/__init__.py
%%                             ^ <====
%%    so change all '.'s to ':' -- this is generally safe
%%    because --pythonpath uses ':' to separate items.
%%    (see fix_dot/2 and unfix_dot/2 and where they're used).
:- module(module_path, [
                          canonical_path/2,
                          full_module_part/2,
                          full_module_pieces/2,
                          full_path_prefixed/5,
                          module_part/2,
                          module_path/2,
                          path_expand/3,
                          path_part/2,
                          path_to_python_module/2,
                          path_to_python_module_or_unknown/2,
                          pythonpath_prefix/2,
                          src_base/2,
                          token_part/2
                       ]).

:- use_module(library(apply), [maplist/3]).
:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(pcre), [re_matchsub/4, re_replace/4]).
:- use_module(must_once, [must_once/1, must_once_msg/2, must_once_msg/3, fail/1]).
:- use_module(pykythe_utils).

:- style_check(+singleton).
:- style_check(+var_branches).
:- style_check(+no_effect).
:- style_check(+discontiguous).
%% :- set_prolog_flag(generate_debug_info, false).


%! module_path(+Module:atom, -Path:atom) is nondet.
%! module_path(-Module:atom, +Path:atom) is nondet.
%% Convert a module ('path.to.module') to a path
%% ('path/to/module.py').  Does not check for existence of the file,
%% nor apply any PythonPath addition. Backtracks through all
%% solutions. At least one of Module and Path must be instantiated.
%% TODO: harmonize this with path_to_python_module / path_to_python_module_or_unknown.
module_path(Module, Path) :-
    (  var(Module)
    -> py_ext(Path0, Path2),
       simple_path_module(Path0, Module)
    ;  simple_path_module(Path0, Module),
       py_ext(Path0, Path2)
    ),
    canonical_path(Path2, Path).

%! simple_path_to_module(+Path, -Module) is det.
%! simple_path_to_module(-Path, +Module) is det.
%% TODO: use library(pcre) re_replace?
simple_path_module(Path, Module) :-
    (  var(Module)
    -> split_path_to_module_parts(Path, ModuleParts),
       atomic_list_concat(ModuleParts, '.', Module)
    ;  split_module_atom(Module, ModuleParts),
       atomic_list_concat(ModuleParts, '/', Path)
    ).

split_path_to_module_parts(Path, ModuleParts) :-
    split_atom(Path, '/', '', ModuleParts0),
    maplist(fix_dot, ModuleParts0, ModuleParts).

fix_dot(In, Out) :-
    re_replace('\\.'/g, ':', In, Out0),
    atom_string(Out, Out0).

unfix_dot(In, Out) :-
    re_replace(':'/g, '.', In, Out0),
    atom_string(Out, Out0).

%! path_to_python_module_or_unknown(+Path, -Fqn) is det.
%% Get the Fqn for the Python module corresponding to Path or
%% a '<unknown>...' atom.
%% TODO: harmonize with path_module/2.
path_to_python_module_or_unknown(Path, Fqn) :-
    (  path_to_python_module(Path, Fqn)
    -> true
    ;  %% Make sure the result conforms with FQN dotted name, so that
       %% match_reversed_module_and_dotted_names/3 works properly
       split_path_to_module_parts(Path, PathParts),
       (  PathParts = ['$PYTHONPATH'|PathParts2]
       -> atomic_list_concat(['<unknown>'|PathParts2], '.', Fqn)
       ;  PathParts = [''|PathParts2]
       -> atomic_list_concat(['<unknown>'|PathParts2], '.', Fqn)
       ;  atomic_list_concat(['<unknown>'|PathParts], '.', Fqn)
       )
    ).

%! path_to_python_module(+Path, -Fqn) is semidet.
%% Get the Fqn for the Python module corresponding to Path or fail.
%%  TODO: harmonize with module_path/2
path_to_python_module(Path, Fqn) :-
    canonical_path(Path, CanonicalPath),
    py_ext(CanonicalPath0, CanonicalPath),
    simple_path_module(CanonicalPath0, Fqn).

%! canonical_path(+Path, -CanonicalPath) is semidet.
%% Get a Path (file or directory) into a canonical (absolute) form.
%% Fails if the file or directory doesn't exist.
canonical_path(Path, CanonicalPath) :-
    (  absolute_file_name(Path, AbsPath, [access(read), file_errors(fail)])
    -> true
    ;  absolute_file_name(Path, AbsPath, [access(read), file_type(directory), file_errors(fail)])
    ),
    atom_string(CanonicalPath, AbsPath).

%! full_path_prefixed(+Path, +DeprefixedPath, +Prefixes:list, ?Module, -ModuleAndMaybeToken) is det.
%% ModuleAndMaybeToken is either module_alone or module_and_token functor.
%% Module is logical variable that gets filled in later
full_path_prefixed(Path, DeprefixedPath, Prefixes, Module, ModuleAndMaybeToken) :-
    (  member(Prefix, Prefixes),
       atom_concat(Prefix, DeprefixedPath, Path0),
       path_expand(Path0, Module, ModuleAndMaybeToken)
    -> true
    ;  Prefix = '$PYTHONPATH/',
       %% Can't path_expand because we don't know what prefix to use.
       ModuleAndMaybeToken = module_alone(Module, Path)
    ),
    path_part(ModuleAndMaybeToken, PathPart),
    must_once(atom_concat(Prefix, _, PathPart)), % Prefix must be first part of PathPart
    must_once(atom_concat(_, '/', Prefix)).      % Prefix must end with '/'.

%! path_expand(+Path0, +Module, -ModuleAndMaybeToken) is semidet.
%% ModuleAndMaybeToken is either module_alone or module_and_token functor.
%% Module is logical variable that gets filled in later
path_expand(Path0, Module, ModuleAndMaybeToken) :-
    (  Path1 = Path0,
       ModuleAndMaybeToken = module_alone(Module, Expanded)
    ;  remove_last_component(Path0, Path1, Token),
       ModuleAndMaybeToken = module_and_token(Module, Expanded, Token)
    ),
    py_ext(Path1, Path),
    canonical_path(Path, Expanded).

%! remove_last_component(+Path, -FirstPart, -Tail) is det.
%% e.g.: 'foo/bar/zot', 'foo/bar', zot
remove_last_component(Path, FirstPart, Tail) :-
    (  split_atom(Path, '/', '', Split),
       Split = [_,_|_],         % at least two components
       append(FirstPathPieces, [Tail], Split)
    -> atomic_list_concat(FirstPathPieces, '/', FirstPart)
    ).

%! pythonpath_prefix(+Full: atom, -Rest: atom) is semidet.
%! pythonpath_prefix(-Full: atom, +Rest: atom) is semidet.
pythonpath_prefix(Full, Rest) :-
    atom_concat('$PYTHONPATH/', Rest, Full).

%! path_part(+ModuleAndMaybeToken, -Path) is det.
%% Extract Path from ModuleAndMaybeToken.
path_part(module_alone(_Module,Path), Path).
path_part(module_and_token(_Module,Path,_Token), Path).
path_part(module_star(_Module,Path), Path).

%! module_part(+module_alone(ModuleAndMaybeToken, -Path), Module) is det.
%% Extract Module from ModuleAndMaybeToken. Any Token is ignored.
module_part(module_alone(Module,_Path), Module).
module_part(module_and_token(Module,_Path,_Token), Module).
module_part(module_star(Module,_Path), Module).

%! full_module_part(+module_alone(ModuleAndMaybeToken, -Path), Module) is det.
%% Extract Module from ModuleAndMaybeToken.
%% If there is a Token, it is added to the Module name.
full_module_part(module_alone(Module,_Path), Module).
full_module_part(module_and_token(Module,_Path,Token), ModuleDotToken) :-
    atomic_list_concat([Module, Token], '.', ModuleDotToken).
%% Shouldn't happen:
%% full_module_part(module_star(Module,_Path), Module).

%! token_part(+module_alone(ModuleAndMaybeToken, -Token), Token) is semidet.
%% If ModuleAndMaybeToken has a Token, get it (else fail).
token_part(module_and_token(_Module,_Path,Token), Token).

%! full_module_pieces(+ModuleAndMaybeToken, -ModulePieces:list(atom)) is det.
%% Extract Module (as a list of pieces) from ModuleAndMaybeToken.
full_module_pieces(module_alone(Module,_Path), ModulePieces) :-
    split_module_atom(Module, ModulePieces).
full_module_pieces(module_and_token(Module,_Path,Token), ModulePieces) :-
    split_module_atom(Module, ModulePieces0),
    append(ModulePieces0, [Token], ModulePieces).
full_module_pieces(module_star(Module,_Path), ModulePieces) :-
    split_module_atom(Module, ModulePieces0),
    append(ModulePieces0, ['*'], ModulePieces). % TODO: need something better here

%! split_module_atom(+Module:atom, -ModulePieces:list(atom) is det.
%% Split module into pieces on '.', with special handling for '<unknown>'
split_module_atom(Module, ModulePiecesFixed) :-
    (  re_matchsub('<unknown>\\.{([^}]*)}$', Module, Sub, [anchored(true)])
    -> %% filename is in absolute form, so the first part of the result
       %% is ''. If we want to get rid of that, then the following should have
       %% [''|ModulePieces0] as the last arg:
       split_atom(Sub.1, '/', '', ModulePieces0),
       ModulePieces = ['<unknown>'|ModulePieces0]
    ;  re_matchsub('(<unknown>\\.{[^}]*})\\.(.*)$', Module, Sub, [anchored(true)])
    -> %% TODO: does this situation ever arise?
       split_atom(Sub.2, '.', '', ModulePieces2),
       ModulePieces = [Sub.1|ModulePieces2]
    ;  split_atom(Module, '.', '', ModulePieces)
    ),
    maplist(unfix_dot, ModulePieces, ModulePiecesFixed).

%! py_ext(+Path:atom, -PathBase:atom is nondet.
%! py_ext(-Path:atom, +PathBase:atom is nondet.
%% Path unifies with all permutations of PathBase plus {.py,.pyi} and
%%  __init__ equivalents and does not check for existence.
py_ext(PathBase, Path) :-
    %% TODO: verify order of testing which file(s) exists.
    %% TODO: allow more than one "hit" (e.g., if there are both a .py and .pyi,
    %%       then use the .pyi to get type info for the .py and possibly create
    %%       kyanchors in both)
    py_ext_ext(Ext),
    atom_concat(PathBase, Ext, Path),
    %% for 'foo/__init__.py', only return 'foo' and not 'foo/__init__':
    \+ atom_concat(_, '__init__', PathBase).

%! py_ext_ext(-Extension:atom) is nondet.
%% "extensions" to append to a module to get a file
%% file_name_extension/3 adds a '.', so can't use for /__init__.*
%% TODO: allow more than .py and .pyi as extensions?
py_ext_ext('.py').
py_ext_ext('.pyi').
py_ext_ext('/__init__.py').
py_ext_ext('/__init__.pyi').

%! src_base(+SrcPath: atom, -SrcPathBase) is det.
%% Remove extension (.py, .pyi) from a source path.
src_base(SrcPath, SrcPathBase) :-
    (  atom_concat(SrcPathBase, '.py', SrcPath)
    -> true
    ;  atom_concat(SrcPathBase, '.pyi', SrcPath)
    -> true
    ;  SrcPathBase = SrcPath
       %% Can't do the following because '<unknown>.foo.bar' will
       %% trigger it:
       %% type_error(file_name_not_ending_in_py_or_pyi, SrcPath)
    ).
