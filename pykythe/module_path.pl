% -*- mode: Prolog -*-

%% Module/path manipulation routines.

%% '.' in a directory name causes problems
%%    ... e.g. /usr/lib/python3.11/test/support/__init__.py
%%    doesn't roundtrip:
%%             /usr/lib/python3.11/test/support/__init__.py
%%                             ^ <====
%%    so change all '.'s to ':' -- this is generally safe
%%    because --pythonpath uses ':' to separate items.
%%    (see fix_dot/2 and unfix_dot/2 and where they're used).

% TODO: get rid of Path's in module types.

:- module(module_path, [append_fqn_dot/2,
                        full_module_part/2,
                        full_path/6,
                        full_path_prefixed/3,
                        join_fqn/2,
                        join_path/2,
                        maybe_canonical_path/2,
                        maybe_token_part/2,
                        module_file_exists/1,
                        module_to_module_alone/3,
                        module_part/2,
                        module_fqn_path/2,
                        path_expand/3,
                        path_pieces_expand/2,
                        path_part/2,
                        path_to_module_fqn_or_unknown/2,
                        split_fqn/2,
                        split_path/2,
                        src_base/2
                       ]).
:- encoding(utf8).
:- use_module(library(debug)). % explicit load to activate optimise_debug/0.

:- set_prolog_flag(warn_autoload, true).
:- set_prolog_flag(autoload, false).

:- use_module(library(apply), [maplist/2, maplist/3]).
:- use_module(library(filesex), [directory_file_path/3]).
:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(pcre), [re_matchsub/4, re_replace/4]).
:- use_module(pykythe_utils).
:- use_module(must_once, [must_once/1]).

:- style_check(+singleton).
:- style_check(+var_branches).
:- style_check(+no_effect).
:- style_check(+discontiguous).
% :- set_prolog_flag(generate_debug_info, false).


:- det(full_path/6).
%! full_path(+FromDots, +Path, +Pythonpaths, +CurrModulePath, -ModuleAndMaybeToken, -ModulePieces:list(atom)) is det.
% Derive a module (and maybe token) for an "import" or
% "from ... import" statement.
% FromDots is only used to determine how many dots are prepended (the
% "dots" will typically be 'ImportDotNode'{dot:DotAstn} but this isn't
% checked.
% - "from .. import i5" has FromImportPath='i5' and FromDots=[_,_]
% - "from os.path import sep" has FromImportPath='os/path/sep' and FromDots=[]
full_path([], Path, Pythonpaths, _CurrModulePath, ModuleAndMaybeToken, ModulePieces) =>
    full_path_prefixed(Path, Pythonpaths, ModuleAndMaybeToken),
    full_module_pieces(ModuleAndMaybeToken, ModulePieces).
full_path([_|Dots], Path, _Pythonpaths, CurrModulePath, ModuleAndMaybeToken, ModulePieces) =>
    directory_file_path(CurrModulePathDir, _, CurrModulePath),
    add_up_dots(Dots, [Path], DotsPath),
    join_path([CurrModulePathDir|DotsPath], FromImportPath2),
    (   path_expand(FromImportPath2, ModuleFqn, ModuleAndMaybeToken)
    ->  path_part_to_python_module_or_unknown(ModuleAndMaybeToken, ModuleFqn)
    ;   % File doesn't exist, e.g. '/home/fred/foo/bar/src/../../xyz',
        % path_to_module_fqn_or_unknown/2 would give
        % '<unknown>.home.fred.foo.bar.src.......xyz'.
        % TODO: AbsFromImportPath2 starts with '.', so this ends up
        %       with '<unknown>..' at the beginning; but fixing needs
        %       to deal with other code that converts to/from list.
        absolute_file_name_rel(FromImportPath2, AbsFromImportPath2), % potentially backtracks & doesn't check existence
        % The '{...}' in the following is because there could be
        % a ".<id>" following, which would be confused with an extension
        % in a file path.
        format(atom(ModuleFqn), '<unknown>.{~w}', [AbsFromImportPath2]),
        ModuleAndMaybeToken = module_alone(ModuleFqn, FromImportPath2)
    ),
    full_module_pieces(ModuleAndMaybeToken, ModulePieces).

:- det(add_up_dots/3).
add_up_dots([], Path, DotsPath) => DotsPath = Path.
add_up_dots([_|Dots], Path, DotsPath) =>
    add_up_dots(Dots, ['..'|Path], DotsPath).

%! module_fqn_path(+ModuleFqn:atom, -Path:atom) is nondet.
%! module_fqn_path(-ModuleFqn:atom, +Path:atom) is nondet.
% Convert a module ('path.to.module') to a path ('path/to/module.py').
% Does not check for existence of the file, nor apply any PythonPath
% addition. Backtracks through all solutions. At least one of
% ModuleFqn and Path must be instantiated.
% TODO: harmonize this with path_to_module_fqn / path_to_module_fqn_or_unknown.
module_fqn_path(ModuleFqn, Path),
        var(ModuleFqn) =>
    py_ext(Path0, Path2),
    simple_path_module_fqn(Path0, ModuleFqn),
    maybe_canonical_path(Path2, Path).
module_fqn_path(ModuleFqn, Path) =>
    simple_path_module_fqn(Path0, ModuleFqn),
    py_ext(Path0, Path2),
    maybe_canonical_path(Path2, Path).

:- det(simple_path_module_fqn/2).
%! simple_path_module_fqn(+Path:atom, -ModuleFqn:atom) is det.
%! simple_path_module_fqn(-Path:atom, +ModuleFqn:atom) is det.
% TODO: use library(pcre) re_replace?
simple_path_module_fqn(Path, ModuleFqn),
        var(ModuleFqn) =>
    split_path_to_module_parts(Path, ModuleFqnParts),
    join_fqn(ModuleFqnParts, ModuleFqn).
simple_path_module_fqn(Path, ModuleFqn) =>
    split_module_atom(ModuleFqn, ModuleFqnParts),
    join_path(ModuleFqnParts, Path).

:- det(split_path/2).
split_path_to_module_parts(Path, ModuleFqnParts) =>
    split_path(Path, ModuleFqnParts0),
    maplist(fix_dot, ModuleFqnParts0, ModuleFqnParts).

:- det(fix_dot/2).
fix_dot(In, Out) =>
    re_replace('\\.'/g, ':', In, Out0),
    atom_string(Out, Out0).

:- det(unfix_dot/2).
unfix_dot(In, Out) =>
    re_replace(':'/g, '.', In, Out0),
    atom_string(Out, Out0).

:- det(path_to_module_fqn_or_unknown/2).
%! path_to_module_fqn_or_unknown(+Path:atom, -ModuleFqn:atom) is det.
% Get the FQN for the Python module corresponding to Path or
% a '<unknown>...' atom.
% TODO: harmonize with path_module_fqn/2.
path_to_module_fqn_or_unknown(Path, ModuleFqn) :-
    maybe_path_to_module_fqn(Path, ModuleFqn),
    !.
path_to_module_fqn_or_unknown(Path, ModuleFqn) :-
    % Make sure the result conforms with FQN dotted name, so that
    % match_reversed_module_and_dotted_names/3 works properly
    split_path_to_module_parts(Path, PathParts),
    (   PathParts = [''|PathParts2] % root ('/') -- DO NOT SUBMIT - absolute_file_name
    ->  join_fqn(['<unknown>'|PathParts2], ModuleFqn)
    ;   join_fqn(['<unknown>'|PathParts], ModuleFqn)
    ).

%! maybe_path_to_module_fqn(+Path:atom, -ModuleFqn:atom) is semidet.
% Get the FQN for the Python module corresponding to Path or fail.
%  TODO: harmonize with module_fqn_path/2
%  TODO: use directory_file_path/3 instead of concat, to allow removing trailing '/'.
maybe_path_to_module_fqn(Path, ModuleFqn) :-
    (   maybe_canonical_path(Path, CanonicalPath),
        py_ext(CanonicalPath0, CanonicalPath)
    ->  true
    ;   absolute_dir(Path, CanonicalPath),
        (   atom_concat(CanonicalPath0, '/', CanonicalPath) % DO NOT SUBMIT - sub_atom/5
        ->  true
        ;   CanonicalPath0 = CanonicalPath
        )
    ),
    simple_path_module_fqn(CanonicalPath0, ModuleFqn).

%! maybe_canonical_path(+Path, -CanonicalPath) is semidet.
% Get a Path (file or directory) into a canonical (absolute) form.
% Fails if the file or directory doesn't exist.
maybe_canonical_path(Path, CanonicalPath) :-
    (   absolute_file_name_rel(Path, AbsPath, [access(read), file_errors(fail)])
    ->  true
    % DO NOT SUBUMIT - delete the following?
    ;   absolute_file_name_rel(Path, AbsPath, [access(read), file_type(directory), file_errors(fail)])
    ),
    atom_string(CanonicalPath, AbsPath).

:- det(full_path_prefixed/3).
%! full_path_prefixed(+DeprefixedPath, +Pythonpaths:list, -ModuleAndMaybeToken) is det.
% ModuleAndMaybeToken is either module_alone or module_and_token functor.
%  TODO: use directory_file_path/3 instead of concat, to allow removing trailing '/'.
full_path_prefixed(DeprefixedPath, Pythonpaths, ModuleAndMaybeToken) =>
    (   member(Prefix, Pythonpaths),
        atom_concat(Prefix, DeprefixedPath, Path0),
        path_expand(Path0, ModuleFqn, ModuleAndMaybeToken)
    ->  true
    ;   Prefix = '$PYTHONPATH/',
        % Can't path_expand because we don't know what prefix to use.
        atom_concat('$PYTHONPATH/', DeprefixedPath, UnknownPath),
        ModuleAndMaybeToken = module_alone(ModuleFqn, UnknownPath)
    ),
    path_part(ModuleAndMaybeToken, PathPart),
    $(atom_concat(Prefix, _, PathPart)), % Prefix must be first part of PathPart
    $(atom_concat(_, '/', Prefix)), % Prefix must end with '/'.
    path_part_to_python_module_or_unknown(ModuleAndMaybeToken, ModuleFqn).

%! path_pieces_expand(PathPieces:list(atom), -ModuleAndMaybeToken) is semidet.
% path_expand(Path, ModuleFqn, ModuleAndMaybeToken), where Path and
% ModuleFqn are constructed from PathPieces.
path_pieces_expand(PathPieces, ModuleAndMaybeToken) =>
    join_path(PathPieces, Path),
    join_fqn(PathPieces, ModuleFqn),
    path_expand(Path, ModuleFqn, ModuleAndMaybeToken).

%! path_expand(+Path0:atom, +ModuleFqn:atom, -ModuleAndMaybeToken) is semidet.
% ModuleAndMaybeToken is either module_alone or module_and_token
% functor. ModuleFqn can be a logical variable that gets filled in
% later.
path_expand(Path0, ModuleFqn, ModuleAndMaybeToken) =>
    (   Path1 = Path0,
        ModuleAndMaybeToken = module_alone(ModuleFqn, Expanded)
    ;   remove_last_component(Path0, Path1, Token),
        ModuleAndMaybeToken = module_and_token(ModuleFqn, Expanded, Token)
    ),
    (   py_ext(Path1, Path),
        maybe_canonical_path(Path, Expanded)
    ->  true % 'foo.bar' can produce
             %    module_alone('foo.bar', 'foo/bar.py' and
             %    module_and_token('foo.bar', 'foo/__init__.py', 'bar')
             % so prevent the 2nd one if the first one succeeds
             % (maybe_canonical_path/2 checks the validity of files but
             % there's nothing for checking whether a token exists
              % within a file).
    ;   Path1 = Path0          % module_alone and not module_and_token
    ->  absolute_dir(Path1, Expanded)
    ;   fail
    ).

:- det(remove_last_component/3).
%! remove_last_component(+Path, -AllButLast, -Last) is semidet.
% e.g.: Path='foo/bar/zot', AllButLast='foo/bar', Last=zot
% Fails if no '/' in Path.
%  TODO: use directory_file_path/3 instead?
remove_last_component(Path, AllButLast, Last) =>
    split_path(Path, Split),
    Split = [_,_|_],            % at least two components
    (   append(FirstPathPieces, [Last], Split)
    ->  join_path(FirstPathPieces, AllButLast)
    ;   fail
    ).

:- det(path_part/2).
%! path_part(+ModuleAndMaybeToken, -Path) is det.
% Extract Path from ModuleAndMaybeToken.
path_part(module_alone(_ModuleFqn,Path), Path).
path_part(module_and_token(_ModuleFqn,Path,_Token), Path).
path_part(module_star(_ModuleFqn,Path), Path).

%! module_file_exists(+ModuleAndMaybeToken) is semidet.
% Throws an exception if the file in ModuleANdMaybeToken doesn't exist
% or if it's not absolute.
module_file_exists(ModuleAndMaybeToken) :-
    path_part(ModuleAndMaybeToken, Path),
    absolute_file_name(Path, AbsPath, [access(read), file_errors(error)]),
    must_once(Path == AbsPath).  % assertion

:- det(module_part/2).
%! module_part(+ModuleAndMaybeToken, -ModuleFqn:atom) is det.
% Extract ModuleFqn from ModuleAndMaybeToken. Any Token is ignored.
module_part(module_alone(ModuleFqn,_Path), ModuleFqn).
module_part(module_and_token(ModuleFqn,_Path,_Token), ModuleFqn).
module_part(module_star(ModuleFqn,_Path), ModuleFqn).

:- det(full_module_part/2).
%! full_module_part(+ModuleAndMaybeToken, -ModuleFqn:atom) is det.
% Extract ModuleFqn from ModuleAndMaybeToken.
% If there is a Token, it is added to the ModuleFqn name.
full_module_part(module_alone(ModuleFqn,_Path), ModuleFqn2) => ModuleFqn = ModuleFqn2.
full_module_part(module_and_token(ModuleFqn,_Path,Token), ModuleDotToken) =>
    join_fqn([ModuleFqn, Token], ModuleDotToken).
% Shouldn't happen:
% full_module_part(module_star(ModuleFqn,_Path), ModuleFqn).

%! maybe_token_part(+ModuleAndMaybeToken, -Token) is semidet.
% If ModuleAndMaybeToken has a Token, get it (else fail).
maybe_token_part(module_and_token(_ModuleFqn,_Path,Token), Token).

:- det(full_module_pieces/2).
%! full_module_pieces(+ModuleAndMaybeToken, -ModulePieces:list(atom)) is det.
% Extract ModuleFqn (as a list of pieces) from ModuleAndMaybeToken.
full_module_pieces(module_alone(ModuleFqn,_Path), ModulePieces) =>
    split_module_atom(ModuleFqn, ModulePieces).
full_module_pieces(module_and_token(ModuleFqn,_Path,Token), ModulePieces) =>
    split_module_atom(ModuleFqn, ModulePieces0), $,
    append(ModulePieces0, [Token], ModulePieces).
full_module_pieces(module_star(ModuleFqn,_Path), ModulePieces) =>
    split_module_atom(ModuleFqn, ModulePieces0), $,
    append(ModulePieces0, ['*'], ModulePieces).

:- det(module_to_module_alone/3).
%! module_to_module_alone(+ModuleAndMaybeToken, -Module, -ModuleFqn:atom) is det.
module_to_module_alone(ModuleAndMaybeToken, ModuleAlone, ModuleFqn) =>
    ModuleAlone = module_alone(ModuleFqn, Path),
    module_part(ModuleAndMaybeToken, ModuleFqn),
    path_part(ModuleAndMaybeToken, Path).

:- det(split_module_atom/2).
%! split_module_atom(+ModuleFqn:atom, -ModulePieces:list(atom)) is det.
% Split module into pieces on '.', with special handling for '<unknown>'
split_module_atom(ModuleFqn, ModulePiecesFixed) =>
    (   re_matchsub('<unknown>\\.{([^}]*)}$', ModuleFqn, Sub, [anchored(true)])  % TODO: pcre: needed to quiet pldoc
    ->  % filename is in absolute form, so the first part of the result
        % is ''. If we want to get rid of that, then the following
        % should have [''|ModulePieces0] as the last arg:
        split_path(Sub.1, ModulePieces0),
        ModulePieces = ['<unknown>'|ModulePieces0]
    ;   re_matchsub('(<unknown>\\.{[^}]*})\\.(.*)$', ModuleFqn, Sub, [anchored(true)])
    ->  % TODO: does this situation ever arise?
        split_fqn(Sub.2, ModulePieces2),
        ModulePieces = [Sub.1|ModulePieces2]
    ;   split_atom(ModuleFqn, '.', '', ModulePieces)
    ),
    maplist(unfix_dot, ModulePieces, ModulePiecesFixed).

%! py_ext(+Path:atom, -PathBase:atom) is nondet.
%! py_ext(-Path:atom, +PathBase:atom) is nondet.
% Path unifies with all permutations of PathBase plus {.py,.pyi} and
%  __init__ equivalents and does not check for existence.
py_ext(PathBase, Path) :-
    % TODO: verify order of testing which file(s) exists.
    % TODO: allow more than one "hit" (e.g., if there are both a .py and .pyi,
    %       then use the .pyi to get type info for the .py and possibly create
    %       kyanchors in both)
    py_ext_ext(Ext),
    atom_concat(PathBase, Ext, Path),
    % for 'foo/__init__.py', only return 'foo' and not 'foo/__init__':
    \+ atom_concat(_, '__init__', PathBase).

%! py_ext_ext(-Extension:atom) is nondet.
% "extensions" to append to a module to get a file
% file_name_extension/3 adds a '.', so can't use for /__init__.*
% TODO: allow more than .py and .pyi as extensions?
py_ext_ext('.py').
py_ext_ext('.pyi').
py_ext_ext('/__init__.py').
py_ext_ext('/__init__.pyi').

:- det(src_base/2).
%! src_base(+SrcPath: atom, -SrcPathBase) is det.
% Remove extension (.py, .pyi, /__init__.{py,pyi}) from a source path.
src_base(SrcPath, SrcPathBase) =>
    (   py_ext_ext(Ext),
        atom_concat(SrcPathBase, Ext, SrcPath)
    ->  true
    ;   SrcPathBase = SrcPath
        % Can't do the following because '<unknown>.foo.bar' will
        % trigger it:
        % type_error(file_name_not_ending_in_py_or_pyi, SrcPath)
    ).

:- det(path_part_to_python_module_or_unknown/2).
%! path_part_to_python_module_or_unknown(+ModuleAndMaybeToken, -ModuleFqn:atom) is det.
% Convenience predicate mapping ModuleAndMaybeToken to ModuleFqn.
path_part_to_python_module_or_unknown(ModuleAndMaybeToken, ModuleFqn) =>
    path_part(ModuleAndMaybeToken, ResolvedPath),
    path_to_module_fqn_or_unknown(ResolvedPath, ModuleFqn).

:- det(split_fqn/2).
split_fqn(Fqn, FqnParts) =>
    split_atom(Fqn, '.', '', FqnParts).

% TODO: there should be something like Python's path-join
%       in library(filesex).
:- det(split_path/2).
split_path(Path, PathParts) =>
    split_atom(Path, '/', '', PathParts).

:- det(join_fqn/2).
join_fqn(FqnParts, Fqn) =>
    atomic_list_concat(FqnParts, '.', Fqn).

:- det(join_path/2).
join_path(PathParts, Path) =>
    atomic_list_concat(PathParts, '/', Path).

:- det(append_fqn_dot/2).
append_fqn_dot(Fqn, FqnDot) =>
    atomic_list_concat([Fqn, '.'], FqnDot).

end_of_file.
