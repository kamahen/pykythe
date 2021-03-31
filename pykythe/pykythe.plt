% -*- mode: Prolog -*-

%% Most tests are outside of this, using Kythe's verifier.  However, a
%% few tests are here, as they were used for developing the code, so
%% why not keep them?

%% :- set_prolog_flag(autoload, false).  % TODO: seems to break plunit

:- use_module(library(plunit)).

:- dynamic
    pykythe_test:src_paths/1,
    pykythe_test:opts/1.

pykythe_run_tests :-
    pykythe:pykythe_opts(SrcPaths, Opts),
    assertz(pykythe_test:src_paths(SrcPaths)),
    assertz(pykythe_test:opts(Opts)),
    plunit:run_tests.

:- begin_tests(pykythe_dev).

:- use_module(library(lists), [subtract/3]).

test_meta(Meta) =>
    Meta = meta{kythe_corpus: 'CORPUS',
                kythe_root: 'ROOT',
                path: SrcPath,
                language: python,
                encoding: 'utf-8',
                contents_base64: '',
                contents_str: _,
                contents_bytes: _,
                sha1: '',
                src_fqn: SrcFqn,
                pythonpath: Pythonpath,
                opts: Opts,
                version: ''},
    pykythe_test:opts(Opts),
    pykythe_test:src_paths([SrcPath]),
    Pythonpath = Opts.pythonpath,
    module_path:path_to_module_fqn_or_unknown(SrcPath, SrcFqn).

kyfact_edge(EdgeKind, Tag, Json, Out) :-
    get_dict(edge_kind, Json, EdgeKind0),
    EdgeKind0 == EdgeKind,
    Out = Tag{source: Json.source.signature, target: Json.target.signature}.

find_anchor(Line, I-Match, Astn) =>
    %% setof guarantees ordering of find_anchor results
    setof(Astn, find_anchor_match(Line, Match, Astn), Astns),
    nth0(I, Astns, Astn).

find_anchor_match(Line, Match, AstnMatch) :-
    AstnMatch = 'Astn'{start:Start, end:End, value:Match},
    sub_atom(Line, Start, Len, _After, Match),
    End is Start + Len.

path_module(Meta, Path, Module) :-
    module_path:full_path_prefixed(Path, Meta.pythonpath, Module).

symtab_subtract(Symtab1, Symtab2, DiffPairs) :-
    pykythe_symtab:symtab_pairs(Symtab1, Pairs1),
    pykythe_symtab:symtab_pairs(Symtab2, Pairs2),
    subtract(Pairs1, Pairs2, DiffPairs).

convset(Pred, L1, S2) :-
    convlist(Pred, L1, L2),
    list_to_set(L2, S2).

round_trip_base64_utf8(Original) :-
    pykythe_utils:base64_utf8(Original, B64),
    pykythe_utils:base64_utf8(RoundTrip, B64),
    assertion(Original == RoundTrip).

test(base64_misc) :-
    Original = '├',
    %% using Python:
    %% >>> '├'.encode('utf8')
    %% '\xe2\x94\x9c'
    %% >>> [c for c in '├'.encode('utf8')]
    %% [226, 148, 156]
    atom_codes(Original, OriginalCodes),
    OriginalCodes = [9500],
    phrase(utf8_codes(OriginalCodes), EncodedUtf8codes),
    assertion(EncodedUtf8codes == [226, 148, 156]),
    atom_codes(EncodedUtf8, EncodedUtf8codes),
    assertion(EncodedUtf8 == '\u00e2\u0094\u009c').

test(base64_utf8) :-
    maplist(round_trip_base64_utf8,
            ['ab', 'AB',
             '├',
             '網目錦蛇 = 1  # アミメニシキヘビ 《網目錦蛇》 【あみめにしきへび】 (n) (uk) reticulated python (Python reticulatus)']).

test(kyImportDottedAsNamesFqn_top) :-
    %% This test is not exhaustive -- it's mainly for developing the code.
    %% Additional tests are done using the Kythe verifier.
    %% Note that this imports a variable (os.path.sep) whereas comb_as test imports
    %% a file (os.path). This shouldn't be any different, but it exercises things a bit more.
    test_meta(Meta),
    Meta.contents_str = 'import os.path.sep',
    Meta.contents_bytes = Meta.contents_str,  % ascii "decoding"
    %% If you change this setup, also change test(kyImportDottedAsNamesFqn1). DO NOT SUBMIT
    maplist(find_anchor(Meta.contents_str),
            [0-os, 0-path, 0-sep],
            [OsAstn, PathAstn, SepAstn]),
    maplist(path_module(Meta),
            ['os', 'os/path', 'os/path/sep'],
            [Module_os, Module_os_path, Module_os_path_sep]),

    %% Module_os_path_sep = module_and_token('___.typeshed.stdlib.os.path',
    %%                                       '___/typeshed/stdlib/os/path.pyi', sep)
    module_path:full_module_part(Module_os_path_sep, ModuleModule_os_path_sep),
    pykythe_utils:remove_suffix(ModuleModule_os_path_sep, '.stdlib.os.path.sep', TypeshedFqn),
    assertion(pykythe_utils:has_suffix(TypeshedFqn, '.typeshed')),
    assertion(module_path:token_part(Module_os_path_sep, sep)),

    %% Module_os_path = module_alone('___.typeshed.stdlib.os.path', '___/typeshed/stdlib/os/path.pyi')
    module_path:full_module_part(Module_os_path, ModuleModule_os_path),
    assertion(atomic_list_concat([TypeshedFqn, '.stdlib.os.path'], ModuleModule_os_path)),

    %% Module_os = module_alone('___.typeshed.stdlib.os', '___/typeshed/stdlib/os/__init__.pyi')
    module_path:full_module_part(Module_os, ModuleModule_os),
    assertion(atomic_list_concat([TypeshedFqn, '.stdlib.os'], ModuleModule_os)),

    _FromDots = [],
    DottedNameItems = ['NameBareNode'{name:OsAstn}, 'NameBareNode'{name:PathAstn}, 'NameBareNode'{name:SepAstn}],
    module_path:join_fqn([Meta.src_fqn, 'os'], BindsFqn),
    BindsNameAstn = OsAstn,
    %% phrase(pykythe:kyImportDottedAsNamesFqn_top(...)):
    pykythe:kyImportDottedAsNamesFqn_top(DottedNameItems, BindsFqn, BindsNameAstn,
                                         KyFactsAll, [], Exprs, [], Meta),

    convset(kyfact_edge('/kythe/edge/ref/imports',     imports), KyFactsAll, KyFactsImports),
    convset(kyfact_edge('/kythe/edge/defines/binding', binding), KyFactsAll, KyFactsBinding),
    maplist(pykythe:anchor_signature_str, [OsAstn, PathAstn, SepAstn], [OsSig, PathSig, SepSig]),
    assertion(KyFactsImports == [imports{source:OsSig,   target:ModuleModule_os},
                                 imports{source:PathSig, target:ModuleModule_os_path},
                                 imports{source:SepSig,  target:ModuleModule_os_path_sep}]),
    pykythe_symtab:list_to_symtab([ModuleModule_os-[module_type(Module_os)],
                                   ModuleModule_os_path-[module_type(Module_os_path)],
                                   ModuleModule_os_path_sep-[module_type(Module_os_path_sep)]],
                                  Symtab0),
    pykythe:assign_exprs_count_impl(Exprs, Meta, Symtab0, SymtabWithRej, Rej, KytheFacts),

    symtab_subtract(SymtabWithRej, Symtab0, SymtabAddedPairs),
    assertion(Rej == SymtabAddedPairs),
    must_once:must_once(SymtabAddedPairs = [OsVar - [ module_type(Module_os) ]]),
    %% The following ("dummy_dir.dummy") depends on the source file
    %% specified when running the tests.
    %% See Makefile rule pykythe_test.
    assertion(pykythe_utils:has_suffix(OsVar, '.pykythe.test_data.dummy_dir.dummy_file.os')),
    pykythe:anchor_signature_str(OsAstn, OsSignature),
    assertion(KyFactsBinding == [binding{source:OsSignature,
                                 target:OsVar}]),
    assertion([] == KytheFacts),
    assertion(Exprs == [assign_import{binds_fqn:BindsFqn,
                                      module_and_maybe_token:Module_os,
                                      modules_to_import:[Module_os,
                                                         Module_os_path,
                                                         Module_os_path_sep]}]).

test(kyImportDottedAsNamesFqn_as) :-
    %% This test is not exhaustive -- it's mainly for developing the code.
    %% Additional tests are done using the Kythe verifier.
    %% Note that this imports a file (os.path) whereas comb_top test imports
    %% a variable within a file (os.path.sep). This shouldn't be any different,
    %% but it exercises things a bit more.
    test_meta(Meta),
    Meta.contents_str = 'import os.path as os_path',
    Meta.contents_bytes = Meta.contents_str,  % ascii "decoding"
    maplist(find_anchor(Meta.contents_str),
            [0-os, 0-path, 0-os_path],
            [OsAstn, PathAstn, OsPathAstn]),
    maplist(path_module(Meta),
            ['os', 'os/path'],
            [Module_os, Module_os_path]),
    %% OsAstn = 'Astn'{start:7, end:9, value:os}
    %% OsSig = '@7:9<os>'

    %% Module_os_path = module_alone('___.typeshed.stdlib.os.path', '___/typeshed/stdlib/os/path.pyi')
    module_path:full_module_part(Module_os_path, ModuleModule_os_path),
    assertion(pykythe_utils:has_suffix(ModuleModule_os_path, '.stdlib.os.path')),

    %% Module_os = module_alone('___.typeshed.stdlib.os', '___/typeshed/stdlib/os/__init__.pyi')
    module_path:full_module_part(Module_os, ModuleModule_os),
    assertion(pykythe_utils:has_suffix(ModuleModule_os, '.stdlib.os')),

    FromDots = [],
    DottedNameItems = ['NameBareNode'{name:OsAstn}, 'NameBareNode'{name:PathAstn}],
    module_path:join_fqn([Meta.src_fqn, 'os_path'], BindsFqn),
    BindsNameAstn = OsPathAstn,
    %% phrase(pykythe:kyImportDottedAsNamesFqn_top(...)):
    pykythe:kyImportDottedAsNamesFqn_as(FromDots, DottedNameItems, BindsFqn, BindsNameAstn,
                                        KyFactsAll, [], Exprs, [], Meta),

    convset(kyfact_edge('/kythe/edge/ref/imports',     imports), KyFactsAll, KyFactsImports),
    convset(kyfact_edge('/kythe/edge/defines/binding', binding), KyFactsAll, KyFactsBinding),
    maplist(pykythe:anchor_signature_str, [OsAstn, PathAstn, OsPathAstn], [OsSig, PathSig, OsPathSig]),
    assertion(KyFactsImports == [imports{source:OsSig,     target:ModuleModule_os},
                                 imports{source:PathSig,   target:ModuleModule_os_path},
                                 imports{source:OsPathSig, target:ModuleModule_os_path}]),
    pykythe_symtab:list_to_symtab([ModuleModule_os-[module_type(Module_os)],
                                   ModuleModule_os_path-[module_type(Module_os_path)]],
                                  Symtab0),
    pykythe:assign_exprs_count_impl(Exprs, Meta, Symtab0, SymtabWithRej, Rej, KytheFacts),

    symtab_subtract(SymtabWithRej, Symtab0, SymtabAddedPairs),
    assertion(Rej == SymtabAddedPairs),
    must_once:must_once(SymtabAddedPairs = [OsPathVar - [ module_type(Module_os_path) ]]),
    %% The following ("dummy_dir.dummy_file.os_path") depends on the
    %% source file specified when running the tests.
    %% See Makefile rule pykythe_test.
    assertion(pykythe_utils:has_suffix(OsPathVar, '.pykythe.test_data.dummy_dir.dummy_file.os_path')),
    assertion(KyFactsBinding == [binding{source:OsPathSig,
                                         target:OsPathVar}]),
    assertion([] == KytheFacts),
    assertion(Exprs == [assign_import{binds_fqn:BindsFqn,
                                      module_and_maybe_token:Module_os_path,
                                      modules_to_import:[Module_os,
                                                         Module_os_path]}]).

:- end_tests(pykythe_dev).

end_of_file.
