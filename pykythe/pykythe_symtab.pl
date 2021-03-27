% -*- mode: Prolog -*-

%% Utilities for pykythe symtab.

%% Uses rbtrees because we do a lot of insertions when reading in a
%% cached symtab compared to the number of lookups that will be done.

% TODO: merge_ord_list_to_rbtree for making the update faster?


:- module(pykythe_symtab, [
                           conv_symtab/3,
                           conv_symtab_pairs/3,
                           is_symtab/1,
                           list_to_symtab/2,
                           ord_list_to_symtab/2,
                           maybe_read_symtab_from_cache/7,
                           read_symtab_from_cache_no_check/2,
                           symtab_empty/1,
                           symtab_insert/4,
                           symtab_lookup/3,
                           symtab_pairs/2,
                           symtab_scope_pairs/3,
                           symtab_size/2,
                           symtab_values/2,
                           write_symtab/4
                          ]).

:- encoding(utf8).
:- set_prolog_flag(optimise, true).
% :- set_prolog_flag(autoload, false).  % TODO: seems to break plunit, qsave

:- use_module(pykythe_utils).

:- meta_predicate
       conv_symtab(2, +, -),
       conv_symtab_pairs(2, +, -),
       rb_conv_pairs(+, 2, -),
       rb_conv_pairs_(+, 2, +, -),
       maybe_read_symtab_from_cache(+, +, +, +, -, 0, 0).

:- use_module(library(apply), [convlist/3]).
:- use_module(library(error), [must_be/2]).
:- use_module(library(pairs), [pairs_values/2]).
:- use_module(library(rbtrees), [is_rbtree/1, list_to_rbtree/2, ord_list_to_rbtree/2, rb_insert/4, rb_lookup/3, rb_visit/2]).
:- use_module(must_once).
:- use_module(pykythe_utils).

is_symtab(Symtab) :-
    % TODO: this is too crude: can we have the symtab
    %       marked by, e.g. symtab(Rbtree)?
    is_rbtree(Symtab).

symtab_empty(Symtab) :-
    rb_empty(Symtab).

ord_list_to_symtab(Pairs, Symtab) :-
    ord_list_to_rbtree(Pairs, Symtab),
    must_once(is_rbtree(Symtab)).  % Ensures no dup keys
list_to_symtab(Pairs, Symtab) :-
    list_to_rbtree(Pairs, Symtab),
    must_once(is_rbtree(Symtab)).  % Ensure no dup keys

symtab_insert(Key, Symtab0, Value, Symtab) :-
    % This weird ordering of params is the same as put_dict/4.
    rb_insert(Symtab0, Key, Value, Symtab).

symtab_lookup(Key, Symtab, Value) :-
    (   ground(Key)
    ->  rb_lookup(Key, Value, Symtab)
    ;   instantiation_error(Key)
    ).

symtab_size(Symtab, Size) :-
    rb_size(Symtab, Size).

symtab_pairs(Symtab, Pairs) :-
    rb_visit(Symtab, Pairs).

%! symtab_values(+Symtab, -Values) is det.
%  True when Values is an ordered set of the values appearing in Symtab.
symtab_values(Symtab, Values) :-
    rb_visit(Symtab, Pairs),
    pairs_values(Pairs, Values).

conv_symtab(Pred, Symtab0, Symtab) :-
    % TODO: see conv_symtab_pairs and do something similar
    %       (might not be worth it; only used in gen_builtins_symtab)
    rb_visit(Symtab0, Pairs0),
    convlist(Pred, Pairs0, Pairs),
    ord_list_to_rbtree(Pairs, Symtab).

conv_symtab_pairs(Pred, Symtab, Pairs) :-
    rb_conv_pairs(Symtab, Pred, Pairs).

% rb_visit_pairs is derived from rb_visit/2 and convlist/3.
% TODO: add this to library(rbtrees)
rb_conv_pairs(t(_,T), Pred, Lf) :-
    rb_conv_pairs_(T, Pred, [], Lf).

rb_conv_pairs_(black('',_,_,_), _Pred, L, L) :- !.
rb_conv_pairs_(red(L,K,V,R), Pred, L0, Lf) :-
    (   call(Pred, K-V, K2V2)
    ->  rb_conv_pairs_(L, Pred, [K2V2|L1], Lf)
    ;   rb_conv_pairs_(L, Pred, L1, Lf)
    ),
    rb_conv_pairs_(R, Pred, L0, L1).
rb_conv_pairs_(black(L,K,V,R), Pred, L0, Lf) :-
    (   call(Pred, K-V, K2V2)
    ->  rb_conv_pairs_(L, Pred, [K2V2|L1], Lf)
    ;   rb_conv_pairs_(L, Pred, L1, Lf)
    ),
    rb_conv_pairs_(R, Pred, L0, L1).

%! maybe_read_symtab_from_cache(+OptsVersion:atom, +PykytheSymtabInputStream, +SrcPath, +Symtab0, -NewSymtab, :IfCacheFail, :IfSha1Fail) is semidet.
% Also merges Symtab0 with SymtabFromCache to create NewSymtab
maybe_read_symtab_from_cache(OptsVersion, PykytheSymtabInputStream, SrcPath, Symtab0, NewSymtab, IfCacheFail, IfSha1Fail) :-
    % See write_batch/2 for how these were output.
    read_term(PykytheSymtabInputStream, CacheVersion, []),
    % short-circuit other tests if version mismatch
    (   CacheVersion == OptsVersion
    ->  true
    ;   call(IfCacheFail),
        fail
    ),
    read_term(PykytheSymtabInputStream, Sha1Hex, []),
    (   maybe_file_sha1(SrcPath, SrcSha1Hex),
        SrcSha1Hex == Sha1Hex   % succeed if SHA1 is expected value.
    ->  true
    ;   call(IfSha1Fail),
        fail
    ),
    % TODO - DO NOT SUBMIT
    % The JSON read is slow (1.6 sec) and probably the write is
    % also slow ... use fast_read/2, fast_write/2.
    % term_string->term: 155ms (27K entries in 7.2MB)
    % term_string->str:  105ms
    % fast_term->term:    25ms
    % fast_term->str:     12ms
    read_term(PykytheSymtabInputStream, SymtabFromCacheKVs, []),
    foldl_rb_insert(SymtabFromCacheKVs, Symtab0, NewSymtab).

foldl_rb_insert([], Rb, Rb).
foldl_rb_insert([K-V|KVs], Rb0, Rb) :-
    rb_insert(Rb0, K, V, Rb1),
    foldl_rb_insert(KVs, Rb1, Rb).

% The following is a cut-down version of maybe_read_symtab_from_cache/6
% used by gen_builtins_symtab.pl
read_symtab_from_cache_no_check(PykytheSymtabInputPath, Symtab) :-
    setup_call_cleanup(
        open(PykytheSymtabInputPath, read, PykytheSymtabInputStream, [type(binary)]),
        (   read_term(PykytheSymtabInputStream, _Version, []),
            read_term(PykytheSymtabInputStream, _Sha1, []),
            read_term(PykytheSymtabInputStream, SymtabKVs, [])
        ),
        close(PykytheSymtabInputStream)
    ),
    must_be(ground, SymtabKVs),
    ord_list_to_rbtree(SymtabKVs, Symtab).

%! write_symtab(+Symtab, +Version, +Sha1, +PykytheBatchOutStream) is det.
write_symtab(Symtab, Version, Sha1, PykytheBatchOutStream) :-
    rb_visit(Symtab, SymtabKVs),
    % DO NOT SUBMIT - fast-serialize
    format(PykytheBatchOutStream, '~k.~n~k.~n~k.~n', [Version, Sha1, SymtabKVs]).

% DO NOT SUBMIT:
% Need to add a portray -- see pykythe:pykythe_portray(Symtab)

%! symtab_scope_pairs(+FqnScope:atom, +Symtab, -SymtabPairsScope) is det.
% For debugging: extract only entries that start with FqnScope + '.'
% This isn't very useful because all the builgins are added to the
% local scope, so it matches a lot of entries.
symtab_scope_pairs(FqnScope, Symtab, SymtabPairsScope) :-
    symtab_pairs(Symtab, SymtabPairs),
    include(symtab_entry_starts_with(FqnScope), SymtabPairs, SymtabPairsScope).

symtab_entry_starts_with(FqnScope, Key-_Type) :-
    (  FqnScope = Key
    ;  atom_concat(FqnScope, '.', FqnScopeDot),
       atom_concat(FqnScopeDot, _, Key)
    ).

end_of_file.
