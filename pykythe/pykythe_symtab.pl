% -*- mode: Prolog -*-

%% Utilities for pykythe symtab.

%% TODO: use library(assoc) or library(rbtrees) or trie or hash
%%       instead of dict for Symtab (performance)
%%       symrej_accum(Fqn-Type.  (Probably rbtrees, because we do a
%%       lot of insertions when reading in a cached symtab compared to
%%       the number of lookups that will be done ... also might want
%%       to create a merge_ord_list_to_rbtree for making the update
%%       faster.)


:- module(pykythe_symtab, [
                           conv_symtab/3,
                           conv_symtab_pairs/3,
                           is_symtab/1,
                           list_to_symtab/2,
                           ord_list_to_symtab/2,
                           maybe_read_cache/6,
                           read_symtab_from_cache_no_check/2,
                           symtab_empty/1,
                           symtab_insert/4,
                           symtab_lookup/3,
                           update_symtab/3,
                           symtab_pairs/2,
                           symtab_values/2,
                           write_symtab/4
                          ]).

:- encoding(utf8).
%% :- set_prolog_flag(autoload, false).  % TODO: seems to break plunit, qsave

:- use_module(pykythe_utils).

:- meta_predicate
       conv_symtab(2, +, -),
       conv_symtab_pairs(2, +, -),
       maybe_read_cache(+, +, +, -, 0, 0).

:- use_module(library(apply), [convlist/3]).
:- use_module(library(pairs), [pairs_values/2]).
:- use_module(library(rbtrees), [ord_list_to_rbtree/2, rb_insert/4, rb_visit/2]).
:- use_module(must_once).
:- use_module(pykythe_utils).

symtab_empty(Symtab) :-
    Symtab = symtab{}.

is_symtab(Symtab) :-
    is_dict(Symtab, symtab),
    ground(Symtab).

ord_list_to_symtab(Pairs, Symtab) :-
    dict_pairs(Symtab, symtab, Pairs).

list_to_symtab(Pairs, Symtab) :-
    dict_pairs(Symtab, symtab, Pairs).

symtab_insert(Key, Symtab0, Value, Symtab) :-
    %% This weird ordering of params is the same as put_dict/4.
    put_dict(Key, Symtab0, Value, Symtab).

symtab_lookup(Key, Symtab, Value) :-
    get_dict(Key, Symtab, Value).

symtab_pairs(Symtab, Pairs) :-
    dict_pairs(Symtab, symtab,  Pairs).

%! symtab_values(+Dict, -Values) is det.
%%    True when Values is an ordered set of the values appearing in Dict.
symtab_values(Dict, Values) :-
    dict_pairs(Dict, symtab, Pairs),
    pairs_values(Pairs, Values).

conv_symtab(Pred, Dict0, Dict) :-
    dict_pairs(Dict0, symtab, Pairs0),
    convlist(Pred, Pairs0, Pairs),
    dict_pairs(Dict, symtab, Pairs).

conv_symtab_pairs(Pred, Dict0, Pairs) :-
    dict_pairs(Dict0, symtab, Pairs0),
    convlist(Pred, Pairs0, Pairs).

update_symtab(Symtab, Dict0, Dict) :-
    %% updating dict is slow: the following code is 20x faster for
    %% 11,000 items, even with the overhead of converting to/from
    %% rbtree.
    dict_pairs(Symtab, symtab, KVs),
    dict_pairs(Dict0, symtab, KVs0),
    ord_list_to_rbtree(KVs0, Rb0),
    make_rb(KVs, Rb0, Rb1),
    rb_visit(Rb1, KVs1),
    dict_pairs(Dict, symtab, KVs1).

make_rb([], Rb, Rb).
make_rb([K-V|KVs], Rb0, Rb) :-
    rb_insert(Rb0, K, V, Rb1),
    make_rb(KVs, Rb1, Rb).

%- maybe_read_cache(+OptsVersion:atom, +PykytheSymtabInputStream, +SrcPath, -SymtabFromCache, :IfCacheFail, :IfSha1Fail) is semidet.
%% Reads just enough to validate.
maybe_read_cache(OptsVersion, PykytheSymtabInputStream, SrcPath, SymtabFromCache, IfCacheFail, IfSha1Fail) :-
    %% See write_batch/2 for how these were output.
    read_term(PykytheSymtabInputStream, CacheVersion, []),
    %% short-circuit other tests if version mismatch
    (  CacheVersion == OptsVersion
    -> true
    ;  call(IfCacheFail),
       fail
    ),
    read_term(PykytheSymtabInputStream, Sha1Hex, []),
    (  maybe_file_sha1(SrcPath, SrcSha1Hex),
       SrcSha1Hex == Sha1Hex  %% succeed if SHA1 is expected value.
    -> true
    ;  call(IfSha1Fail),
       fail
    ),
    %% TODO - DO NOT SUBMIT
    %% The JSON read is slow (1.6 sec) and probably the write is
    %% also slow ... ue fast_read/2, fast_write/2.
    %% term_string->term: 155ms (27K entries in 7.2MB)
    %% term_string->str:  105ms
    %% fast_term->term:    25ms
    %% fast_term->str:     12ms
    read_term(PykytheSymtabInputStream, SymtabFromCache, []),
    must_once(is_symtab(SymtabFromCache)).

%% The following is a cut-down version of maybe_read_cache/6
read_symtab_from_cache_no_check(PykytheSymtabInputPath, Symtab) :-
    open(PykytheSymtabInputPath, read, PykytheSymtabInputStream, [type(binary)]),
    read_term(PykytheSymtabInputStream, _Version, []),
    read_term(PykytheSymtabInputStream, _Sha1, []),
    read_term(PykytheSymtabInputStream, Symtab, []),
    must_once(is_symtab(Symtab)).

%! write_symtab(+Symtab, +Version, +Sha1, +PykytheBatchOutStream) is det.
write_symtab(Symtab, Version, Sha1, PykytheBatchOutStream) :-
    % DO NOT SUBMIT - fast-serialize
    format(PykytheBatchOutStream, '~k.~n~k.~n~k.~n', [Version, Sha1, Symtab]).

%% DO NOT SUBMIT:
%% Need to add a portray -- see pykythe:pykythe_portray(Symtab)

end_of_file.
