% -*- mode: Prolog -*-

%% Utilities for pykythe symtab.

:- module(pykythe_symtab, [
                           conv_symtab/3,
                           conv_symtab_pairs/3,
                           is_symtab/1,
                           list_to_symtab/2,
                           ord_list_to_symtab/2,
                           symtab_empty/1,
                           symtab_insert/4,
                           symtab_lookup/3,
                           update_symtab/3,
                           symtab_pairs/2,
                           symtab_values/2
                          ]).

:- meta_predicate
       conv_symtab(2, +, -),
       conv_symtab_pairs(2, +, -).

:- use_module(library(apply), [convlist/3]).
:- use_module(library(pairs), [pairs_values/2]).

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
