% -*- mode: Prolog -*-

:- module(must_once, [must_once/1, must_once/3, must_once/5]).

%% Like once/1, but also works with EDCGs.
%% You must add edcg:pred_info(must_once, 1, ...) facts in the using module.

must_once(Goal) :-
    (  call(Goal)
    -> true
    ;  throw(error(failed(Goal), _))
    ).

%% edcg doesn't understand the meta-pred "call", so expand -->> by hand:

must_once(Goal, AccumA0, AccumA) :-
    (  call(Goal, AccumA0, AccumA)
    -> true
    ;  throw(error(failed(Goal), _))
    ).

must_once(Goal, AccumA0, AccumA, AccumB0, AccumB) :-
    (  call(Goal, AccumA0, AccumA, AccumB0, AccumB)
    -> true
    ;  throw(error(failed(Goal), _))
    ).
