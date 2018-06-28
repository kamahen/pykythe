% -*- mode: Prolog -*-

:- module(must_once, [must_once/1,
                      must_once_msg/3,
                      must_once/3,
                      must_once/4,
                      must_once/5,
                      must_once/6]).
:- meta_predicate must_once(1).
:- meta_predicate must_once_msg(1, +, +).
:- meta_predicate must_once(2, ?, ?).
:- meta_predicate must_once(3, ?, ?, ?).
:- meta_predicate must_once(4, ?, ?, ?, ?).
:- meta_predicate must_once(5, ?, ?, ?, ?, ?).

% Like once/1, but also works with EDCGs.
% You must add edcg:pred_info(must_once, 1, ...) facts in the using module.

%! must_once(:Goal) is det.
%  Throws an error if Goal doesn't succeed.
must_once(Goal) :-
    (  call(Goal)
    -> true
    ;  throw(error(failed(Goal), _))
    ).

must_once_msg(Goal, Msg, MsgArgs) :-
    (  call(Goal)
    -> true
     ; functor(Goal, Pred, Arity),
       format(string(MsgStr), Msg, MsgArgs),
       throw(error(failed(Goal), context(Pred/Arity, MsgStr)))
    ).

% edcg doesn't understand the meta-pred "call", so expand -->> by hand.
% The arg names are suggestive of use; in reallity, these work with
% any combination of extra params that givde the appropriate arity.

must_once(Goal, AccumA0, AccumA) :-
    (  call(Goal, AccumA0, AccumA)
    -> true
    ;  throw(error(failed(Goal), _))
    ).

must_once(Goal, AccumA0, AccumA, PassA) :-
    (  call(Goal, AccumA0, AccumA, PassA)
    -> true
    ;  throw(error(failed(Goal), _))
    ).

must_once(Goal, AccumA0, AccumA, AccumB0, AccumB) :-
    (  call(Goal, AccumA0, AccumA, AccumB0, AccumB)
    -> true
    ;  throw(error(failed(Goal), _))
    ).

must_once(Goal, AccumA0, AccumA, AccumB0, AccumB, PassA) :-
    (  call(Goal, AccumA0, AccumA, AccumB0, AccumB, PassA)
    -> true
    ;  throw(error(failed(Goal), _))
    ).
