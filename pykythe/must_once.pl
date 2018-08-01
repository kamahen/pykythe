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

% Like once/1, throws an exception on failure.
% Also works with works with EDCGs.
% You must add edcg:pred_info(must_once, 1, ...) facts in the using module.

% TODO: write a user:term_expansion that transforms
%         :- det_pred(foo/_).
%         foo(X, Y, Z) :- % clause 1 ...
%         foo(X, Y, Z) :- % clause 2 ...
%       into
%         foo(X, Y, Z) :-
%             (  'foo must_once'(X, Y, Z)
%             -> true
%             ;  throw(error(failed(X, Y, Z), _))
%         ).
%         'foo must_once'(X, Y, Z) :- % clause 1 ...
%         'foo must_once'(X, Y, Z) :- % clause 2 ...
%       and similar for -->> clauses.

% TODO: something like this, to detect non-determinism
%% deterministic(Goal) :-
%%     setup_call_cleanup(true, Goal, Deterministic = true),
%%     (  var(Deterministic)
%%     -> !,
%%        throw(error(failed(Goal), _))
%%     ;  true
%%     ).
%% deterministic(Goal, Deterministic) :-
%%     setup_call_cleanup(true, Goal, Deterministic = true),
%%     (  var(Deterministic)
%%     -> Deterministic = false
%%     ;  true
%%     ),
%%     !.

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
