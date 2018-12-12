% -*- mode: Prolog -*-

:- module(must_once, [must_once/1,
                      must_once_msg/2,
                      must_once_msg/3,
                      must_once/3,
                      must_once/4,
                      must_once/5,
                      must_once/6,
                      fail/1]).
:- meta_predicate
       must_once(0),
       must_once_msg(0, +, +),
       must_once(4, ?, ?),
       must_once(5, ?, ?, ?),
       must_once(6, ?, ?, ?, ?),
       must_once(7, ?, ?, ?, ?, ?).

% Like once/1, throws an exception on failure.
% Also works with works with EDCGs.
% You must add edcg:pred_info(must_once, 1, ...) facts in the using module.

% TODO: throw(error(must_once_failed(Goal))) without the extra "_" arg?

% TODO: write a user:term_expansion that transforms
%         :- det_pred(foo/_).
%         foo(X, Y, Z) :- % clause 1 ...
%         foo(X, Y, Z) :- % clause 2 ...
%       into
%         foo(X, Y, Z) :-
%             (  'foo must_once'(X, Y, Z)
%             -> true
%             ;  throw(error(must_once_failed(foo(X, Y, Z)), _))
%         ).
%         'foo must_once'(X, Y, Z) :- % clause 1 ...
%         'foo must_once'(X, Y, Z) :- % clause 2 ...
%       and similar for -->> clauses.

% TODO: something like this, to detect non-determinism
%% deterministic(Goal) :-
%%     setup_call_cleanup(true, Goal, Deterministic = true),
%%     (  var(Deterministic)
%%     -> !,
%%        throw(error(must_once_failed(Goal), _))
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
    ;  throw(error(must_once_failed(Goal), _))
    ).

must_once_msg(Goal, Msg) :-
    must_once_msg(Goal, Msg,  []).

must_once_msg(Goal, Msg, MsgArgs) :-
    (  call(Goal)
    -> true
     ; functor(Goal, Pred, Arity),
       format(string(MsgStr), Msg, MsgArgs),
       throw(error(must_once_failed(Goal), context(Pred/Arity, MsgStr)))
    ).

%% edcg doesn't understand the meta-pred "call", so expand -->> by
%% hand. Arg names AccumA0, etc. are suggestive of use; in reality,
%% these predicates work with any combination of extra params that
%% give the appropriate arity.

must_once(Goal, AccumA0, AccumA) :-
    (  call(Goal, AccumA0, AccumA)
    -> true
    ;  throw(error(must_once_failed(Goal), _))
    ).

must_once(Goal, AccumA0, AccumA, PassA) :-
    (  call(Goal, AccumA0, AccumA, PassA)
    -> true
    ;  throw(error(must_once_failed(Goal), _))
    ).

must_once(Goal, AccumA0, AccumA, AccumB0, AccumB) :-
    (  call(Goal, AccumA0, AccumA, AccumB0, AccumB)
    -> true
    ;  throw(error(must_once_failed(Goal), _))
    ).

must_once(Goal, AccumA0, AccumA, AccumB0, AccumB, PassA) :-
    (  call(Goal, AccumA0, AccumA, AccumB0, AccumB, PassA)
    -> true
    ;  throw(error(must_once_failed(Goal), _))
    ).

fail(_) :-
    fail.


% TODO: (taken from library(rdet))
%   % TODO: message_hook instead of prolog:message?
%   :- multifile(prolog:message//1).
%   prolog:message(error(must_once_failed(Goal), _)) -->
%       ['Goal failed: ~w'-[Goal]].
