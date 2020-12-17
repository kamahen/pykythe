% -*- mode: Prolog -*-

%% Taken from library(rdet) - https://github.com/rla/rdet.git
%% And modified to provide a back-tracking version and also
%% a version that throw an exception if there are choicepoints left.

%% TODO: merge into https://github.com/kamahen/rdet.git
%% TODO: add test cases.

:- module(rdet2, [
    rdet/1,         % +PredicateIndicator
    rdet_bt/1,      % +PredicateIndicator
    rdet_semidet/1, % +PredicateIndicator
    rdet_det/1      % +PredicateIndicator
]).

:- use_module(library(error)).
:- use_module(library(debug), [debug/3]).
:- use_module(library(prolog_code), [pi_head/2]).
:- use_module(library(prolog_wrap), [wrap_predicate/4]).

:- meta_predicate rdet(0), rdet_bt(0), rdet_semidet(0), rdet_det(0).

%! rdet(:PredicateIndicator) is det.
% Mark PredicateIndicator as "must succeed once".
rdet(PredicateIndicator) :-
    rdet_head(PredicateIndicator, Head),
    debug(rdet, 'rdet: adding goal: ~w', [PredicateIndicator]),
    wrap_predicate(Head, rdet_wrapper, Closure,
                   (Closure -> true ; throw(error(goal_failed, context(_, PredicateIndicator))))).

%! rdet_bt(:PredicateIndicator) is det.
% Mark PredicateIndicator as "must succeed at least once (can backtrack)".
rdet_bt(PredicateIndicator) :-
    rdet_head(PredicateIndicator, Head),
    debug(rdet, 'rdet_bt: adding goal: ~w', [PredicateIndicator]),
    wrap_predicate(Head, rdet_bt_wrapper, Closure,
                   (Closure *-> true ; throw(error(goal_failed, context(_, PredicateIndicator))))).

%! rdet_semidet(:PredicateIndicator) is det.
% Mark PredicateIndicator as semidet (throw exception if choicepoint)
rdet_semidet(PredicateIndicator) :-
    rdet_head(PredicateIndicator, Head),
    debug(rdet, 'rdet_semidet: adding goal: ~w', [PredicateIndicator]),
    wrap_predicate(Head, rdet_semidet_wrapper, Closure,
                   (setup_call_cleanup(true, Closure, Det=yes),
                    (Det==yes -> true ;  throw(error(goal_not_det, context(_, PredicateIndicator)))))).

%! rdet_det(:PredicateIndicator) is det.
% Mark PredicateIndicator as det (throw exception if choicepoint or failure)
rdet_det(PredicateIndicator) :-
    rdet_semidet(PredicateIndicator),
    rdet_bt(PredicateIndicator).

rdet_head(PredicateIndicator, Head) :-
    must_be(ground, PredicateIndicator),
    (   (   PredicateIndicator = Module:Name/Arity
        ;   PredicateIndicator = Module:Name//Arity)
    ->  must_be(atom, Module),
        must_be(atom, Name),
        must_be(integer, Arity)
    ;   throw(error(invalid_rdet_pi, context(_, PredicateIndicator)))
    ),
    pi_head(PredicateIndicator, Head).

:- multifile(prolog:message//1).

% Provides messages to terminal.

prolog:message(error(goal_failed(Name/Arity), _)) -->
    ['Goal ~w failed.'-[Name/Arity]].

prolog:message(error(goal_not_det(Name/Arity), _)) -->
    ['Goal ~w not deterministic.'-[Name/Arity]].

prolog:message(error(invalid_rdet_pi(PredicateIndicator), _)) -->
    ['Invalid rdet annotation: ~w (not a predicate indicator).'-[PredicateIndicator]].
