:- module(rdet, [
    rdet/1  % +PredicateIndicator
]).

:- use_module(library(error)).
:- use_module(library(debug), [debug/3]).
:- use_module(library(prolog_code), [pi_head/2]).
:- use_module(library(prolog_wrap), [wrap_predicate/4]).
:- meta_predicate(rdet(:)).

rdet(PredicateIndicator):-
    must_be(ground, PredicateIndicator),
    (   (   PredicateIndicator = Module:Name/Arity
        ;   PredicateIndicator = Module:Name//Arity)
    ->  must_be(atom, Module),
        must_be(atom, Name),
        must_be(integer, Arity)
    ;   throw(error(invalid_rdet_pi(PredicateIndicator), _))),
    debug(rdet, 'rdet: adding goal: ~w', [PredicateIndicator]),
    pi_head(PredicateIndicator, Head),
    wrap_predicate(Head, rdet_wrapper, Closure,
        (Closure -> true ; throw(error(goal_failed(PredicateIndicator), _)))).

:- multifile(prolog:message//1).

% Provides messages to terminal.

prolog:message(error(goal_failed(Name/Arity), _)) -->
    ['Goal ~w failed.'-[Name/Arity]].

prolog:message(error(invalid_rdet_pi(PredicateIndicator), _)) -->
    ['Invalid rdet annotation: ~w (not a predicate indicator).'-[PredicateIndicator]].
