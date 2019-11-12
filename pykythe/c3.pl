% -*- mode: Prolog -*-

%% Translation of c3.dylan into Prolog, with some help from the Python
%% definition.
%% https://en.wikipedia.org/wiki/C3_linearization
%% https://www.python.org/download/releases/2.3/mro/
%% http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.19.3910&rep=rep1&type=pdf

%% See also
%% https://www.python.org/dev/peps/pep-0253/#mro-method-resolution-order-the-lookup-rule
%% which gives the old rule, which we can fall back to on failure:
%% left-right depth-first-search of the class hierarchy and then
%% discard all but the last occurence of each class.

%% This code depends on classes having "clean" base classes (that is,
%% only class_type and no import_ref_type).

:- module(c3, [mro/2, mro/3]).

:- use_module(library(apply), [include/3, maplist/3]).
:- use_module(library(lists), [append/2, member/2]).

:- meta_predicate mro(2, +, -).

%! mro(:Bases, +Class:atom, -Mro:list(atom)) is semidet.
%% Failure means an inconsistent hierarchy
%% Requres bases/2 facts, each mapping a class name to a list of base class names
mro(Bases, Class, Mro) :-
    call(Bases, Class, ClassDirectBases),
    maplist(mro(Bases), ClassDirectBases, ClassMro),
    append([[[Class]], ClassMro, [ClassDirectBases]], ToMerge),
    mro_merge(ToMerge, Mro).

%! mro(+Class, -Mro:list) is nondet.
%% Like mro/3, but expects class_type(ClassName, ListOfBases), where
%% ListOfBases are recursively a list of ordset of class_type (or [], of course).
%% Fails if there's an inconsistency.
%% Deterministic if all the base classes are single type (not a union);
%% otherwise non-deterministic.
mro([], []). %% TODO: delete this clause?
mro(class_type(Class,ClassDirectBases), Mro) :-
    maplist(select_one, ClassDirectBases, ClassDirectBasesOne),
    maplist(mro, ClassDirectBasesOne, ClassMro),
    maplist(class_only, ClassDirectBases, ClassDirectBasesNames),
    append([[[Class]], ClassMro, [ClassDirectBasesNames]], ToMerge),
    mro_merge(ToMerge, Mro).
%% A module type can happen if an invalid module has been specified
%% (that is, we can't resolve it), so just skip it.
mro(module_type(_ModuleType), []).

select_one(List, One) :-
    member(One, List).

class_only(TypeUnion, Class) :-
    member(class_type(Class, _ClassDirectBases), TypeUnion).

%! mro_merge(+Seqs:list(list(atom)), -Mro:list(atom)) is semidet.
mro_merge([], []) :- !.
mro_merge([[]|Seqs], Mro) :- !,
    mro_merge(Seqs, Mro).
mro_merge(Seqs, [Candidate|Mro2]) :-
    %% TODO: if mro_merge_candidate fails, skip and keep going?
    mro_merge_candidate(Seqs, Candidate),  %% can fail if inconsistent hierarchy
    maplist(remove_candidate(Candidate), Seqs, Seqs2),
    mro_merge(Seqs2, Mro2).

%! mro_merg_candidate(+Seqs:list(list(atom)), +Candidate:atom) is semidet.
mro_merge_candidate(Seqs, Candidate) :-
    Seqs = [[Candidate0|_]|SeqsTail],
    (  include(in_tail(Candidate0), Seqs, [])
    -> Candidate = Candidate0
    ;  mro_merge_candidate(SeqsTail, Candidate)
    ).

%! in_tail(+X, List:list) is semidet.
in_tail(X, [_|Tail]) :-
    memberchk(X, Tail).

%! remove_candidate(+Candidate:atom, +Seq:list(atom), -SeqOut:list(atom)) is det.
remove_candidate(Candidate, [Candidate|Seq], Seq) :- !.
remove_candidate(_, Seq, Seq).
