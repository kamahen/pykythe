%% Tests for c3.pl

:- use_module(library(plunit)).

:- begin_tests(c3).

:- use_module(library(apply), [maplist/3]).
:- use_module('../pykythe/c3', [mro/2, mro/3]).

%% Define bases from c3.py example
bases(object, []).
bases('O', [object]).
bases('A', ['O']).
bases('B', ['O']).
bases('C', ['O']).
bases('D', ['O']).
bases('E', ['O']).
bases('K1', ['A', 'B', 'C']).
bases('K2', ['D', 'B', 'E']).
bases('K3', ['D', 'A']).
bases('Z', ['K1', 'K2', 'K3']).

class_and_bases(Class, class_type(Class,BaseClasses)) :-
    bases(Class, BaseClassNames),
    maplist(class_and_bases, BaseClassNames, BaseClasses0),
    maplist(wrap_in_list, BaseClasses0, BaseClasses).

wrap_in_list(X, [X]).

test_mro_both(Class, ExpectedResult) :-
    mro(bases, Class, Result1),
    Result1 == ExpectedResult,
    class_and_bases(Class, ClassAndBases),
    mro(ClassAndBases, Result2),
    Result2 == ExpectedResult.

test(z) :-
    test_mro_both('Z',  ['Z', 'K1', 'K2', 'K3', 'D', 'A', 'B', 'C', 'E', 'O', object]).
test(k1) :-
    test_mro_both('K1', ['K1', 'A', 'B', 'C', 'O', object]).
test(k2) :-
    test_mro_both('K2', ['K2', 'D', 'B', 'E', 'O', object]).
test(k3) :-
    test_mro_both('K3', ['K3', 'D', 'A', 'O', object]).
test(d) :-
    test_mro_both('D', ['D', 'O', object]).
test(a) :-
    test_mro_both('A', ['A', 'O', object]).
test(b) :-
    test_mro_both('B', ['B', 'O', object]).
test(c) :-
    test_mro_both('C', ['C', 'O', object]).
test(e) :-
    test_mro_both('E', ['E', 'O', object]).
test(o) :-
    test_mro_both('O', ['O', object]).
test(object) :-
    test_mro_both(object, [object]).

end_tests(c3).

?- run_tests.
%% ?- halt.
