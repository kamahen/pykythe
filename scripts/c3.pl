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

%! mro(+Class:atom, -Mro:list(atom)) is semidet
%% Failure means an inconsistent hierarchy
mro(Class, Mro) :-
    bases(Class, ClassDirectBases),
    maplist(mro, ClassDirectBases, ClassMro),
    append([[[Class]], ClassMro, [ClassDirectBases]], ToMerge),
    mro_merge(ToMerge, Mro).

%! mro_merge(+Seqs:list(list(atom)), -Mro:list(atom) is semidet.
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

%% Tests
%% TODO: use test framework for these

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

?- mro('Z',  ['Z', 'K1', 'K2', 'K3', 'D', 'A', 'B', 'C', 'E', 'O', object]).
?- mro('K1', ['K1', 'A', 'B', 'C', 'O', object]).
?- mro('K2', ['K2', 'D', 'B', 'E', 'O', object]).
?- mro('K3', ['K3', 'D', 'A', 'O', object]).
?- mro('D', ['D', 'O', object]).
?- mro('A', ['A', 'O', object]).
?- mro('B', ['B', 'O', object]).
?- mro('C', ['C', 'O', object]).
?- mro('E', ['E', 'O', object]).
?- mro('O', ['O', object]).
?- mro(object, [object]).

