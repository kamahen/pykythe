#!/usr/bin/env swipl
% -*- Mode: Prolog -*-

    % This reads in the "fqn_expr" facts (in JSON format), processes
    % them, and outputs additional Kythe facts (in JSON format).

:- use_module(library(http/json)).
:- use_module(library(optparse)).

:- initialization main.

main :-
    OptsSpec = [],
    opt_arguments(OptsSpec, _Opts, PositionalArgs),
    (  PositionalArgs = [FqnExprFile]
    -> true
    ;  PositionalArgs = []
    -> FqnExprFile = '/dev/stdin'
    ;  throw(error(bad_positional_args, context(args, PositionalArgs)))
    ),
    process(FqnExprFile),
    halt.

process(FqnExprFile) :-
    writeln('Reading from': FqnExprFile),
    open(FqnExprFile, read, FqnExprStream),
    load_json_facts(FqnExprStream),
    process_facts,
    current_output(KytheStream),
    output_facts(KytheStream).

load_json_facts(FqnExprStream) :-
    ( at_end_of_stream(FqnExprStream)
     -> true
     ; json_read_dict(FqnExprStream, JsonDict),
       % writeq(JsonDict), nl,
       simplify_json(JsonDict, Processed),
       print_term(Processed, [tab_width(0), indent_arguments(2), right_margin(80)]),
       nl, nl,
       load_json_facts(FqnExprStream)
    ).

%% Simplify the JSON dict into more Prolog-ish terms.
%% simplify_json/2 matches on the possible dicts -- in the general case,
%% it extracts the slots as key-value pairs and uses simplify_term/3 to
%% match on them.

simplify_json([], []) :- !.
simplify_json([V|Vs], Values) :- !,
    simplify_json_list([V|Vs], Values).
simplify_json(_{type: "str", value: Value}, str(Value)) :- !.
simplify_json(_{type: "int", value: Value}, int(Value)) :- !.
simplify_json(_{type: "bool", value: Value}, bool(Value)) :- !.
simplify_json(_{type: "None"}, none) :- !.
%% TODO: "dict", "Leaf"
simplify_json(_{type: Type, slots: Slots}, Value) :- !,
    dict_pairs(Slots, _, SlotPairs),
    simplify_json_slot_pairs(SlotPairs, SlotPairs2),
    atom_string(TypeAtom, Type),
    simplify_term(TypeAtom, SlotPairs2, Value).
simplify_json(Fact) :-
    throw(error(bad_json, context(json, Fact))).

simplify_json_slot_pairs([], []).
simplify_json_slot_pairs([K-V|KVs], [K-V2|KVs2]) :-
    simplify_json(V, V2),
    simplify_json_slot_pairs(KVs, KVs2).

simplify_json_list([], []).
simplify_json_list([V|Vs], [Value|Values]) :-
    simplify_json(V, Value),
    simplify_json_list(Vs, Values).

simplify_term('Assign', [expr-Expr, lhs-Lhs], assign(Lhs, Expr)) :- !.
simplify_term('Call', [atom-Atom], call(Atom)) :- !.
simplify_term('Class', [bases-Bases, name-str(Name), scope_bindings-ScopeBindings],
             class(Name, Bases, ScopeBindings)) :- !.
simplify_term('Colon', [items-[One, Two]], colon(One, Two)) :- !.
simplify_term('Dict', [value-Value], dict(Value)) :- !.
simplify_term('Dot', [atom-Atom, attr_name-AttrName], dot(Atom, AttrName)) :- !.
simplify_term('EllipsisConst', [], ellipsis) :- !.
simplify_term('ExprList', [items-Items], exprlist(Items)) :- !.
simplify_term('Func', [fqn-str(Name), return_type-ReturnType], func(Name, ReturnType)) :- !.
simplify_term('ListMaker', [items-Items], listmaker(Items)) :- !.
simplify_term('Name', [fqn-str(Str)], name(Str)) :- !.
simplify_term('Number', [value-str(Str)], number(Str)) :- !.
simplify_term('Omitted', [], omitted) :- !.
simplify_term('Op', [items-Items], op(Items)) :- !.
simplify_term('String', [value-str(Str)], string(Str)) :- !.
simplify_term('Subscr', [atom-Atom], subscr(Atom)) :- !.
simplify_term('TestList', [items-Items], testlist(Items)) :- !.  %% TODO: exprlist(Items) ?
simplify_term('Union', [items-Items], union(Items)) :- !.
simplify_term(TypeAtom, SlotPairs, _) :- !,
    throw(error(unknown_json_dict, context(item, type_slots(TypeAtom, SlotPairs)))).
simplify_term(TypeAtom, SlotPairs, Result) :-
    dict_create(Result, TypeAtom, SlotPairs).

process_facts.  %% TODO: implement this

output_facts(_KytheStream).
