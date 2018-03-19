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
    % writeln('Reading from': FqnExprFile),
    open(FqnExprFile, read, FqnExprStream),
    json_read_dict(FqnExprStream, MetaDict),
    assert_meta(MetaDict),
    json_read_dict(FqnExprStream, JsonDict),
    must_once(at_end_of_stream(FqnExprStream)),
    simplify_json(JsonDict, Node),
    %% print_term(Node, [tab_width(0),
    %%                   indent_arguments(2),
    %%                   right_margin(80)]),
    %% write('.'), nl,
    phrase(kythe_json(Node), Result),
    % sort(Result, ResultDedup),
    ResultDedup = Result,
    current_output(KytheStream),
    %% write(KytheStream, "%% === Kythe ==="), nl(KytheStream),
    output_anchors(ResultDedup, KytheStream).

must_once(Goal) :-
    (  Goal
    -> true
    ;  throw(error(failed, context(goal, Goal)))
    ).

must_once(Goal) -->
    (  Goal
    -> { true }
    ;  { throw(error(failed, context(goal, Goal))) }
    ).


%% Save the meta-data as facts.

assert_meta(_{kind: "Meta",
              slots: _{
                  corpus: _{
                      kind: "str",
                      value: Corpus},
                  root: _{
                      kind: "str",
                      value: Root},
                  path: _{
                      kind: "str",
                      value: Path},
                  language: _{
                      kind: "str",
                      value: Language},
                  contents_b64: _{
                      kind: "str",
                      value: ContentsB64}}}) :-
    retractall(corpus_root_path_language(_Corpus, _Root, _Path, _Language)),
    retractall(file_contents(_)),
    assertz(corpus_root_path_language(Corpus, Root, Path, Language)),
    assertz(file_contents_b64(ContentsB64)).

%% Simplify the JSON term into more specific dicts, each one
%% distinguished by its tag.

simplify_json(Json, Prolog) :-
    must_once(simplify_json2(Json, Prolog)).

simplify_json2([], []).
simplify_json2([V|Vs], Values) :-
    simplify_json_list([V|Vs], Values).
simplify_json2(_{kind: "str", value: Value}, str(Value)).
simplify_json2(_{kind: "int", value: Value}, int(Value)).
simplify_json2(_{kind: "bool", value: Value}, bool(Value)).
simplify_json2(_{kind: "None"}, none).
simplify_json2(_{kind: "dict", items: Items}, Value) :-
    dict_pairs(Items, _, ItemPairs),
    simplify_json_slot_pairs(ItemPairs, ItemPairs2),
    dict_create(Value, dict, ItemPairs2).
simplify_json2(_{kind: Kind, slots: Slots}, Value) :-
    dict_pairs(Slots, _, SlotPairs),
    simplify_json_slot_pairs(SlotPairs, SlotPairs2),
    atom_string(KindAtom, Kind),
    dict_create(Value, KindAtom, SlotPairs2).

simplify_json_list([], []).
simplify_json_list([V|Vs], [Value|Values]) :-
    simplify_json(V, Value),
    simplify_json_list(Vs, Values).

simplify_json_slot_pairs([], []).
simplify_json_slot_pairs([K-V|KVs], [K-V2|KVs2]) :-
    simplify_json(V, V2),
    simplify_json_slot_pairs(KVs, KVs2).

kythe_json(Node) -->
    { corpus_root_path_language(Corpus, Root, Path, _Language) },
    { Source = _{corpus: Corpus, root: Root, path: Path} },
    kythe_fact(Source, 'kythe/node/kind/', 'file'),
    { file_contents_b64(ContentsB64) },
    kythe_fact_b64(Source, 'kythe/text', ContentsB64),
    % x-numlines
    % x-html
    node_anchors(Node).

%% Extract anchors (with FQNs) from the the AST nodes

node_anchors(Node) -->
    must_once(node_anchors2(Node)).

node_anchors2([]) --> [].
node_anchors2([Node|Nodes]) --> !,
    node_anchors(Node),
    node_anchors(Nodes).
node_anchors2(bool(_)) --> [].
node_anchors2(dict(_)) --> [].
node_anchors2(int(_)) --> [].
node_anchors2(none) --> [].
node_anchors2(str(_)) --> [].

node_anchors2('AnnAssignStmt'{expr: Expr, lhs_type: Lhs_type, lhs: Lhs}) -->
    node_anchors(Expr),
    node_anchors(Lhs_type),
    node_anchors(Lhs).
node_anchors2('ArgumentNode'{arg: Arg, name: _Name}) -->
    node_anchors(Arg).
    % node_anchors(_Name).
node_anchors2('AsNameNode'{as_name: As_name, name: _Name}) -->
    node_anchors(As_name).
    % node_anchors(_Name).
node_anchors2('AssertStmt'{items: Items}) -->
    node_anchors(Items).
node_anchors2('AssignExprStmt'{expr: Expr, lhs: Lhs}) -->
    node_anchors(Expr),
    node_anchors(Lhs).
node_anchors2('Astn'{end: End, start: Start, value: Value}) -->
    node_anchors(End),
    node_anchors(Start),
    node_anchors(Value).
node_anchors2('AtomCallNode'{args: Args, atom: Atom}) -->
    node_anchors(Args),
    node_anchors(Atom).
node_anchors2('AtomDotNode'{atom: Atom, attr_name: Attr_name}) -->
    node_anchors(Atom),
    node_anchors(Attr_name).
node_anchors2('AtomSubscriptNode'{atom: Atom, subscripts: Subscripts}) -->
    node_anchors(Atom),
    node_anchors(Subscripts).
node_anchors2('AugAssignStmt'{augassign: Augassign, expr: Expr, lhs: Lhs}) -->
    node_anchors(Augassign),
    node_anchors(Expr),
    node_anchors(Lhs).
node_anchors2('BreakStmt'{items: Items}) -->
    node_anchors(Items).
node_anchors2('Class'{bases: Bases, fqn: Fqn, name: _Name}) -->
    % TODO: /kythe/node/kind record
    % TODO: /kythe/subkind class
    % Do not do this becuase it'll cause a dup defines/binding: node_anchors(_Name).
    node_anchors(Bases),
    node_anchors(Fqn).
node_anchors2('ColonNode'{items: Items}) -->
    node_anchors(Items).
node_anchors2('CompFor'{comp_iter: Comp_iter, for_astn: For_astn, for_exprlist: For_exprlist, in_testlist: In_testlist}) -->
    node_anchors(Comp_iter),
    node_anchors(For_astn),
    node_anchors(For_exprlist),
    node_anchors(In_testlist).
node_anchors2('CompIfCompIterNode'{comp_iter: Comp_iter, value_expr: Value_expr}) -->
    node_anchors(Comp_iter),
    node_anchors(Value_expr).
node_anchors2('ContinueStmt'{items: Items}) -->
    node_anchors(Items).
node_anchors2('DecoratedStmt'{items: Items}) -->
    node_anchors(Items).
node_anchors2('DecoratorsNode'{items: Items}) -->
    node_anchors(Items).
node_anchors2('DelStmt'{items: Items}) -->
    node_anchors(Items).
node_anchors2('DictGenListSetMakerCompForNode'{comp_for: Comp_for, value_expr: Value_expr}) -->
    node_anchors(Comp_for),
    node_anchors(Value_expr).
node_anchors2('DictSetMakerNode'{items: Items}) -->
    node_anchors(Items).
node_anchors2('DottedNameNode'{items: Items}) -->
    node_anchors(Items).
node_anchors2('EllipsisNode'{}) --> !.
node_anchors2('ExceptClauseNode'{as_item: As_item, expr: Expr}) -->
    node_anchors(As_item),
    node_anchors(Expr).
node_anchors2('FileInput'{scope_bindings: _Scope_bindings, stmts: Stmts}) -->
    %% node_anchors(Scope_bindings),
    node_anchors(Stmts).
node_anchors2('ForStmt'{else_suite: Else_suite, for_exprlist: For_exprlist, in_testlist: In_testlist, suite: Suite}) -->
    node_anchors(Else_suite),
    node_anchors(For_exprlist),
    node_anchors(In_testlist),
    node_anchors(Suite).
node_anchors2('Func'{fqn: Fqn, name: _Name, parameters: Parameters, return_type: Return_type}) -->
    % TODO: /kythe/node/kind function
    node_anchors(Fqn),
    % Do not do this becuase it'll cause a dup defines/binding: node_anchors(_Name).
    node_anchors(Parameters),
    node_anchors(Return_type).
node_anchors2('GlobalStmt'{items: Items}) -->
    node_anchors(Items).
node_anchors2('IfStmt'{items: Items}) -->
    node_anchors(Items).
node_anchors2('ImportAsNamesNode'{items: Items}) -->
    node_anchors(Items).
node_anchors2('ImportDottedAsNameNode'{as_name: As_name, dotted_name: Dotted_name}) -->
    node_anchors(As_name),
    node_anchors(Dotted_name).
node_anchors2('ImportDottedAsNamesNode'{items: Items}) -->
    node_anchors(Items).
node_anchors2('ImportFromStmt'{from_name: From_name, import_part: Import_part}) -->
    node_anchors(From_name),
    node_anchors(Import_part).
node_anchors2('ImportNameNode'{dotted_as_names: Dotted_as_names}) -->
    node_anchors(Dotted_as_names).
node_anchors2('ListMakerNode'{items: Items}) -->
    node_anchors(Items).
%% TODO: remove NameNode -- it seems to come fromImportFromStmt et al
%%       and should be refined.
node_anchors2('NameNode'{binds: Binds, name: Name}) -->
    node_anchors(Binds),
    node_anchors(Name).
node_anchors2('NameBinds'{fqn: str(Fqn), name: 'Astn'{value:str(_Token), start: int(Start), end: int(End)}}) -->
    anchor(Start, End, Source),
    %% TODO: /kythe/node/kind variable  (but not if it's a Class or Function)
    edge(Source, 'kythe/edge/defines/binding', Fqn).
node_anchors2('NameRef'{fqn: str(Fqn), name: 'Astn'{value:str(_Token), start: int(Start), end: int(End)}}) -->
    anchor(Start, End, Source),
    edge(Source, '/kythe/edge/ref', Fqn).
node_anchors2('NonLocalStmt'{items: Items}) -->
    node_anchors(Items).
node_anchors2('NumberNode'{astn: Astn}) -->
    node_anchors(Astn).
node_anchors2('OmittedNode'{}) --> !.
node_anchors2('OpNode'{args: Args, op_astns: Op_astns}) -->
    node_anchors(Args),
    node_anchors(Op_astns).
node_anchors2('PassStmt'{items: Items}) -->
    node_anchors(Items).
node_anchors2('RaiseStmt'{items: Items}) -->
    node_anchors(Items).
node_anchors2('StarNode'{}) --> !.
node_anchors2('Stmts'{items: Items}) -->
    node_anchors(Items).
node_anchors2('StringNode'{astns: Astns}) -->
    node_anchors(Astns).
node_anchors2('SubscriptNode'{expr1: Expr1, expr2: Expr2, expr3: Expr3}) -->
    node_anchors(Expr1),
    node_anchors(Expr2),
    node_anchors(Expr3).
node_anchors2('TestListNode'{items: Items}) -->
    node_anchors(Items).
node_anchors2('TnameNode'{name: Name, type_expr: Type_expr}) -->
    node_anchors(Name),
    node_anchors(Type_expr).
node_anchors2('TryStmt'{items: Items}) -->
    node_anchors(Items).
node_anchors2('TypedArgNode'{expr: Expr, name: Name}) -->
    node_anchors(Expr),
    node_anchors(Name).
node_anchors2('WhileStmt'{else_suite: Else_suite, suite: Suite, test: Test}) -->
    node_anchors(Else_suite),
    node_anchors(Suite),
    node_anchors(Test).
node_anchors2('WithItemNode'{as_item: As_item, item: Item}) -->
    node_anchors(As_item),
    node_anchors(Item).
node_anchors2('WithStmt'{items: Items, suite: Suite}) -->
    node_anchors(Items),
    node_anchors(Suite).

anchor(Start, End, Source) -->
    { format(string(Signature), '@~d:~d', [Start, End]) },
    { corpus_root_path_language(Corpus, Root, Path, _Language) },
    { Source = _{signature: Signature, corpus: Corpus, root: Root, path: Path} },
    kythe_fact(Source, '/kythe/node/kind', anchor),
    kythe_fact(Source, '/kythe/loc/start', Start),
    kythe_fact(Source, '/kythe/loc/end', End).

edge(Source, EdgeKind, Fqn) -->
    { corpus_root_path_language(Corpus, Root, _Path, Language) },
    { Target = _{signature: Fqn, corpus: Corpus, root: Root, language: Language} },
    kythe_edge(Source, EdgeKind, Target).

kythe_fact(Source, FactName, FactValue) -->
    { base64(FactValue, FactBase64) },
    kythe_fact_b64(Source, FactName, FactBase64).

kythe_fact_b64(Source, FactName, FactBase64) -->
    [ _{source: Source, fact_name: FactName, fact_value: FactBase64} ].

kythe_edge(Source, EdgeKind, Target) -->
    [ _{source: Source, edge_kind: EdgeKind, target: Target, fact_name: '/'} ].

%% The following was used to create the node_anchors patterns above:
%% node_anchors(Dict) -->
%%     { dict_pairs(Dict, Tag, Pairs) }, !,
%%     { keys(Pairs, Keys) },
%%     [ dict(Tag, Keys) ],
%%     values(Pairs).

%% keys([], []).
%% keys([K-_|KVs], [K|Ks]) :-
%%     keys(KVs, Ks).

%% values([]) --> [].
%% values([_-V|KVs]) -->
%%     node_anchors(V),
%%     values(KVs).

%% output_facts([], _KytheStream).
%% output_facts([dict(Tag, Keys)|Rs], KytheStream) :-
%%     dict_keys(Keys, DictKeys),
%%     dict_create(Rdict, Tag, DictKeys),
%%     writeq(KytheStream, node_anchors(Rdict)),
%%     write(KytheStream, ' :- !, '),
%%     output_facts_goals(Keys, KytheStream),
%%     write(KytheStream, '.'),
%%     nl(KytheStream),
%%     output_facts(Rs, KytheStream).

%% dict_keys([], []).
%% dict_keys([Key|Keys], [Key-Key|DictKeys]) :-
%%     dict_keys(Keys, DictKeys).

%% output_facts_goals([], _).
%% output_facts_goals([Key|Keys], KytheStream) :-
%%     write(KytheStream, ','),
%%     nl(KytheStream),
%%     write(KytheStream, '    '),
%%     write(KytheStream, node_anchors(-Key)),
%%     output_facts_goals(Keys, KytheStream).

output_anchors([], KytheStream) :-
    nl(KytheStream).
output_anchors([Anchor|Anchors], KytheStream) :-
    must_once(output_anchor(Anchor, KytheStream)),
    output_anchors(Anchors, KytheStream).

output_anchor(Dict, KytheStream) :-
    json_write_dict(KytheStream, Dict, [width(0)]),
    nl(KytheStream).
