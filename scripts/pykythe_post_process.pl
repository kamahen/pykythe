% -*- Mode: Prolog -*-

    % This reads in the "fqn_expr" facts (in JSON format), processes
    % them, and outputs additional Kythe facts (in JSON format).

:- use_module(library(http/json)).
:- use_module(library(optparse)).
:- use_module(library(edcg)).  % requires: ?- pack_install(edcg).
:- use_module(must_once).


%% "fqn" accumulator gets FQN anchor facts, each a dict to be output in JSON.
edcg:acc_info(fqn, T, Out, In, Out=[T|In]).
%% "expr" accumulator gets expressions that need interpreting.
edcg:acc_info(expr, T, Out, In, Out=[T|In]).

%% All -->> predicates have the same hidden arguments:
% edcg:pred_info(_, _, [fqn, expr]).
edcg:pred_info(must_once, 1, [fqn, expr]).
edcg:pred_info(kythe_json, 2, [fqn, expr]).
edcg:pred_info(kythe_file, 0, [fqn, expr]).
edcg:pred_info(node_anchors, 2, [fqn, expr]).
edcg:pred_info(node_anchors2, 2, [fqn, expr]).
edcg:pred_info(node_anchors_list, 2, [fqn, expr]).
edcg:pred_info(anchor, 3, [fqn, expr]).
edcg:pred_info(edge, 3, [fqn, expr]).
edcg:pred_info(kythe_fact, 3, [fqn, expr]).
edcg:pred_info(kythe_fact_b64, 3, [fqn, expr]).
edcg:pred_info(kythe_edge, 3, [fqn, expr]).


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
    must_once(process(FqnExprFile)),
    halt.

process(FqnExprFile) :-
    % writeln('Reading from': FqnExprFile),
    read_anchors_exprs(FqnExprFile, Nodes, Fqns, Exprs),
    (  fail  %% TODO: remove this debugging code
    -> write('% === EXPRS ==='), nl, nl,
       print_term(Exprs, [indent_arguments(auto),
                          right_margin(60)]),
       write('.'), nl, nl,
       write('% === end EXPRS ==='), nl, nl
    ;  true
    ),
    (  fail  %% TODO: remove this debugging code
    -> write('% === NODES ==='), nl, nl,
       print_term(Nodes, [tab_width(0),
                         indent_arguments(2),
                         right_margin(120)]),
       write('.'), nl, nl,
       write('% === end NODES ==='), nl, nl
    ;  true
    ),
    % sort(Fqns, FqnsDedup),
    FqnsDedup = Fqns,
    current_output(KytheStream),
    %% write(KytheStream, "%% === Kythe ==="), nl(KytheStream),
    output_anchors(FqnsDedup, KytheStream).

%% Read the JSON, convert to nodes tree, convert to nodes tree
%% with FQNs and extract exprs.
read_anchors_exprs(FqnExprFile, Nodes, Fqns, Exprs) :-
    open(FqnExprFile, read, FqnExprStream),
    must_once(json_read_dict(FqnExprStream, MetaDict)),
    must_once(assert_meta(MetaDict)),
    must_once(json_read_dict(FqnExprStream, JsonDict)),
    must_once(at_end_of_stream(FqnExprStream)),
    simplify_json(JsonDict, Nodes),
    kythe_json(Nodes, _Expr, Fqns, Exprs).

%% Save the meta-data as facts.
assert_meta(
        _{kind: "Meta",
          slots: _{
            corpus: _{kind: "str", value: Corpus},
            root: _{kind: "str", value: Root},
            path: _{kind: "str", value: Path},
            language: _{kind: "str", value: Language},
            contents_b64: _{kind: "str", value: ContentsB64}}}) :-
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

kythe_json(Node, Expr, Fqns, Exprs) :- kythe_json(Node, Expr, Fqns, [], Exprs, []).  % phrase(kythe_json(...))
kythe_json(Node, Expr) -->>
    kythe_file,
    node_anchors(Node, Expr).

kythe_file -->>
    % TODO: output x-numlines, x-html ?
    { corpus_root_path_language(Corpus, Root, Path, _Language) },
    { Source = _{corpus: Corpus, root: Root, path: Path} },
    kythe_fact(Source, 'kythe/node/kind/', 'file'),
    { file_contents_b64(ContentsB64) },
    kythe_fact_b64(Source, 'kythe/text', ContentsB64).

%% Extract anchors (with FQNs) from the the AST nodes

node_anchors(Node, Expr) -->>
    must_once(node_anchors2(Node, Expr)).

% TODO: the following items (lists, atoms) should be subsumed into where
%       they're needed
node_anchors2([], todo_list([])) -->> [ ].
node_anchors2([Node|Nodes], todo_list([NodeExpr|NodesExpr])) -->>
    node_anchors(Node, NodeExpr),
    node_anchors(Nodes, NodesExpr).
node_anchors2(bool(_), bool) -->> [ ].
node_anchors2(dict(T), dict(T)) -->> [ ].
node_anchors2(int(_), int) -->> [ ].  % TODO: delete (used only by 'Astn'?)
node_anchors2(none, none) -->> [ ].
node_anchors2(str(_), str) -->> [ ].  % TODO: delete (used only by 'Astn'?)

node_anchors2('AnnAssignStmt'{expr: RhsExpr, lhs_type: LhsType, lhs: Lhs}, ann_assign) -->>
    node_anchors(RhsExpr, RhsExprExpr),
    node_anchors(LhsType, LhsTypeExpr),
    node_anchors(Lhs, LhsExpr),
    [ annotate_var(LhsExpr, LhsTypeExpr) ]:expr,
    (  { RhsExprExpr = omitted }
    -> [ ]
    ;  [ assign(LhsExpr, RhsExprExpr) ]:expr
    ).
node_anchors2('ArgumentNode'{name: _Name, arg: Arg}, arg) -->>
    % TODO: match _Name to func def param
    node_anchors(Arg, _).
node_anchors2('AsNameNode'{name: Name, as_name: AsName}, Expr) -->>
    node_anchors(AsName, AsNameExpr),
    { Name = 'NameRef'{fqn:Fqn, name:_} },
    { Expr = asname(Fqn, AsNameExpr) },
    [ Expr ]:expr.
node_anchors2('AssertStmt'{items: Items}, stmt) -->>
    node_anchors(Items, _).
node_anchors2('AssignExprStmt'{expr: RhsExpr, lhs_list: LhsList}, Expr) -->>
    node_anchors(RhsExpr, RhsExprExpr),
    node_anchors_list(LhsList, LhsListExpr),
    { Expr = assign(LhsListExpr, RhsExprExpr) },
    [ Expr ]:expr.
node_anchors2('Astn'{start: int(Start), end: int(End), value: str(Value)}, astn(Start, End, Value)) -->> [ ].
node_anchors2('AtomCallNode'{args: Args, atom: Atom}, call(AtomExpr)) -->>
    node_anchors(Atom, AtomExpr),
    node_anchors(Args, _).
node_anchors2('AtomDotNode'{atom: Atom, attr_name: AttrName, binds: bool(Binds)}, dot(AtomExpr, AttrNameExpr, BindsAtom)) -->>
    { binds_atom(Binds, BindsAtom) },
    node_anchors(Atom, AtomExpr),
    node_anchors(AttrName, AttrNameExpr).  %% 'Astn'{...} => astn(Start, End, Value)
node_anchors2('AtomSubscriptNode'{atom: Atom, subscripts: Subscripts}, subscr(AtomExpr)) -->>
    node_anchors(Atom, AtomExpr),
    node_anchors(Subscripts, _).
node_anchors2('AugAssignStmt'{augassign: Augassign, expr: Expr, lhs: Lhs}, todo_expr(augassign)) -->>
    node_anchors(Augassign, _),
    node_anchors(Expr, _),
    node_anchors(Lhs, _).
node_anchors2('BreakStmt'{items: Items}, todo_expr(break)) -->>
    node_anchors(Items, _).
node_anchors2('Class'{bases: Bases, fqn: str(Fqn), name: _Name}, Expr) -->>
    % TODO: /kythe/node/kind record
    % TODO: /kythe/subkind class
    % Note: node_anchors(_Name) would cause a dup defines/binding because it's in an 'AssignExprStmt'
    node_anchors_list(Bases, BasesExpr),
    { Expr = class(Fqn, BasesExpr) }.
node_anchors2('ColonNode'{items: Items}, todo_expr(colon)) -->>
    node_anchors(Items, _).
node_anchors2('CompFor'{comp_iter: Comp_iter, for_astn: For_astn, for_exprlist: For_exprlist, in_testlist: In_testlist}, todo_expr(compfor)) -->>
    node_anchors(Comp_iter, _),
    node_anchors(For_astn, _),
    node_anchors(For_exprlist, _),
    node_anchors(In_testlist, _).
node_anchors2('CompIfCompIterNode'{comp_iter: Comp_iter, value_expr: Value_expr}, todo_expr(compifcomiter)) -->>
    node_anchors(Comp_iter, _),
    node_anchors(Value_expr, _).
node_anchors2('ContinueStmt'{items: Items}, todo_expr(continue)) -->>
    node_anchors(Items, _).
node_anchors2('DecoratedStmt'{items: Items}, todo_expr(decorated)) -->>
    node_anchors(Items, _).
node_anchors2('DecoratorsNode'{items: Items}, todo_expr(decorators)) -->>
    node_anchors(Items, _).
node_anchors2('DelStmt'{items: Items}, todo_expr(del)) -->>
    node_anchors(Items, _).
node_anchors2('DictGenListSetMakerCompForNode'{comp_for: Comp_for, value_expr: Value_expr}, todo_expr(dictgen)) -->>
    node_anchors(Comp_for, _),
    node_anchors(Value_expr, _).
node_anchors2('DictSetMakerNode'{items: Items}, todo_expr(dictset)) -->>
    node_anchors(Items, _).
node_anchors2('DottedNameNode'{items: Items}, todo_expr(dottedname)) -->>  % TODO: fqn accumulator
    node_anchors(Items, _).
node_anchors2('EllipsisNode'{}, ellipsis) -->> [ ].
node_anchors2('ExceptClauseNode'{as_item: As_item, expr: Expr}, todo_expr(except)) -->>
    node_anchors(As_item, _),
    node_anchors(Expr, _).
node_anchors2('ExprListNode'{items: Items}, exprlist(ItemsExpr)) -->>
    node_anchors_list(Items, ItemsExpr).
node_anchors2('FileInput'{scope_bindings: _ScopeBindings, stmts: Stmts}, todo_expr(file)) -->>
    %% node_anchors(ScopeBindings, _),
    node_anchors(Stmts, _).
node_anchors2('ForStmt'{else_suite: Else_suite, for_exprlist: For_exprlist, in_testlist: In_testlist, suite: Suite}, todo_expr(for)) -->>
    node_anchors(Else_suite, _),
    node_anchors(For_exprlist, _),
    node_anchors(In_testlist, _),
    node_anchors(Suite, _).
node_anchors2('Func'{fqn: str(Fqn), name: _Name, parameters: Parameters, return_type: ReturnType}, Expr) -->>
    % TODO: /kythe/node/kind function
    % Note: node_anchors(_Name) would cause a dup defines/binding because it's also in an 'AssignExprStmt'
    node_anchors(Parameters, _),
    node_anchors(ReturnType, ReturnTypeExpr),
    { Expr = func(Fqn, ReturnTypeExpr) }.
node_anchors2('GlobalStmt'{items: Items}, todo_expr(global)) -->>
    node_anchors(Items, _).
node_anchors2('IfStmt'{items: Items}, todo_expr(if)) -->>
    node_anchors(Items, _).
node_anchors2('ImportAsNamesNode'{items: Items}, todo_expr(importas)) -->>  % TODO: fqn accumulator
    node_anchors(Items, _).
node_anchors2('ImportDottedAsNameNode'{as_name: As_name, dotted_name: Dotted_name}, todo_expr(importdotted)) -->>  % TODO: fqn accumulator
    node_anchors(As_name, _),
    node_anchors(Dotted_name, _).
node_anchors2('ImportDottedAsNamesNode'{items: Items}, todo_expr(importdotteds)) -->> % TODO: fqn accumulator
    node_anchors(Items, _).
node_anchors2('ImportFromStmt'{from_name: From_name, import_part: Import_part}, todo_expr(importfrom)) -->> % TODO: fqn accumulator
    node_anchors(From_name, _),
    node_anchors(Import_part, _).
node_anchors2('ImportNameNode'{dotted_as_names: Dotted_as_names}, todo_expr(import)) -->> % TODO: fqn accumulator
    node_anchors(Dotted_as_names, _).
node_anchors2('ListMakerNode'{items: Items}, todo_expr(list)) -->>
    node_anchors(Items, _).
%% TODO: remove NameNode -- it seems to come fromImportFromStmt et al
%%       and should be refined.
node_anchors2('NameNode'{binds: Binds, name: Name}, todo_expr(name___)) -->>
    node_anchors(Binds, _),
    node_anchors(Name, _).
node_anchors2('NameBinds'{fqn: str(Fqn), name: 'Astn'{value:str(_Token), start: int(Start), end: int(End)}}, Expr) -->>
    anchor(Start, End, Source),
    %% TODO: /kythe/node/kind variable  (but not if it's a Class or Function)
    edge(Source, 'kythe/edge/defines/binding', Fqn),
    { Expr = namebinds(Fqn) },
    [ ].  % [ Expr ]:expr.  % TODO: this is also in fqns from anchor
node_anchors2('NameRef'{fqn: str(Fqn), name: 'Astn'{value:str(_Token), start: int(Start), end: int(End)}}, Expr) -->>
    anchor(Start, End, Source),
    edge(Source, '/kythe/edge/ref', Fqn),
    { Expr = nameref(Fqn) },
    [ ].  % [ Expr ]:expr.  % TODO: this is also in fqns from anchor
node_anchors2('NonLocalStmt'{items: Items}, todo_expr(nonlocal)) -->>
    node_anchors(Items, _).
node_anchors2('NumberNode'{astn: Astn}, todo_expr(number)) -->>
    node_anchors(Astn, _).
node_anchors2('OmittedNode'{}, omitted) -->> [ ].
node_anchors2('OpNode'{args: Args, op_astns: Op_astns}, todo_expr(op)) -->>
    node_anchors(Args, _),
    node_anchors(Op_astns, _).
node_anchors2('PassStmt'{items: Items}, todo_expr(pass)) -->>
    node_anchors(Items, _).
node_anchors2('RaiseStmt'{items: Items}, todo_expr(raises)) -->>
    node_anchors(Items, _).
node_anchors2('StarNode'{}, todo_expr(star)) -->> [ ].
node_anchors2('Stmts'{items: Items}, todo_expr(stmts)) -->>
    node_anchors(Items, _).
node_anchors2('StringNode'{astns: Astns}, todo_expr(string)) -->>
    node_anchors(Astns, _).
node_anchors2('SubscriptNode'{expr1: Expr1, expr2: Expr2, expr3: Expr3}, todo_expr(subscr)) -->>
    node_anchors(Expr1, _),
    node_anchors(Expr2, _),
    node_anchors(Expr3, _).
node_anchors2('TnameNode'{name: Name, type_expr: Type_expr}, todo_expr(tname)) -->>
    node_anchors(Name, _),
    node_anchors(Type_expr, _).
node_anchors2('TryStmt'{items: Items}, todo_expr(try)) -->>
    node_anchors(Items, _).
node_anchors2('TypedArgNode'{expr: Expr, name: Name}, todo_expr(typedarg)) -->>
    node_anchors(Expr, _),
    node_anchors(Name, _).
node_anchors2('WhileStmt'{else_suite: Else_suite, suite: Suite, test: Test}, todo_expr(while)) -->>
    node_anchors(Else_suite, _),
    node_anchors(Suite, _),
    node_anchors(Test, _).
node_anchors2('WithItemNode'{as_item: As_item, item: Item}, todo_expr(withitem)) -->>
    node_anchors(As_item, _),
    node_anchors(Item, _).
node_anchors2('WithStmt'{items: Items, suite: Suite}, todo_expr(withstmt)) -->>
    node_anchors(Items, _),
    node_anchors(Suite, _).

node_anchors_list([], []) -->> [ ].
node_anchors_list([Node|Nodes], [NodeExpr|NodeExprs]) -->>
    node_anchors(Node, NodeExpr),
    node_anchors_list(Nodes, NodeExprs).

binds_atom("False", ref).
binds_atom("True", binds).

anchor(Start, End, Source) -->>
    { format(string(Signature), '@~d:~d', [Start, End]) },
    { signature_source(Signature, Source) },
    kythe_fact(Source, '/kythe/node/kind', anchor),
    kythe_fact(Source, '/kythe/loc/start', Start),
    kythe_fact(Source, '/kythe/loc/end', End).

edge(Source, EdgeKind, Fqn) -->>
    { corpus_root_path_language(Corpus, Root, _Path, Language) },
    { Target = _{signature: Fqn, corpus: Corpus, root: Root, language: Language} },
    kythe_edge(Source, EdgeKind, Target).

kythe_fact(Source, FactName, FactValue) -->>
    { base64(FactValue, FactBase64) },
    kythe_fact_b64(Source, FactName, FactBase64).

kythe_fact_b64(Source, FactName, FactBase64) -->>
    [ _{source: Source, fact_name: FactName, fact_value: FactBase64} ]:fqn.

kythe_edge(Source, EdgeKind, Target) -->>
    [ _{source: Source, edge_kind: EdgeKind, target: Target, fact_name: '/'} ]:fqn.

signature_source(Signature, Source) :-
    corpus_root_path_language(Corpus, Root, Path, _Language),
    Source = _{signature: Signature, corpus: Corpus, root: Root, path: Path}.

output_anchors([], KytheStream) :-
    nl(KytheStream).
output_anchors([Anchor|Anchors], KytheStream) :-
    must_once(output_anchor(Anchor, KytheStream)),
    output_anchors(Anchors, KytheStream).

output_anchor(Dict, KytheStream) :-
    json_write_dict(KytheStream, Dict, [width(0)]),
    nl(KytheStream).


%% For more compact output (debugging):
portray('Astn'{start: int(Start), end: int(End), value: str(Value)}) :-
    format('ASTN(~d:~d ~q)', [Start, End, Value]).
