% -*- mode: Prolog -*-

%% This reads in the "fqn_expr" facts (in JSON format), processes
%% them, and outputs additional Kythe facts (in JSON format).
%% TODO: :- use_module(library(protobufs)).  % instead of outputting JSON

%% There are two passes:

%% 1. Read in the "cooked" ASTN (see ast_cooked.Base), transform them
%%    into a simpler form, and produce two outputs (using
%%    accumulators):
%%    - Kythe facts (e.g., anchors) for all the variables and
%%      parameters in the code (but not the attributes)
%%    - references and assignments (in the assign/2 facts).
%%    Each predicate also returns a "type" result that is used to
%%    populate the right-hand-side of assign/2 facts (for statements,
%%    a stmt/1 term is returned for completeness.  For more details,
%%    see node_anchors//2.
%% 2. Process the assign/2 facts, by intepreting the expressions and
%%    recording the results in a symtab (symbol table), then
%%    outputting Kythe facts for the attributes, calls, etc.  Note
%%    that expr([]) is a no-op.

%% A word on "types" and "eval".

%% The input consists of a list of simplified items from the AST.
%% For example, this Python line (in class C2's __init__):
%%   self.x = 'C2_x'
%% is turned into the following (in portray-output format):
%%    'AssignExprStmt'{
%%        expr:'StringNode'{astn:['ASTN'(1160:1166, "'C2_x'")]},
%%        left:'AtomDotNode'{
%%               atom:'NameRefFqn'{
%%                       fqn:str("test_data.simple.C2.__init__.<local>.self"),
%%                       name:'ASTN'(1151:1155, "self")
%%               },
%%               attr_name:'ASTN'(1156:1157, "x"),
%%               binds:bool("True")
%%         }
%%      }
%%
%% When this is read in, it is simplified to:
%%   assign(dot([fqn('test_data.simple.C2.__init__.<local>.self')],
%%              astn(1156,1157, "x"),
%%              '/kythe/edge/defines/binding'),
%%          [class('builtin.str', [])])
%%                     %% (Py2.7 would be __builtin__.str)
%%             -- TODO: need to incorporate $PYTHONPATH
%%
%% To process this, we need to resolve the FQNs (in this case,
%% fqn('test_data.simple.C2.__init__.<local>.self') by looking up in
%% the symtab, eventually resulting in the full dot(...) being reduced
%% to fqn('test_data.simple.C2.x')).

%% The symtab mappings are an ord_union (possibly empty) of:
%%     fqn(Fqn) - a global or local name (fully qualified)
%%     class(Fqn, Bases)  %% Bases is a list of union types
%%     func(Fqn, ReturnType)  %% ReturnType is a union type
%%     import(Fqn, Path)  %% TODO: this will probably change somewhat

%% There are two flavors of the "eval" predicates, depending on the
%% behavior with fqn(Fqn): the ..._and_lookup will the Fqn and return
%% the associated value (or add it to symtab and return []).  For the
%% left-hand-side of an assignment, the lookup isn't done (using an
%% "eval" predicate that doesn't do lookup).
%%   (Implementation detail: this is done using
%%        [ Fqn-Result ]:sym_rej
%%   which calls add_kythe_fact_accum/3.
%%
%% All types are unions (represented as an ordset); [] means that
%% there's no information and is effectively "Any". Many of the
%% predicates come in two versions: on that works with a type union
%% (ordset), and one that works on single "types", such as fqn(...),
%% class(...), func(...), etc. -- typically the predicate that works
%% with a type union iterates over the single items, calling the
%% predicate for single "types", and uses ord_union/3 to combine them
%% all. (This use or ord_union ensures that there's no need to
%% "flatten" the list and that the single types are kept in a
%% canonical order).

%% The list of assign(Left, Right) and expr(Right) terms is repeatedly
%% reprocessed until no changes occur. When a FQN is first
%% encountered, it is put into the symtab with its type ([] if the
%% type can't be determined) -- when subsequently encountered, any
%% inconsistency in type is added to a "reject" list. After a pass is
%% complete, the rejects are unioned with the symtab and if there were
%% changes, another pass is done. In this way, each expression is
%% repeatedly reprocessed until no more changes (in practice, only one
%% or two passes are needed).


%% TODO: can we remove the kythe_fact accumulator from the first pass
%%       and generate all the Kythe information from the second pass?

%% TODO: Use QLF: http://www.swi-prolog.org/pldoc/man?section=qlf


:- use_module(library(http/json)).
:- use_module(library(optparse)).
:- use_module(library(edcg)).  % requires: ?- pack_install(edcg).
:- use_module(library(pprint), [print_term/2]).
:- use_module(library(ordsets)).
:- use_module(library(assoc)).
:- use_module(must_once, [must_once/1,
                          must_once/3 as must_once_kythe_fact,
                          must_once/5 as must_once_kythe_fact_expr,
                          must_once/5 as must_once_fqn_sym_rej]).

:- style_check(+singleton).
:- style_check(+no_effect).
:- style_check(+var_branches).
:- style_check(+discontiguous).
%% :- set_prolog_flag(generate_debug_info, false).

%% "kythe_fact" accumulator gets FQN anchor facts, in an association
%% list, with the key being Source-FactName and the value being a dict
%% to be output in JSON.
edcg:acc_info(kythe_fact, T, In, Out, kythe_fact_accum(T, In, Out)).
%% "expr" accumulator gets expressions that need interpreting.
edcg:acc_info(expr, T, Out, In, Out=[T|In]).
%% "sym_rej" accumulator is for symtab + items that need reprocessing.
edcg:acc_info(sym_rej, FqnType, In, Out, sym_rej_accum(FqnType, In, Out)).

edcg:pred_info(must_once_kythe_fact_expr, 1,       [kythe_fact, expr]).

edcg:pred_info(must_once_kythe_fact, 1,            [kythe_fact]).

edcg:pred_info(anchor, 3,                          [kythe_fact]).
edcg:pred_info(dots_and_dotted_name, 3,            [kythe_fact]).
edcg:pred_info(edge, 3,                            [kythe_fact]).
edcg:pred_info(from_import_part, 3,                [kythe_fact]).
edcg:pred_info(kythe_edge, 3,                      [kythe_fact]).
edcg:pred_info(kythe_fact, 3,                      [kythe_fact]).
edcg:pred_info(kythe_fact_b64, 3,                  [kythe_fact]).
edcg:pred_info(kythe_file, 0,                      [kythe_fact]).

edcg:pred_info(assign_normalized, 2,               [kythe_fact, expr]).
edcg:pred_info(expr_normalized, 1,                 [kythe_fact, expr]).
edcg:pred_info(kythe_json, 2,                      [kythe_fact, expr]).
edcg:pred_info(node_anchors, 2,                    [kythe_fact, expr]).
edcg:pred_info(node_anchors2, 2,                   [kythe_fact, expr]).
edcg:pred_info(node_anchors_expr_list, 1,          [kythe_fact, expr]).
edcg:pred_info(node_anchors_import_from, 1,        [kythe_fact, expr]).
edcg:pred_info(node_anchors_list, 2,               [kythe_fact, expr]).

edcg:pred_info(must_once_fqn_sym_rej, 1,           [kythe_fact, sym_rej]).

edcg:pred_info(assign_expr_eval, 1,                [kythe_fact, sym_rej]).
edcg:pred_info(assign_exprs_count, 2,              [kythe_fact, sym_rej]).
edcg:pred_info(assign_exprs_eval_list, 1,          [kythe_fact, sym_rej]).
edcg:pred_info(eval_atom_call_single, 4,           [kythe_fact, sym_rej]).
edcg:pred_info(eval_atom_call_union, 3,            [kythe_fact, sym_rej]).
edcg:pred_info(eval_atom_dot_single, 5,            [kythe_fact, sym_rej]).
edcg:pred_info(eval_atom_dot_union, 4,             [kythe_fact, sym_rej]).
edcg:pred_info(eval_atom_dot_union, 5,             [kythe_fact, sym_rej]).
edcg:pred_info(eval_lookup, 2,                     [kythe_fact, sym_rej]).
edcg:pred_info(eval_lookup, 3,                     [kythe_fact, sym_rej]).
edcg:pred_info(eval_lookup_single, 2,              [kythe_fact, sym_rej]).
edcg:pred_info(eval_single_type, 2,                [kythe_fact, sym_rej]).
edcg:pred_info(eval_single_type_and_lookup, 2,     [kythe_fact, sym_rej]).
edcg:pred_info(eval_union_type, 2,                 [kythe_fact, sym_rej]).
edcg:pred_info(eval_union_type, 3,                 [kythe_fact, sym_rej]).
edcg:pred_info(eval_union_type_and_lookup, 2,      [kythe_fact, sym_rej]).
edcg:pred_info(eval_union_type_and_lookup_list, 2, [kythe_fact, sym_rej]).
edcg:pred_info(eval_union_type_list, 2,            [kythe_fact, sym_rej]).

%% TODO: process typeshed builtins.
%%       also module special attributes:
%%           '__build_class__', '__debug__', '__doc__', '__import__',
%%           '__loader__', '__name__', '__package__', '__spec__'.

%% builtin_names(['ArithmeticError', 'AssertionError', 'AttributeError',
%%     'BaseException', 'BlockingIOError', 'BrokenPipeError',
%%     'BufferError', 'BytesWarning', 'ChildProcessError',
%%     'ConnectionAbortedError', 'ConnectionError',
%%     'ConnectionRefusedError', 'ConnectionResetError',
%%     'DeprecationWarning', 'EOFError', 'Ellipsis', 'EnvironmentError',
%%     'Exception', 'False', 'FileExistsError', 'FileNotFoundError',
%%     'FloatingPointError', 'FutureWarning', 'GeneratorExit', 'IOError',
%%     'ImportError', 'ImportWarning', 'IndentationError', 'IndexError',
%%     'InterruptedError', 'IsADirectoryError', 'KeyError',
%%     'KeyboardInterrupt', 'LookupError', 'MemoryError',
%%     'ModuleNotFoundError', 'NameError', 'None', 'NotADirectoryError',
%%     'NotImplemented', 'NotImplementedError', 'OSError',
%%     'OverflowError', 'PendingDeprecationWarning', 'PermissionError',
%%     'ProcessLookupError', 'RecursionError', 'ReferenceError',
%%     'ResourceWarning', 'RuntimeError', 'RuntimeWarning',
%%     'StopAsyncIteration', 'StopIteration', 'SyntaxError',
%%     'SyntaxWarning', 'SystemError', 'SystemExit', 'TabError',
%%     'TimeoutError', 'True', 'TypeError', 'UnboundLocalError',
%%     'UnicodeDecodeError', 'UnicodeEncodeError', 'UnicodeError',
%%     'UnicodeTranslateError', 'UnicodeWarning', 'UserWarning',
%%     'ValueError', 'Warning', 'ZeroDivisionError', 'abs', 'all', 'any',
%%     'ascii', 'bin', 'bool', 'bytearray', 'bytes', 'callable', 'chr',
%%     'classmethod', 'compile', 'complex', 'copyright', 'credits',
%%     'delattr', 'dict', 'dir', 'divmod', 'enumerate', 'eval', 'exec',
%%     'exit', 'filter', 'float', 'format', 'frozenset', 'getattr',
%%     'globals', 'hasattr', 'hash', 'help', 'hex', 'id', 'input', 'int',
%%     'isinstance', 'issubclass', 'iter', 'len', 'license', 'list',
%%     'locals', 'map', 'max', 'memoryview', 'min', 'next', 'object',
%%     'oct', 'open', 'ord', 'pow', 'print', 'property', 'quit', 'range',
%%     'repr', 'reversed', 'round', 'set', 'setattr', 'slice', 'sorted',
%%     'staticmethod', 'str', 'sum', 'super', 'tuple', 'type', 'vars',
%%     'zip']).

builtin_names([]).  %% TODO: use the commented-out list above
builtin_name(Name) :-
    builtin_names(Names),
    member(Name, Names).

initial_symtab(Symtab) :-
    (  bagof(BuiltinName-Type,
            (builtin_name(Name),
             atomic_list_concat([builtin, Name], '.', BuiltinName),
             list_to_ord_set([class(BuiltinName, [])], Type)
            ),
            SymtabPairs)
    ;  SymtabPairs = []
    ),
    %% TODO: string, number are provisional
    %%       also use list_to_ord_set
    %% TODO: add builtin.int, etc. should really process builtin.pyi
    dict_create(Symtab, symtab,
                ['builtin.str'-[class('builtin.str', [])],
                 'builtin.Number'-[class('builtin.Number', [])]
                | SymtabPairs]).


:- initialization main.

main :-
    OptsSpec = [],
    opt_arguments(OptsSpec, _Opts, PositionalArgs),
    (  PositionalArgs = [FqnExprFile]
    -> true
    ;  PositionalArgs = []
    -> FqnExprFile = '/dev/stdin'
    ;  FqnExprFile = '/dev/stdin',  % quiet var_branches style check
       throw(error(bad_positional_args(PositionalArgs), _))
    ),
    must_once(
        process(FqnExprFile)),
    halt.

process(FqnExprFile) :-
    read_anchors_exprs(FqnExprFile, KytheFacts, Exprs),
    do_if(false,
          dump_term('EXPRS', Exprs, [indent_arguments(auto),
                                     right_margin(72)])),
    must_once(
        assign_exprs(Exprs, Symtab, KytheFacts2)),
    do_if(false,
          dump_term('SYMTAB', Symtab)),
    current_output(KytheStream),
    %% write(KytheStream, "%% === Kythe ==="), nl(KytheStream),
    output_anchors(KytheFacts, KytheStream),
    output_anchors(KytheFacts2, KytheStream).

%% Read the JSON, convert to nodes tree, convert to nodes tree with
%% FQNs and extract anchors and related facts into KytheFacts, plus
%% Exprs (see node_anchors//2).
read_anchors_exprs(FqnExprFile, KytheFacts, Exprs) :-
    open(FqnExprFile, read, FqnExprStream),
    must_once(
        json_read_dict(FqnExprStream, MetaDict)),
    must_once(
        assert_meta(MetaDict)),
    must_once(
        json_read_dict(FqnExprStream, JsonDict)),
    must_once(
        at_end_of_stream(FqnExprStream)),
    simplify_json(JsonDict, Nodes),
    do_if(false,
          dump_term('NODES', Nodes)),
    kythe_json(Nodes, _Expr, KytheFacts, Exprs).

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
    must_once(
        simplify_json2(Json, Prolog)).

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
    dict_pairs(Value, dict, ItemPairs2).
simplify_json2(_{kind: Kind, slots: Slots}, Value) :-
    dict_pairs(Slots, _, SlotPairs),
    simplify_json_slot_pairs(SlotPairs, SlotPairs2),
    atom_string(KindAtom, Kind),
    dict_pairs(Value, KindAtom, SlotPairs2).

simplify_json_list([], []).
simplify_json_list([V|Vs], [Value|Values]) :-
    simplify_json(V, Value),
    simplify_json_list(Vs, Values).

simplify_json_slot_pairs([], []).
simplify_json_slot_pairs([K-V|KVs], [K-V2|KVs2]) :-
    simplify_json(V, V2),
    simplify_json_slot_pairs(KVs, KVs2).

kythe_json(Node, Expr, KytheFacts, Exprs) :-
    empty_assoc(KytheFacts0),
    kythe_json(Node, Expr, KytheFacts0, KytheFacts1, Exprs, []),  % phrase(kythe_json(Node, Expr), KytheFacts, Exprs)
    assoc_to_values(KytheFacts1, KytheFacts).

kythe_json(Node, Expr) -->>  % [kythe_fact, expr]
    kythe_file,
    node_anchors(Node, Expr).

kythe_file -->>  % [kythe_fact]
    % TODO: output x-numlines, x-html ?
    { corpus_root_path_language(Corpus, Root, Path, _Language) },
    { Source = json{corpus: Corpus, root: Root, path: Path} },
    kythe_fact(Source, '/kythe/node/kind', 'file'),
    { file_contents_b64(ContentsB64) },
    kythe_fact_b64(Source, '/kythe/text', ContentsB64).


%% Extract anchors (with FQNs) from the the AST nodes.  The anchors go
%% into accumulator 'kythe_fact' and the expressions (for further
%% processing) go into accumulator 'expr'. The predicate returns a
%% "type", which is usesd to populate the right-hand-sides of assign/2
%% terms in the 'expr' accumulator.

node_anchors(Node, Type) -->>  % [kythe_fact, expr]
    must_once_kythe_fact_expr(
        node_anchors2(Node, Type)).

% For descriptions of the following, and how they relate to the raw
% ASTN, see ast_cooked.py.

% [], [_|_], bool(_), dict(_), int(_), none, str(_), 'Astn'{...}'
% are all handled by higher-level nodes.
%   (e.g., 'Astn'{start: int(Start), end: int(End), value: str(Value)}}
%   in node_astn/4, which is uesd by 'ArgumentNode', 'AtomDotNode', etc.;
%   str(_) is used by 'Class', 'Func', etc.)

% assign/2 facts are made up of a left-hand-side (a list) and a
% right-hand-side (a singleton list or an empty list). These
% correspond to the LHS and RHS of an expression, and have a few
% variants:
%   assign(a, [b]) corresponds to the statement `a = b`
%   assign(a, []) corresponds to the definition of a name, e.g. `def foo(a)`
% expr/1 are like assign/2 but with nothing to assign to (expr([]) is a no-op).

% See comments at the top of this file on union and single types.

% The following are handled by the container (e.g., ImportFromStmt):
%   AsNameNode
%   NameRawNode  (from DottedNameNode, ImportFromStmt, etc.)
%   NameNode

node_anchors2('AnnAssignStmt'{left_annotation: LeftAnnotation, expr: Expr, left: Left}, stmt(annassign)) -->>  % [kythe_fact, expr]
    expr_normalized(Expr),
    assign_normalized(Left, LeftAnnotation).
node_anchors2('ArgumentNode'{name: NameAstn, arg: Arg}, todo_arg(Name, ArgType)) -->>  % [kythe_fact, expr]
    % ast_raw creates ArgumentNode only for `test '=' test`; all other cases
    % just generate the expr (or similar)
    % TODO: match Name to func def param
    { node_astn(NameAstn, _, _, Name) },
    node_anchors(Arg, ArgType).
node_anchors2('AssertStmt'{items: Items}, stmt(assert)) -->>  % [kythe_fact, expr]
    node_anchors_expr_list(Items).
node_anchors2('AssignExprStmt'{expr: Expr, left: Left}, stmt(assign)) -->>  % [kythe_fact, expr]
    assign_normalized(Left, Expr).
node_anchors2('AtomCallNode'{args: Args, atom: Atom}, call([AtomType], ArgsType)) -->>  % [kythe_fact, expr]
    node_anchors(Atom, AtomType),
    node_anchors_list(Args, ArgsType).
node_anchors2('AtomDotNode'{atom: Atom, binds: bool(Binds), attr_name: AttrNameAstn},  % [kythe_fact, expr]
              dot([AtomType], astn(Start, End, AttrName), BindsAtom)) -->>
    { dot_binds_atom(Binds, BindsAtom) },
    { node_astn(AttrNameAstn, Start, End, AttrName) },
    node_anchors(Atom, AtomType).
node_anchors2('AtomSubscriptNode'{atom: Atom, subscripts: Subscripts}, todo_subscr([AtomType])) -->>  % [kythe_fact, expr]
    node_anchors(Atom, AtomType),
    node_anchors_list(Subscripts, _).
node_anchors2('AugAssignStmt'{augassign: _OpAstn,  expr: Expr, left: Left}, stmt(augassign)) -->>  % [kythe_fact, expr]
    % { node_astn(OpAstn, _, _, _Op) },
    expr_normalized(Left),
    expr_normalized(Expr).
node_anchors2('BreakStmt'{}, stmt(break)) -->> [ ].  % [kythe_fact, expr]
node_anchors2('Class'{bases: Bases, fqn: str(Fqn), name: NameAstn},  % [kythe_fact, expr]
              class(FqnAtom, BasesType)) -->>
    { atom_string(FqnAtom, Fqn) },
    { node_astn(NameAstn, Start, End, _Token) },
    { signature_node(FqnAtom, Signature) },
    anchor(Start, End, Source),
    edge(Source, '/kythe/edge/defines/binding', FqnAtom),
    kythe_fact(Signature, '/kythe/node/kind', 'record'),
    kythe_fact(Signature, '/kythe/subkind', 'class'),
    node_anchors_list(Bases, BasesType),
    [ class(FqnAtom, BasesType) ]:expr.
node_anchors2('CompFor'{for_astn: _ForAstn, for_exprlist: ForExprlist,
                        in_testlist: InTestlist, comp_iter: CompIter},
              todo_compfor(iter:CompIterType, for:ForExprlistType, in:InTestlistType)) -->>  % [kythe_fact, expr]
    node_anchors(ForExprlist, ForExprlistType),
    node_anchors(InTestlist, InTestlistType),
    node_anchors(CompIter, CompIterType).
node_anchors2('CompIfCompIterNode'{value_expr: ValueExpr, comp_iter: CompIter}, todo_compifcompiter(ValueExprType, CompIterType)) -->>  % [kythe_fact, expr]
    node_anchors(ValueExpr, ValueExprType),
    node_anchors(CompIter, CompIterType).
node_anchors2('ContinueStmt'{}, stmt(continue)) -->> [ ].  % [kythe_fact, expr]
node_anchors2('DecoratedStmt'{items: Items}, todo_decorated(ItemsType)) -->>  % [kythe_fact, expr]
             node_anchors_list(Items, ItemsType).
node_anchors2('DecoratorDottedNameNode'{items: Items}, todo_decorator_dottedname(ItemsType)) -->>  % [kythe_fact, expr]
    from_dots(Items, ItemsType).
node_anchors2('DecoratorsNode'{items: Items}, todo_decorators(ItemsType)) -->>  % [kythe_fact, expr]
    node_anchors_list(Items, ItemsType).
node_anchors2('DelStmt'{items: Items}, stmt(del)) -->>  % [kythe_fact, expr]
    node_anchors_expr_list(Items).
node_anchors2('DictGenListSetMakerCompFor'{value_expr: ValueExpr, comp_for: CompFor}, todo_dictgen(ValueExprType, CompForType)) -->>  % [kythe_fact, expr]
    node_anchors(ValueExpr, ValueExprType),
    node_anchors(CompFor, CompForType).
node_anchors2('DictKeyValue'{items: Items}, todo_dictkeyvaluelist(ItemsType)) -->>  % [kythe_fact, expr]
    node_anchors_list(Items, ItemsType).
node_anchors2('DictSetMakerNode'{items: Items}, todo_dictset(ItemsType)) -->>  % [kythe_fact, expr]
    node_anchors_list(Items, ItemsType).
%% DottedNameNode is restricted to import contexts (see also DecoratorDottedNameNode)
node_anchors2('DottedNameNode'{items: Items}, todo_dottedname(ItemsType)) -->>  % [kythe_fact, expr]
    { must_once(
          dotted_name_raw(Items, ItemsType)) }.
node_anchors2('EllipsisNode'{}, ellipsis) -->> [ ].  % [kythe_fact, expr]
node_anchors2('ExceptClauseNode'{expr: Expr, as_item: AsItem}, stmt(except)) -->>  % [kythe_fact, expr]
    node_anchors(Expr, ExprType),
    node_anchors(AsItem, AsItemType),
    (  { AsItem = omitted }
    -> [ expr([ExprType]) ]:expr
    ;  [ assign(AsItemType, [ExprType]) ]:expr
    ).
node_anchors2('ExprListNode'{items: Items}, todo_exprlist(ItemsType)) -->>  % [kythe_fact, expr]
    node_anchors_list(Items, ItemsType).
node_anchors2('ExprStmt'{expr: Expr}, stmt(assign)) -->>  % [kythe_fact, expr]
    node_anchors(Expr, ExprType),
    [ expr([ExprType]) ]:expr.
node_anchors2('FileInput'{scope_bindings: _ScopeBindings, stmts: Stmts, path: _Path}, stmt(file)) -->>  % [kythe_fact, expr]
    %% node_anchors(ScopeBindings, _),
    node_anchors_list(Stmts, _).
node_anchors2('ForStmt'{for_exprlist: ForExprlist, in_testlist: InTestlist,
                        suite: Suite, else_suite: ElseSuite}, stmt(for)) -->>  % [kythe_fact, expr]
    node_anchors(ElseSuite, _),  % node_anchors(ElseSuite, stmt(_))
    node_anchors(ForExprlist, _),
    node_anchors(InTestlist, _),
    node_anchors(Suite, _).
node_anchors2('Func'{fqn: str(Fqn), name: NameAstn, parameters: Parameters, return_type: ReturnType},
              func(FqnAtom, [ReturnTypeType])) -->>  % [kythe_fact, expr]
    { atom_string(FqnAtom, Fqn) },
    { node_astn(NameAstn, Start, End, _Token) },
    { signature_node(FqnAtom, Signature) },
    anchor(Start, End, Source),
    edge(Source, '/kythe/edge/defines/binding', FqnAtom),
    kythe_fact(Signature, '/kythe/node/kind', 'function'),
    node_anchors_list(Parameters, _),
    node_anchors(ReturnType, ReturnTypeType),
    [ func(FqnAtom, [ReturnTypeType]) ]:expr.
node_anchors2('GlobalStmt'{items: Items}, stmt(global)) -->>  % [kythe_fact, expr]
    node_anchors_expr_list(Items).
node_anchors2('IfStmt'{items: Items}, stmt(if)) -->>  % [kythe_fact, expr]
             node_anchors_list(Items, _).
node_anchors2('ImportDottedAsNameFqn'{dotted_name: DottedName, as_name: AsName},
              unused_importdotted(DottedNameType, AsNameType)) -->>  % [kythe_fact, expr]
    node_anchors(AsName, AsNameType),
    node_anchors(DottedName, DottedNameType).
node_anchors2('ImportDottedAsNamesFqn'{items: Items},
              unused_importdotteds) -->>  % [kythe_fact, expr]
    { must_once(map_eq('ImportDottedAsNameFqn'{as_name:_, dotted_name: _}, Items)) },  % TODO: remove
    node_anchors_list(Items, _).
node_anchors2('ImportFromStmt'{from_name: DotsAndDottedName,
                               import_part: 'ImportAsNamesNode'{items: ImportPartItems}},
              unused_importfrom(CombImportPart)) -->>  % [kythe_fact, expr]
    must_once_kythe_fact(
        dots_and_dotted_name(DotsAndDottedName, ImportPartItems, CombImportPart)),
    % TOO: ref/import
    node_anchors_import_from(CombImportPart).
node_anchors2('ImportFromStmt'{from_name: DotsAndDottedName,
                               import_part: 'StarNode'{}},
              unused_importfrom_star) -->>  % [kythe_fact, expr]
    must_once_kythe_fact(
        dots_and_dotted_name(DotsAndDottedName, '*', _CombImportPart)),
    % TODO: expand '*'
    % TOO: ref/import
    [ ].
node_anchors2('ImportNameFqn'{dotted_as_names: DottedAsNames},
              unused_import(DottedAsNamesType)) -->>  % [kythe_fact, expr]
    { must_once(DottedAsNames = 'ImportDottedAsNamesFqn'{items:_}) },  % TODO: remove
    node_anchors(DottedAsNames, DottedAsNamesType).

node_anchors2('ListMakerNode'{items: Items}, todo_list(ItemsType)) -->>  % [kythe_fact, expr]
    node_anchors_list(Items, ItemsType).
node_anchors2('NameBindsFqn'{fqn: str(Fqn), name: NameAstn}, fqn(FqnAtom)) -->>  % [kythe_fact, expr]  %% result is same as NameRefFqn
    { atom_string(FqnAtom, Fqn) },
    { node_astn(NameAstn, Start, End, _Token) },
    { signature_node(FqnAtom, Signature) },
    anchor(Start, End, Source),
    edge(Source, '/kythe/edge/defines/binding', FqnAtom),  %% only difference from NameRef
    kythe_fact(Signature, '/kythe/node/kind', 'variable').
node_anchors2('NameRefFqn'{fqn: str(Fqn), name: NameAstn}, fqn(FqnAtom)) -->>  % [kythe_fact, expr]  %% result is same as NameBinds
    { atom_string(FqnAtom, Fqn) },
    { node_astn(NameAstn, Start, End, _Token) },
    anchor(Start, End, Source),
    edge(Source, '/kythe/edge/ref', FqnAtom).  %% only difference from NameBindsFqn
node_anchors2('NameRefGenerated'{fqn: str(Fqn)}, fqn(FqnAtom)) -->>  % [kythe_fact, expr]  %% result is same as NameBinds
    { atom_string(FqnAtom, Fqn) }.
node_anchors2('NonLocalStmt'{items: Items}, stmt(nonlocal)) -->>  % [kythe_fact, expr]
    node_anchors_expr_list(Items).
node_anchors2('NumberNode'{astn: _Astn}, class('builtin.Number', [])) -->> [ ].  % [kythe_fact, expr]
node_anchors2('OmittedNode'{}, omitted) -->> [ ].  % [kythe_fact, expr]
node_anchors2('OpNode'{args: Args, op_astns: OpAstns}, call_op(OpAstns, ArgsType)) -->>  % [kythe_fact, expr]
    node_anchors_list(Args, ArgsType).
node_anchors2('PassStmt'{}, stmt(break)) -->> [ ].  % [kythe_fact, expr]
node_anchors2('RaiseStmt'{items: Items}, stmt(raise)) -->>  % [kythe_fact, expr]
    node_anchors_list(Items, _).
node_anchors2('StarNode'{}, star) -->> [ ].  % [kythe_fact, expr]  % TODO: can we get rid of this in ast_cooked?
node_anchors2('Stmts'{items: Items}, todo_expr(stmts)) -->>  % [kythe_fact, expr]
    node_anchors_list(Items, _).
node_anchors2('StringNode'{astns: _Astns}, class('builtin.str', [])) -->> [ ].  % [kythe_fact, expr]
node_anchors2('SubscriptNode'{expr1: Expr1, expr2: Expr2, expr3: Expr3},
              subscr([Expr1Type, Expr2Type, Expr3Type])) -->>  % [kythe_fact, expr]
    node_anchors(Expr1, Expr1Type),
    node_anchors(Expr2, Expr2Type),
    node_anchors(Expr3, Expr3Type).
node_anchors2('TnameNode'{name: Name, type_expr: TypeType}, stmt(tname)) -->>  % [kythe_fact, expr]
    assign_normalized(Name, TypeType).
node_anchors2('TryStmt'{items: Items}, stmt(try)) -->>  % [kythe_fact, expr]
    node_anchors_list(Items, _).
node_anchors2('TypedArgNode'{tname: 'TnameNode'{name: Name, type_expr: TypeExpr}, expr: Expr}, todo_typedarg()) -->>  % [kythe_fact, expr]
    assign_normalized(Name, TypeExpr),
    expr_normalized(Expr).  %% assign_normalized(Name, Expr) would cause duplicate facts
node_anchors2('WhileStmt'{else_suite: ElseSuite, suite: Suite, test: Test}, stmt(while)) -->>  % [kythe_fact, expr]
    node_anchors(ElseSuite, _),
    node_anchors(Suite, _),
    node_anchors(Test, _).
node_anchors2('WithItemNode'{item: Item, as_item: AsItem}, stmt(with_item)) -->>  % [kythe_fact, expr]
    node_anchors(Item, ItemType),
    node_anchors(AsItem, AsItemType),
    (  { AsItemType = omitted }
    -> [ expr([ItemType]) ]:expr
    ;  [ assign(AsItemType, [ItemType]) ]:expr
    ).
node_anchors2('WithStmt'{items: Items, suite: Suite}, stmt(with)) -->>  % [kythe_fact, expr]
    node_anchors_list(Items, _),  % handled by WithItemNode
    node_anchors(Suite, _).

dots_and_dotted_name(DotsAndDottedName, ImportPart, CombImportPart) -->> % [kythe_fact]
    %% the name is zero or more ImportDotNode's followed by zero or
    %% one DottedNameNode. If there are no ImportDotNode's, then the
    %% result is $PYTHONPATH.DottedName/ImportPart. If there are
    %% ImportDotNode's, then the result is FilePath/ImportPart, where
    %% FilePath is derived from the Meta information for the file,
    %% followed by '/..' as needed.
    { corpus_root_path_language(_Corpus, _Root, Path, _Language) },
    { must_once(
          atom_concat(PathBase, '.py', Path)) },  %% TODO: don't rely on '.py' being the extension?
    { from_dots_import(DotsAndDottedName, PathBase, Dots, DottedNameList) },
    { atomic_list_concat(DottedNameList, '.', DottedName) },
    (  { Dots = [] }
    -> { atomic_list_concat(['$PYTHONPATH/', DottedName], CombFromName) }
    ;  { atomic_list_concat(Dots, CombFromName) }
    ),
    from_import_part(ImportPart, CombFromName, CombImportPart).

from_dots_import([], _, [], []) :- !.
from_dots_import(['ImportDotNode'{}|Ds], PathBase, [PathBase|Dots], DottedNameList) :- !,
    from_dots_import(Ds, '/..', Dots, DottedNameList).
from_dots_import(['DottedNameNode'{items: Ds}], _, [], DottedNameList) :-
    from_dots(Ds, DottedNameList).

from_dots([], []).
from_dots(['NameRawNode'{name: NameAstn}|Ns], [Name|Names]) :-
    node_astn(NameAstn, _, _, Name),
    from_dots(Ns, Names).

from_import_part('*', _, '*') -->> [ ].   % [kythe_fact] % TODO: probably needs a better convention
from_import_part([], _, []) -->> [ ].  % [kythe_fact]
from_import_part(['AsNameNode'{as_name: 'NameBindsFqn'{fqn: str(AsName), name: AsNameAstn},
                               name: 'NameRawNode'{name: NameAstn}}|Ns],
                 CombFromName,
                 [ConcatName-AsNameAtom|NANs]) -->> % [kythe_fact]
    { node_astn(NameAstn, _, _, Name) },
    { node_astn(AsNameAstn, Start, End, _) },
    { atomic_list_concat([CombFromName, '/', Name], ConcatName) },
    { atom_string(AsNameAtom, AsName) },
    anchor(Start, End, Source),
    edge(Source, '/kythe/edge/defines/binding', AsName),
    from_import_part(Ns, CombFromName, NANs).

node_anchors_import_from([]) -->> [ ].  % [kythe_fact, expr]
node_anchors_import_from([Path-Fqn|AsItems]) -->>  % [kythe_fact, expr]
    [ import_from(Path, Fqn) ]:expr,
    % TODO: ref/import
    node_anchors_import_from(AsItems).

%% Handle DottedNameNode  TODO: needs some file resolution
dotted_name_raw([], []).
dotted_name_raw(['NameRawNode'{name: NameAstn}|Raws], [astn(Start, End, Name)|Astns]) :-
    node_astn(NameAstn, Start, End, Name),
    dotted_name_raw(Raws, Astns).

node_anchors_list([], []) -->> [ ].  % [kythe_fact, expr]
node_anchors_list([Node|Nodes], [[NodeType]|NodeTypes]) -->>  % [kythe_fact, expr]
    node_anchors(Node, NodeType),
    node_anchors_list(Nodes, NodeTypes).

node_anchors_expr_list([]) -->> [ ].  % [kythe_fact, expr]
node_anchors_expr_list([Type|Types]) -->>  % [kythe_fact, expr]
    expr_normalized(Type),
    node_anchors_expr_list(Types).

assign_normalized(Left, Right) -->>  % [kythe_fact, expr]
    node_anchors(Left, LeftType),
    node_anchors(Right, RightType),
    (  { LeftType = omitted }
    -> [ ]
    ;  { RightType = omitted ; RightType = ellipsis }
    -> [ assign(LeftType, []) ]:expr  % TODO: Right is left uninstantiated
    ;  [ assign(LeftType, [RightType]) ]:expr
    ).

expr_normalized(Right) -->>  % [kythe_fact, expr]
    node_anchors(Right, RightType),
    (  { RightType = [omitted] ; RightType = [ellipsis] }
    -> [ ]
    ;  [ expr([RightType]) ]:expr
    ).

node_astn('Astn'{start: int(Start), end: int(End), value: str(Value)},
          Start, End, Value).
% See also portray/1 rule for 'Astn' (uses node_astn/4).

dot_binds_atom("False", '/kythe/edge/ref').
dot_binds_atom("True", '/kythe/edge/defines/binding').

anchor(Start, End, Source) -->>  % [kythe_fact]
    { format(string(Signature), '@~d:~d', [Start, End]) },
{ signature_source(Signature, Source) },
    kythe_fact(Source, '/kythe/node/kind', anchor),
    kythe_fact(Source, '/kythe/loc/start', Start),
    kythe_fact(Source, '/kythe/loc/end', End).

edge(Source, EdgeKind, Fqn) -->>  % [kythe_fact]
    { signature_node(Fqn, Target) },
    kythe_edge(Source, EdgeKind, Target).

kythe_fact(Source, FactName, FactValue) -->>  % [kythe_fact]
    { base64(FactValue, FactBase64) },
    kythe_fact_b64(Source, FactName, FactBase64).

kythe_fact_b64(Source, FactName, FactBase64) -->>  % [kythe_fact]
    % The accumulator takes care of duplicate removal.
    [ json{source: Source, fact_name: FactName, fact_value: FactBase64} ]:kythe_fact.

kythe_edge(Source, EdgeKind, Target) -->>  % [kythe_fact]
    [ json{source: Source, edge_kind: EdgeKind, target: Target, fact_name: '/'} ]:kythe_fact.

signature_source(Signature, Source) :-
    corpus_root_path_language(Corpus, Root, Path, _Language),
    Source = json{signature: Signature, corpus: Corpus, root: Root, path: Path}.

signature_node(Signature, Vname) :-
    corpus_root_path_language(Corpus, Root, _Path, Language),
    Vname = json{signature: Signature, corpus: Corpus, root: Root, language: Language}.

%% The anchors are in *reverse* order (see the definition of the 'fqn'
%% accumulator. For debugging, you might want to reverse them first.
output_anchors([], _KytheStream).
output_anchors([Anchor|Anchors], KytheStream) :-
    must_once(
        output_anchor(Anchor, KytheStream)),
    output_anchors(Anchors, KytheStream).

output_anchor(AnchorAsDict, KytheStream) :-
    % The tags are ignored unless option tag(type) is specified (which
    % it isn't). All dicts should have the tag 'json', for simplicity.
    % (See also kythe_fact_accum/3 for a bit more on the dicts.)
    json_write_dict(KytheStream, AnchorAsDict, [width(0)]),
    nl(KytheStream).

%%%%%%              %%%%%%%
%%%%%% assign exprs %%%%%%%
%%%%%%              %%%%%%%

assign_exprs(Exprs, Symtab, KytheFacts) :-
    initial_symtab(Symtab0),
    assign_exprs_count(1, Exprs, Symtab0, Symtab, KytheFacts).

assign_exprs_count(Count, Exprs, Symtab0, Symtab, KytheFacts) :-
    do_if(false,
          format(user_error, '% === EXPRS === ~q~n~n', [Count])),
    assign_exprs_count2(Exprs, Symtab0, Symtab1, Rej, KytheFacts1),  % phrase(assign_exprs_count(...))
    length(Rej, RejLen),
    do_if(true,
          format(user_error, 'Pass ~q (rej=~q)~n', [Count, RejLen])),
    CountIncr is Count + 1,
    (  (Rej = [] ; CountIncr > 5)  % TODO: is 5 too high?
    -> Symtab = Symtab1,
       KytheFacts = KytheFacts1
    ;  assign_exprs_count(CountIncr, Exprs, Symtab1, Symtab, KytheFacts)
    ).

assign_exprs_count2(Exprs, Symtab0, SymtabWithRej, Rej, KytheFacts) :-  % [kythe_fact, sym_rej]
    dict_pairs(Symtab0, symtab, SymtabPairs0),
    exprs_from_symtab(SymtabPairs0, ExprsFromSymtab),
    append(ExprsFromSymtab, Exprs, ExprsCombined),  %% TODO: difference list
    empty_assoc(KytheFacts0),
    must_once(
        assign_exprs_eval_list(ExprsCombined, KytheFacts0, KytheFacts1, Symtab0-[], SymtabAfterEval-Rej)),  % phrase(assign_exprs_eval_list(...))
    assoc_to_values(KytheFacts1, KytheFacts),
    do_if(false,
          dump_term('REJ', Rej)),
    must_once(
        add_rej_to_symtab(Rej, SymtabAfterEval, SymtabWithRej)).

assign_exprs_eval_list([]) -->> [ ].  % [kythe_fact, sym_rej]
assign_exprs_eval_list([Assign|Assigns]) -->>  % [kythe_fact, sym_rej]
    SymtabRej/sym_rej,  %% TODO: delete (it's only used for debug logging)
    { do_if(false,
            dump_term('', SymtabRej)) },
    { do_if(false,
            dump_term('', Assign, [indent_arguments(auto),
                                   right_margin(60)])) },
    must_once_fqn_sym_rej(
        assign_expr_eval(Assign)),
    assign_exprs_eval_list(Assigns).

assign_expr_eval(assign(Left, Right)) -->>  % [kythe_fact, sym_rej]
    eval_union_type_and_lookup(Right, RightEval),
    eval_single_type(Left, LeftEval),
    (  { LeftEval = [LeftEvalSingle] }
    -> eval_lookup_single(LeftEvalSingle, RightEval)
    ;  [ ]
    ).
assign_expr_eval(expr(Right)) -->>  % [kythe_fact, sym_rej]
    %% TODO: do we need _and_lookup (for processing anchors)?
    eval_union_type_and_lookup(Right, _RightEval).
assign_expr_eval(class(Fqn, Bases)) -->>  % [kythe_fact, sym_rej]
    [ Fqn-[class(Fqn, Bases)] ]:sym_rej.
assign_expr_eval(func(Fqn, ReturnType)) -->>  % [kythe_fact, sym_rej]
    [ Fqn-[func(Fqn, ReturnType)] ]:sym_rej.
assign_expr_eval(import_from(Path, Fqn)) -->>  % [kythe_fact, sym_rej]
    [ Fqn-[import(Fqn, Path)] ]: sym_rej.

eval_union_type(Type, EvalType) -->>  % [kythe_fact, sym_rej]
    { ord_empty(EvalType0) },
    eval_union_type(Type, EvalType0, EvalType).

eval_union_type([], UnionSofar, UnionSofar) -->> [ ].  % [kythe_fact, sym_rej]
eval_union_type([T|Ts], UnionSoFar, EvalTypes) -->>  % [kythe_fact, sym_rej]
    must_once_fqn_sym_rej(
        eval_single_type_and_lookup(T, ET)),
    { ord_union(UnionSoFar, ET, UnionSoFar2) },
    eval_union_type(Ts, UnionSoFar2, EvalTypes).

eval_union_type_and_lookup(Expr, Result) -->>  % [kythe_fact, sym_rej]
    eval_union_type(Expr, Result0),
    eval_lookup(Result0, Result).

eval_single_type_and_lookup(Expr, Result) -->>  % [kythe_fact, sym_rej]
    eval_single_type(Expr, Result0),
    eval_lookup(Result0, Result).

%% TODO: handle [string], [number], etc.
%%       (this is a nice-to-do, for when we add more support for Kythe's
%%       type annotations; but for now, we really only need lookups for
%%       functions (calls) and classes/imports (',' operation))

eval_lookup(UnionType, Result) -->>  % [kythe_fact, sym_rej]
    { ord_empty(Result0) },
    eval_lookup(UnionType, Result0, Result).

eval_lookup([], Result, Result) -->> [ ].  % [kythe_fact, sym_rej]
eval_lookup([X|Xs], Result0, Result) -->>  % [kythe_fact, sym_rej]
    eval_lookup_single(X, Y),
    { ord_union(Result0, Y, Result1) },
    eval_lookup(Xs, Result1, Result).

eval_lookup_single(fqn(Fqn), Result) -->> !,  % [kythe_fact, sym_rej]
    [ Fqn-Result ]:sym_rej.
eval_lookup_single(class(ClassName, Bases0),
                   [class(ClassName, Bases)]) -->> !,  % [kythe_fact, sym_rej]
    eval_union_type_and_lookup_list(Bases0, Bases).
eval_lookup_single(func(FuncName, ReturnType0),
                   [func(FuncName, ReturnType)]) -->> !,  % [kythe_fact, sym_rej]
    eval_lookup(ReturnType0, ReturnType).
eval_lookup_single(import(Fqn, Path),
                   [import(Fqn, Path)]) -->> !,  % [kythe_fact, sym_rej]
    [ ].
eval_lookup_single(var(Fqn),
                   [var(Fqn)]) -->> !, [ ].  % [kythe_fact, sym_rej]
eval_lookup_single(_EvalType, []) -->> [ ].  % [kythe_fact, sym_rej]

eval_single_type(fqn(Fqn), [fqn(Fqn)]) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(dot(Atom, Astn, DotBinds), EvalType) -->>  % [kythe_fact, sym_rej]
    eval_union_type_and_lookup(Atom, AtomEval),
    %% TODO: MRO for class -- watch out for Bases containing Unions!
    eval_atom_dot_union(AtomEval, Astn, DotBinds, EvalType).
eval_single_type(call(Atom, Parms), EvalType) -->>  % [kythe_fact, sym_rej]
    eval_union_type_and_lookup(Atom, AtomEval),
    eval_union_type_list(Parms, ParmsEval),
    eval_atom_call_union(AtomEval, ParmsEval, EvalType).
eval_single_type(call_op(OpAstns, ArgsType), [call_op(OpAstns, ArgsTypeEval)]) -->>  % [kythe_fact, sym_rej]
    eval_union_type_list(ArgsType, ArgsTypeEval).
eval_single_type(class(Name, Bases), [class(Name, BasesEval)]) -->>  % [kythe_fact, sym_rej]
    eval_union_type_list(Bases, BasesEval).
eval_single_type(import(Fqn, Path), [import(Fqn, Path)]) -->>  % [kythe_fact, sym_rej]
    [ ].  % TODO: look-up
eval_single_type(func(Name, ReturnType), [func(Name, ReturnTypeEval)]) -->>  % [kythe_fact, sym_rej]
    eval_union_type_and_lookup(ReturnType, ReturnTypeEval).
eval_single_type(ellipsis, []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(omitted, []) -->> [ ].  % [kythe_fact, sym_rej]

%% TODO: implement the following:
eval_single_type(todo_compfor(iter:_CompIterType, for:_ForExprlistType, in:_InTestlistType), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_compifcompiter(_ValueExprType, _CompIterType), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_decorated(_ItemsType), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_decorator_dottedname(_ItemsType), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_decorators(_ItemsType), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_dictgen(_ValueExprType, _CompForType), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_dictkeyvaluelist(_ItemsType), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_dictset(_ItemsType), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_dottedname(_ItemsType), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_expr(stmts), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_typedarg(), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_subscr(_), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_arg(_, _), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_list(_), []) -->> [ ].  % [kythe_fact, sym_rej]
eval_single_type(todo_exprlist(_), []) -->> [ ].  % [kythe_fact, sym_rej]

eval_atom_dot_union(AtomEval, Astn, DotBinds, EvalType) -->>  % [kythe_fact, sym_rej]
    { ord_empty(EvalType0) },
    eval_atom_dot_union(AtomEval, Astn, DotBinds, EvalType0, EvalType).

eval_atom_dot_union([], _Astn, _DotBinds, EvalType, EvalType) -->> [ ].  % [kythe_fact, sym_rej]
eval_atom_dot_union([T|Ts], Astn, DotBinds, EvalType0, EvalType) -->>  % [kythe_fact, sym_rej]
    eval_single_type(T, ET0),
    eval_atom_dot_single(ET0, Astn, DotBinds, EvalType0, EvalType1),
    eval_atom_dot_union(Ts, Astn, DotBinds, EvalType1, EvalType).

eval_atom_dot_single([], _Astn, _DotBinds, EvalType, EvalType) -->> [ ].  % [kythe_fact, sym_rej]
eval_atom_dot_single([T|Ts], Astn, DotBinds, EvalType0, EvalType) -->>  % [kythe_fact, sym_rej]
    %% TODO: also allow func(...).attr (currently only allows class(...).attr
    Astn = astn(Start, End, Attr),
    (  { T = class(ClassName, _) }
    -> { atomic_list_concat([ClassName, '.', Attr], FqnAttr) },
       { ord_add_element(EvalType0, fqn(FqnAttr), EvalType1) },
       anchor(Start, End, Source),
       edge(Source, DotBinds, FqnAttr)
    ;  { T = import(_Fqn, Path) },
       { atomic_list_concat([Path, '::', Attr], FqnAttr) },  % TODO: need to resolve path
       anchor(Start, End, Source),
       edge(Source, DotBinds, FqnAttr),
       { EvalType1 = EvalType0 }
    ;  { EvalType1 = EvalType0 }
    ),
    eval_atom_dot_single(Ts, Astn, DotBinds, EvalType1, EvalType).

eval_atom_call_union(AtomEval, Parms, EvalType) -->>  % [kythe_fact, sym_rej]
   { ord_empty(EvalType0) },
    eval_atom_call_union(AtomEval, Parms, EvalType0, EvalType).

eval_atom_call_union([], _Parms, EvalType, EvalType) -->> [ ].  % [kythe_fact, sym_rej]
eval_atom_call_union([T|Ts], Parms, EvalType0, EvalType) -->>  % [kythe_fact, sym_rej]
    eval_single_type(T, ET0),
    eval_atom_call_single(ET0, Parms, EvalType0, EvalType1),
    eval_atom_call_union(Ts, Parms, EvalType1, EvalType).

eval_atom_call_single([], _Parms, EvalType, EvalType) -->> [ ].  % [kythe_fact, sym_rej]
eval_atom_call_single([T|Ts], Parms, EvalType0, EvalType) -->>  % [kythe_fact, sym_rej]
    (  { T = class(_, _) }  %% TODO: MRO for__init__ and output ref to it
    -> { Type = [T] }
    ;  { T = func(_, ReturnType) }
    -> { Type = ReturnType }
    ;  { Type = [func(T, Parms)] }
    ),
    { ord_union(EvalType0, Type, EvalType1) },
    eval_atom_call_single(Ts, Parms, EvalType1, EvalType).

% maplist(eval_union_type, Ts, ETs).
eval_union_type_list([], []) -->> [ ].  % [kythe_fact, sym_rej]
eval_union_type_list([T|Ts], [ET|ETs]) -->>  % [kythe_fact, sym_rej]
    eval_union_type(T, ET),
    eval_union_type_list(Ts, ETs).

% maplist(eval_union_type_and_lookup, Ts, ETs).
eval_union_type_and_lookup_list([], []) -->> [ ].  % [kythe_fact, sym_rej]
eval_union_type_and_lookup_list([T|Ts], [ET|ETs]) -->>  % [kythe_fact, sym_rej]
    eval_union_type_and_lookup(T, ET),
    eval_union_type_and_lookup_list(Ts, ETs).

exprs_from_symtab(SymtabPairs, Exprs) :-
    exprs_from_symtab2(SymtabPairs, ExprsRaw),
    sort(ExprsRaw, Exprs).  % removes dups
exprs_from_symtab2([], []).
exprs_from_symtab2([_Fqn-Type|FTs], Exprs) :-
    (  Type = []
    -> Exprs = Exprs2
    ;  Exprs = [expr(Type)|Exprs2]
    ),
    exprs_from_symtab2(FTs, Exprs2).

add_rej_to_symtab([], Symtab, Symtab).
add_rej_to_symtab([Fqn-RejType|FTs], Symtab0, Symtab) :-
    must_once(
        get_dict(Fqn, Symtab0, FqnType)),
    ord_union(FqnType, RejType, CombinedType),
    put_dict(Fqn, Symtab0, CombinedType, Symtab1),
    add_rej_to_symtab(FTs, Symtab1, Symtab).

%% The accumulator for 'sym_rej'.
%% Tries to unify Key-Type unifies with what's already in symtab;
%% if that fails because it's not in the symtab, adds it; otherwise
%% adds it Rej.
%% TODO: use library(assoc) instead of dict for Symtab (performance)
sym_rej_accum(Fqn-Type, Symtab0-Rej0, Symtab-Rej) :-
    (  get_dict(Fqn, Symtab0, TypeSymtab)
    -> Symtab = Symtab0,
       (  Type = TypeSymtab  %% in case Type is not instantiated (i.e., a lookup)
       -> Rej = Rej0
       ;  ord_union(TypeSymtab, Type, TypeComb),  % TODO: ord_intersect(TypeSymtab, Type) ?
          TypeComb = TypeSymtab
       -> Rej = Rej0
       ;  Rej = [Fqn-Type|Rej0]
       )
    ;  Type = []  %% in case Type is not instantiated (i.e., a lookup)
    -> Rej = Rej0,
       put_dict(Fqn, Symtab0, Type, Symtab)
    ;  Rej = Rej0,
       put_dict(Fqn, Symtab0, Type, Symtab)
    ).

dict_values(Dict, Values) :-
    dict_pairs(Dict, _, Pairs),
    pairs_values(Pairs, Values).

%% The accumulator for 'kythe_fact'.
kythe_fact_accum(T, In, Out) :-
    % If a fact is already there, keep it and ignore subsequent value (e.g.,
    % for redefining a variable).
    % This code depends on all the JSON dicts having a ground tag (e.g., the
    % Source and Target are typically dicts, and they must have ground tags ...
    % for simplicity, these are all 'json').
    (  T = json{source: Source, fact_name: FactName, fact_value: _FactBase64}
    -> (  get_assoc(Source-FactName, In, _)
       -> In = Out  % already there - ignore subsequent values
       ;  put_assoc(Source-FactName, In, T, Out)
       )
    ;  T = json{source: Source, edge_kind: EdgeKind, target: Target, fact_name: '/'},
       must_once(\+ get_assoc(Source-EdgeKind-Target, In, _)),
       put_assoc(Source-EdgeKind-Target, In, T, Out)
    ;  throw(error(bad_kythe_fact(T), _))
    ).

%% For more compact output (debugging):
portray(Astn) :-
    node_astn(Astn, Start, End, Value), !,
    format("'ASTN'(~d:~d, ~q)", [Start, End, Value]).
portray(str(S)) :-
    format('str(~q)', [S]), !.
portray(bool(B)) :-
    format('bool(~q)', [B]), !.
portray('StringNode'{astns: [Astn]}) :- !,  % doesn't handle "foo" "bar"
    format("'StringNode'{astn:[~p]}", [Astn]).
portray('NumberNode'{astn: Astn}) :-
    format("'StringNode'{astn:[~p]}", [Astn]).
portray(op([Astn])) :-
    node_astn(Astn, _, _, _), !,
    format('op([~p])', [Astn]).
portray(fqn(X)) :- !,
    format('fqn(~p)', [X]).
portray(func(F, R)) :- !,
    format('func(~p, ~p)', [F, R]).
portray(class(F, R)) :- !,
    format('class(~p, ~p)', [F, R]).
portray(union(U)) :- !,
    format('union(~p)', [U]).
portray(astn(Start, End, String)) :- !,
    format('astn(~p,~p, ~p)', [Start, End, String]).
%% work around a bug in print_term (the default numbervars(true)
%%     should handle this):
portray('$VAR'('_')) :- !,
    format('_', []).
portray('$VAR'(N)) :- !,
    Chr is N + 65,
    format('~c', [Chr]).
% portray(Symtab) :-
%     is_dict(Symtab, Tag),
%     ground(Tag),
%     Tag = symtab, !,
%     format('symtab{...}').

do_if(Cond, Pred) :-
    (  call(Cond)
    -> call(Pred)
    ;  true
    ).

%% Check that all items in a list can unify without instantiating
%% anything in the pattern.
map_eq(_Pattern, []).
map_eq(Pattern, [X|Xs]) :-
    \+ \+ Pattern = X,
    map_eq(Pattern, Xs).

%% TODO: Remove this debugging code
dump_term(Msg, Term) :-
    dump_term(Msg, Term, [tab_width(0),
                          indent_arguments(2),
                          right_margin(100)]).
dump_term(Msg, Term, Options) :-
    (  Msg = ''
    -> true
    ;  format(user_error, '% === ~w ===~n~n', [Msg])
    ),
    % print_term leaves trailing whitespace, so remove it
    with_output_to(
            string(TermStr),
            (current_output(TermStream),
             print_term(Term, [output(TermStream)|Options]))),
    re_replace(" *\n"/g, "\n", TermStr, TermStr2),
    (  Msg = ''
    -> format(user_error, '~s.~n', [TermStr2])
    ;  format(user_error, '~s.~n~n', [TermStr2]),
       format(user_error, '% === end ~w ===~n~n', [Msg])
    ).
