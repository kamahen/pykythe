% -*- mode: Prolog -*-

%% Post-processor for the simplified nodes with FQNs that is generated
%% by pykythe (see ast_cooked.Base.add_fqns).  The post-processing is
%% mainly:
%% - add Kythe anchor facts
%% - add facts/edges for attributes (e.g., "foo" in self.foo)
%% - resolve and process imports
%% - in future, things like function call references

%% TODO: :- use_module(library(protobufs)).  % instead of input/output JSON
%%        ... handling JSON seems to be the most expensive thing,
%%            according to profile/1 (it also seems to be the main
%%            contributor to garbage collection; base64 manipulation is
%%            also expensive).

%% Names and naming conventions:
%%  'astn' is an AST (Abstract Syntax Tree) node.
%%  'fqn' is fully qualified name
%%  'ky' as prefix means 'kythe' (e.g. kyfact instead of kythe_fact)
%%  'symtab' is symbol table
%%  'symrej' is symbol table (symtab) + rejects

%% There are multiple passes over the AST:
%%
%% 0. [run_parse_cmd] Create a "cooked" AST (Python program specified
%%    by --parsecmd), generating a JSON data structures of the AST.
%%
%% 1. [process_nodes] Read in the "cooked" AST (see ast_cooked.Base),
%%    transform the nodes into a simpler form, and produce (using
%%    accumulators):
%%    - Kythe facts (e.g., anchors) for all the variables and
%%      parameters in the code (but not the attributes);
%%    - references and assignments (in the assign/2 facts);
%%    - a few special items, such as anchors for import statements'
%%      dotted names.
%%    Each predicate also returns a "type" result that is used to
%%    populate the right-hand-side of assign/2 facts (for statements,
%%    a expr/1 term is returned for completeness.  For more details,
%%    see kynode//2.
%%
%% 2. [assign_exprs] Process the assign/2 and expr/1 facts, by
%%    interpreting the expressions and recording the results in a
%%    symtab (symbol table), then outputting Kythe facts for the
%%    attributes, calls, etc.  Note that expr([]) is a no-op.

%% Processing of "types" and "eval" ...

%% The input consists of a list of simplified items from the AST.
%% For example, this Python line (in class C2's __init__):
%%   self.x = 'C2_x'
%% is turned into something like the following (in portray-output format):
%%    'AssignExprStmt'{
%%        expr: 'StringNode'{astn: ['ASTN'(1160:1166, "'C2_x'")]},
%%        left: 'AtomDotNode'{
%%               atom: 'NameRefFqn'{
%%                         fqn: str("test_data.simple.C2.__init__.<local>.self"),
%%                         name: 'ASTN'(1151:1155, "self") },
%%               attr_name: 'ASTN'(1156:1157, "x"),
%%               binds: bool("True") } }
%% When this is read in, it is simplified to something like this:
%%   assign([dot(fqn('test_data.simple.C2.__init__.<local>.self')],
%%              astn(1156,1157, "x"),
%%              '/kythe/edge/defines/binding'),
%%          [class('typeshed.stdlib.3.builtin.str', [])])
%%                 %% (Py2.7 would be __builtin__.str)
%%
%% To process this, we need to resolve the FQNs (in this case,
%% fqn('test_data.simple.C2.__init__.<local>.self') by looking up in
%% the symtab, eventually resulting in the dot(...) expression being
%% reduced to fqn('test_data.simple.C2.x')). [Lookup also consists of
%% looking in the builtins and modules list; for simplicity, these are
%% considered to be part of the symtab although for implementation
%% reasons, they are kept separate.]

%% The symtab mappings are an ord_union (possibly empty) of:
%%     fqn(Fqn) - a global or local name (fully qualified)
%%     class(Fqn, Bases)  %% Bases is a list of union types
%%     func(Fqn, ReturnType)  %% ReturnType is a union type
%%     import_module(Fqn, {
%%         module_alone(Module,Path),
%%         module_and_token(Module,Path,Token),
%%         module_star(Module,Path)})

%% The "eval" predicates come in two flavors, depending on the
%% behavior with fqn(Fqn): eval..._and_lookup predicates are for the
%% right-hand-side of assignments; they use the symtab to look up any
%% resulting Fqn and return the associated value (or add it to symtab
%% and return []).  For the left-hand-side of an assignment, the
%% lookup isn't done.
%%   Implementation detail: lookup is done using
%%        [ Fqn-Result ]:symrej
%%   which calls symrej_accum/3 and uses the sym_rej_mod/3 functor to
%%   record the symtab, rejected symtab entries, and modules. [There
%%   are also global builtins, so a full lookup uses symtab, modules,
%%   global-builtins symtab.]
%%
%% A symtab lookup can occur either as in a right-hand (evaluation)
%% context or left-hand (assignment) context (see symtab_accum/3).
%%
%%  +---------+--------------+------------------------------+
%%  | Context | Entry exists |                              |
%%  |         | in symtab    | Action                       |
%%  +---------+--------------+------------------------------+
%%  | eval    | no           | add with type=[] ("Any")     |
%%  +---------+--------------+------------------------------+
%%  | eval    | yes          | use symtab type              |
%%  |         |              | add to "reject" list if new  |
%%  |         |              | type info                    |
%%  +---------+--------------+------------------------------+
%%  | assign  | no           | add with type of r.h.s.      |
%%  +---------+--------------+------------------------------+
%%  | assign  | yes          | unify with type of r.h.s     |
%%  |         |              | add to "reject" list if new  |
%%  |         |              | type info                    |
%%  +---------+--------------+------------------------------+
%%
%% New type info is determined by union-ing the two types and seeing
%% if the result is different from what's in the symtab -- if
%% different, the Fqn-Type is added to the "reject" list and the
%% symtab entry is updated with the additional type information.  If
%% there is new type information, it is because a previous "eval" node
%% was processed without all available information, so another pass
%% needs to be done over the "eval" nodes.  The next pass will also
%% add the new type information from the "reject" list to the symtab
%% before reprocessing all the "eval" nodes.
%%
%% See discussion below about reprocessing of "reject"ed items.

%% All types are unions (represented as an ordset); [] means that
%% there's no information and is effectively "Any". Many of the
%% predicates come in two versions: one that works with a type union
%% (ordset), and one that works on single "types", such as fqn(...),
%% class(...), func(...), etc. -- typically the predicate that works
%% with a type union iterates over the single items, calling the
%% predicate for single "types", and uses ord_union/3 to combine the
%% results.  (This use of ord_union ensures that there's no need to
%% "flatten" the list and that the single types are kept in a
%% canonical order).

%% The list of assign(Left, Right) terms and expr(Right) terms is
%% repeatedly reprocessed until no changes occur (a count is kept of
%% eval passes, to prevent an infinite loop -- I suspect that an
%% infinite loop isn't possible; but it's a lot of work to actually
%% prove that). When a FQN is first encountered, it is put into the
%% symtab with its type (the type is [] if it can't be determined) --
%% when subsequently encountered, any inconsistency in type is added
%% to the "reject" list. After a pass is complete, the rejects' types
%% are union-ed with the corresponding symtab entries' types and if
%% there were changes, another pass is done. In this way, each
%% expression is repeatedly reprocessed until no more changes (in
%% practice, only one or two passes are needed).

%% Processing of modules ...
%%
%% TODO: this isn't yet implemented -- review the documentation when
%%       implementation is done.
%%
%% Module imports are detected during the first pass, but are deferred
%% to the second pass (which builds up the symbol table). It would be
%% nice to put modules into the symbol table, but that doesn't quite
%% work because we need a "global" list of modules (to handle
%% recursive imports) and we want to start the processig of each
%% module with an empty symtab.
%%
%% When a module (import) is first encountered, it is put into the
%% modules dict with the tag 'importing'". When it is finished being
%% imported, this is changed to the tag symtab(Symtab). In this way,
%% we can prevent infinite loops on imports, but still have partial
%% values to evaluate (which will typically cause another pass).
%%
%% In addition to tracking modules as a separate dict, an entry is
%% made into the symtab (e.g., "from foo import bar" causes a symtab
%% entry of "bar") which contains module(Fqn), which can be used to
%% index into the Modules dict and to continue processing "."s.

%% TODO: can we remove the kyfact accumulator from the first pass
%%       and generate all the Kythe information from the second pass?

%% TODO: Use QLF: http://www.swi-prolog.org/pldoc/man?section=qlf

:- module(pykythe, [pykythe_main/0]).

:- use_module(library(aggregate), [aggregate_all/3, foreach/2]).
:- use_module(library(apply), [maplist/2, maplist/3, maplist/4, foldl/4, convlist/3]).
:- use_module(library(assoc), [is_assoc/1]).
:- use_module(library(base64), [base64/2]).
:- use_module(library(debug), [assertion/1, debug/3]).
:- use_module(library(edcg)).   % requires: ?- pack_install(edcg).
:- use_module(library(error), [type_error/2]).
:- use_module(library(filesex), [make_directory_path/1, directory_file_path/3]).
:- use_module(library(http/json), [json_read_dict/2, json_write_dict/3]).
:- use_module(library(lists), [append/3, list_to_set/2, member/2, reverse/2, select/3]).
:- use_module(library(optparse), [opt_arguments/3]).
:- use_module(library(ordsets), [list_to_ord_set/2, ord_empty/1, ord_union/3, ord_add_element/3]).
:- use_module(library(pairs), [pairs_keys/2, pairs_values/2]).
:- use_module(library(pcre), [re_replace/4, re_match/2, re_matchsub/4]).
:- use_module(library(pprint), [print_term/2]).
:- use_module(library(readutil), [read_file_to_string/3]).
:- use_module(library(yall)).
%% :- use_module(library(apply_macros).  % TODO: for performance
:- use_module(must_once, [must_once/1, must_once_msg/2, must_once_msg/3, fail/1]).

:- meta_predicate
       maplist_kyfact(4, +, +, -, +),
       maplist_kyfact(5, +, -, +, -, +),
       maplist_kyfact_symrej(7, +, -, +, -, +, -, +),
       maplist_kyfact_expr(6, +, +, -, +, -, +),
       maplist_kyfact_expr(7, +, +, +, -, +, -, +),
       maplist_foldl_kyfact_expr(8, +, +, -, +, -, +, -, +),
       maplist_foldl_kyfact_symrej(8, +, +, -, +, -, +, -, +).

:- style_check(+singleton).
:- style_check(+var_branches).
:- style_check(+no_effect).
:- style_check(+discontiguous).
%% :- set_prolog_flag(generate_debug_info, false).

:- use_module(library(rdet), [rdet/1]).
%% Note: library(rdet) expands a call to
%%       ( Call -> true ; throw(...) )
%%       which causes a warning about variable not introduced in all
%%       branches if the Call instantiates something that's used
%%       later.  Therefore, we don't enable style_check(+var_branches)
:- style_check(-var_branches).

%% TODO: there are too many rdet declarations, and they slow things
%%       down (although this might be only compilation).

%% Higher-level predicates that we use deterministically:
:- maplist(rdet, [convlist/3,
                  foldl/4,
                  maplist/2,
                  maplist/3,
                  maplist/4
                 ]).

%% Other imported predicates:
:- maplist(rdet, [aggregate_all/3,
                  base64/2,
                  dict_values/2,
                  directory_file_path/3,
                  foreach/2,
                  json_read_dict/2,
                  json_write_dict/3,
                  list_to_ord_set/2,
                  list_to_set/2,
                  opt_arguments/3
                 ]).

%% Deterministic predicates in this module
%% You can generate an approximation of these by:
%%     forall(current_predicate(pykythe:Pred), format('>>> ~q~n', [Pred])).
%% Commented-out items are non-deterministic.

:- maplist(rdet, ['NameRawNode_astn_and_name'/3,
                  absolute_dir/2,
                  add_rej_to_symtab/3,
                  assign_expr_eval/6,
                  assign_exprs/7,
                  assign_exprs_count/8,
                  assign_exprs_count_impl/8,
                  assign_normalized/7,
                  base64_string/2,
                  %% builtin_name/1,
                  builtin_names/1,
                  %% canonical_path/2,
                  do_if/2,
                  dot_edge_name/2,
                  dotted_name_imports/7,
                  double_dot/2,
                  dump_term/2,
                  dump_term/3,
                  eval_atom_call_single/9,
                  eval_atom_call_single_of_type/9,
                  eval_atom_call_union/8,
                  eval_atom_dot_single/10,
                  eval_atom_dot_union/9,
                  eval_atom_dot_union_of_type/10,
                  eval_lookup/7,
                  eval_lookup_single/7,
                  eval_single_type/7,
                  eval_single_type_and_lookup/7,
                  eval_union_type/7,
                  eval_union_type_and_lookup/7,
                  %% expr_from_symtab/2,
                  expr_normalized/6,
                  exprs/3,
                  full_module_pieces/2,
                  full_path/5,
                  full_path_prefixed/5,
                  initial_symtab/1,
                  json_read_dict/2,
                  json_write_dict/3,
                  kyImportDotNode/3,
                  kyImportDottedAsNamesFqn/7,
                  kyImportDottedAsNamesFqn_comb/11,
                  kyImportDottedAsNamesFqn_dots/5,
                  kyImportDottedAsNamesFqn_dots2/3,
                  kyImportFromStmt/7,
                  kyNameRawNode/3,
                  kyanchor/6,
                  kyanchor_kyedge_fqn/7,
                  kyanchor_kyedge_fqn_pieces/8,
                  kyanchor_node/5,
                  kyanchor_node/6,
                  kyanchor_node_kyedge_fqn/6,
                  kyanchor_node_kyedge_fqn_pieces/7,
                  kyanchor_node_kyedge_fqn/7,
                  kyedge/6,
                  kyedge_fqn/6,
                  kyfact/6,
                  kyfact_b64/6,
                  kyfacts/5,
                  kyfile/4,
                  kynode/7,
                  lookup_module/2,
                  make_directory_path/1,
                  maplist_assign_expr_eval/6,
                  maplist_foldl_eval_lookup/8,
                  maplist_foldl_eval_union_type/8,
                  maplist_foldl_kyfact_expr/9,
                  maplist_foldl_kyfact_symrej/9,
                  maplist_kyfact/5,
                  maplist_kyfact/6,
                  maplist_kyfact_expr/7,
                  maplist_kyfact_expr/8,
                  maplist_kyfact_symrej/8,
                  maplist_kynode/7,
                  %% maybe_close/1,
                  %% maybe_open_read/2,
                  module_part/2,
                  %% module_path/2,
                  %% must_once/1,
                  %% must_once_msg/2,
                  %% must_once_msg/3,
                  my_json_read_dict/2,
                  %% my_portray/1,
                  %% my_portray_unify/2,
                  %% node_astn/4,
                  %% node_astn0/4,
                  opt/2,
                  opts/2,
                  output_kyfact/2,
                  parse_and_process_module/6,
                  %% parse_and_process_module_cached/6,
                  parse_and_process_module_fresh/6,
                  %% path_expand/3,
                  path_part/2,
                  %% path_to_python_module/2,
                  path_part_to_python_module_or_unknown/2,
                  path_to_python_module_or_unknown/2,
                  print_term_cleaned/3,
                  process_nodes/5,
                  process_nodes/7,
                  %% py_ext/2,
                  %% py_ext_ext/1,
                  %% pykythe_main/0,
                  pykythe_main2/0,
                  pykythe_opts/2,
                  %% pythonpath_prefix/2,
                  read_nodes/4,
                  ref_import/4,
                  remove_last_component/3,
                  remove_suffix_star/3,
                  run_parse_cmd/4,
                  set_json_dict_tag/2,
                  signature_node/3,
                  signature_node_kyfact/6,
                  signature_node_kyfacts/5,
                  signature_source/3,
                  simple_path_module/2,
                  simplify_json/2,
                  simplify_json_slot_pair/2,
                  simplify_meta/3,
                  split_atom/4,
                  split_module_atom/2,
                  split_path_string_and_canonicalize/3,
                  src_base/2,
                  symrej_accum/3,
                  symrej_accum_found/7,
                  symtab_as_kyfact/3,
                  zip_merge/3]).

%% The autoload directive needs to be after rdet/1 is used once, to
%% allow its autoload to get rdet:debug.
%% The "-O" flag changes things slightly; the following directive
%% needs to be here (and not earlier) with "-O".

:- set_prolog_flag(autoload, false).  % See below json:term_to_dict/3

%% "kyfact" accumulator gets FQN anchor facts, in an ordinary list
%% with each value being a dict to be output in JSON. The list may
%% contain duplicates, which are removed before output.

%% TODO: Need test cases for this:
%% Duplicates can arise if a variable is redefined in the Python
%% source; our de-dup keeps the type information for the first such
%% definition and outputs defines/binding for all instances.

%% TODO: check for duplicate edge facts, which indicate a bug.
edcg:acc_info(kyfact, T, Out, In, Out=[T|In]).

%% "expr" accumulator gets expressions that need interpreting.
edcg:acc_info(expr, T, Out, In, Out=[T|In]).

%% "symrej" accumulator is for symtab + rejected items that need reprocessing.
edcg:acc_info(symrej, FqnType, In, Out, symrej_accum(FqnType, In, Out)).

%% "file_meta" passed arg contains meta-info about the current file.
edcg:pass_info(file_meta).

edcg:pred_info(maplist_kyfact, 2,                [kyfact, file_meta]).
edcg:pred_info(maplist_kyfact, 3,                [kyfact, file_meta]).

edcg:pred_info(dotted_name_imports, 4,           [kyfact, file_meta]).
edcg:pred_info(kyImportFromStmt, 4,              [kyfact, file_meta]).
edcg:pred_info(kyanchor, 3,                      [kyfact, file_meta]).
edcg:pred_info(kyanchor_kyedge_fqn, 4,           [kyfact, file_meta]).
edcg:pred_info(kyanchor_kyedge_fqn_pieces, 5,    [kyfact, file_meta]).
edcg:pred_info(kyanchor_node, 2,                 [kyfact, file_meta]).
edcg:pred_info(kyanchor_node, 3,                 [kyfact, file_meta]).
edcg:pred_info(kyanchor_node_kyedge_fqn, 3,      [kyfact, file_meta]).
edcg:pred_info(kyanchor_node_kyedge_fqn_pieces, 4, [kyfact, file_meta]).
edcg:pred_info(kyanchor_node_kyedge_fqn, 4,      [kyfact, file_meta]).
edcg:pred_info(kyedge, 3,                        [kyfact, file_meta]).
edcg:pred_info(kyedge_fqn, 3,                    [kyfact, file_meta]).
edcg:pred_info(kyfact, 3,                        [kyfact, file_meta]).
edcg:pred_info(kyfact_b64, 3,                    [kyfact, file_meta]).
edcg:pred_info(kyfacts, 2,                       [kyfact, file_meta]).
edcg:pred_info(kyfile, 1,                        [kyfact, file_meta]).
edcg:pred_info(ref_import, 1,                    [kyfact, file_meta]).
edcg:pred_info(ref_imports, 1,                   [kyfact, file_meta]).
edcg:pred_info(signature_node_kyfact, 3,         [kyfact, file_meta]).
edcg:pred_info(signature_node_kyfacts, 2,        [kyfact, file_meta]).

edcg:pred_info(maplist_foldl_kyfact_expr, 4,     [kyfact, expr, file_meta]).
edcg:pred_info(maplist_kyfact_expr, 2,           [kyfact, expr, file_meta]).
edcg:pred_info(maplist_kyfact_expr, 3,           [kyfact, expr, file_meta]).

edcg:pred_info(assign_normalized, 2,             [kyfact, expr, file_meta]).
edcg:pred_info(expr_normalized, 1,               [kyfact, expr, file_meta]).
edcg:pred_info(import_from, 1,                   [kyfact, expr, file_meta]).
edcg:pred_info(kyImportDottedAsNamesFqn, 2,      [kyfact, expr, file_meta]).
edcg:pred_info(kyImportDottedAsNamesFqn_comb, 6, [kyfact, expr, file_meta]).
edcg:pred_info(kynode, 2,                        [kyfact, expr, file_meta]).
edcg:pred_info(kynode_impl, 2,                   [kyfact, expr, file_meta]).
edcg:pred_info(maplist_kynode, 2,                [kyfact, expr, file_meta]).
edcg:pred_info(process_nodes, 2,                 [kyfact, expr, file_meta]).

edcg:pred_info(maplist_foldl_kyfact_symrej, 4,   [kyfact, symrej, file_meta]).
edcg:pred_info(maplist_kyfact_symrej, 3,         [kyfact, symrej, file_meta]).

edcg:pred_info(assign_expr_eval, 1,              [kyfact, symrej, file_meta]).
edcg:pred_info(assign_exprs_count, 2,            [kyfact, symrej, file_meta]).
edcg:pred_info(eval_atom_call_single, 4,         [kyfact, symrej, file_meta]).
edcg:pred_info(eval_atom_call_single_of_type, 4, [kyfact, symrej, file_meta]).
edcg:pred_info(eval_atom_call_union, 3,          [kyfact, symrej, file_meta]).
edcg:pred_info(eval_atom_dot_single, 5,          [kyfact, symrej, file_meta]).
edcg:pred_info(eval_atom_dot_union, 4,           [kyfact, symrej, file_meta]).
edcg:pred_info(eval_atom_dot_union_of_type, 5,   [kyfact, symrej, file_meta]).
edcg:pred_info(eval_lookup, 2,                   [kyfact, symrej, file_meta]).
edcg:pred_info(eval_lookup_single, 2,            [kyfact, symrej, file_meta]).
edcg:pred_info(eval_single_type, 2,              [kyfact, symrej, file_meta]).
edcg:pred_info(eval_single_type_and_lookup, 2,   [kyfact, symrej, file_meta]).
edcg:pred_info(eval_union_type, 2,               [kyfact, symrej, file_meta]).
edcg:pred_info(eval_union_type_and_lookup, 2,    [kyfact, symrej, file_meta]).
edcg:pred_info(maplist_assign_expr_eval, 1,      [kyfact, symrej, file_meta]).
edcg:pred_info(maplist_foldl_eval_lookup, 3,     [kyfact, symrej, file_meta]).
edcg:pred_info(maplist_foldl_eval_union_type, 3, [kyfact, symrej, file_meta]).

edcg:pred_info(signature_node, 2,                [file_meta]).
edcg:pred_info(signature_source, 2,              [file_meta]).

edcg:pred_info(exprs, 1,                         [expr]).

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

%! builtin_names(-BuiltinNames:list) is det.
builtin_names([]).  % TODO: use the commented-out list above

%! builtin_name(+BuiltinName:atom) is det.
%! builtin_name(-BuiltinName:atom) is nondet.
%% True if BuiltinName is in the initial Python (builtins) symbol table.
builtin_name(Name) :-
    builtin_names(Names),
    member(Name, Names).

%! initial_symtab(-Symtab:dict) is det.
%%  creates a symtab with the contents of typeshed/stdlib/3/builtins.pyi
%% TODO: implement this fully
initial_symtab(Symtab) :-
    (  bagof(BuiltinName-Type,
            (builtin_name(Name),
             atomic_list_concat([builtin, Name], '.', BuiltinName),
             list_to_ord_set([class(BuiltinName, [])], Type)
            ),
            SymtabPairs)
    ;  SymtabPairs = []
    ),
    % TODO: string, number are provisional
    %       also use list_to_ord_set
    % TODO: add builtin.int, etc. should really process builtin.pyi
    dict_create(Symtab, symtab,
                ['builtin.str'-[class('builtin.str', [])],
                 'builtin.Number'-[class('builtin.Number', [])]
                | SymtabPairs]).

%% For debugging, comment out the following and run:
%%       set_prolog_flag(autoload,true).  debug.
%%       pykythe:pykythe_main2.
%% or from a script:
%%      echo "pykythe:pykythe_main" | swipl ...
%% Note that for running in an emacs shell, you might want swipl --no-tty
%% See https://groups.google.com/forum/#!topic/swi-prolog/WrC9x3vQBBY
%% :- initialization(pykythe_main, main). % TODO: reinstate this (see comment in Makefile).

%! main is det.
%% The main predicate, run during initialization.
%%  See also library(main)'s definition of main
%%  This simply calls pykythe_main2, so that we can do:
%%    :- use_module(library(test_cover), [show_coverage/1]).
%%    pykythe_main :-
%%        set_prolog_flag(autoload, true),
%%        show_coverage(pykythe_main2).
pykythe_main :-
    set_prolog_flag(report_error, true),     % TODO: remove
    set_prolog_flag(backtrace, true),        % TODO: remove
    set_prolog_flag(backtrace_show_lines, true), % TODO: remove
    %% Play nice with emacs *compilation*:
    set_prolog_flag(color_term, false),      % TODO: remove (to ~/.plrc)
    pykythe_main2,
    halt.

pykythe_main2 :-
    % set_prolog_flag(gc, true),  % TODO: tune GC for performance
    % set_prolog_flag(agc_margin, 0),  % TODO: tune GC for performance
    on_signal(int, _, interrupt),
    pykythe_opts(SrcPath, Opts),
    path_to_python_module_or_unknown(SrcPath, SrcFqn),
    parse_and_process_module(SrcPath, SrcFqn, Opts, _Symtab, modules{}, _Modules).

%! pykythe_opts(-SrcPath:atom, -Opts:list(pair)) is det.
%% Process the command line, getting the source file and options.
pykythe_opts(SrcPath, Opts) :-
    current_prolog_flag(version, PrologVersion),
    must_once_msg(PrologVersion >= 70720, 'SWI-Prolog version is too old', []),  % Sync this with README.md
    OptsSpec = [
        [opt(parsecmd), type(atom), longflags([parsecmd]),
         help('Command for running parser than generates fqn.json file')],
        [opt(kythe_corpus), type(atom), default(''), longflags(['kythe_corpus']),
        help('Value of "corpus" in Kythe facts')],
        [opt(kythe_root), type(atom), default(''), longflags(['kythe_root']),
         help('Value of "root" in Kythe facts')],
        [opt(pythonpath), type(atom), default(''), longflags(['pythonpath']),
         help('Similar to $PYTHONPATH for resolving imports (":"-separated paths)')],
        [opt(kytheout), type(atom), default(''), longflags(['kytheout']),
         help('Directory for output of imported files (including "main" file)')],
        [opt(kytheout_suffix), type(atom), default('.kythe.json'), longflags(['kythout-suffix']),
         help('Suffix (extension including leading ".") for output files')],
        [opt(python_version), type(integer), default(3), longflags(python_version),
         help('Python major version')]
    ],
    opt_arguments(OptsSpec, Opts0, PositionalArgs),
    must_once_msg(PositionalArgs = [SrcPath0], 'Missing/extra positional args', []),
    absolute_file_name(SrcPath0, SrcPath),
    split_path_string_and_canonicalize(pythonpath, Opts0, Opts).

%! split_path_string_and_canonicalize(+OptName:atom, +Opts0:list, -Opts:list) is det.
%%  Find the option given by OptName in Opts0, split the value into
%%  components in a list, add back into Opts (can be in a different
%%  position in the list).  The resulting list of files are all in
%%  canonical form, using absolute_file_name/3.
split_path_string_and_canonicalize(OptName, Opts0, [NewOpt|Opts1]) :-
    Opt =.. [OptName, PathStr],
    select(Opt, Opts0, Opts1),
    split_atom(PathStr, ':', '', PathList0),
    maplist(absolute_dir, PathList0, PathList),
    NewOpt =.. [OptName, PathList].

%! absolute_dir(+Path0:atom, -AbsPath:atom) is det.
%%  Apply absolute_file_name to Path0, giving AbsPath, ensuring it's a
%%  directory and appending '/' to the name.
absolute_dir(/, /) :- !.  % Special case for root dir, which otherwise would become '//'
absolute_dir(Path0, AbsPath) :-
    remove_suffix_star(Path0, '/', Path),
    absolute_file_name(Path, AbsPath0, [access(read), file_type(directory), file_errors(fail)]),
    atom_concat(AbsPath0, '/', AbsPath).

%! split_atom(+Atom:atom, +SepChars:atom, +PadChars:atom, -SubAtoms:list(atom)) is det.
%% Like split_string, but result is a list of atoms.
split_atom(Atom, SepChars, PadChars, SubAtoms) :-
    split_string(Atom, SepChars, PadChars, SubStrings),
    maplist([S,A]>>atom_string(A,S), SubStrings, SubAtoms).

%! remove_suffix_star(+Full:atom, +Suffix:atom, -NoSuffix:atom) is det.
%% Repeatedly removes suffix if present.
remove_suffix_star(Full, Suffix, NoSuffix) :-
    (  atom_concat(Full1, Suffix, Full)
    -> remove_suffix_star(Full1, Suffix, NoSuffix)
    ;  NoSuffix = Full
    ).

%! lookup_module(+Module:atom, -FullPath:atom) is det.
%% TODO: document this.
lookup_module(Module, FullPath) :-
    module_path(Module, CanonicalPath),
    atom_string(FullPath, CanonicalPath),  % TODO - use string instead of atom
    path_to_python_module(FullPath, Fqn),  % TODO - if path_to_python_module_or_unknown, fails consistency check below
    must_once_msg(
        Module == Fqn,
        'Derived FQN differs from Module name ... canonical(~q) full(~q)',
        [CanonicalPath, FullPath]).

%! module_path(+Module:atom, -Path:atom) is nondet.
%! module_path(-Module:atom, +Path:atom) is nondet.
%% Convert a module ('path.to.module') to a path
%% ('path/to/module.py').  Does not check for existence of the file,
%% nor apply any PythonPath addition. Backtracks through all
%% solutions. At least one of Module and Path must be instantiated.
%% TODO: harmonize this with path_to_python_module / path_to_python_module_or_unknown.
module_path(Module, Path) :-
    (  var(Module)
    -> py_ext(Path0, Path2),
       simple_path_module(Path0, Module)
    ;  simple_path_module(Path0, Module),
       py_ext(Path0, Path2)
    ),
    canonical_path(Path2, Path).

%! simple_path_to_module(+Path, -Module) is det.
%! simple_path_to_module(-Path, +Module) is det.
%% TODO: use library(pcre) re_replace?
simple_path_module(Path, Module) :-
    (  var(Module)
    -> split_atom(Path, '/', '', ModuleParts),
       atomic_list_concat(ModuleParts, '.', Module)
    ;  split_module_atom(Module, ModuleParts),
       atomic_list_concat(ModuleParts, '/', Path)
    ).

%! path_to_python_module_or_unknown(+Path, -Fqn) is det.
%% Get the Fqn for the Python module corresponding to Path or
%% a '<unknown>...' atom.
%% TODO: harmonize this with path_module/2.
path_to_python_module_or_unknown(Path, Fqn) :-
    (  path_to_python_module(Path, Fqn)
    -> true
    ;  %% Make sure the result conforms with FQN dotted name, so that
       %% match_reversed_module_and_dotted_names/3 works properly
       split_atom(Path, '/', '', PathParts),
       (  PathParts = ['$PYTHONPATH'|PathParts2]
       -> atomic_list_concat(['<unknown>'|PathParts2], '.', Fqn)
       ;  PathParts = [''|PathParts2]
       -> atomic_list_concat(['<unknown>'|PathParts2], '.', Fqn)
       ;  atomic_list_concat(['<unknown>'|PathParts], '.', Fqn)
       )
    ).

%! path_to_python_module(+Path, -Fqn) is semidet.
%% Get the Fqn for the Python module corresponding to Path or fail.
%%  TODO: harmonize with module_path/2
path_to_python_module(Path, Fqn) :-
    canonical_path(Path, CanonicalPath),
    py_ext(CanonicalPath0, CanonicalPath),
    simple_path_module(CanonicalPath0, Fqn).

%! canonical_path(+Path, -CanonicalPath) is semidet.
%% Get a Path into a canonical (absolute) form.
%% Fails if the file or directory doesn't exist.
canonical_path(Path, CanonicalPath) :-
    %% TODO: besides being slightly less efficient, this doesn't do
    %       quite what we want -- probably want to change
    %       path_expand/2 to use py_ext/2 for files and to use the
    %       given file name for directories. However, it's unlikely
    %       that anyone would notice the subtlely different semantics
    %       -- e.g., we allow a directory whose name ends in '.py' or
    %       a file without a '.py' extension, but these wouldn't be
    %       allowed by the Python interpreter, so the extra
    %       permissivity is shouldn't be a problem.
    (  absolute_file_name(Path, AbsPath, [access(read), file_errors(fail)])
    -> true
    ;  absolute_file_name(Path, AbsPath, [access(read), file_type(directory), file_errors(fail)])
    ),
    atom_string(CanonicalPath, AbsPath).  % TODO: use string

%! parse_and_process_module(+SrcPath:atom, +SrcFqn:atom, +Opts:list, -Symtab, +Modules0, -Modules) is det.
%% Read in a single file (JSON output from pykythe module, which
%% encodes the AST nodes with FQNs), output Kythe JSON to current
%% output stream. SrcPath is assumed to be in absolute form (leading
%% '/').
parse_and_process_module(SrcPath, SrcFqn, Opts, Symtab, Modules0, Modules) :-
    must_once(is_absolute_file_name(SrcPath)),
    opts(Opts, [kytheout(KytheOutDir), kytheout_suffix(KytheOutSuffix)]),
    src_base(SrcPath, SrcPathBase),
    atomic_list_concat([KytheOutDir, SrcPathBase, KytheOutSuffix], KythePath),
    directory_file_path(KythePathDir, _, KythePath),
    make_directory_path(KythePathDir),
    (  setup_call_cleanup(maybe_open_read(KythePath, KytheInputStream),
                          parse_and_process_module_cached(KytheInputStream, KythePath, SrcPath, Symtab, Modules0, Modules),
                          close(KytheInputStream))
    ;  parse_and_process_module_fresh(SrcFqn, KythePath, Opts, Symtab, Modules0, Modules)
    ),
    do_if(false,
          dump_term('SYMTAB', Symtab)),
    do_if(true,
          dump_term('MODULES', Modules)).

%! maybe_open_read(+Path, -InputStream) is semidet.
%% Open Path for read or fail.
maybe_open_read(Path, InputStream) :-
    catch(open(Path, read, InputStream), _, fail).

%! maybe_close(?Stream) is det.
%% Close Stream, catching any errors (e.g., Stream is uninstantiated).
maybe_close(Stream) :-
    catch(close(Stream), _, true).

%! parse_and_process_module_cached(+KytheInputStream, +KythePath:atom, +SrcPath:atom, -Symtab, +Modules0, -Modules) is semidet.
%% TODO: needs to set Modules (see parse_and_process_module_fresh/6)
parse_and_process_module_cached(KytheInputStream, KythePath, SrcPath, Symtab, Modules0, Modules)  :-
    do_if(false, format(user_error, 'Trying to reuse ~q for ~q~n', [KythePath, SrcPath])), % TODO: delete
    %% The following validation depends on what kyfile//1 generates.
    %% Note that the items are sorted by default.
    %% TODO: read in everything, so no need for order dependency?
    my_json_read_dict(KytheInputStream, JsonSymtab),
    my_json_read_dict(KytheInputStream, JsonPath),
    my_json_read_dict(KytheInputStream, JsonEncoding),
    my_json_read_dict(KytheInputStream, JsonText),
    %% The following tests can die with cryptic error messages ... we
    %% could make things a bit nicer by first doing, e.g.
    %%     must_once_msg(get_dict(fact_name, JsonSymtab, "/pykythe/symtab"),
    %%         'Invalid JSON, expecting fact_name="/pykythe/symtab": ~q', [JsonSymtab]),
    must_once(JsonSymtab.fact_name == "/pykythe/symtab"),
    must_once(JsonPath.fact_name == "/kythe/node/kind"),
    base64_string(JsonPath.fact_value, JsonPathFactValue),
    must_once(JsonPathFactValue == "file"),
    must_once(JsonEncoding.fact_name == "/kythe/text/encoding"),
    must_once(JsonText.fact_name == "/kythe/text"),
    atom_string(JsonTextSourcePath, JsonText.source.path),
    must_once(JsonTextSourcePath == SrcPath),
    base64_string(JsonText.fact_value, JsonTextString),
    read_file_to_string(SrcPath, SrcText, [file_errors(fail)]),
    SrcText == JsonTextString,  % TODO: other conditions, such as pykythe version?
    base64_string(JsonSymtab.fact_value, SymtabString),
    term_string(Symtab, SymtabString),
    %% TODO: Check the "version" of pykythe.pl against the version
    %%       that created the KythePath file and not reuse if there's
    %%       been a change.
    Modules = Modules0,        % TODO: need to add new (cached) module
                               % See also "read in everything" comment, above.
    do_if(true,
          format(user_error, 'Reusing ~q for ~q: ~p~n', [KythePath, SrcPath, Symtab])).  % TODO: delete

%! parse_and_process_module_fresh(+SrcFqn:atom, +KythePath:atom, -Symtab, +Modules0, -Modules) is det.
parse_and_process_module_fresh(SrcFqn, KythePath, Opts, Symtab, Modules0, Modules) :-
    opts(Opts, [pythonpath(Pythonpaths)]),
    do_if(false, dump_term('PYTHONPATHS', Pythonpaths)),  % TODO: delete
    lookup_module(SrcFqn, SrcPath),
    do_if(true,
          format(user_error, 'Processing ~q (~q) to ~q~n', [SrcPath, SrcFqn, KythePath])),
    run_parse_cmd(Opts, SrcPath, SrcFqn, ParsedPath),
    read_nodes(ParsedPath, Pythonpaths, Nodes, Meta),
    do_if(false,
          dump_term('NODES', Nodes)),
    process_nodes(Nodes, src{src_fqn: SrcFqn, src: SrcPath},
                  KytheFacts, Exprs, Meta),
    do_if(false,
          dump_term('EXPRS', Exprs, [indent_arguments(auto),
                                     right_margin(72)])),
    assign_exprs(Exprs, Meta, SrcFqn, Symtab, KytheFacts2, Modules0, Modules),
    open(KythePath, write, KytheStream),
    % write(KytheStream, "%% === Kythe ==="), nl(KytheStream),
    symtab_as_kyfact(Symtab, Meta, SymtabKytheFact),
    output_kyfact(KytheStream, SymtabKytheFact),
    maplist(output_kyfact(KytheStream), KytheFacts),
    maplist(output_kyfact(KytheStream), KytheFacts2),
    close(KytheStream).

%! src_base(+SrcPath: atom, -SrcPathBase) is det.
%% Remove extension (.py, .pyi) from a source path.
src_base(SrcPath, SrcPathBase) :-
    (  atom_concat(SrcPathBase, '.py', SrcPath)
    -> true
    ;  atom_concat(SrcPathBase, '.pyi', SrcPath)
    -> true
    ;  type_error(file_name_not_ending_in_py_or_pyi, SrcPath)
    ).

%! run_parse_cmd(+Opts, +SrcPath, +SrcFqn, -OutPath) is det.
%% Run the parse command into a temporary file. (The temp file is
%% automatically deleted on graceful termination.)
%% An alternative would be to run the parse command as a process, into
%% a a pipe. This needs more memory, is more complicated to manage,
%% and is a bit more difficult to debug.
run_parse_cmd(Opts, SrcPath, SrcFqn, OutPath) :-
    must_once_msg(ground(Opts), 'Invalid command line options', []),
    opts(Opts, [python_version(PythonVersion), parsecmd(ParseCmd), kythe_corpus(KytheCorpus), kythe_root(KytheRoot)]),
    must_once_msg(memberchk(PythonVersion, [2, 3]), 'Invalid Python version: ~q', [PythonVersion]),
    tmp_file_stream(OutPath, OutPathStream, [encoding(binary), extension('fqn-json')]),
    close(OutPathStream),
    atomic_list_concat(
            [ParseCmd,
             " --kythe_corpus='", KytheCorpus, "'",
             " --kythe_root='", KytheRoot, "'",
             " --python_version='", PythonVersion, "'",
             " --srcpath='", SrcPath, "'",
             " --module='", SrcFqn, "'",
             " --out_fqn_expr='", OutPath, "'"],
            Cmd),
    do_if(false, dump_term('CMD', Cmd)),
    must_once_msg(shell(Cmd, 0), 'Parse failed', []).

%! symtab_as_kyfact(+Symtab, +Meta, -KytheFactAsJsonDict) is det.
%% Convert the symtab into a Kythe fact.
symtab_as_kyfact(Symtab, Meta,
                 json{source: Source,
                      fact_name: '/pykythe/symtab',
                      fact_value: SymtabStr64}) :-
    term_string(Symtab, SymtabStr),
    % TODO: the following is dup-ed from kyfile//0 but
    %       with Language specified
    base64(SymtabStr, SymtabStr64),
    Source = json{path: Meta.path, language: Meta.language}.

%! read_nodes(+FqnExprPath:atom, +Pythonpaths:list, -Nodes, -Meta:dict) is det.
%% Read the JSON node tree (with FQNs) into Nodes and file meta-data into Meta.
read_nodes(FqnExprPath, Pythonpaths, Nodes, Meta) :-
    open(FqnExprPath, read, FqnExprStream),
    my_json_read_dict(FqnExprStream, MetaDict),
    my_json_read_dict(FqnExprStream, JsonDict),
    simplify_meta(MetaDict, Pythonpaths, Meta),
    must_once(
        at_end_of_stream(FqnExprStream)),
    do_if(false,
        dump_term('JSON_DICT', JsonDict)),
    simplify_json(JsonDict, Nodes).

%! simplify_meta(+MetaDictJson:dict, +Pythonpaths:list, -Meta:dict) is det.
%% Simplify the file meta-data. The argument is the Prolog dict form
%% of the first JSON item (see ast_cooked.Meta).
simplify_meta(MetaDictJson, Pythonpaths, Meta) :-
    %% Note that my_json_read_dict/2 sets the tag to 'json'.
    MetaDictJson = json{kind: "Meta",
        slots: json{
            kythe_corpus: json{kind: "str", value: KytheCorpus},
            kythe_root: json{kind: "str", value: KytheRoot},
            path: json{kind: "str", value: Path},
            language: json{kind: "str", value: Language},
            contents_b64: json{kind: "str", value: ContentsB64},
            encoding: json{kind: "str", value: Encoding}}},
    canonical_path(Path, CanonicalPath),
    %% For debugging, might want to use the value "LS0t", derived from:
    %%     base64('---', 'LS0t').
    Meta = meta{
        kythe_corpus: KytheCorpus,
        kythe_root: KytheRoot,
        path: CanonicalPath,
        language: Language,
        encoding: Encoding,
        file_contents_b64: ContentsB64,
        pythonpaths: Pythonpaths}.

%! simplify_json(+Json, -Prolog) is det.
%% Simplify the JSON term into more specific dicts, each one
%% distinguished by its tag. The input dicts for base types (str, int,
%% etc.) are turned into simpler functors.
simplify_json([], []).
simplify_json([V|Vs], Values) :-
    maplist(simplify_json, [V|Vs], Values).
simplify_json(json{kind: "str", value: Value}, str(Value)).
simplify_json(json{kind: "int", value: Value}, int(Value)).
simplify_json(json{kind: "bool", value: Value}, bool(Value)).
simplify_json(json{kind: "None"}, none).  % Shouldn't be generated by pod.PlainOldDataExtended.make_json_dict
simplify_json(json{kind: "dict", items: Items}, Value) :-
    dict_pairs(Items, _, ItemPairs),
    maplist(simplify_json_slot_pair, ItemPairs, ItemPairs2),
    dict_pairs(Value, dict, ItemPairs2).
simplify_json(json{kind: Kind, slots: Slots}, Value) :-
    dict_pairs(Slots, _, SlotPairs),
    maplist(simplify_json_slot_pair, SlotPairs, SlotPairs2),
    atom_string(KindAtom, Kind),
    dict_pairs(Value, KindAtom, SlotPairs2).

%! simplify_json_slot_pair(+KeyValue:pair, -KeyValue2:pair) is det.
simplify_json_slot_pair(Key-Value, Key-Value2) :-
    simplify_json(Value, Value2).

%! process_nodes(+Nodes, +SrcInfo:dict, -KytheFacts:list, -Exprs:list, +Meta:dict) is det.
%% Wrapper for process_nodes//[kyfact, expr, file_meta].
%% TODO: separate KytheFacts into those that require de-duping and
%%       those that can be simply appended, to minimize the final
%%       de-dup.
process_nodes(Node, SrcInfo, KytheFacts, Exprs, Meta) :-
    process_nodes(Node, SrcInfo, KytheFacts1, [], Exprs, [], Meta),  % phrase(process_nodes(Node), KytheFacts, Exprs, Meta)
    % TODO: don't preserve order (for debugging) - use sort/2 to dedup:
    list_to_set(KytheFacts1, KytheFacts).

%! process_nodes(+Nodes)//[kyfact, expr, file_meta] is det.
%% Traverse the Nodes, accumulating in KytheFacts (mostly anchors) and
%% Expr (which will be traversed later, to fill in dynamically created
%% attribtes (e.g., self.foo).
process_nodes(Node, SrcInfo) -->>
    kyfile(SrcInfo),
    kynode(Node, _Expr).

%! kyfile(+SrcInfo)//[kyfact, file_meta] is det.
%% Generate the KytheFacts at the file level.
kyfile(SrcInfo) -->>
    % TODO: output x-numlines, x-html ?
    Meta/file_meta,
    { must_once(Meta.path == SrcInfo.src) },
    { Source = json{path: Meta.path} },
    %% If the following is changed, also change the validation
    %% in parse_and_process_module_cached/4.
    kyfact(Source, '/kythe/node/kind', 'file'),
    kyfact(Source, '/kythe/text/encoding', Meta.encoding),
    kyfact_b64(Source, '/kythe/text', Meta.file_contents_b64),
    kyedge_fqn(Source, '/kythe/edge/childof', SrcInfo.src_fqn),
    %% Kythe's "package" is the equivalent of Python's "module".
    %% (There is no equivalent of Python's "package" ... we just use
    %% ref/imports on the import statements.)
    signature_node_kyfact(SrcInfo.src_fqn, '/kythe/node/kind', 'package').

%! kynode(+Node:json_dict, -Type)//[kyfact, expr, file_meta] is det.
%% Extract anchors (with FQNs) from the the AST nodes.  The anchors go
%% into accumulator 'kyfact' and the expressions (for further
%% processing) go into accumulator 'expr'. The predicate returns a
%% "type", which is used to populate the right-hand-sides of assign/2
%% terms in the 'expr' accumulator.

%% (The "type" is in a list, corresponding to a union of types.)

%% For nodes that can't appear on the right-hand side of an
%% assignment, the "type" is stmt(...) or unused_XXX(...). These
%% values aren't used anywhere; they're simply to help with debugging
%% and will cause an error in assign_expr_eval//2 if they appear on
%% the r.h.s. of an assignment.

%% For descriptions of the various types of Node, and how they relate
%% to the raw AST, see ast_cooked.py.

%% [], [_|_], bool(_), dict(_), int(_), none, str(_), 'Astn'{...}'
%% are all handled by higher-level nodes.
%%   (e.g., 'Astn'{start: int(Start), end: int(End), value: str(Value)}}
%%   in node_astn/4, which is uesd by 'ArgumentNode', 'AtomDotNode', etc.;
%%   str(_) is used by 'Class', 'Func', etc.)

%% assign/2 facts are made up of a left-hand-side (assigned-to) and a
%% right-hand-side (expression. These correspond to the LHS and RHS
%% of an expression, and have a few variants:
%%   assign([a], [b]) corresponds to the statement `a = b`
%%   assign([a], []) corresponds to the definition of a name, e.g. `def foo(a)`
%% expr/1 are like assign/2 but with nothing to assign to (expr([]) is a no-op).

%% See comments at the top of this file on union and single types.

%% The following are handled by the container (e.g., ImportFromStmt):
%%   AsNameNode
%%   NameRawNode  (from DottedNameNode, ImportFromStmt, etc.)
%%   NameNode
kynode('AnnAssignStmt'{left_annotation: LeftAnnotation,
                       expr: Expr,
                       left: Left},
             [stmt(annassign)]) -->>
    %% Corresponds to `expr_stmt: testlist_star_expr annassign`.
    expr_normalized(Expr),
    assign_normalized(Left, LeftAnnotation).
kynode('ArgumentNode'{name: NameAstn, arg: Arg},
       [todo_arg(Name, ArgType)]) -->>
    %% Corresponds to `argument: test '=' test`.  ast_raw creates
    %% ArgumentNode only for `test '=' test`; all other cases just
    %% generate the expr (or similar)
    %% TODO: match Name to func def param
    { node_astn(NameAstn, _, _, Name) },
    kynode(Arg, ArgType).
kynode('AssertStmt'{items: Items},
       [stmt(assert)]) -->>
     % Corresponds to `assert_stmt`.
     maplist_kyfact_expr(expr_normalized, Items).
kynode('AssignExprStmt'{expr: Expr, left: Left},
       [stmt(assign)]) -->>
    assign_normalized(Left, Expr).
kynode('AtomCallNode'{args: Args, atom: Atom},
       [call(AtomType, ArgsType)]) -->>
    kynode(Atom, AtomType),
    maplist_kynode(Args, ArgsType).
kynode('AtomDotNode'{atom: Atom, binds: bool(Binds), attr_name: AttrNameAstn},
       [dot(AtomType, astn(Start, End, AttrName), DotEdgeName)]) -->>
    { dot_edge_name(Binds, DotEdgeName) },
    { node_astn(AttrNameAstn, Start, End, AttrName) },
    kynode(Atom, AtomType).
kynode('AtomSubscriptNode'{atom: Atom,
                           subscripts: Subscripts},
       [todo_subscr(AtomType)]) -->>
    kynode(Atom, AtomType),
    maplist_kynode(Subscripts, _).
kynode('AugAssignStmt'{augassign: _OpAstn, expr: Expr, left: Left},
       [stmt(augassign)]) -->>
    % { node_astn(OpAstn, _, _, _Op) },
    expr_normalized(Left),
    expr_normalized(Expr).
kynode('BreakStmt'{},
       [stmt(break)]) -->> [ ].
kynode('Class'{bases: Bases, fqn: str(Fqn), name: NameAstn},
       [class(FqnAtom, BasesType)]) -->>
    { atom_string(FqnAtom, Fqn) },
    kyanchor_node_kyedge_fqn(NameAstn, '/kythe/edge/defines/binding', FqnAtom),
    signature_node_kyfacts(FqnAtom,
                           ['/kythe/node/kind'-'record',
                            '/kythe/subkind'-'class']),
    maplist_kynode(Bases, BasesType),
    [ class(FqnAtom, BasesType) ]:expr.
kynode('CompFor'{for_astn: _ForAstn,
                 for_exprlist: ForExprlist,
                 in_testlist: InTestlist,
                 comp_iter: CompIter},
       [todo_compfor(iter:CompIterType,
                     for:ForExprlistType,
                     in:InTestlistType)]) -->>
    kynode(ForExprlist, ForExprlistType),
    kynode(InTestlist, InTestlistType),
    kynode(CompIter, CompIterType).
kynode('CompIfCompIterNode'{value_expr: ValueExpr,
                            comp_iter: CompIter},
       [todo_compifcompiter(ValueExprType, CompIterType)]) -->>
    kynode(ValueExpr, ValueExprType),
    kynode(CompIter, CompIterType).
kynode('ContinueStmt'{},
       [stmt(continue)]) -->> [ ].
kynode('DecoratedStmt'{items: Items},
       [todo_decorated(ItemsType)]) -->>
    maplist_kynode(Items, ItemsType).
kynode('DecoratorDottedNameNode'{items: Items},
       [todo_decorator_dottedname(ItemsType)]) -->>
    maplist('NameRawNode_astn_and_name', Items, _, ItemsType).
kynode('DecoratorsNode'{items: Items},
       [todo_decorators(ItemsType)]) -->>
    maplist_kynode(Items, ItemsType).
kynode('DelStmt'{items: Items},
       [stmt(del)]) -->>
    maplist_kyfact_expr(expr_normalized, Items).
kynode('DictGenListSetMakerCompFor'{value_expr: ValueExpr, comp_for: CompFor},
       [todo_dictgen(ValueExprType, CompForType)]) -->>
    kynode(ValueExpr, ValueExprType),
    kynode(CompFor, CompForType).
kynode('DictKeyValue'{items: Items},
       [todo_dictkeyvaluelist(ItemsType)]) -->>
    maplist_kynode(Items, ItemsType).
kynode('DictSetMakerNode'{items: Items},
       [todo_dictset(ItemsType)]) -->>
    maplist_kynode(Items, ItemsType).
kynode('EllipsisNode'{},
       [ellipsis]) -->> [ ].
kynode('ExceptClauseNode'{expr: Expr, as_item: AsItem},
       [stmt(except)]) -->>
    kynode(Expr, ExprType),
    kynode(AsItem, AsItemType),
    (  { AsItemType = omitted }
    -> [ expr(ExprType) ]:expr
    ;  [ assign(AsItemType, ExprType) ]:expr
    ).
kynode('ExprListNode'{items: Items},
       [todo_exprlist(ItemsType)]) -->>
    maplist_kynode(Items, ItemsType).
kynode('ExprStmt'{expr: Expr},
       [stmt(assign)]) -->>
    kynode(Expr, ExprType),
    [ expr(ExprType) ]:expr.
kynode('FileInput'{scope_bindings: _ScopeBindings,
                   stmts: Stmts,
                   path: _Path},
       [stmt(file)]) -->>
    %% kynode(ScopeBindings, _),
    maplist_kynode(Stmts, _).
kynode('ForStmt'{for_exprlist:
                 ForExprlist,
                 in_testlist: InTestlist,
                 suite: Suite,
                 else_suite: ElseSuite},
       [stmt(for)]) -->>
    kynode(ElseSuite, _),  % kynode(ElseSuite, [stmt(_)])
    kynode(ForExprlist, _),
    kynode(InTestlist, _),
    kynode(Suite, _).
kynode('Func'{fqn: str(Fqn),
                    name: NameAstn,
                    parameters: Parameters,
                    return_type: ReturnType},
       [func(FqnAtom, [ReturnTypeType])]) -->>
    { atom_string(FqnAtom, Fqn) },
    kyanchor_node_kyedge_fqn(NameAstn, '/kythe/edge/defines/binding', FqnAtom),
    signature_node_kyfact(FqnAtom, '/kythe/node/kind', 'function'),
    maplist_kynode(Parameters, _),
    kynode(ReturnType, ReturnTypeType),
    [ func(FqnAtom, ReturnTypeType) ]:expr.
kynode('GlobalStmt'{items: Items},
       [stmt(global)]) -->>
    maplist_kyfact_expr(expr_normalized, Items).
kynode('IfStmt'{items: Items},
       [stmt(if)]) -->>
    maplist_kynode(Items, _).
kynode('ImportFromStmt'{from_dots: FromDots,
                        import_part: ImportPart}, Type) -->>
    %% The parser doesn't output a field if it's None, so add
    %% from_name and recurse.
    kynode('ImportFromStmt'{from_dots: FromDots,
                            from_name: 'DottedNameNode'{items:[]},
                            import_part: ImportPart},
           Type).
kynode('ImportFromStmt'{from_dots: FromDots,
                        from_name: FromName,
                        import_part: 'ImportAsNamesNode'{items: ImportPartItems}},
       [unused_importfrom(Types)]) -->>
    maplist_kyfact(kyImportFromStmt(FromDots, FromName), ImportPartItems, Types),
    exprs(Types).
kynode('ImportFromStmt'{from_dots: FromDots,
                        from_name: FromName,
                        import_part: 'StarFqn'{star:StarAstn, fqn:StarFqn}},
       Type) -->>
    %% TODO: process this properly
    ImportPartItems = [
        'AsNameNode'{as_name:'NameBindsFqn'{fqn:StarFqn, name:StarAstn},
                     name:'NameRawNode'{name:StarAstn}}],
    ImportPart = 'ImportAsNamesNode'{items: ImportPartItems},
    kynode('ImportFromStmt'{from_dots: FromDots, from_name: FromName, import_part: ImportPart}, Type).
kynode('ImportNameFqn'{dotted_as_names: 'ImportDottedAsNamesFqn'{items: DottedAsNames}},
       [unused_import(DottedAsNamesType)]) -->>
    maplist_kyfact_expr(kyImportDottedAsNamesFqn, DottedAsNames, DottedAsNamesType).
kynode('ListMakerNode'{items: Items},
       [todo_list(ItemsType)]) -->>
    maplist_kynode(Items, ItemsType).
%% 'NameBindsFqn' is only for 'AssignExprStmt' -- for import statements,
%% it's handled separately.
%% TODO: special case this within processing of AssignExprStmt?  IMPORTANT
kynode('NameBindsFqn'{fqn: str(Fqn), name: NameAstn},
       [fqn(FqnAtom)]) -->>  %% result is same as NameRefFqn
    { atom_string(FqnAtom, Fqn) },
    kyanchor_node_kyedge_fqn(NameAstn, '/kythe/edge/defines/binding', FqnAtom),  %% only difference from NameRef
    signature_node_kyfact(FqnAtom, '/kythe/node/kind', 'variable').
kynode('NameRefFqn'{fqn: str(Fqn), name: NameAstn},
       [fqn(FqnAtom)]) -->>  %% result is same as NameBinds
    { atom_string(FqnAtom, Fqn) },
    kyanchor_node_kyedge_fqn(NameAstn, '/kythe/edge/ref', FqnAtom).  %% only difference from NameBindsFqn
kynode('NameRefGenerated'{fqn: str(Fqn)},
       [fqn(FqnAtom)]) -->>  %% result is same as NameBinds
    { atom_string(FqnAtom, Fqn) }.
kynode('NonLocalStmt'{items: Items},
       [stmt(nonlocal)]) -->>
    maplist_kyfact_expr(expr_normalized, Items).
kynode('NumberNode'{astn: _Astn},
       [class('builtin.Number', [])]) -->> [ ].
kynode('OmittedNode'{},
       [omitted]) -->> [ ].
kynode('OpNode'{args: Args, op_astns: OpAstns},
       [call_op(OpAstns, ArgsType)]) -->>
    maplist_kynode(Args, ArgsType).
kynode('PassStmt'{},
       [stmt(break)]) -->> [ ].
kynode('RaiseStmt'{items: Items},
       [stmt(raise)]) -->>
    maplist_kynode(Items, _).
kynode('StarNode'{},
      [star]) -->> [ ].  % TODO: can we get rid of this in ast_cooked?
kynode('Stmts'{items: Items},
       [todo_expr(stmts)]) -->>
    maplist_kynode(Items, _).
kynode('StringNode'{astns: _Astns},
       [class('builtin.str', [])]) -->> [ ].
kynode('SubscriptNode'{expr1: Expr1, expr2: Expr2, expr3: Expr3},
       [todo_subscr(Expr1Type, Expr2Type, Expr3Type)]) -->>
    kynode(Expr1, Expr1Type),
    kynode(Expr2, Expr2Type),
    kynode(Expr3, Expr3Type).
kynode('TnameNode'{name: Name, type_expr: TypeType},
       [stmt(tname)]) -->>
    assign_normalized(Name, TypeType).
kynode('TryStmt'{items: Items},
       [stmt(try)]) -->>
    maplist_kynode(Items, _).
kynode('TypedArgNode'{tname: 'TnameNode'{name: Name, type_expr: TypeExpr},
                      expr: Expr},
       [todo_typedarg()]) -->>
    assign_normalized(Name, TypeExpr),
    expr_normalized(Expr).  %% assign_normalized(Name, Expr) would cause duplicate facts
kynode('WhileStmt'{else_suite: ElseSuite,
                   suite: Suite,
                   test: Test},
       [stmt(while)]) -->>
    kynode(ElseSuite, _),
    kynode(Suite, _),
    kynode(Test, _).
kynode('WithItemNode'{item: Item, as_item: AsItem},
       [stmt(with_item)]) -->>
    kynode(Item, ItemType),
    kynode(AsItem, AsItemType),
    (  { AsItemType = [omitted] }
    -> [ expr(ItemType) ]:expr
    ;  [ assign(AsItemType, ItemType) ]:expr
    ).
kynode('WithStmt'{items: Items, suite: Suite},
       [stmt(with)]) -->>
    maplist_kynode(Items, _),  % handled by WithItemNode
    kynode(Suite, _).
kynode(X, Ty) -->>  % TODO: remove this "catchall" clause
    { type_error(kynode, [X,Ty]) }.

%% === IMPORT and FROM-IMPORT ===

%% Design decision: No need for ref/file anywhere except for undefined
%% files that we can't resolve - just ref/imports
%%      - the file itself will have
%%          File=vname("", _, _, FileName, "").node/kind file)
%%          Pkg=vname(PkgName, _, _, "", python).node/kind package
%%          File childof Pkg
%% Note that for "from foo.bar import zot as qqsv", the file is
%% foo/bar.py or foo/bar/__init__.py or foo/bar/zot.py but the
%% (ref/imports is always foo.bar.zot (this might be a reference to a
%% module ("package" in Kythe-speak) or an individual item within a
%% module).
%%
%% An import statement generates the following facts:
%%     ref/imports      - corresponds to the file (foo.bar)
%%     defines/binding  - what gets added to the symtab
%% Details are given with each clause (below) and in the test cases.

%! kyImportDottedAsNamesFqn(+DottedName, -DottedAsNamesType)//[kyfact, expr, file_meta] is det.
%% Corresponds to a single item from `dotted_as_names`: "import" and "import ... as ...".
%% The Fqn is either the top-level of the import (e.g., "os" in "import os.path")
%% or the "as" name (e.g., "os_path" in "import os.path as os_path").
%% This code is similar to kyImportFromStmt, but deviates in a few places:
%% - Leading dots (relative imports) are not allowed by the grammar.
%% - "import foo.bar" adds "foo" to the symtab; but "import foo.bar as foo_bar" adds
%%   "foo_bar".
%% * from foo.bar import zot [as qqsv]
%%     @foo  ref/imports      foo
%%     @bar  ref/imports      foo.bar
%%     @zot  ref/imports      foo.bar.zot
%%     @foo  defines/binding  $FQN.foo
%%     @zot  defines/binding  $FQN.zot  [$FQN.qqsv]
%% NOTE: there are some weird corner cases ... for example, "import
%%       os.path" gets a name in /usr/lib/python3.7/os.py that happens
%%       to be a module but alternatively gets
%%       typeshed/stdlib/3/os/path.pyi ... we therefore have to allow
%%       for module_alone and module_and_token everywhere.
kyImportDottedAsNamesFqn('ImportDottedFqn'{
                             dotted_name: 'DottedNameNode'{items: DottedNameItems},
                             top_name: 'NameBindsFqn'{fqn: str(BindsFqn), name: BindsNameAstn}},
                         import_module(BindsFqnAtom, ModuleAndMaybeToken)) -->>
    %% Note that BindsFqn is just the "top name" (e.g., "${FQN}.os" for "os.path")
    %% so we don't need to do anything special for it.
    kyImportDottedAsNamesFqn_comb([], % FromDots
                                  DottedNameItems, BindsFqn, BindsNameAstn,
                                  BindsFqnAtom, ModuleAndMaybeToken).
kyImportDottedAsNamesFqn('ImportDottedAsNameFqn'{
                             dotted_name: 'DottedNameNode'{items:DottedNameItems},
                             as_name: 'NameBindsFqn'{fqn: str(BindsFqn), name: BindsNameAstn}},
                         import_module(BindsFqnAtom, ModuleAndMaybeToken)) -->>
    kyImportDottedAsNamesFqn_comb([],    % FromDots
                                  DottedNameItems, BindsFqn, BindsNameAstn,
                                  BindsFqnAtom, ModuleAndMaybeToken).

%%! kyImportDottedAsNamesFqn_comb(+FromDots, +DottedNameItems:list, +BindsFqn:string, +BindsNameAstn:astn, -BindsFqnAtom:atom, -ModuleAndMaybeToken)//[kyfact, expr, file_meta] is det.
%%  Combined code for ImportDottedFqn, ImportDottedAsNameFqn.
%% FromDots is list of ImportDotNode{dot:ASTN(Start,End,'.')}
%% DottedNameItems is list of NameRawNode{name:ASTN(Start,End,Name)}
%%  Adds anchors and binding facts for an imported FQN.
%%   for "import os.path.sep as os_path_sep" (which is not valid Python, unless
%%   os.path.sep happens to be a module):
%%      FromDots = []   % Always [] for "import"; "from ... import" can have non-[]
%%      DottedNameItems = [NameRawNode{name:ASTN(os)}, NameRawNode{name:ASTN(path){, NameRawNode{name:ASTN(sep)}}]
%%      BindsFqn = "$FQN.os_path_sep"
%%      BindsNameAstn = ASTN(os_path_sep)  % 'os' if there's no "as" part to the import
%%      BindsFqnAtom = '$FQN.os_path-sep'  % '$FQN.os' if there's no "as" part to the import
%%      ModuleAndMaybeToken = module_and_token('$FQN.os.path', '$DIR.os.path.__init__.pyi', 'sep')
%%
%%  TODO: This code has "evolved" and is more complicated than needed;
%%         also has some duplicate checks (see the stuff with
%%         '$PYTHONPATH' and '<unknown>', also tests for FromDots being
%%         [] or [_|_] in full_path/5).
%%  TODO: clean up the use of '<unknown>' etc, probably by creating new functors
%%        that encapsulate information such as '<unknown>' and '$PYTHONPATH'
%%
%% * import foo.bar.bazz [as zot]
%%      @foo  ref/imports      foo
%%      @bar  ref/imports      foo.bar
%%      @bazz ref/imports      foo.bar.bazz
%%      @foo  defines/binding  $FQN.foo  [$FQN.zot -- with different binding]
kyImportDottedAsNamesFqn_comb(FromDots, DottedNameItems, BindsFqn, BindsNameAstn,
                              BindsFqnAtom, ModuleAndMaybeToken) -->>
    Meta/file_meta,
    MetaPath = Meta.path,
    MetaPythonpaths = Meta.pythonpaths,
    { atom_string(BindsFqnAtom, BindsFqn) },
    { kyImportDottedAsNamesFqn_dots(FromDots, DottedNameItems, FromDotAstns, DottedNameAstns, FromImportPath) },
    { full_path(FromDots, FromImportPath, MetaPythonpaths, MetaPath, ModuleAndMaybeToken) },
    { full_module_pieces(ModuleAndMaybeToken, FullModulePieces) },
    (  FullModulePieces = ['<unknown>'|FullModulePieces2]
    -> ImportsEdgeKind = '/kythe/edge/ref/file',
       ImportsSep = '/'
    ;  ImportsEdgeKind = '/kythe/edge/ref/imports',
       ImportsSep = '.',
       FullModulePieces2 = FullModulePieces
    ),
    { append(FromDotAstns, DottedNameAstns, DotNameAstns) },
    { reverse(DotNameAstns, ReversedDotNameAstns) },
    dotted_name_imports(ReversedDotNameAstns, FullModulePieces, ImportsEdgeKind, ImportsSep),
    kyanchor_node_kyedge_fqn_pieces(BindsNameAstn, ImportsEdgeKind, ImportsSep, FullModulePieces2),
    signature_node_kyfact(BindsFqnAtom, '/kythe/node/kind', 'variable'),
    kyanchor_node_kyedge_fqn(BindsNameAstn, '/kythe/edge/defines/binding', BindsFqnAtom),
    [ import_module(BindsFqnAtom, ModuleAndMaybeToken) ]:expr.

%% FromDots is only used to determine how things are processed -- the
%% needed information is in FromImportPath
%% - "from .. import i5" has FromImportPath='../i5' and FromDots=[_|_]
%% - "from os.path import sep" has FromImportPath='$PYTHONPATH/os/path/sep' and FromDots=[]
full_path([], Path, Prefixes, _CurrModulePath, ModuleAndMaybeToken) :-
    must_once(pythonpath_prefix(Path, DeprefixedPath)),
    full_path_prefixed(Path, DeprefixedPath, Prefixes, Module, ModuleAndMaybeToken),
    path_part_to_python_module_or_unknown(ModuleAndMaybeToken, Module).
full_path([_|_], Path, _Prefixes, CurrModulePath, ModuleAndMaybeToken) :-
    directory_file_path(CurrModulePathDir, _, CurrModulePath),
    atomic_list_concat([CurrModulePathDir, Path], '/', FromImportPath2),
    (  path_expand(FromImportPath2, Module, ModuleAndMaybeToken)
    -> path_part_to_python_module_or_unknown(ModuleAndMaybeToken, Module)
    ;  %% File doesn't exist, e.g. '/home/fred/foo/bar/src/../../xyz',
       %% path_to_python_module_or_unknown/2 would give
       %% '<unknown>.home.fred.foo.bar.src.......xyz'.
       absolute_file_name(FromImportPath2, AbsFromImportPath2),
       format(atom(Module), '<unknown>.{~w}', [AbsFromImportPath2]),
       ModuleAndMaybeToken = module_alone(Module, FromImportPath2)
    ).

%! path_part_to_python_module_or_unknown(+ModuleAndMaybeToken, -Module) is det.
%% Convenience predicate maping ModuleAndMaybeToken to Module.
path_part_to_python_module_or_unknown(ModuleAndMaybeToken, Module) :-
    path_part(ModuleAndMaybeToken, ResolvedPath),
    path_to_python_module_or_unknown(ResolvedPath, Module).

%! dotted_name_imports(+ReversedDotsAndNames:list, +ModulePieces:list, +ImportsEdgeKind:atom, +ImportsSep:atom)//kyfact, file_meta] is det.
%% Note: in the case of an invalid path (no file), it's possible to
%%       have more dots than module pieces (hence, the 2nd clause).
dotted_name_imports([], _, _, _) -->> !, [ ].
dotted_name_imports(_, [], _, _) -->> !, [ ].
dotted_name_imports([astn(Start, End, _)|ReversedDotsAndNames], ModulePieces, ImportsEdgeKind, ImportsSep) -->>
    kyanchor_kyedge_fqn_pieces(Start, End, ImportsEdgeKind, ImportsSep, ModulePieces),
    { append(ModulePieces2, [_], ModulePieces) }, % Remove last item
    dotted_name_imports(ReversedDotsAndNames, ModulePieces2, ImportsEdgeKind, ImportsSep).

%! kyImportDottedAsNamesFqn_dots(+FromDots:list, +DottedNameItems:list, -FromDotAstns:list, -DottedNameAstns:list, -FromImportPath:atom) is det.
%% If FromDots is not [], FromImportPath will have leading "../"s.
%% FromDots is list of ImportDotNode{dot:ASTN(Start,End,'.')}
%% DottedNameItems is list of NameRawNode{name:ASTN(Start,End,Name)}
%% DottedNameAstns is list of ASTNS from DottedNameItems
%% FromImportPath is atom of path with (maybe) leading dots.
kyImportDottedAsNamesFqn_dots(FromDots, DottedNameItems, FromDotAstns, DottedNameAstns, FromImportPath) :-
    maplist(kyImportDotNode, FromDots, FromDotAstns, Dots),
    maplist(kyNameRawNode, DottedNameItems, DottedNameAstns, Names),
    kyImportDottedAsNamesFqn_dots2(Dots, Names, FromPathParts),
    atomic_list_concat(FromPathParts, '/', FromImportPath).

%! kyImportDottedAsNamesFqn_dots2(+Dots:list, +Names:list, -FromPathParts:list) is det.
kyImportDottedAsNamesFqn_dots2([], Names, ['$PYTHONPATH'|Names]).
kyImportDottedAsNamesFqn_dots2([_|UpDots], Names, DotNames) :-
    maplist(double_dot, UpDots, UpDoubleDots),
    append(UpDoubleDots, Names, DotNames).

%! double_dot(SingleDot, DoubleDot) is det
%% For mapping single dots (Python) to double dots (POSIX file)
double_dot('.', '..').

%! kyImportFromStmt(+FromDots:list, +FromName, +AsNameNode, +ImportPart)//[kyfact, expr, file_meta] is det.
%% Corresponds to a single item of `import_from`: "from ... import ..."
%% TODO: (excluding "from ... import *", to be handled by kyImportFromStmt_star).
%%
%% This predicate called from a maplist over the import parts, which
%% means that the FromDots and FromName will be re-processed for each
%% item. This is slightly inefficient but it simplifies things because
%% the processing of FromDots and FromName depends on the import part
%% (in "from foo import bar", we could be processing foo/bar.py or
%% foo.py and the token "bar").
%% - FromDots is zero or more  ImportDotNode's.
%% - FromName is none or DottedNameNode.
%% If there are no ImportDotNode's, then the result is
%% $PYTHONPATH/Path/To/From/Pat/ImportPart.
%% If there are ImportDotNode's, then the result is
%% FilePath/ImportPart, where FilePath is derived from the Meta
%% information for the file, followed by '/..' as needed.
kyImportFromStmt(FromDots,
                 'DottedNameNode'{items:DottedNameItems},
                 'AsNameNode'{name:RawNameAstn,  % 'NameRawNode'{name:NameAstn},
                              as_name:'NameBindsFqn'{
                                          fqn:str(BindsFqn),
                                          name:AsNameAstn}},
                 import_module(BindsFqnAtom, ModuleAndMaybeToken)) -->>
    { append(DottedNameItems, [RawNameAstn], DottedNameItemsComb) },
    kyImportDottedAsNamesFqn_comb(FromDots, DottedNameItemsComb, BindsFqn, AsNameAstn,
                                  BindsFqnAtom, ModuleAndMaybeToken).

%! 'NameRawNode_astn_and_name'(+DottedNameItem, -DottedName) is det.
%% Process a NameRawNode node into a name
'NameRawNode_astn_and_name'('NameRawNode'{name: NameAstn}, NameAstn, Name) :-
    node_astn(NameAstn, _, _, Name).

%! full_path_prefixed(+Path, +DeprefixedPath, +Prefixes:list, ?Module, -ModuleAndMaybeToken) is det.
%% ModuleAndMaybeToken is either module_alone or module_and_token functor.
%% Module is logical variable that gets filled in later
full_path_prefixed(Path, DeprefixedPath, Prefixes, Module, ModuleAndMaybeToken) :-
    (  member(Prefix, Prefixes),
       atom_concat(Prefix, DeprefixedPath, Path0),
       path_expand(Path0, Module, ModuleAndMaybeToken)
    -> true
    ;  Prefix = '$PYTHONPATH/',
       %% Can't path_expand because we don't know what prefix to use.
       ModuleAndMaybeToken = module_alone(Module, Path)
    ),
    path_part(ModuleAndMaybeToken, PathPart),
    must_once(atom_concat(Prefix, _, PathPart)), % Prefix must be first part of PathPart
    must_once(atom_concat(_, '/', Prefix)). % Prefix must end with '/'.

%! path_expand(+Path0, +Module, -ModuleAndMaybeToken) is semidet.
%% ModuleAndMaybeToken is either module_alone or module_and_token functor.
%% Module is logical variable that gets filled in later
path_expand(Path0, Module, ModuleAndMaybeToken) :-
    (  Path1 = Path0,
       ModuleAndMaybeToken = module_alone(Module, Expanded)
    ;  remove_last_component(Path0, Path1, Token),
       ModuleAndMaybeToken = module_and_token(Module, Expanded, Token)
    ),
    py_ext(Path1, Path),
    canonical_path(Path, Expanded).

%! remove_last_component(+Path, -FirstPart, -Tail) is det.
%% e.g.: 'foo/bar/zot', 'foo/bar', zot
remove_last_component(Path, FirstPart, Tail) :-
    (  split_atom(Path, '/', '', Split),
       Split = [_,_|_],  % at least two components
       append(FirstPathPieces, [Tail], Split)
    -> atomic_list_concat(FirstPathPieces, '/', FirstPart)
    ).

%! pythonpath_prefix(+Full: atom, -Rest: atom) is semidet.
%! pythonpath_prefix(-Full: atom, +Rest: atom) is semidet.
pythonpath_prefix(Full, Rest) :-
    atom_concat('$PYTHONPATH/', Rest, Full).

%! py_ext(+Path:atom, -PathBase:atom is nondet.
%! py_ext(-Path:atom, +PathBase:atom is nondet.
%% Path unifies with all permutations of PathBase plus {.py,.pyi} and
%%  __init__ equivalents and does not check for existence.
py_ext(PathBase, Path) :-
    % TODO: verify order of testing which file(s) exists.
    % TODO: allow more than one "hit" (e.g., if there are both a .py and .pyi,
    %       then use the .pyi to get type info for the .py and possibly create
    %       kyanchors in both)
    py_ext_ext(Ext),
    atom_concat(PathBase, Ext, Path),
    % for 'foo/__init__.py', only return 'foo' and not 'foo/__init__':
    \+ atom_concat(_, '__init__', PathBase).

%! py_ext_ext(-Extension:atom) is nondet.
%% "extensions" to append to a module to get a file
%% file_name_extension/3 adds a '.', so can't use for /__init__.*
%% TODO: allow more than .py and .pyi as extensions?
py_ext_ext('.py').
py_ext_ext('.pyi').
py_ext_ext('/__init__.py').
py_ext_ext('/__init__.pyi').

%! exprs(+Exprs)//[expr] is det.
%% Adds the Exprs to the "expr" accumulator.
exprs([]) -->> [ ].
exprs([E|Es]) -->>
     [ E ]:expr,
     exprs(Es).

%! kyNameRawNode(+Node, -Astn, -Name:atom) is det.
%  Used by DottedNameNode to process a list of NameRawNode into a list of atoms.
% TODO: needs some file resolution
kyNameRawNode('NameRawNode'{name: NameAstn}, astn(Start, End, Name), NameAtom) :-
    node_astn(NameAstn, Start, End, Name),
    atom_string(NameAtom, Name).

%! kyImportDotNode(+Node, -Astn, -Name:atom) is det.
kyImportDotNode('ImportDotNode'{dot:DotAstn}, astn(Start, End, Dot), DotAtom) :-
    node_astn(DotAstn, Start, End, Dot),
    atom_string(DotAtom, Dot).

%! maplist_kynode(+Nodes:list, -NodeTypes:list)//[kyfact, expr, file_meta] is det.
%% maplist_kyfact_expr(kynode, Nodes, NodeTypes)
%% TODO: for some reason this fails when maplist meta-predicate is used
%%       (maybe due to handling of _? in a meta-call?)
maplist_kynode([], []) -->> [ ].
maplist_kynode([Node|Nodes], [NodeTypeWrap|NodeTypes]) -->>
    kynode(Node, NodeTypeWrap),
    maplist_kynode(Nodes, NodeTypes).

%! assign_normalized(+Left, +Right)//[kyfact, expr, file_meta] is det.
%% Process the Left and Right parts of an assign/2 term, handling
%% things like `omitted` and `ellipsis`.
assign_normalized(Left, Right) -->>
    kynode(Left, LeftType),
    kynode(Right, RightType),
    (  { LeftType = [omitted] }
    -> [ ]
    ;  { RightType = [omitted] ; RightType = [ellipsis] }
    -> [ assign(LeftType, []) ]:expr  % TODO: Right is left uninstantiated
    ;  [ assign(LeftType, RightType) ]:expr
    ).

%! expr_normalized(+Right)//[kyfact, expr, file_meta] is det.
%% Process the Right parts of an expr/1 term, handling
%% things like `omitted` and `ellipsis`.
expr_normalized(Right) -->>
    kynode(Right, [RightType]),
    (  { RightType = omitted ; RightType = ellipsis }
    -> [ ]
    ;  [ expr([RightType]) ]:expr
    ).

%! node_astn(+AstnNode, -Start, -End, -Value) is semidet.
%! node_astn(-AstnNode, +Start, +End, +Value) is det.
%%  Access the inner parts of an Astn node and ensure
%%  Value is an atom.
node_astn(Astn, Start, End, ValueAtom) :-
    node_astn0(Astn, Start, End, Value),
    atom_string(ValueAtom, Value).

%! node_astn0(+AstnNode, -Start, -End, -Value) is semidet.
%! node_astn0(-AstnNode, +Start, +End, +Value) is det.
%% Access the inner parts of an Astn node.
%% See also portray/1 rule for 'Astn' (uses node_astn0/4).
node_astn0('Astn'{start: int(Start), end: int(End), value: str(Value)},
           Start, End, Value).

%! dot_edge_name(+TrueFalse:string, -KytheEdge:atom) is det.
%% Translate True/False to Kythe ref or binding edge type
dot_edge_name("False", '/kythe/edge/ref').
dot_edge_name("True", '/kythe/edge/defines/binding').

%! kyanchor_kyedge_fqn(+Start:int, +End:int, +EdgeKind:atom, +Fqn:atom)//kyfact, file_meta is det.
kyanchor_kyedge_fqn(Start, End, EdgeKind, Fqn) -->>
    kyanchor(Start, End, Source),
    kyedge_fqn(Source, EdgeKind, Fqn).

%! kyanchor_kyedge_fqn(+Start:int, +End:int, +EdgeKind:atom, +Sep:atom, +FqnPices:list)//kyfact, file_meta is det.
kyanchor_kyedge_fqn_pieces(Start, End, EdgeKind, Sep, FqnPieces) -->>
    { atomic_list_concat(FqnPieces, Sep, Fqn) },
    kyanchor_kyedge_fqn(Start, End, EdgeKind, Fqn).

%! kyanchor_node_kyedge_fqn(+Astn, +EdgeKind:atom, +Fqn:atom, -Token)//[kyfact, file_meta] is det.
kyanchor_node_kyedge_fqn(Astn, EdgeKind, Fqn, Token) -->>
    kyanchor_node(Astn, Source, Token),
    kyedge_fqn(Source, EdgeKind, Fqn).

%! kyanchor_node_kyedge_fqn(+Astn, +EdgeKind:atom, +Fqn:atom)//[kyfact, file_meta] is det.
kyanchor_node_kyedge_fqn(Astn, EdgeKind, Fqn) -->>
    kyanchor_node(Astn, Source),
    kyedge_fqn(Source, EdgeKind, Fqn).

%! kyanchor_node_kyedge_fqn_pieces(+Astn, +EdgeKind:atom, +Sep:atom +FqnPieces:atom)//[kyfact, file_meta] is det.
kyanchor_node_kyedge_fqn_pieces(Astn, EdgeKind, Sep, FqnPieces) -->>
    { atomic_list_concat(FqnPieces, Sep, Fqn)},
    kyanchor_node_kyedge_fqn(Astn, EdgeKind, Fqn).

%! kyanchor_node(+Astn, -Source)/[kyfact, file_meta] is det.
kyanchor_node(Astn, Source) -->>
    kyanchor_node(Astn, Source, _Token).

%! kyanchor_node(+Astn, -Source, -Token)/[kyfact, file_meta] is det.
kyanchor_node(Astn, Source, Token) -->>
    { node_astn(Astn, Start, End, Token) },
    kyanchor(Start, End, Source).

%! kyanchor(+Start, +End, -Source)//[kyfact, file_meta] is det.
%% Create the Kythe facts for an anchor. Source gets the source signature.
kyanchor(Start, End, Source) -->>
    { format(string(Signature), '@~d:~d', [Start, End]) },
    signature_source(Signature, Source),
    kyfact(Source, '/kythe/node/kind', 'anchor'),
    kyfact(Source, '/kythe/loc/start', Start),
    kyfact(Source, '/kythe/loc/end', End).

%! kyedge_fqn(+Source, +EdgeKind:atom, +Fqn:atom)//[kyfact, file_meta] is det.
%% High-level create a Kythe edge fact to a target identified by an FQN.
kyedge_fqn(Source, EdgeKind, Fqn) -->>
    signature_node(Fqn, Target),
    kyedge(Source, EdgeKind, Target).

%! kyedge(+Source, +EdgeKind:atom, +Target:atom)//{kyfact, file_meta] is det.
%% Low-level create a Kythe edge fact -- for both Source and Target,
%% corpus and root are filled in from file_meta.
kyedge(Source, EdgeKind, Target) -->>
    Meta/file_meta,
    [ json{source: Source.put(corpus, Meta.kythe_corpus).put(root, Meta.kythe_root),
           edge_kind: EdgeKind,
           target: Target.put(corpus, Meta.kythe_corpus).put(root, Meta.kythe_root),
           fact_name: '/'} ]:kyfact.

%! kyfacts(+Vname, FactValues:list)//[kyfact, file_meta] is det.
%% kyfact over a list of FactName-FactValue
kyfacts(_Vname, []) -->> [ ].
kyfacts(Vname, [FactName-FactValue|FactValues]) -->>
    kyfact(Vname, FactName, FactValue),
    kyfacts(Vname, FactValues).

%! kyfact(+Source, +FactName, +FactValue)//[kyfact, file_meta] is det.
%% Low-level create a Kythe fact or edge -- for Source, corpus and root
%% are filled in from file_meta.
kyfact(Source, FactName, FactValue) -->>
    { base64(FactValue, FactBase64) },
    kyfact_b64(Source, FactName, FactBase64).

%! kyfact_64(+Source, +FactName, +FactBase64)//[kyfact, file_meta] is det.
%% Low-level create a Kythe fact or edge inputting the base64 of the
%% fact value -- for Source, corpus and root are filled in from file_meta.
%% The accumulator takes care of duplicate removal.
kyfact_b64(Source, FactName, FactBase64) -->>
    Meta/file_meta,
    { put_dict([corpus=Meta.kythe_corpus, root=Meta.kythe_root],
               Source, Source2) },
    [ json{source: Source2, fact_name: FactName, fact_value: FactBase64} ]:kyfact.

%! signature_source(+Signature:string, -Source)//[file_meta] is det.
%% Create a Kythe "source" tuple from a Signature string.
signature_source(Signature, Source) -->>
    Meta/file_meta,
    { Source = json{signature: Signature, path: Meta.path} }.

%! signature_node_kyfact(+Signature:string, +FactName, +FactValue)//[kyfact, file_meta is det.
signature_node_kyfact(Signature, FactName, FactValue) -->>
    signature_node(Signature, Vname),
    kyfact(Vname, FactName, FactValue).

%! signature_node_kyfacts(+Signature:string, +FactValues:list)//[kyfact, file_meta] is det.
signature_node_kyfacts(Signature, FactValues) -->>
    signature_node(Signature, Vname),
    kyfacts(Vname, FactValues).

%! signature_node(+Signature:string, -Vname)//[file_meta] is det.
%% Create a Kythe "vname" from a Signature string
signature_node(Signature, Vname) -->>
    Meta/file_meta,
    { Vname = json{signature: Signature, language: Meta.language} }.

%! output_kyfact(+KytheStream:stream, +AnchorAsDict:json_dict) is det.
%% Output a single Kythe fact.
output_kyfact(KytheStream, AnchorAsDict) :-
    %% The tags are ignored unless option tag(type) is specified
    %% (which it isn't). All dicts should have the tag 'json', for
    %% simplicity.
    json_write_dict(KytheStream, AnchorAsDict, [width(0)]),
    nl(KytheStream).

%%%%%%        %%%%%%%
%%%%%% Pass 2 %%%%%%%
%%%%%%        %%%%%%%

%! assign_exprs(+Exprs:list, +Meta: dict, +ModuleFqn:atom, -Symtab:dict, -KytheFacts:list, +Modules0, -Modules) is det.
%% Process a list of Exprs, generating a Symtab and list of KytheFacts.
assign_exprs(Exprs, Meta, ModuleFqn, Symtab, KytheFacts, Modules0, Modules) :-
    initial_symtab(Symtab0),
    put_dict(ModuleFqn, Symtab0, [module(ModuleFqn, Meta.path)], Symtab1),
    assign_exprs_count(1, Exprs, Meta, Symtab1, Symtab, KytheFacts, Modules0, Modules).

%! assign_exprs(+Count, +Exprs:list, +Meta:dict, +Symtab0:dict, -Symtab:dict, -KytheFacts:list, +Modules0, -Modules) is det.
%% Process a list of Exprs, generating a Symtab and list of KytheFacts.
%% Count tracks the number of passes over Exprs; if too large, the
%% processing stops.
%% TODO: Improved output when too many passes are needed.
%% TODO: Parameterize max number of passes.
assign_exprs_count(Count, Exprs, Meta, Symtab0, Symtab, KytheFacts, Modules0, Modules) :-
    do_if(false,  % TODO: delete
          format(user_error, '% === EXPRS === ~q~n~n', [Count])),
    assign_exprs_count_impl(Exprs, Meta, Symtab0, Symtab1, Rej, KytheFacts1, Modules0, Modules1), % phrase(assign_exprs_count(...))
    length(Rej, RejLen),
    do_if(RejLen > 0,
          format(user_error, 'Pass ~q (rej=~q) for ~q~n', [Count, RejLen, Meta.path])),
    CountIncr is Count + 1,
    (  (Rej = [] ; CountIncr > 5)  % TODO: parameterize.
    -> Symtab = Symtab1,
       KytheFacts = KytheFacts1,
       Modules = Modules1
    ;  assign_exprs_count(CountIncr, Exprs, Meta, Symtab1, Symtab, KytheFacts, Modules1, Modules)
    ).

%! assign_exprs_count_impl(+Exprs, +Meta:dict, +Symtab0:dict, -SymtabWithRej:dict, -Rej:dict, -KytheFacts, +Modules0, -Modules) :-
%% Helper for assign_exprs_count, which does the actual processing.
assign_exprs_count_impl(Exprs, Meta, Symtab0, SymtabWithRej, Rej, KytheFacts, Modules0, Modules) :-
    dict_pairs(Symtab0, symtab, SymtabPairs0),
    convlist(expr_from_symtab, SymtabPairs0, ExprsFromSymtab1),
    sort(ExprsFromSymtab1, ExprsFromSymtab),  % remove dups
    append(ExprsFromSymtab, Exprs, ExprsCombined),  % TODO: difference list
    maplist_assign_expr_eval(ExprsCombined, KytheFacts1, [], sym_rej_mod(Symtab0,[],Modules0), sym_rej_mod(SymtabAfterEval,Rej,Modules), Meta),  % phrase(assign_exprs_eval_list(...))
    list_to_set(KytheFacts1, KytheFacts),
    do_if(false,
          dump_term('REJ', Rej)),
    foldl(add_rej_to_symtab, Rej, SymtabAfterEval, SymtabWithRej).

%! maplist_assign_exprs_eval_list(+Assign:list)//[kyfact, symrej, file_meta] is det.
%% Process a list of assign or eval nodes.
maplist_assign_expr_eval([]) -->> [ ].
maplist_assign_expr_eval([Assign|Assigns]) -->>
    SymtabRej/symrej,  % TODO: delete (it's only used for debug logging)
    { do_if(false,
            dump_term('', SymtabRej)) },
    { do_if(false,
            dump_term('', Assign, [indent_arguments(auto), right_margin(60)])) },
    assign_expr_eval(Assign),
    maplist_assign_expr_eval(Assigns).

%! assign_expr_eval(+Node)//[kyfact, symrej, file_meta] is det.
%% Process a single assign/2 or expr/1 node.
assign_expr_eval(assign(Left, Right)) -->>
    eval_union_type_and_lookup(Right, RightEval),
    { must_once(Left = [LeftSingle]) },  % TODO: Can Left by a union with more than one type?
    eval_single_type(LeftSingle, LeftEval),  % if so - eval_union_type([Left]
    (  { LeftEval = [LeftEvalSingle] }
    -> eval_lookup_single(LeftEvalSingle, RightEval)
    ;  [ ]
    ).
assign_expr_eval(expr(Right)) -->>
    %% TODO: do we need _and_lookup (for processing anchors)?
    eval_union_type_and_lookup(Right, _RightEval).
assign_expr_eval(class(Fqn, Bases)) -->>
    [ Fqn-[class(Fqn, Bases)] ]:symrej.
assign_expr_eval(func(Fqn, ReturnType)) -->>
    [ Fqn-[func(Fqn, ReturnType)] ]:symrej.
assign_expr_eval(import_module(Fqn, ModuleAndMaybeToken)) -->>
    { do_if(false, dump_term('assign_expr-IMPORT_MODULE', [Fqn, ModuleAndMaybeToken])) },  % TODO: DELETE
    [ Fqn-[import_module(Fqn, ModuleAndMaybeToken)] ]:symrej.
assign_expr_eval(Expr) -->>  % TODO: remove this "catchall" clause
    { type_error(assign_expr_eval, Expr) }.

%! eval_union_type(+Type:ordset, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
%% Evaluate a Type, generating a new (union) EvalType.
eval_union_type(Type, EvalType) -->>
    { ord_empty(EvalType0) },
    maplist_foldl_eval_union_type(Type, EvalType0, EvalType).

%! eval_union_type(+Type:ordset, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
%% Evaluate a Type, generating a new (union) EvalType, using an explicit
%% accumulator (UnionSoFar).
maplist_foldl_eval_union_type([], UnionSofar, UnionSofar) -->> [ ].
maplist_foldl_eval_union_type([T|Ts], UnionSoFar, EvalTypes) -->>
    eval_single_type_and_lookup(T, ET),
    { ord_union(UnionSoFar, ET, UnionSoFar2) },
    maplist_foldl_eval_union_type(Ts, UnionSoFar2, EvalTypes).

%! eval_union_type_and_lookup(+Expr, -UnionEvalType)//[kyfact, symrej, file_meta] is det.
%% Evaluate (union) Expr and look it up in the symtab.
eval_union_type_and_lookup(Expr, UnionEvalType) -->>
    eval_union_type(Expr, UnionEvalType0),
    eval_lookup(UnionEvalType0, UnionEvalType).

%! eval_single_type_and_lookup(+Expr, -UnionEvalType)//[kyfact, symrej, file_meta] is det.
%% Evaluate (non-union) Expr and look it up in the symtab.
eval_single_type_and_lookup(Expr, UnionEvalType) -->>
    eval_single_type(Expr, UnionEvalType0),
    eval_lookup(UnionEvalType0, UnionEvalType).

%! eval_lookup(+UnionType, -UnionEvalType)//[kyfact, symrej, file_meta] is det.
%% Look up an evaluated union type, generating a union UnionEvalType.
%% TODO: handle [string], [number], etc.
%%       (this is a nice-to-do, for when we add more support for
%%       Kythe's type annotations; but for now, we really only need
%%       lookups for functions (calls) and classes/imports (','
%%       operation))
eval_lookup(UnionType, UnionEvalType) -->>
    { ord_empty(UnionEvalType0) },
    maplist_foldl_eval_lookup(UnionType, UnionEvalType0, UnionEvalType).

%! maplist_foldl_eval_lookup(+Types:ordset, +UnionEvalType0:ordset, -UnionEvalType:ordset)//[kyfact, symrej, file_meta] is det.
maplist_foldl_eval_lookup([], UnionEvalType, UnionEvalType) -->> [ ].
maplist_foldl_eval_lookup([X|Xs], UnionEvalType0, UnionEvalType) -->>
    eval_lookup_single(X, Y),
    { ord_union(UnionEvalType0, Y, UnionEvalType1) },
    maplist_foldl_eval_lookup(Xs, UnionEvalType1, UnionEvalType).

%! eval_lookup_single(+Type, -UnionEvalType:ordset) -->> [kyfact, symrej, file_meta] is det.
eval_lookup_single(fqn(Fqn), UnionEvalType) -->> !,
    [ Fqn-UnionEvalType ]:symrej.
eval_lookup_single(class(ClassName, Bases0),
                   [class(ClassName, Bases)]) -->> !,
    maplist_kyfact_symrej(eval_union_type_and_lookup, Bases0, Bases).
eval_lookup_single(func(FuncName, ReturnType0),
                   [func(FuncName, ReturnType)]) -->> !,
    eval_lookup(ReturnType0, ReturnType).
eval_lookup_single(import_module(Fqn, ModuleAndMaybeToken),
                   [import_module(Fqn, ModuleAndMaybeToken)]) -->> !,
    [ ].
eval_lookup_single(var(Fqn),
                   [var(Fqn)]) -->> !, [ ].
eval_lookup_single(_EvalType, []) -->> [ ].

%! eval_single_type(+Type, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
eval_single_type(fqn(Fqn), [fqn(Fqn)]) -->> !, [ ].
eval_single_type(dot(Atom, Astn, DotEdgeName), EvalType) -->> !,
    eval_union_type_and_lookup(Atom, AtomEval),
    %% TODO: MRO for class -- watch out for Bases containing Unions!
    eval_atom_dot_union(AtomEval, Astn, DotEdgeName, EvalType).
eval_single_type(call(Atom, Parms), EvalType) -->> !,
    eval_union_type_and_lookup(Atom, AtomEval),
    maplist_kyfact_symrej(eval_union_type, Parms, ParmsEval),
    eval_atom_call_union(ParmsEval, AtomEval, EvalType).
eval_single_type(call_op(OpAstns, ArgsType), [call_op(OpAstns, ArgsTypeEval)]) -->> !,
    maplist_kyfact_symrej(eval_union_type, ArgsType, ArgsTypeEval).
eval_single_type(class(Name, Bases), [class(Name, BasesEval)]) -->> !,
    maplist_kyfact_symrej(eval_union_type, Bases, BasesEval).
eval_single_type(import_module(Fqn, ModuleAndMaybeToken), [import_module(Fqn, ModuleAndMaybeToken)]) -->> !,
    { do_if(false, dump_term('eval-IMPORT', [fqn=Fqn, module=ModuleAndMaybeToken])) },  % TODO: DELETE
    [ ].  % TODO: look-up
eval_single_type(func(Name, ReturnType), [func(Name, ReturnTypeEval)]) -->> !,
    eval_union_type_and_lookup(ReturnType, ReturnTypeEval).
eval_single_type(ellipsis, []) -->> !, [ ].
eval_single_type(module(Fqn, Path), [module(Fqn, Path)]) -->> !, [ ].
eval_single_type(omitted, []) -->> !, [ ].

%% TODO: implement the following:
eval_single_type(todo_compfor(iter:_CompIterType, for:_ForExprlistType, in:_InTestlistType), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_compifcompiter(_ValueExprType, _CompIterType), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_decorated(_ItemsType), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_decorator_dottedname(_ItemsType), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_decorators(_ItemsType), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_dictgen(_ValueExprType, _CompForType), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_dictkeyvaluelist(_ItemsType), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_dictset(_ItemsType), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_dottedname(_ItemsType), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_expr(stmts), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_typedarg(), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_subscr(_), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_arg(_, _), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_list(_), []) -->> !, [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_exprlist(_), []) -->> !, [ ].  % [kyfact, symrej, file_meta]

eval_single_type(X, Y) -->>  % TODO: remove this "catchall" clause and the cuts above
    { type_error(eval_single_type, [X, Y]) }.

eval_import_path_module(module_alone(Module), Module).
eval_import_path_module(module_and_token(Module,_Path,_Token), Module).
eval_import_path_module(module_star(Module), Module).

%! path_part(+ModuleAndMaybeToken, -Path) is det.
%% Extract Path from ModuleAndMaybeToken.
path_part(module_alone(_Module,Path), Path).
path_part(module_and_token(_Module,Path,_Token), Path).
path_part(module_star(_Module,Path), Path).

%! full_module_pieces(+ModuleAndMaybeToken, -ModulePieces:list(atom)) is det.
%% Extract Module (as a list of pieces) from ModuleAndMaybeToken.
full_module_pieces(module_alone(Module,_Path), ModulePieces) :-
    split_module_atom(Module, ModulePieces).
full_module_pieces(module_and_token(Module,_Path,Token), ModulePieces) :-
    split_module_atom(Module, ModulePieces0),
    append(ModulePieces0, [Token], ModulePieces).
full_module_pieces(module_star(Module,_Path), ModulePieces) :-
    split_module_atom(Module, ModulePieces0),
    append(ModulePieces0, ['*'], ModulePieces). % TODO: need something better here

%! split_module_atom(-Module:atom, +ModulePieces:list(atom) is det.
%% Split module into pieces on '.', with special handling for '<unknown>'
split_module_atom(Module, ModulePieces) :-
    (  re_matchsub('<unknown>\\.{([^}]*)}$', Module, Sub, [anchored(true)])
    -> %% filename is in absolute form, so the first part of the result
       %% is ''. If we want to get rid of that, then the following should have
       %% [''|ModulePieces0] as the last arg:
       split_atom(Sub.1, '/', '', ModulePieces0),
       ModulePieces = ['<unknown>'|ModulePieces0]
    ;  re_matchsub('(<unknown>\\.{[^}]*})\\.(.*)$', Module, Sub, [anchored(true)])
    -> %% TODO: does this situation every arise?
       split_atom(Sub.2, '.', '', ModulePieces2),
       ModulePieces = [Sub.1|ModulePieces2]
    ;  split_atom(Module, '.', '', ModulePieces)
    ).

%! eval_atom_dot_union(+AtomEval:ordset, +Astn, +DotEdgeName:atom, -EvalType:ordset)//[kyfact, symrej, file_meta]
%% Helper for eval(dot(Atom, Astn, DotEdgeName)), which loops over the
%% individual types in the (union) AtomEval and creates a union type
%% of all the possibilities.
eval_atom_dot_union(AtomEval, Astn, DotEdgeName, EvalType) -->>
    { ord_empty(EvalType0) },
    maplist_foldl_kyfact_symrej(
        eval_atom_dot_union_of_type(Astn, DotEdgeName), AtomEval, EvalType0, EvalType).

eval_atom_dot_union_of_type(Astn, DotEdgeName, T, EvalType0, EvalType) -->>
    eval_single_type(T, ET0),
    maplist_foldl_kyfact_symrej(
            eval_atom_dot_single(Astn, DotEdgeName), ET0, EvalType0, EvalType).

%! eval_atom_dot_single(+Astn, +DotEdgeName:atom, +Type, +EvalType0:ordset, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
%% Process a single type-dot-attr, adding to EvalType
%% TODO: also allow func(...).attr (currently only allows class(...).attr
eval_atom_dot_single(astn(Start, End, Attr), DotEdgeName, class(ClassName, _), EvalType0, EvalType) -->> !,
    { atomic_list_concat([ClassName, '.', Attr], FqnAttr) },
    { ord_add_element(EvalType0, fqn(FqnAttr), EvalType) },
    kyanchor_kyedge_fqn(Start, End, DotEdgeName, FqnAttr).
eval_atom_dot_single(astn(Start, End, Attr), DotEdgeName, import_module(Fqn, module_alone(Module,Path)), EvalType0, EvalType) -->> !,
    { do_if(false, dump_term('dot-IMPORT_MODULE_ALONE', [fqn=Fqn, attr=Attr, module=Module, path=Path, dot_edge=DotEdgeName])) },  % TODO: DELETE
    { atomic_list_concat([Module, '.', Attr], FqnAttr) },  % TODO: need to resolve path
    kyanchor_kyedge_fqn(Start, End, DotEdgeName, FqnAttr),  % TODO: does this belong here?
    { EvalType = EvalType0 }.
eval_atom_dot_single(astn(Start, End, Attr), DotEdgeName, import_module(Fqn, module_and_token(Module, Path, Token)), EvalType0, EvalType) -->> !,
    % TODO: test case -- see i1.py (III().x)
    { do_if(false, dump_term('dot-IMPORT_MODULE_AND_TOKEN', [fqn=Fqn, attr=Attr, module=Module, path=Path, token=Token, dot_edge=DotEdgeName])) },  % TODO: DELETE
    { atomic_list_concat([Module, '.', Token, '::', Attr], FqnAttr) },  % TODO: need to resolve path
    kyanchor_kyedge_fqn(Start, End, DotEdgeName, FqnAttr),  % TODO: does this belong here?
    { EvalType = EvalType0 }.
eval_atom_dot_single(_Astn, _DotEdgeName, _Type, EvalType, EvalType) -->> [ ].

%! eval_atom_call_union(+Parms, +AtomEval:ordset, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
%% Helper for eval_single_type(call(Atom, Parms)), which loops over
%% the individual types in the (union) AtomEval and creates a union
%% type of all the possibilities.
eval_atom_call_union(Parms, AtomEval, EvalType) -->>
    { ord_empty(EvalType0) },
    maplist_foldl_kyfact_symrej(
        eval_atom_call_single_of_type(Parms), AtomEval, EvalType0, EvalType).

%! eval_atom_call_single_of_type(+Parms, +Type, +EvalType0, -EvalType) is det.
%% Helper for eval_atom_call_union
eval_atom_call_single_of_type(Parms, Type, EvalType0, EvalType) -->>
    eval_single_type(Type, TypeEval),
    maplist_foldl_kyfact_symrej(
        eval_atom_call_single(Parms), TypeEval, EvalType0, EvalType).

%! eval_atom_call_single(+Parms, +Type, +EvalType0:ordset, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
%% Process a single call, adding to EvalType
eval_atom_call_single(_Parms, class(Fqn, Bases), EvalType0, EvalType) -->>  !,
    %% TODO: MRO for__init__ and output ref to it
    { ord_add_element(EvalType0, class(Fqn, Bases), EvalType) }.
eval_atom_call_single(_Parms, func(_, ReturnType), EvalType0, EvalType) -->>  !,
    %% ord_union because ReturnTYpe is a union type
    { ord_union(EvalType0, ReturnType, EvalType) }.
eval_atom_call_single(Parms, T, EvalType0, EvalType) -->>
    { ord_add_element(EvalType0, func(T, Parms), EvalType) }.

%! exprs_from_symtab(+SymtabPair:pair, -Exprs) is semidet.
%% Using the Fqn-Type pairs dict_pairs, get expr if it has a non-[] type.
expr_from_symtab(_Fqn-Type, expr(Type)) :-
    Type = [_|_].

%! add_rej_to_symtab(+FqnRejType:pair, +Symtab0, -Symtab) is det.
%% For Fqn-RejType pairs in FqnRejTypes, add to symtab.
add_rej_to_symtab(Fqn-RejType, Symtab0, Symtab) :-
    get_dict(Fqn, Symtab0, FqnType),
    ord_union(FqnType, RejType, CombinedType),
    put_dict(Fqn, Symtab0, CombinedType, Symtab).

%! symrej_accum(+FqnType:pair, +Symtab0Rej0Mod0, +SymtabRejMod) is det.
%% The accumulator for 'symrej'.
%% Tries to unify Key-Type with what's already in symtab; if that
%% fails because it's not in the symtab, adds it to symtab; otherwise
%% adds it Rej.
%% See table of actions in the top-level documentation.
%% Symtab0Rej0Mod0 and SymtabRejMod aresym_rej_mod/3 functors.
%% If Type is uninstantiated it gets set to []
%% TODO: can we eliminate the "(Type=[]->true;true)" ?
%% TODO: use library(assoc) or library(rbtrees) or trie or hash
%%       instead of dict for Symtab (performance)
symrej_accum(Fqn-Type, sym_rej_mod(Symtab0,Rej0,Modules0), sym_rej_mod(Symtab,Rej,Modules)) :-
    Modules = Modules0,
    (  get_dict(Fqn, Symtab0, TypeSymtab)
    -> symrej_accum_found(Fqn, Type, TypeSymtab, Symtab0, Symtab, Rej0, Rej)
    ;  Rej = Rej0,
       %% ensure Type is instantiated (defaults to []), if this is a lookup
       ( Type = [] -> true ; true ),
       put_dict(Fqn, Symtab0, Type, Symtab)
    ).

%! symrej_accum_found(+Fqn, +Type, +TypeSymtab, +Symtab0, -Symtab, +Rej0, -Rej).
%% Helper for symrej_accum/3 for when Fqn is in Symtab with value
%% TypeSymtab (Type is the new type).
symrej_accum_found(Fqn, Type, TypeSymtab, Symtab0, Symtab, Rej0, Rej) :-
   (  Type = TypeSymtab  % also handles Type is uninstantiated (i.e., a lookup)
   -> Symtab = Symtab0,
      Rej = Rej0
   ;  ord_union(TypeSymtab, Type, TypeComb),
      %% any new information from Type? - if so, add to symtab and rejects list
      (  TypeComb = TypeSymtab
      -> Symtab = Symtab0,
         Rej = Rej0
      ;  put_dict(Fqn, Symtab0, TypeComb, Symtab),
         Rej = [Fqn-Type|Rej0]
      )
   ).

%! dict_values(+Dict, -Values) is det.
%%    True when Values is an ordered set of the values appearing in Dict.
%% TODO: this should be in library(dicts).
%% TODO: this isn't used?
dict_values(Dict, Values) :-
    dict_pairs(Dict, _Tag, Pairs),
    pairs_values(Pairs, Values).

portray(Term) :-
    %% in the following, format/2 is used because
    %% print_message(warning, E) gives an infinite recursion.
    E = error(_, _),            % avoid trapping abort, timeout, etc.
    catch(my_portray(Term),
          E,
          format('EXCEPTION:portray(~q)', [Term])).

%! my_portray is semidet.
%%  For more compact output (debugging) from dump_term/2.
%%  TODO: change all to use my_portray_unify (see 1st clause).
my_portray(Astn) :-
    node_astn0(Astn0, Start, End, Value),
    my_portray_unify(Astn0, Astn), !,
    format("'ASTN'(~d:~d, ~q)", [Start, End, Value]).
my_portray('NameRawNode'{name:Astn}) :-
    format("'NameRawNode'{name:~p}", [Astn]).
my_portray(module_and_token(Path, Token)) :-
    format("module_and_token(~p, ~p)", [Path, Token]).
my_portray(str(S)) :-
    format('str(~q)', [S]), !.
my_portray(bool(B)) :-
    format('bool(~q)', [B]), !.
my_portray('StringNode'{astns: [Astn]}) :- !,  % doesn't handle "foo" "bar"
    format("'StringNode'{astn:[~p]}", [Astn]).
my_portray('NumberNode'{astn: Astn}) :-
    format("'NumberNode'{astn:[~p]}", [Astn]).
my_portray(op([Astn])) :-
    node_astn(Astn, _, _, _), !,
    format('op([~p])', [Astn]).
my_portray(fqn(X)) :- !,
    format('fqn(~p)', [X]).
my_portray(func(F, R)) :- !,
    format('func(~p, ~p)', [F, R]).
my_portray(class(F, R)) :- !,
    format('class(~p, ~p)', [F, R]).
my_portray(union(U)) :- !,
    format('union(~p)', [U]).
my_portray(astn(Start, End, String)) :- !,
    format('astn(~p,~p, ~p)', [Start, End, String]).
my_portray('*list*'(List)) :- !,  % To make print_term output more compact
    format('~p', [List]).
my_portray(Meta) :-
    my_portray_unify(meta{encoding: _Encoding,
                          file_contents_b64: _ContentsB64,
                          kythe_corpus: KytheCorpus,
                          kythe_root: KytheRoot,
                          language: _Language,
                          path: Path,
                          pythonpaths: _Pythonpaths}, Meta), !,
    format('meta{~q, ~q, ~q <language><encoding><file_contents_b64><pythonpaths>}',
           [KytheCorpus, KytheRoot, Path]).
my_portray(Dict) :-
    %% In case the above portray fails
    is_dict(Dict, DictTag),
    DictTag == meta,        % ==/2 in case dict has uninstantiated tag
    dict_pairs(Dict, DictTag, Pairs),
    get_dict(kythe_corpus, Dict, Corpus),
    get_dict(kythe_root, Dict, Root),
    get_dict(path, Dict, Path),
    pairs_keys(Pairs, PairsKeys),
    !,
    format('META{~q, ~q, ~q ~w}', [Corpus, Root, Path, PairsKeys]).
my_portray('$VAR'('_')) :- !,  % work around a bug in print_term
    format('_', []).        % (numbervars(true) should handle this):
my_portray('$VAR'(N)) :- !,
    Chr is N + 65,
    format('~c', [Chr]).
my_portray(Assoc) :-
    is_assoc(Assoc), !,
    aggregate_all(count, gen_assoc(_, Assoc, _), Length),
    min_assoc(Assoc, MinKey, MinValue),
    max_assoc(Assoc, MaxKey, MaxValue),
    format('<assoc:~d, ~p: ~p ... ~p: ~p>', [Length, MinKey, MinValue, MaxKey, MaxValue]).
my_portray(Symtab) :-
    is_dict(Symtab, Tag), !,
    ground(Tag),
    Tag = symtab, !,
    dict_pairs(Symtab, _, Entries),
    length(Entries, NumEntries),
    (  NumEntries < 10
    -> format('symtab{<~d items> ~q}', [NumEntries, Entries])
    ;  format('symtab{<~d items>...}', [NumEntries])
    ).

%! my_portray_unify(?Generic, ?Term).
%% A "safe" version of unify, for portray.
my_portray_unify(Generic, Term) :-
    subsumes_term(Generic, Term),
    Generic = Term.

%! do_if(:Cond, :Pred) is det.
%% A handy meta-predicate for turning debug stuff on/off, according to Cond
do_if(Cond, Pred) :-
    (  call(Cond)
    -> call(Pred)
    ;  true
    ).

%! dump_term(+Msg:atom, +Term) is det.
%% TODO: Delete this debugging code
dump_term(Msg, Term) :-
    dump_term(Msg, Term, [tab_width(0),
                          indent_arguments(2),
                          right_margin(120)]).
%! dump_term(+Msg:atom, +Term, +Options:list) is det.
%% TODO: use debug/3, etc. instead (also print_message/2).
%% TODO: Delete this debugging code
dump_term(Msg, Term, Options) :-
    (  Msg = ''
    -> true
    ;  format(user_error, '% === ~w ===~n~n', [Msg])
    ),
    print_term_cleaned(Term, Options, TermStr),
    (  Msg = ''
    -> format(user_error, '~s.~n', [TermStr])
    ;  format(user_error, '~s.~n~n', [TermStr]),
       format(user_error, '% === end ~w ===~n~n', [Msg])
    ).

%! print_term_cleaned(+Term, +Options, -TermStr) is det.
%% print_term, cleaned up
print_term_cleaned(Term, Options, TermStr) :-
    %% print_term leaves trailing whitespace, so remove it
    with_output_to(
            string(TermStr0),
            (current_output(TermStream),
             print_term(Term, [output(TermStream)|Options]))),
    re_replace(" *\n"/g, "\n", TermStr0, TermStr).

%! zip_merge(+Xs:list, Ys:list, -XYs:list) is det.
%% zip_merge([a,b], [1,2,], [a-1, b-2])
zip_merge([], [], []).
zip_merge([X|Xs], [Y|Ys], [X-Y, XYs]) :-
    zip_merge(Xs, Ys, XYs).

%! opts(Opts:list, Items:list) is det.
opts(Opts, Items) :- maplist(opt(Opts), Items).

%! opt(+Opts:list, Item) is det.
%% Allows maplist(opt(Opts), [option1(X), option2(Y)]) instead of
%%     maplist({Opts}/[X]>>memberchk(X, Opts), [option1(X), option2(Y)]).
opt(Opts, Item) :- memberchk(Item, Opts).

%! base64_string(+Value, -String) is det.
%% Decode a base64 string to a string.
base64_string(Value, String) :-
    base64(Atom, Value),
    atom_string(Atom, String).

%! my_json_read_dict(+Stream, -Dict) is det.
%%  Wrapper on library(http/json, [json_read_dict/2]) that works
%%  if autoload is turned off.
%%  Also sets the dict tags to 'json' (json_read_dict/2 leaves the tag
%%  as an uninstantiated variable).
my_json_read_dict(Stream, Dict) :-
    % TODO: fix library(http/json): use_module(library(lists)).
    current_prolog_flag(autoload, AutoloadFlag),
    set_prolog_flag(autoload, true), % TODO: Otherwise gets error: json:term_to_dict/3 - undefined select/3
    json_read_dict(Stream, Dict),
    %% use the tag 'json' for json dicts, to ensure we don't accidentally
    %% instantiate to something unintended, e.g., in portray/1.
    set_json_dict_tag(json, Dict),
    set_prolog_flag(autoload, AutoloadFlag).

set_json_dict_tag(DefaultTag, Term) :-
    (  is_dict(Term),
       dict_pairs(Term, DefaultTag, Pairs) % instantiates the tag
    -> pairs_values(Pairs, Values),
       maplist(set_json_dict_tag(DefaultTag), Values)
    ;  is_dict(Term)            % tag != DefaultTag
    -> dict_pairs(Term, _, Pairs),
       pairs_values(Pairs, Values),
       maplist(set_json_dict_tag(DefaultTag), Values)
    ;  is_list(Term)
    -> maplist(set_json_dict_tag(DefaultTag), Term)
    ;  true                     % do nothing for non-dicts
    ).

%% Variants on maplist, foldl (and combinations of them) for EDCGs

%! maplist_kyfact(:Pred, +L:list)//[kyfact, file_meta] is det.
%% maplist/2 for EDCG [kyfact, file_meta]
maplist_kyfact(_Pred, []) -->> [ ].
maplist_kyfact(Pred, [X|Xs]) -->>
    call(Pred, X):[kyfact,file_meta],
    maplist_kyfact(Pred, Xs).

%! maplist_kyfact(:Pred, +L0:list, -L:list)//[kyfact, file_meta] is det.
%% maplist/3 for EDCG [kyfact, file_meta]
maplist_kyfact(_Pred, [], []) -->> [ ].
maplist_kyfact(Pred, [X|Xs], [Y|Ys]) -->>
    call(Pred, X, Y):[kyfact,file_meta],
    maplist_kyfact(Pred, Xs, Ys).

%! maplist_kyfact_symrej(:Pred, +L0:list, -L:list)//[kyfact, symrej, file_meta] is det.
%% maplist/3 for EDCG [kyfact, symrej, file_meta]
maplist_kyfact_symrej(_Pred, [], []) -->> [ ].
maplist_kyfact_symrej(Pred, [X|Xs], [Y|Ys]) -->>
    call(Pred, X, Y):[kyfact,symrej,file_meta],
    maplist_kyfact_symrej(Pred, Xs, Ys).

%! maplist_kyfact_expr(:Pred, +L0:list)//[kyfact, expr, file_meta] is det.
%% maplist/2 for EDCG [kyfact, expr, file_meta]
maplist_kyfact_expr(_Pred, []) -->> [ ].
maplist_kyfact_expr(Pred, [X|Xs]) -->>
    call(Pred, X):[kyfact,expr,file_meta],
    maplist_kyfact_expr(Pred, Xs).

%! maplist_kyfact_expr(:Pred, +L0:list, -L:list)//[kyfact, expr, file_meta] is det.
%% maplist/3 for EDCG [kyfact, expr, file_meta]
maplist_kyfact_expr(_Pred, [], []) -->> [ ].
maplist_kyfact_expr(Pred, [X|Xs], [Y|Ys]) -->>
    call(Pred, X, Y):[kyfact,expr,file_meta],
    maplist_kyfact_expr(Pred, Xs, Ys).

%! maplist_foldl_kyfact_expr(:Pred, +L:list, -V0, +V)//[kyfact, expr, file_meta] is det.
%% maplist/2 plus fold/4 for EDCG [kyfact, expr, file_meta]
maplist_foldl_kyfact_expr(_Pred, [], V, V) -->> [ ].
maplist_foldl_kyfact_expr(Pred, [X|Xs], V0, V) -->>
    call(Pred, X, V0, V1):[kyfact,expr,file_meta],
    maplist_foldl_kyfact_expr(Pred, Xs, V1, V).

%! maplist_foldl_kyfact_symrej(:Pred, +L:list, -V0, +V) is det.
%% maplist/2 plus foldl/4 for EDCG [kyfact, symrej, file_meta]
maplist_foldl_kyfact_symrej(_Pred, [], V, V) -->> [ ].
maplist_foldl_kyfact_symrej(Pred, [X|Xs], V0, V) -->>
    call(Pred, X, V0, V1):[kyfact,symrej,file_meta],
    maplist_foldl_kyfact_symrej(Pred, Xs, V1, V).
