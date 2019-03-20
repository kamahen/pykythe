% -*- mode: Prolog -*-

%% Post-processor for the simplified nodes with FQNs that is generated
%% by pykythe (see ast_cooked.Base.add_fqns).  The post-processing is
%% mainly:
%% - add Kythe anchor facts
%% - add facts/edges for attributes (e.g., "foo" in self.foo)
%% - resolve and process imports
%% - in future, things like function call references

%% This heavily uses the EDCG notation (-->>), which might be
%% unfamiliar to you -- I suggest reading Peter Van Roy's paper,
%% referenced at the bottom of https://github.com/mndrix/edcg
%%
%% There are 3 accumulators and one passed arg -- look for the
%% edcg:acc_info and edcg:pass_info facts for more information.  In
%% particular, `[ Fqn-Type ]:symrej` does a lookup into the symbol
%% table and inserts Fqn-[] (the "Any" type) if it's not there (if it
%% is there, Type is union-ed with whatever is alrady in the symtab).

%% Names and naming conventions:
%%  'astn' is an AST (Abstract Syntax Tree) node.
%%  'fqn' is fully qualified name
%%  'ky' as prefix means 'kythe' (e.g. kyfact instead of kythe_fact)
%%  'symtab' is symbol table
%%  'symrej' is symbol table (symtab) + rejects (+ modules)

%% There are multiple passes over the AST:
%%
%% 0. [run_parse_cmd] Create a "cooked" AST (Python program specified
%%    by --parsecmd), generating a Prolog data structures of the AST
%%    (as a string).
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
%%        expr: 'StringNode'{astns: ['ASTN'(1160:1166, '\'C2_x\'')]},
%%        left: 'AtomDotNode'{
%%               atom: 'NameRefFqn'{
%%                         fqn: 'test_data.simple.C2.__init__.<local>.self',
%%                         name: 'ASTN'(1151:1155, 'self') },
%%               attr_name: 'ASTN'(1156:1157, 'x'),
%%               binds: bool('True') } }
%% When this is read in, it is simplified to something like this:
%%   assign([dot_op(var('test_data.simple.C2.__init__.<local>.self')],
%%              astn(1156,1157, 'x'),
%%              '/kythe/edge/defines/binding'),
%%          [class_type('.home.peter.src.typeshed.stdlib.2and3.builtins.str', [])])
%%                 %% (Py2.7 would be __builtin__.str)
%%
%% To process this, we need to resolve the FQNs (in this case,
%% var('test_data.simple.C2.__init__.<local>.self') by looking up
%% in the symtab, eventually resulting in the dot_op(...) expression
%% being reduced to var('test_data.simple.C2.x')), which is then
%% looked up in the symtab.

%% The symtab dict maps an fqn to an ordset (possibly empty) of:
%%     class_type(Fqn, Bases)         % Bases is a list of union types
%%     function_type(Fqn, ReturnType) % ReturnType is a union type
%%     module_type(ModuleType)        % ModuleType is one of:
%%                                          module_alone(Module,Path)
%%                                          module_and_token(Module,Path,Token)
%%                                          module_star(Module,Path)}
%%                                    % all of which contain the FQN of the module
%% In most cases, the FQN is known, but for some (e.g., from `import *`),
%% a "dynamic" lookup is done (see 'NameBindsGlobalUnknown', which
%% use an "expr" type of `var_binds_lookup`).
%%
%% There is always an entry ModuleFqn-module_type(module_alone(ModuleFqn,ModulePath)),
%% which (amongst other things) is usesd to prevent circular imports going into
%% an infinite loop.

%% Implementation detail: lookup is done using
%%        [ Fqn-Type ]:symrej
%% which calls symrej_accum/3 and uses the sym_rej/2 functor to record
%% the symtab, rejected symtab entries. It acts as both lookup and
%% insert - if the Fqn isn't in the symtab, it is added (with the
%% "Any" type or []); if it is in the symtab, Result is either unified
%% with the symtab value, or Result is unioned with the symtab value
%% (and the "rej" list is added to, if needed).
%%
%% A symtab lookup can occur either as in a right-hand (evaluation)
%% context or left-hand (assignment) context (see symrej_accum/3).
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

%% All types are unions (represented as an ord_set); [] means that
%% there's no information and is effectively "Any". Many of the
%% predicates come in two versions: one that works with a type union
%% (ordset), and one that works on single "types", such as
%% class_type(...), function_type(...), etc. -- typically the predicate
%% that works with a type union iterates over the single items,
%% calling the predicate for single "types", and uses ord_union/3 to
%% combine the results.  (This use of ord_union/3 ensures that there's
%% no need to "flatten" the list and that the single types are kept in
%% a canonical order).  (There's a wrapper around this, to remove
%% unnecessary pieces -- see type_union/3 and friends.)

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
%% Module imports are detected during the first pass, but are deferred
%% to the second pass (which builds up the symbol table). Modules are
%% also in the symtab -- they are used for processing the "." operator
%% (which can be used on either a class or a module [actually, on
%% anything]) and to stop recursive imports.

%% TODO: try to remove the kyfact accumulator from the first pass
%%       and generate all the Kythe information from the second pass

%% TODO: Use QLF: http://www.swi-prolog.org/pldoc/man?section=qlf

:- module(pykythe, [pykythe_main/0]).

:- use_module(library(aggregate), [aggregate_all/3, foreach/2]).
:- use_module(library(apply), [exclude/3, include/3, maplist/2, maplist/3, maplist/4, foldl/4, convlist/3]).
:- use_module(library(base64), [base64/2]).
:- use_module(library(assoc), [is_assoc/1]).
:- use_module(library(debug), [assertion/1, debug/3]).
:- use_module(library(edcg)).   % requires: ?- pack_install(edcg).
:- use_module(library(error), [must_be/2, type_error/2]).
:- use_module(library(filesex), [directory_file_path/3, link_file/3]).
:- use_module(library(lists), [append/2, append/3, list_to_set/2, member/2, reverse/2, select/3]).
:- use_module(library(pcre), [re_replace/4]).
:- use_module(library(optparse), [opt_arguments/3]).
:- use_module(library(ordsets), [list_to_ord_set/2, ord_empty/1, ord_union/2, ord_union/3, ord_add_element/3]).
:- use_module(library(pairs), [pairs_keys/2, pairs_values/2, pairs_keys_values/3]).
:- use_module(library(prolog_stack)).  % For catch_with_backtrace
:- use_module(library(readutil), [read_file_to_string/3]).
:- use_module(library(yall)).
%% :- use_module(library(apply_macros).  % TODO: for performance (also maplist_kyfact_syrej etc)
:- use_module(must_once, [must_once/1, must_once_msg/2, must_once_msg/3, fail/1,
                          must_once/6 as must_once_kyfact_symrej,
                          must_once/3 as must_once_symrej]).
:- use_module(c3, [mro/2]).
:- use_module(pykythe_utils).
:- use_module(module_path).

:- meta_predicate
       include_kyfact(4, +, -, +),
       maplist_kyfact(4, +, +, -, +),
       maplist_kyfact(5, +, -, +, -, +),
       maplist_kyfact_symrej(6, +, +, -, +, -, +),
       maplist_kyfact_symrej(7, +, -, +, -, +, -, +),
       maplist_kyfact_symrej_combine(7, +, -, +, -, +, -, +),
       maplist_kyfact_expr(6, +, +, -, +, -, +),
       maplist_kyfact_expr(7, +, +, +, -, +, -, +).

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

%% TODO: there are too many rdet declarations, and they seem to
%%       have negligible effect on performance.
%%       Ideally, we should use some expansion that
%%       uses setup_call_cleanup/3 to check for determinism
%%       (or determiistic/1).

%% Higher-level predicates that we use deterministically:
:- maplist(rdet, [convlist/3,
                  exclude/3,
                  foldl/4,
                  include/3,
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

%% Deterministic predicates in this module.
%% You can generate an approximation of these by:
%%     forall(current_predicate(pykythe:Pred), format('>>> ~q~n', [Pred])).
%% Commented-out items are non-deterministic.

:- maplist(rdet, [
                  'NameRawNode_astn_and_name'/3,
                  absolute_dir/2,
                  add_rej_to_symtab/3,
                  assign_exprs/5,
                  assign_exprs_count/6,
                  assign_exprs_count_impl/6,
                  assign_normalized/7,
                  %% base64_string/2, % handled by must_once
                  base64_term/2,
                  builtins_symtab_extend/3,
                  clean_class/3,
                  %% canonical_path/2,
                  clean_kind/2,
                  clean_kythe_facts/2,
                  combine_types/2,
                  convdict/3,
                  do_if/2,
                  dotted_name_imports/7,
                  double_dot/2,
                  dump_term/2,
                  dump_term/3,
                  %% ensure_dict_fact/3, % det or throws
                  %% ensure_dict_fact_base64/3, % det or throws
                  ensure_class_mro_object/3,
                  eval_assign_expr/6,
                  eval_assign_single/7,
                  eval_assign_dot_op_binds_single/8,
                  eval_assign_subscr_op_binds_single/7,
                  eval_atom_call_single/8,
                  eval_atom_dot_single/8,
                  eval_atom_subscr_binds_single/7,
                  eval_atom_subscr_single/7,
                  eval_lookup/7,
                  eval_lookup_single/7,
                  eval_single_type/7,
                  eval_union_type/7,
                  expr_normalized/6,
                  foldl_process_cached_or_from_src_module/5,
                  full_path/6,
                  hash_hex/2,
                  if_stmt_elses/2,
                  include_kyfact/5,
                  json_read_dict/2,
                  json_read_dict_validate/3,
                  json_write_dict_nl/2,
                  kyImportDotNode/3,
                  kyImportDottedAsNamesFqn/7,
                  kyImportDottedAsNamesFqn_comb/10,
                  kyImportDottedAsNamesFqn_dots/5,
                  kyImportDottedAsNamesFqn_dots2/3,
                  kyImportFromStmt/9,
                  kyNameRawNode/3,
                  kyanchor/6,
                  kyanchor_kyedge_fqn/7,
                  kyanchor_kyedge_fqn_pieces/8,
                  kyanchor_node/5,
                  kyanchor_node/6,
                  kyanchor_node_kyedge_fqn/6,
                  kyanchor_node_kyedge_fqn/7,
                  kyanchor_node_kyedge_fqn_pieces/7,
                  kyedge/6,
                  kyedge_fqn/6,
                  kyfact/6,
                  kyfact_base64/6,
                  kyfact_signature_node/6,
                  kyfacts/5,
                  kyfacts_signature_node/5,
                  kyfile/4,
                  kynode/7,
                  kynode_if_stmt/7,
                  kythe_kinds/4,
                  make_directory_path/1,
                  maplist_eval_assign_expr/6,
                  maplist_kyfact/5,
                  maplist_kyfact/6,
                  maplist_kyfact_expr/7,
                  maplist_kyfact_expr/8,
                  maplist_kyfact_symrej/7,
                  maplist_kyfact_symrej/8,
                  maplist_kyfact_symrej_combine/8,
                  maplist_kynode/7,
                  %% maybe_open_read/2,
                  merge_cache_into_symtab/3,
                  module_part/2,
                  %% module_path/2,
                  modules_in_exprs/2,
                  modules_in_symtab/2,
                  %% must_once/1,
                  %% must_once_msg/2,
                  %% must_once_msg/3,
                  %% my_portray/1,
                  %% my_portray_unify/2,
                  %% node_astn/4,
                  %% node_astn0/4,
                  opt/2,
                  opts/2,
                  %% process_module_cached/6,
                  %% process_module_cached_impl/7,
                  %% process_module_cached_or_from_src/6,
                  process_module_cached_or_from_src_setup/4,
                  %% process_module_from_src/6,
                  %% process_module_from_src_impl/6,
                  %% path_expand/3,
                  %% path_part/2,
                  path_to_python_module_or_unknown/2,
                  print_term_cleaned/3,
                  process_nodes/5,
                  process_nodes_impl/7,
                  %% py_ext/2,
                  %% py_ext_ext/1,
                  %% pykythe_main/0,
                  pykythe_main2/0,
                  pykythe_opts/2,
                  %% read_cache_and_validate/5,
                  read_nodes/3,
                  remove_class_cycles/3,
                  remove_class_cycles_one/4,
                  remove_last_component/3,
                  remove_suffix_star/3,
                  reorder_kythefacts_1/3,
                  resolve_mro_dot/9,
                  resolve_unknown_fqn/7,
                  run_parse_cmd/4,
                  safe_delete_file/1,
                  signature_node/3,
                  signature_source/3,
                  simple_path_module/2,
                  simplify_ast/2,
                  simplify_ast_slot_pair/2,
                  simplify_meta/2,
                  split_atom/4,
                  split_module_atom/2,
                  split_path_string_and_canonicalize/3,
                  src_base/2,
                  subscr_resolve_dot_binds/7,
                  %% symtab_filter/2,
                  symrej_accum/3,
                  symrej_accum_found/7,
                  symtab_as_kyfact/3,
                  symtab_pykythe_types/4,
                  %% symtab_lookup/4,
                  term_to_canonical_atom/2,
                  %% path_batch_suffix/3,
                  %% update_dict/3, % This is deterministic
                  %% update_new_dict/3, % This is deterministic
                  write_atomic_stream/2,
                  write_atomic_file/2,
                  %% write_batch_symtab/2, % Is det, but expansion confuses write_atomic_stream/2.
                  %% write_to_protobuf/4,  % Is det, but expansion confuses write_atomic_file/2.
                  write_facts/3
                       ]).

%% The autoload directive needs to be after rdet/1 is used once, to
%% allow its autoload to get rdet:debug.
%% The "-O" flag changes things slightly; the following directive
%% needs to be here (and not earlier) with "-O".

:- set_prolog_flag(autoload, false). % See comment in pykythe_utils:my_json_read_dict/2.

%% "kyfact" accumulator gets FQN anchor facts, in an ordinary list
%% with each value being a dict to be output in JSON (and eventually
%% to a protobuf). The list may contain duplicates, which are removed
%% before output.

%% TODO: Need test cases for this:
%% Duplicates can arise if a variable is redefined in the Python
%% source; our de-dup keeps the type information for the first such
%% definition and outputs defines/binding for all instances.

%% TODO: check for duplicate edge facts that indicate a bug.
edcg:acc_info(kyfact, T, Out, In, Out=[T|In]).

%% "expr" accumulator gets expressions that need interpreting.
edcg:acc_info(expr, T, Out, In, Out=[T|In]).

%% "symrej" accumulator is for symtab + rejected items that need reprocessing.
edcg:acc_info(symrej, FqnType, In, Out, symrej_accum(FqnType, In, Out)).

%% "file_meta" passed arg contains meta-info about the current file.
edcg:pass_info(file_meta).

edcg:pred_info(include_kyfact, 2,                   [kyfact,file_meta]).
edcg:pred_info(maplist_kyfact, 2,                   [kyfact,file_meta]).
edcg:pred_info(maplist_kyfact, 3,                   [kyfact,file_meta]).

edcg:pred_info(dotted_name_imports, 4,              [kyfact,file_meta]).
edcg:pred_info(kyanchor, 3,                         [kyfact,file_meta]).
edcg:pred_info(kyanchor_kyedge_fqn, 4,              [kyfact,file_meta]).
edcg:pred_info(kyanchor_kyedge_fqn_pieces, 5,       [kyfact,file_meta]).
edcg:pred_info(kyanchor_node, 2,                    [kyfact,file_meta]).
edcg:pred_info(kyanchor_node, 3,                    [kyfact,file_meta]).
edcg:pred_info(kyanchor_node_kyedge_fqn, 3,         [kyfact,file_meta]).
edcg:pred_info(kyanchor_node_kyedge_fqn, 4,         [kyfact,file_meta]).
edcg:pred_info(kyanchor_node_kyedge_fqn_pieces, 4,  [kyfact,file_meta]).
edcg:pred_info(kyedge, 3,                           [kyfact,file_meta]).
edcg:pred_info(kyedge_fqn, 3,                       [kyfact,file_meta]).
edcg:pred_info(kyfact, 3,                           [kyfact,file_meta]).
edcg:pred_info(kyfact_base64, 3,                    [kyfact,file_meta]).
edcg:pred_info(kyfact_signature_node, 3,            [kyfact,file_meta]).
edcg:pred_info(kyfacts, 2,                          [kyfact,file_meta]).
edcg:pred_info(kyfacts_signature_node, 2,           [kyfact,file_meta]).
edcg:pred_info(kyfile, 1,                           [kyfact,file_meta]).
edcg:pred_info(starts_with_kyfact, 2,               [kyfact,file_meta]).
edcg:pred_info(symtab_pykythe_types, 1,             [kyfact,file_meta]).

edcg:pred_info(maplist_kyfact_expr, 2,              [kyfact,expr,file_meta]).
edcg:pred_info(maplist_kyfact_expr, 3,              [kyfact,expr,file_meta]).

edcg:pred_info(assign_normalized, 2,                [kyfact,expr,file_meta]).
edcg:pred_info(expr_normalized, 1,                  [kyfact,expr,file_meta]).
edcg:pred_info(import_from, 1,                      [kyfact,expr,file_meta]).
edcg:pred_info(kyImportDottedAsNamesFqn, 2,         [kyfact,expr,file_meta]).
edcg:pred_info(kyImportDottedAsNamesFqn_comb, 5,    [kyfact,expr,file_meta]).
edcg:pred_info(kyImportFromStmt, 4,                 [kyfact,expr,file_meta]).
edcg:pred_info(kynode, 2,                           [kyfact,expr,file_meta]).
edcg:pred_info(kynode_if_stmt, 2,                   [kyfact,expr,file_meta]).
edcg:pred_info(kynode_impl, 2,                      [kyfact,expr,file_meta]).
edcg:pred_info(maplist_kynode, 2,                   [kyfact,expr,file_meta]).
edcg:pred_info(process_nodes_impl, 2,               [kyfact,expr,file_meta]).

edcg:pred_info(maplist_kyfact_symrej, 2,            [kyfact,symrej,file_meta]).
edcg:pred_info(maplist_kyfact_symrej, 3,            [kyfact,symrej,file_meta]).
edcg:pred_info(maplist_kyfact_symrej_combine, 3,    [kyfact,symrej,file_meta]).
edcg:pred_info(must_once_kyfact_symrej, 1,          [kyfact,symrej,file_meta]).

edcg:pred_info(eval_assign_expr, 1,                 [kyfact,symrej,file_meta]).
edcg:pred_info(eval_assign_single, 2,               [kyfact,symrej,file_meta]).
edcg:pred_info(eval_assign_dot_op_binds_single, 3,  [kyfact,symrej,file_meta]).
edcg:pred_info(eval_assign_subscr_op_binds_single,2,[kyfact,symrej,file_meta]). % TODO: alignment
edcg:pred_info(eval_atom_call_single, 3,            [kyfact,symrej,file_meta]).
edcg:pred_info(eval_atom_dot_single, 3,             [kyfact,symrej,file_meta]).
edcg:pred_info(eval_atom_subscr_binds_single, 2,    [kyfact,symrej,file_meta]).
edcg:pred_info(eval_atom_subscr_single, 2,          [kyfact,symrej,file_meta]).
edcg:pred_info(eval_lookup, 2,                      [kyfact,symrej,file_meta]).
edcg:pred_info(eval_lookup_single, 2,               [kyfact,symrej,file_meta]).
edcg:pred_info(eval_single_type, 2,                 [kyfact,symrej,file_meta]).
edcg:pred_info(eval_union_type, 2,                  [kyfact,symrej,file_meta]).
edcg:pred_info(maplist_eval_assign_expr, 1,         [kyfact,symrej,file_meta]).
edcg:pred_info(maybe_resolve_mro_dot, 5,            [kyfact,symrej,file_meta]).
edcg:pred_info(resolve_mro_dot, 4,                  [kyfact,symrej,file_meta]).
edcg:pred_info(subscr_resolve_dot_binds, 3,         [kyfact,symrej,file_meta]).

edcg:pred_info(resolve_unknown_fqn, 4,              [symrej,file_meta]).
edcg:pred_info(symtab_if_file, 1,                   [symrej,file_meta]).

edcg:pred_info(symtab_lookup, 2,                    [symrej]).
edcg:pred_info(must_once_symrej, 1,                 [symrej]).

edcg:pred_info(do_if_file, 1,                       [file_meta]).
edcg:pred_info(log_if_file, 2,                      [file_meta]).
edcg:pred_info(signature_node, 2,                   [file_meta]).
edcg:pred_info(signature_source, 2,                 [file_meta]).

edcg:pred_info(exprs, 1,                            [expr]).

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
%% See also library(main)'s definition of main
%% This simply calls pykythe_main2, so that we can do:
%%   :- use_module(library(test_cover), [show_coverage/1]).
%%   pykythe_main :-
%%       set_prolog_flag(autoload, true),
%%       profile(pykythe:pykythe_main2).
%% or:
%%
%% :- use_module(library(statistics)).
%% pykythe_main :-
%%     set_prolog_flag(autoload, true),
%%     profile(pykythe:pykythe_main2).
%%
%% Profiling results appeared to show that over 87% of time was spent
%% in reading in the JSON from the parser, writing out the results, or
%% garbage collection. So, to improve performance, the low hanging
%% fruit seemed to be:
%%   - output the AST as a Prolog term.
%%   - use another output format than JSON (e.g., protobufs)
%% So, I changed the AST to be a Prolog term, but that only gave a 10%
%% performance boost. :(
%%
%% After changing to Prolog term for AST, the low-hanging fruit
%% appears to be:
%%    write_facts/2        (15%)
%%    write_batch_symtab/3 (11%) <== mostly in format/3 - would term_string/2 be better?
%%    put_dict/4           (25%)
%%      - garbage_collect             (13%)
%%      from builtins_symtab_extend/4 (11%)
%%      from symrej_accum/3           (18%)
%%
%% When processing from a cache file (batch), almost all the time
%% is taken by:
%%     update_new_dict
%%       put_dict/4         27%
%%       garbage collection 67%

%% For this, the low-hanging fruit is changing symtab from dict to
%% assoc or rbtree (probably rbtree). We also need a faster merge of
%% symtab entries (neither library(assoc) nor library(rbtrees) has a
%% "merge", although they both have a way of converting an ordered
%% list to a tree, which can probably be modified to add to an
%% existing tree.
%%
%% Old profiling results:
%%   CONCLUSION: it's worth computing SHA-1 for source, to avoid
%%               all the decoding.
%%   CONCLUSION: cache is worthwhile (11.5s vs 2.3s for builtins and imports)
%%               -- 2.0s when SHA1 used (disappointing optimization)
%%   for processing builtins.pyi (2.3s user time) from all cache
%%                       (15 files, 64K lines, 17MB, src: 186KB,
%%                        which ends up with 2318 symtab entries):
%%   (for builtins.kythe.json only, ~0.4s, so eliminating all the
%%    recursive checking of imports would save ~2s)
%%   Most expensive operations were:
%%      read_cache_and_validate/5    70%
%%          base64_term/2                 42%
%%            base64/2                         41%
%%          json_read_dict_validate/3     28%
%%            json_read_dict/2                 28%
%%      merge_cache_into_symtab/3    30%
%%          put_dict/4                    28%
%%   After removing the base64 encoding/decoding for symtab, this became (1.7s):
%%      merge_cache_into_symtab/3    56%
%%          put_dict/4                    55%
%%      read_cache_and_validate/5    41%
%%          json_read_dict/2              34%
%%      $garbage_collect/1           29%       (mostly from put_dict/4)
pykythe_main :-
    log_if(true, 'Start'),      % TODO: delete
    %% TODO: delete {debugger,print}_write_options
    set_prolog_flag(debugger_write_options, [quoted(true), portray(true), max_depth(14), attributes(portray), spacing(next_argument)]),
    set_prolog_flag(print_write_options, [quoted(true), portray(true), max_depth(14), attributes(portray), spacing(next_argument)]),
    set_prolog_flag(report_error, true),         % TODO: delete
    set_prolog_flag(backtrace, true),            % TODO: delete
    set_prolog_flag(backtrace_show_lines, true), % TODO: delete
    %% Play nice with emacs *compilation* (except it doesn't
    %% quite work properly ... no idea why):
    %% set_prolog_flag(color_term, false), % TODO: delete (to ~/.plrc)
    %% TODO: the following might not be needed when the
    %%       initialization/2 directive is enabled.
    catch_with_backtrace(pykythe_main2,
                         Error,
                         ( print_message(error, Error),
                           halt(1) )),
    log_if(true, 'End'),                         % TODO: delete
    halt(0).

pykythe_main2 :-
    retractall(rdet:det(_)), % TODO: delete this crude work-around a weird bug with backtrace and rdet.
    %% set_prolog_flag(gc, true),  % TODO: tune GC for performance
    %% set_prolog_flag(agc_margin, 0),  % TODO: tune GC for performance
    on_signal(int, _, throw),  % TODO: delete?
    on_signal(term, _, throw),  % TODO: delete?
    %% on_signal(int, _, interrupt),  % TODO: reinstate if don't need traceback
    pykythe_opts(SrcPath, Opts),
    opt(Opts, builtins_symtab(BuiltinsSymtabFile)),
    %% BuiltinsSymtabFile is created by gen_builtins_symtab.pl
    %% TODO: dynamic builtins_version/1, builtins_symtab/1, builtins_pairs/1,
    %%               builtins_symtab_primitive/2, builtins_symtab_modules/1.
    ensure_loaded(BuiltinsSymtabFile), % TODO: should be a module and list the predicates
    path_to_python_module_or_unknown(SrcPath, SrcFqn),
    builtins_symtab(Symtab0),
    must_once(
        process_module_cached_or_from_src(Opts, from_src_ok, SrcPath, SrcFqn, Symtab0, _Symtab)).

%! interrupt(+Signal)
%% An interrupt handler, installed by on_signal/3.
interrupt(_Signal) :-
    halt(1).

%! pykythe_opts(-SrcPath:atom, -Opts:list(pair)) is det.
%% Process the command line, getting the source file and options.
pykythe_opts(SrcPath, Opts) :-
    current_prolog_flag(version, PrologVersion),
    must_once_msg(PrologVersion >= 80101, 'SWI-Prolog version is too old'),  % Sync this with README.md
    OptsSpec =
       [[opt(parsecmd), type(atom), default('parsecmd-must-be-specified'), longflags([parsecmd]),
         help('Command for running parser than generates fqn.kythe.json file')],
        [opt(entriescmd), type(atom), default('entriescmd-must-be-specified'), longflags([entriescmd]),
         help('Command for running conversion of .kythe.json to .kythe.entries')],
        [opt(kythe_corpus), type(atom), default(''), longflags(['kythe_corpus']),
         help('Value of "corpus" in Kythe facts')],
        [opt(kythe_root), type(atom), default(''), longflags(['kythe_root']),
         help('Value of "root" in Kythe facts')],
        [opt(pythonpath), type(atom), default(''), longflags(['pythonpath']),
         help('Similar to $PYTHONPATH for resolving imports (":"-separated paths)')],
        [opt(kytheout), type(atom), default(''), longflags(['kytheout']),
         help('Directory for output of imported files (including "main" file)')],
        [opt(kytheout_suffix), type(atom), default('.kythe.json'), longflags(['kythout_suffix']),
         help('Suffix (extension) for output files - should have leading ".".')],
        [opt(entries_suffix), type(atom), default('.kythe.entries'), longflags(['entries_suffix']),
         help('Suffix (extension for Kythe protouf output files - should have leading "."')],
        [opt(version), type(atom), default(''), longflags(['version']),
         help('Pykythe version, used to validate cache entries')],
        [opt(builtins_symtab), type(atom), default(''), longflags(['builtins_symtab']),
         help('File containing a builtins_symtab/1 fact')],
        [opt(batch_suffix), type(atom), default(''), longflags(['batch_suffix']),
         help(['Suffix (extension) for creating cache batch files (see README).',
               '- this is concatenated to the --kytheout_suffix value.',
               'If omitted or "", batch cache isn\'t used.'])],
        [opt(python_version), type(integer), default(3), longflags(python_version),
         help('Python major version')]], % TODO: should be a triple: see ast_raw.FAKE_SYS
    opt_arguments(OptsSpec, Opts0, PositionalArgs),
    must_once_msg(PositionalArgs = [SrcPath0], 'Missing/extra positional args'),
    absolute_file_name(SrcPath0, SrcPath),
    must_once(split_path_string_and_canonicalize(pythonpath, Opts0, Opts)).

%! process_module_cached_or_from_src(+Opts:list, +FromSrcOk:{from_src_ok,cached_only}, +SrcPath:atom, +SrcFqn:atom, +Symtab0, -Symtab) is semidet.
%%
%% General algorithm for processing modules.
%%
%% Modules are handled by the symrej accumulator and are therefore not
%% processed when they are first imported but instead are processed as
%% part of "pass 2" (assign_exprs/5)
%%
%% A module might be "from_src" (hasn't been previously processed) or
%% "cached". If it's cached, the cached value is used only if:
%%    - the source file is the same (using hash_hex/2)
%%    - the cache file was processed with the same version of pykythe
%%    - (recursively) all of the modules that it uses are cached
%% Checking this can be slow, so an optimization is to use the "batch
%% ID" to avoid the recursive check for all modules being cached (the
%% "batch" cache file is also in a different format, for performance).
%%
%% If the value of FromSrcOk is cached_only, the predicate will fail
%% if any attempt is made to use a from_src version (that is, if the
%% above conditions for using a cache file fail); and this is
%% propagated up by failing all the way to the top, at which point,
%% processing is done with the from_src_ok value.
%%
%% When a module is processed, it updates the symtab.
%% When a module is output (to the cache), all its symtab entries are output,
%% including (recursively) imported symbols.
%%
%% One more detail ... it's possible that there are circular recursive
%% imports, so the module's entry in the symtab is used to prevent an
%% infinite recursion.
process_module_cached_or_from_src(Opts, FromSrcOk, SrcPath, SrcFqn, Symtab0, Symtab) :-
    process_module_cached_or_from_src_setup(Opts, SrcPath, SrcPathAbs, KytheJsonPath),
    (  get_dict(SrcFqn, Symtab0, _ModuleValue)
    -> %% Module in symtab (possibly recursive import): skip it.
       Symtab = Symtab0,
       log_if(true, 'Skipping (already processed/processing) ~q: ~q', [SrcFqn, SrcPath])
    ;  process_module_cached(Opts, FromSrcOk, KytheJsonPath, SrcPathAbs, Symtab0, Symtab)
    -> true
    ;  process_module_from_src(Opts, FromSrcOk, KytheJsonPath, SrcFqn, Symtab0, Symtab)
    ).

%! modules_in_symtab(+Symtab, -Modules:list) is det.
%% Create a set of all modules that appear as the type for symtab entries.
modules_in_symtab(Symtab, Modules) :-
    dict_values(Symtab, SymtabValues),
    append(SymtabValues, AllTypes),
    include(is_module, AllTypes, Modules0),
    list_to_union_type(Modules0, Modules).

%! is_module(+SingleType) is semidet.
%% Used by modules_in_symtab/2.
is_module(module_type(_)).

%! modules_in_exprs(+Exprs, -Modules:list) is det.
%% Create a set of all modules that appear in an "import" statement.
modules_in_exprs(Exprs, Modules) :-
    convlist(is_assign_import_module, Exprs, Modules0),
    list_to_union_type(Modules0, Modules).

%! is_assign_import_module(+Expr, -SingleType) is semidet.
%% Used by modules_in_exprs/2.
is_assign_import_module(assign_import_module(_, Module), module_type(Module)).

%! process_module_cached(+Opts:list, +FromSrcOk:{from_src_ok,cached_only}, +KytheJsonPath:atom, +SrcPath:atom, +Symtab0, -Symtab) is semidet.
%% Doesn't actually do "parse" - instead uses the cache file to
%% have the same effect as running process_module_from_src/6.
%% The logic is:
%%   conditionally open KytheJsonPath (which should be an absolute file name)
%%   if it succeeeds, run process_module_cached_impl/7
%%     this can fail if the cached file isn't valid (e.g., older than the source)
%%   [ensure that any open file is closed]
process_module_cached(Opts, FromSrcOk, KytheJsonPath, SrcPath, Symtab0, Symtab) :-
    setup_call_cleanup(
        maybe_open_read(KytheJsonPath, KytheInputStream),
        process_module_cached_impl(Opts, FromSrcOk, KytheInputStream, KytheJsonPath, SrcPath, Symtab0, Symtab),
        close(KytheInputStream)),
    log_if(false, 'Reused ~q for ~q', [KytheJsonPath, SrcPath]), % msg is output by process_module_cached_impl
    !. % TODO: delete (when the fail catch-all clause is removed).
process_module_cached(_Opts, _FromSrcOk, _KytheInputStream, KytheJsonPath, SrcPath, _Symtab0, _Symtab)  :-
    %% TODO: need a better reason for failure ... the tests in
    %%       process_module_cached_impl/7 should give a
    %%       message about which one failed (for now, we have
    %%       must_once/2 and simple tests).
    %% (Currently, some of the failures output a message; perhaps we can
    %% remove the following when all failures output a message?)
    log_if(access_file(KytheJsonPath, read),
           'Could not use reuse ~q for ~q', [KytheJsonPath, SrcPath]),
    fail.

%! process_module_cached_or_from_src_setup(+Opts, +SrcPath, -SrcPathAbs, -KytheJsonPath) is det.
%% Make sure that the output directory exists,
%% SrcPathAbs = absolute_file_name(SrcPath),
%% KytheJsonPath gets a path name in the output directory.
process_module_cached_or_from_src_setup(Opts, SrcPath, SrcPathAbs, KytheJsonPath) :-
    opts(Opts, [kytheout(KytheOutDir), kytheout_suffix(KytheOutSuffix)]),
    absolute_file_name(SrcPath, SrcPathAbs), % just in case
    src_base(SrcPathAbs, SrcPathBase),
    atomic_list_concat([KytheOutDir, SrcPathBase, KytheOutSuffix], KytheJsonPath).

%! process_module_cached_impl(+Opts:list, +FromSrcOk:{from_src_ok,cached_only}, +KytheInputStream, +KytheJsonPath:atom, +SrcPath:atom, +Symtab0, -Symtab) is semidet.
process_module_cached_impl(Opts, FromSrcOk, KytheInputStream, KytheJsonPath, SrcPath, Symtab0, Symtab)  :-
    opt(Opts, version(Version)),
    (  process_module_cached_batch(Opts, KytheJsonPath, SrcPath, SymtabFromCache, KytheBatchPath)
    -> merge_cache_into_symtab(SymtabFromCache, Symtab0, Symtab),
       log_if(true, 'Reused/batch(~w) ~q for ~q', [FromSrcOk, KytheBatchPath, SrcPath])
    ;  %% The following validation depends on what kyfile//1 generates.
       read_cache_and_validate(KytheInputStream, KytheJsonPath, SrcPath, Version, SymtabFromCache),
       %% TODO: modules_in_symtab not needed because foldl_process_module_cached_or_from_src/5
       %%       skips non-modules.
       modules_in_symtab(SymtabFromCache, ModulesInSymtab),
       %% recursively process modules, failing if any is "from_src"
       %% (not cached). This will cause failure of
       %% process_module_cached_impl/7, which will result in calling
       %% process_module_from_src/6 for this module.  Any imported
       %% modules that were processed will get re-processed (but use
       %% the cached result).
       merge_cache_into_symtab(SymtabFromCache, Symtab0, Symtab1),
       foldl_process_module_cached_or_from_src(Opts, cached_only, ModulesInSymtab, Symtab1, Symtab),
       log_if(true, 'Reussed/cache(~w) ~q for ~q', [FromSrcOk, KytheJsonPath, SrcPath])
    ).

%! process_module_cached_batch(+Opts:list, +KytheJsonPath:atom, +SrcPath:atom, -Symtab:dict, -KytheBatchPath:atom) is semidet.
process_module_cached_batch(Opts, KytheJsonPath, SrcPath, Symtab, KytheBatchPath) :-
    opt(Opts, version(Version)),
    path_batch_suffix(KytheJsonPath, Opts, KytheBatchPath),
    maybe_open_read(KytheBatchPath, KytheStreamBatch),
    read_term(KytheStreamBatch, BatchVersion, []),
    (  BatchVersion == Version
    -> read_term(KytheStreamBatch, Symtab, [])
    ;  log_if(true,
              'Cannot reuse batch cache (different version) ~q for ~q', [KytheJsonPath, SrcPath]),
       fail
    ).

%! foldl_process_module_cached_or_from_src(+Opts:list, +FromSrcOk:{from_src_ok,cached_only}, +Modules:list, -Symtab0:dict, +Symtab:dict) is semidet.
foldl_process_module_cached_or_from_src(_Opts, _FromSrcOk, [], Symtab, Symtab).
foldl_process_module_cached_or_from_src(Opts, FromSrcOk, [M|Modules], Symtab0, Symtab) :-
    %% TODO: handle module_star, merging its names into the symtab
    (  M = module_type(Module),
       path_part(Module, SrcPath), % TODO: fix failure for "import *"
       module_part(Module, SrcFqn)
    -> log_if(false, 'Trying/2(~w) imported ~q: ~q', [FromSrcOk, Module, SrcPath]),
       %% TODO: don't do this if module_and_token and module !=
       %% module_path or something like that (although it'll just fail
       %% because the file doesn't exist)
       process_module_cached_or_from_src(Opts, FromSrcOk, SrcPath, SrcFqn, Symtab0, Symtab1)
    ;  Symtab1 = Symtab0
    ),
    foldl_process_module_cached_or_from_src(Opts, FromSrcOk, Modules, Symtab1, Symtab).

%- read_cache_and_validate(+KytheInputStream, +KytheJsonPath, +SrcPath, +Version, -SymtabFromCache) is det.
%% Reads just enough to validate.
read_cache_and_validate(KytheInputStream, KytheJsonPath, SrcPath, Version, SymtabFromCache) :-
    json_read_dict_validate(KytheInputStream, '/pykythe/version', JsonVersion),
    %% ignore Signature for /pykythe/version
    ensure_dict_fact_base64(JsonVersion, fact_value, CacheVersion),
    %% short-circuit other tests if version mismatch
    (  CacheVersion == Version
    -> true
    ;  log_if(true,
              'Cannot reuse cache (different version) ~q for ~q', [KytheJsonPath, SrcPath]),
       fail
    ),
    json_read_dict_validate(KytheInputStream, '/pykythe/text/sha1', JsonSha1),
    ensure_dict_fact_base64(JsonSha1, fact_value, JsonSha1Hex),
    read_file_to_string(SrcPath, SrcText, [file_errors(fail)]),
    hash_hex(SrcText, SrcSha1Hex),
    (  SrcSha1Hex == JsonSha1Hex
    -> true
    ;  log_if(true,
              'Cannot reuse cache (different source ~q - ~q) ~q for ~q', [JsonSha1Hex, SrcSha1Hex, KytheJsonPath, SrcPath]),
       fail
    ),
    json_read_dict_validate(KytheInputStream, '/kythe/node/kind', JsonPath),
    ensure_dict_fact_base64(JsonPath, fact_value, 'file'),
    json_read_dict_validate(KytheInputStream, '/kythe/text/encoding', _JsonEncoding),
    json_read_dict_validate(KytheInputStream, '/pykythe/symtab', JsonSymtab),
    ensure_dict_fact(JsonSymtab, fact_value, SymtabFromCacheStr),
    %% base64_term(SymtabFromCacheBase64, SymtabFromCache). - Too slow.
    term_string(SymtabFromCache, SymtabFromCacheStr).

%! merge_cache_into_symtab(+SymtabFromCache, +Symtab0,- Symtab) is det.
%% Can fail if the cached text is different from the current file contents.
%% If a value is already in the cache, it is preserved.
merge_cache_into_symtab(SymtabFromCache, Symtab0, Symtab) :-
    dict_pairs(SymtabFromCache, symtab, SymtabFromCachePairs),
    update_new_dict(SymtabFromCachePairs, Symtab0, Symtab).

%! process_module_from_src(+Opts:list, +FromSrcOk, +KytheJsonPath:atom, +SrcFqn:atom, +Symtab0, -Symtab) is semidet.
%% Read in a single file (JSON output from running --parsecmd, which
%% encodes the AST nodes with FQNs), output Kythe JSON to current
%% output stream. SrcPath must be in absolute form (leading '/').
%% Fails if FromSrcOk isn't from_src_ok, otherwise succeeds.
process_module_from_src(Opts, from_src_ok, KytheJsonPath, SrcFqn, Symtab0, Symtab) :-
    (  module_path(SrcFqn, SrcPath) % fails if file doesn't exist
    -> process_module_from_src_impl(Opts, KytheJsonPath, SrcPath, SrcFqn, Symtab0, Symtab)
    ;  Symtab = Symtab0,
       log_if(true,
              'Invalid/nonexistant module (~q) for ~q', [KytheJsonPath, SrcFqn])
       %% TODO: output a dummy item, so that we don't unnecessarily
       %%       reprocess (when looking for cache) things that depend
       %%       on this module
    ).

%! process_module_from_src_impl(+Opts:list, +KytheJsonPath:atom, +SrcPath, +SrcFqn:atom, +Symtab0, -Symtab) is det.
process_module_from_src_impl(Opts, KytheJsonPath, SrcPath, SrcFqn, Symtab0, Symtab) :-
    log_if(true,
           'Processing from source ~q (output: ~q) for ~q', [SrcPath, KytheJsonPath, SrcFqn]),
    opts(Opts, [pythonpath(Pythonpaths), version(Version),
                kytheout_suffix(KytheOutSuffix), entries_suffix(EntriesSuffix),
                entriescmd(EntriesCmd)]),
    run_parse_cmd(Opts, SrcPath, SrcFqn, ParsedPath),
    log_if(true, 'Python parser: finished'),
    read_nodes(ParsedPath, Nodes, Meta),
    log_if(true, 'Processed AST nodes from Python parser'),
    Meta.pythonpaths = Pythonpaths,
    Meta.opts = Opts,
    Meta.version = Version,
    Meta.src_fqn = SrcFqn,
    do_if(false, dump_term('NODES', Nodes)),
    process_nodes(Nodes, src{src_fqn: SrcFqn, src: SrcPath}, KytheFacts1, Exprs, Meta),
    builtins_pairs(BuiltinsPairs),
    builtins_symtab_extend(BuiltinsPairs, SrcFqn, Symtab0, Symtab0a),
    put_dict(SrcFqn, Symtab0a, [module_type(module_alone(SrcFqn,SrcPath))], Symtab1),
    builtins_version(BuiltinsVersion),
    must_once_msg(BuiltinsVersion == Version,
                  'builtins_version(~q) should be ~q', [BuiltinsVersion, Version]),
    modules_in_exprs(Exprs, ModulesInExprs),
    do_if(trace_file(Meta.path),
                     dump_term('PASS1-EXPR_MODULES', ModulesInExprs)),
    do_if(trace_file(Meta.path),
          dump_term('PASS1-EXPR', Exprs)),
    %% Note that the following allows any imported module to be from_src
    %% (FromSrcOk to process_module_cached_or_from_src is from_src_ok).
    %% TODO: for ModulesInExprs that are module_star, need
    %%       to update symtab with top-level items (starts
    %%       with module. and doesn't have '.' inside).
    log_if(true, 'Pass 1: process nodes for ~q', [Meta.path]),
    foldl_process_module_cached_or_from_src(Opts, from_src_ok, ModulesInExprs, Symtab1, Symtab1WithImports),
    log_if(true, 'Pass 2: process exprs for ~q', [Meta.path]),
    assign_exprs(Exprs, Meta, Symtab1WithImports, Symtab, KytheFacts2),
    validate_symtab(Symtab),
    symtab_as_kyfact(Symtab, Meta, SymtabKytheFact),
    %% Output /pykythe/type facts, for debugging.
    symtab_pykythe_types(Symtab, SymtabPykytheTypes, [], Meta), % phrase(symtab_pykythe_types(Symtab), SymtabPYkytheTypes, Meta)
    reorder_kythefacts_1(KytheFacts1, SymtabKytheFact, KytheFacts1a),
    append([KytheFacts1a, KytheFacts2, SymtabPykytheTypes], KytheFacts3),
    clean_kythe_facts(KytheFacts3, KytheFacts),
    log_if(true, 'Writing Kythe facts for ~q', [Meta.path]),
    write_atomic_stream(write_facts(KytheFacts), KytheJsonPath),
    (  path_batch_suffix(KytheJsonPath, Opts, KytheBatchPath)
    -> write_atomic_stream(write_batch_symtab(Symtab, Version), KytheBatchPath)
    ;  true
    ),
    log_if(true, 'Converting to Kythe protobuf'),
    must_once(atom_concat(KytheJsonPathPrefix, KytheOutSuffix, KytheJsonPath)),
    atom_concat(KytheJsonPathPrefix, EntriesSuffix, KytheEntriesPath),
    write_atomic_file(write_to_protobuf(EntriesCmd, SrcPath, KytheJsonPath), KytheEntriesPath),
    log_if(true, 'Finished output ~q (~q) to ~q (~q)', [SrcPath, SrcFqn, KytheEntriesPath, KytheJsonPath]),
    !.
process_module_from_src_impl(_Opts, KytheJsonPath, SrcPath, SrcFqn, _Symtab0, _Symtab) :-
    %% TODO: delete this catch-all clause
    type_error(process_module_from_src_impl, [KytheJsonPath, SrcPath, SrcFqn]),
    fail.

%! builtins_symtab_extend(+FqnType:list(pair), +SrcFqn:atom, Symtab0:dict, +Symtab:dict) is det.
%% Add the builtins to the symtab with the current  SrcFqn.
builtins_symtab_extend([], _SrcFqn, Symtab, Symtab).
builtins_symtab_extend([Name-Type|FqnTypes], SrcFqn, Symtab0, Symtab) :-
    atomic_list_concat([SrcFqn, Name], '.', NameExt),
    put_dict(NameExt, Symtab0, Type, Symtab1),
    builtins_symtab_extend(FqnTypes, SrcFqn, Symtab1, Symtab).

validate_symtab(Symtab) :-
    must_once(dict_pairs(Symtab, symtab, SymtabPairs)),
    maplist(validate_symtab_pair, SymtabPairs).

validate_symtab_pair(Fqn-Type) :-
    must_be(atom, Fqn),
    must_be(list, Type).

%! path_batch_suffix(+Path:atom, +Opts:list, -PathWithSuffix:atom) is semidet.
%% Generate a path with the batch suffix, if the batch_suffix option
%% is specified (non-empty); fails if batch_suffix isn't specified.
path_batch_suffix(Path, Opts, PathWithSuffix) :-
    opt(Opts, batch_suffix(BatchSuffix)),
    BatchSuffix \= '',
    atomic_list_concat([Path, BatchSuffix], PathWithSuffix).

%! reorder_kythefacts_1(+KytheFacts1, +SymtabKytheFact, -KytheFacts1a) is det.
%% Put the facts into the order that is expected when reading them in
%% (this is for efficiency; no need to read unneeded stuff).
reorder_kythefacts_1(KytheFacts1, SymtabKytheFact, KytheFacts1a) :-
    KytheFacts1 = [VersionKytheFact, KindFile, Encoding, Text|Rest],
    KytheFacts1a = [VersionKytheFact, KindFile, Encoding, Text, SymtabKytheFact|Rest].

%! clean_kythe_facts(-KytheFacts0:list, +KytheFacts:list) is det.
%% Clean the Kythe facts so that they're acceptable to Kythe verifier
%% and other downstream processing (e.g., remove inconsistent "kind" info).
%% TODO: See https://github.com/kythe/kythe/issues/2381
clean_kythe_facts(KytheFacts0, KytheFacts) :-
    kythe_kinds(KytheFacts0, kinds{}, KytheFacts2, Kinds),
    dict_pairs(Kinds, _, KindsPairs),
    maplist(clean_kind, KindsPairs, Kinds2),
    %% Kinds2 must come *after* KytheFacts - read_cache_and_validate/5.
    append(KytheFacts2, Kinds2, KytheFacts).

%! kythe_kinds(+Facts:list, +KindsIn:dict, -FactsOut, -KindsOut:dict) is det.
%% Selects '/kythe/node/kind' facts and puts them into KindsOut
%% (except for some "special" facts: anchor, package, file).
%% The `Kindsout` dict is keyed by the `Source` values of the 'kind' facts
%% and contains a set of all the fact_values.
kythe_kinds([], Kinds, [], Kinds).
kythe_kinds([json{fact_name:'/kythe/node/kind', fact_value:KindValue64, source:Source}|Facts],
            KindsIn, FactsOut, KindsOut) :-
    base64(KindValue, KindValue64),
    KindValue \= 'anchor',  % should never have another kind
    KindValue \= 'package', % should never have another kind
    KindValue \= 'file',    % This is special (see read_cache_and_validate/5)
    !,
    term_to_canonical_atom(Source, SourceAtom),
    get_dict_default(SourceAtom, KindsIn, [], KindSeen),
    type_add_element(KindSeen, KindValue, KindSeen2),
    put_dict(SourceAtom, KindsIn, KindSeen2, Kinds2),
    kythe_kinds(Facts, Kinds2, FactsOut, KindsOut).
kythe_kinds([Fact|Facts], Kinds, [Fact|FactsOut], KindsOut) :-
    kythe_kinds(Facts, Kinds, FactsOut, KindsOut).

%! clean_kind(+SourceAtom-Kinds:pair, -Fact:dict) is det.
%% Clean a single item, creating its JSON representation.
clean_kind(SourceAtom-Kinds,
           json{source:Source, fact_name:'/kythe/node/kind', fact_value:Kind64}) :-
    %% See kyfact//3.
    term_to_atom(Source, SourceAtom),
    (  Kinds = [Kind]
    -> true
    ;  must_once(
           maplist(precedence_and_kind, Kinds, PKs)),
       keysort(PKs, [_-Kind|_]),
       log_if(true, 'Cleaned kind: ~q->~q for ~q', [Kinds, Kind, Source])
    ),
    base64(Kind, Kind64).

%! precedence_and_kind(+Kind, -Precedence-Kind:pair) is det.
%% Map each kind to a pair with it precedence (for keysort)
precedence_and_kind(Kind, Precedence-Kind) :-
    must_once(kind_precedence(Kind, Precedence)).

%! kind_prededence(+Kind, -Precedence) is det.
%% The precedence for each 'kind' that we output.
%% This is used for "disambiguating" when there are multiple 'kind's
%% for a node, with the lowest (most negative) precedence being chosen).
%% See also https://github.com/kythe/kythe/issues/2381
kind_precedence(file, -100).
kind_precedence(package, -99).
kind_precedence(anchor, -98).
kind_precedence(variable, -80).
kind_precedence(record, -50).
kind_precedence(function, -49).

%! write_facts(+KytheFacts, +KytheStream) is det.
write_facts(KytheFacts, KytheStream) :-
    %% write(KytheStream, "%% === Kythe ==="), nl(KytheStream),
    maplist(json_write_dict_nl(KytheStream), KytheFacts).

%! write_batch_symtab(+Symtab, +Version, +KytheStream) is det.
write_batch_symtab(Symtab, Version, KytheStream) :-
    format(KytheStream, '~k.~n~k.~n', [Version, Symtab]).

%! write_to_protobuf(+EntriesCmd, +SrcPath, +KytheJsonPath, +KytheEntriesPath) is det.
write_to_protobuf(EntriesCmd, SrcPath, KytheJsonPath, KytheEntriesPath) :-
    atomic_list_concat(
        [% "set -o pipefail; ",  %% TODO: use bash
         "egrep -v '^{\"fact_name\":\"/pykythe/symtab\"' <",
         KytheJsonPath,
         " | ",
         EntriesCmd,
         " --read_format=json",
         " >", KytheEntriesPath],
         Cmd),
    do_if(trace_file(SrcPath), dump_term('CMD-cvt', Cmd)),
    must_once_msg(shell(Cmd, 0), 'Convert to protobuf failed').

%! run_parse_cmd(+Opts, +SrcPath, +SrcFqn, -OutPath) is det.
%% Run the parse command into a temporary file. (The temp file is
%% automatically deleted on graceful termination.)
%% An alternative would be to run the parse command as a process, into
%% a a pipe. This needs more memory, is more complicated to manage,
%% and is a bit more difficult to debug.
run_parse_cmd(Opts, SrcPath, SrcFqn, OutPath) :-
    must_once_msg(ground(Opts), 'Invalid command line options'),
    opts(Opts, [python_version(PythonVersion), parsecmd(ParseCmd),
                kythe_corpus(KytheCorpus), kythe_root(KytheRoot)]),
    must_once_msg(memberchk(PythonVersion, [2, 3]), 'Invalid Python version: ~q', [PythonVersion]),
    tmp_file_stream(OutPath, OutPathStream, [encoding(binary), extension('fqn-ast.pl')]),
    do_if(false, ( % TODO: delete
                   re_replace("/"/g, "@", SrcPath, SrcPathSubs),
                   atomic_list_concat(['/tmp/pykythe-parser-output--', SrcPathSubs], TmpParserOutput),
                   pykythe_utils:safe_delete_file(TmpParserOutput),
                   link_file(OutPath, TmpParserOutput, hard))),
    close(OutPathStream),
    atomic_list_concat(
        [ParseCmd,
         " --kythe_corpus='", KytheCorpus, "'",
         " --kythe_root='", KytheRoot, "'",
         " --python_version='", PythonVersion, "'",
         " --srcpath='", SrcPath, "'",
         " --module='", SrcFqn, "'",
         " --out_fqn_ast='", OutPath, "'"],
        Cmd),
    do_if(trace_file(SrcPath), dump_term('CMD-parse', Cmd)),
    %% TODO: An alternative way of doing the following is to have
    %% ParseCmd output to stdout and then get it by:
    %%   process_create(ParseCmd, ParseCmdArgs, [stdout(pipe(CmdPipe))]),
    %%   my_json_read_dict(CmdPipe, ...), ...
    must_once_msg(shell(Cmd, 0), 'Parse failed').

%! version_as_kyfact(+Version, +Meta, -KytheFactsAsJsonDict) is det.
%% Convert the version into a Kythe fact.
version_as_kyfact(Version, Meta,
                  json{source: json{language: Meta.language,
                                    corpus: Meta.kythe_corpus,
                                    root: Meta.kythe_root},
                       fact_name: '/pykythe/version',
                       fact_value: VersionStr64}) :-
    base64(Version, VersionStr64).

%! symtab_as_kyfact(+Symtab, +Meta, -KytheFactAsJsonDict) is det.
%% Convert the symtab into a Kythe fact.
%% The entire symtab is output, including all imported symbols.
%% Note that the vaue is an unencoded string, for performance (reading and writing).
symtab_as_kyfact(Symtab, Meta,
                 json{source: Source,
                      fact_name: '/pykythe/symtab',
                      fact_value: SymtabStr}) :-
    %% too slow: base64_term(SymtabStr64, Symtab),
    term_string(Symtab, SymtabStr),
    %% TODO: see also kyfile//1 Source which has only `path` (not `language`):
    Source = json{path: Meta.path, language: Meta.language}.

%! symtab_pykythe_types(+Symtab)//[kyfact,file_meta] is det.
%% Generate /pykythe/type facts from the symtab (for debugging).
symtab_pykythe_types(Symtab) -->>
    Meta/file_meta,
    { atomic_list_concat([Meta.src_fqn, '.'], SrcFqnDot) },
    { dict_pairs(Symtab, symtab, SymtabPairs) },
    include_kyfact(starts_with_kyfact(SrcFqnDot), SymtabPairs).

%! starts_with_kyfact(+Prefix, +Fqn-Type:pair)//[kyfact,file_meta] is semidet.
%% Generate kyfact if its FQN (in symtab) starts with Prefix
starts_with_kyfact(Prefix, Fqn-Type) -->>
    { atom_concat(Prefix, _Fqn2, Fqn) },
    %% If we don't want the builtins, do this test:
    %%   builtins_pairs(BuiltinsPairs), % inefficient - should use a dict
    %%   \+ memberchk(Fqn2-_, BuiltinsPairs),
    { term_to_canonical_atom(Type, TypeAsAtom) },
    signature_node(Fqn, FqnSource),
    kyfact(FqnSource, '/pykythe/type', TypeAsAtom).

%! read_nodes(+FqnExprPath:atom, -Nodes, -Meta:dict) is det.
%% Read the JSON node tree (with FQNs) into Nodes and file meta-data into Meta.
read_nodes(FqnExprPath, Nodes, Meta) :-
    open(FqnExprPath, read, FqnExprStream),
    read_term(FqnExprStream, MetaDict, []),
    read_term(FqnExprStream, NodesDict, []),
    %% sanity check that capitalized strings were quoted:
    must_once(ground(MetaDict)),
    must_once(ground(NodesDict)),
    simplify_meta(MetaDict, Meta),
    simplify_ast(NodesDict, Nodes).

%! simplify_meta(+MetaDictJson:dict, -Meta:dict) is det.
%% Simplify the file meta-data. The argument is the Prolog dict form
%% of the first JSON item (see ast_cooked.Meta).
simplify_meta(MetaDictJson, Meta) :-
    MetaDictJson = json{
        kind: 'Meta',
        slots: json{
            kythe_corpus: KytheCorpus,
            kythe_root: KytheRoot,
            path: Path,
            language: Language,
            contents_base64: ContentsBase64,
            sha1: Sha1,
            encoding: Encoding}},
    canonical_path(Path, CanonicalPath),
    %% For debugging, might want to use file_contents_base64:"LS0t",
    %%     derived from:
    %%     base64('---', 'LS0t').
    %% Note that keys 'src_fqn', 'pythonpaths', 'opts', 'version' get added later
    Meta = meta{kythe_corpus: KytheCorpus,
                kythe_root: KytheRoot,
                path: CanonicalPath,
                language: Language,
                encoding: Encoding,
                file_contents_base64: ContentsBase64,
                sha1: Sha1,
                src_fqn: _,
                pythonpaths: _,
                opts: _,
                version: _}.

%! simplify_ast(+Json, -Prolog) is det.
%% Simplify the JSON term into more specific dicts, each one
%% distinguished by its tag. The input dicts for base types (str, int,
%% etc.) are turned into simpler functors.
simplify_ast([], []).
simplify_ast([V|Vs], Values) :-
    maplist(simplify_ast, [V|Vs], Values).
%% Originally, pod._as_json_dict_full output str, int as wrapped items
%% (like bool), but removing the wrapper gave an overall 10%
%% performance improvement.
simplify_ast(Value, Value) :- atom(Value), !.
simplify_ast(Value, Value) :- integer(Value), !.
simplify_ast(json{kind: 'bool', value: Value}, bool(Value)).
simplify_ast(json{kind: 'None'}, none). % Shouldn't be generated by pod.PlainOldDataExtended.as_json_dict
simplify_ast(json{kind: 'dict', items: Items}, Value) :-
    dict_pairs(Items, _, ItemPairs),
    maplist(simplify_ast_slot_pair, ItemPairs, ItemPairs2),
    dict_pairs(Value, dict, ItemPairs2).
simplify_ast(json{kind: 'Exception', value:ValueStr}, exception(ValueStr)).
simplify_ast(json{kind: Kind, slots: Slots}, Value) :-
    dict_pairs(Slots, _, SlotPairs),
    maplist(simplify_ast_slot_pair, SlotPairs, SlotPairs2),
    dict_pairs(Value, Kind, SlotPairs2).

%! simplify_ast_slot_pair(+KeyValue:pair, -KeyValue2:pair) is det.
simplify_ast_slot_pair(Key-Value, Key-Value2) :-
    simplify_ast(Value, Value2).

%! process_nodes(+Nodes, +SrcInfo:dict, -KytheFacts:list, -Exprs:list, +Meta:dict) is det.
%% Wrapper for process_nodes//[kyfact,expr,file_meta].
%% TODO: separate KytheFacts into those that require de-duping and
%%       those that can be simply appended, to minimize the final
%%       de-dup.
process_nodes(Node, SrcInfo, KytheFacts, Exprs, Meta) :-
    process_nodes_impl(Node, SrcInfo, KytheFacts1, [], Exprs, [], Meta), % phrase(process_nodes(Node), KytheFacts, Exprs, Meta)
    %% TODO: don't preserve order (for debugging) - use sort/2 to dedup:
    list_to_set(KytheFacts1, KytheFacts).

%! process_nodes_impl(+Nodes)//[kyfact,expr,file_meta] is det.
%% Traverse the Nodes, accumulating in KytheFacts (mostly anchors) and
%% Expr (which will be traversed later, to fill in dynamically created
%% attribtes (e.g., self.foo).
process_nodes_impl(Node, SrcInfo) -->>
    kyfile(SrcInfo),
    kynode(Node, _Expr).

%! kyfile(+SrcInfo)//[kyfact,file_meta] is det.
%% Generate the KytheFacts at the file level.
kyfile(SrcInfo) -->>
    %% TODO: output x-numlines, x-html ?
    Meta/file_meta,
    { must_once(Meta.path == SrcInfo.src) },
    { Source = json{path: Meta.path} },
    %% If the following is changed, also change the validation
    %% in process_module_cached_impl/7.
    kyfact(Source, '/pykythe/version', Meta.version),
    kyfact(Source, '/pykythe/text/sha1', Meta.sha1),
    kyfact(Source, '/kythe/node/kind', 'file'),
    kyfact(Source, '/kythe/text/encoding', Meta.encoding),
    kyfact_base64(Source, '/kythe/text', Meta.file_contents_base64),
    kyedge_fqn(Source, '/kythe/edge/childof', SrcInfo.src_fqn),
    %% Kythe's "package" is the equivalent of Python's "module".
    %% (There is no equivalent of Python's "package" ... we just use
    %% ref/imports on the import statements.)
    kyfact_signature_node(SrcInfo.src_fqn, '/kythe/node/kind', 'package').

%! kynode(+Node:json_dict, -Type)//[kyfact,expr,file_meta] is det.
%% Extract anchors (with FQNs) from the the AST nodes.  The anchors go
%% into accumulator 'kyfact' and the expressions (for further
%% processing) go into accumulator 'expr'. The predicate returns a
%% "type", which is used to populate the right-hand-sides of assign/2
%% terms in the 'expr' accumulator.

%% (The "type" is in a list, corresponding to a union of types.)

%% For nodes that can't appear on the right-hand side of an
%% assignment, the "type" is stmt(...) or unused_XXX(...). These
%% values aren't used anywhere; they're simply to help with debugging
%% and will cause an error in eval_assign_expr//2 if they appear on
%% the r.h.s. of an assignment.

%% For descriptions of the various types of Node, and how they relate
%% to the raw AST, see ast_cooked.py.

%% [], [_|_], bool(_), dict(_), 'Astn'{...}' are all handled by
%% higher-level nodes. (int, str are already unwrapped)
%%   (e.g., 'Astn'{start: Start, end: End, value: Value}}
%%   in node_astn/4, which is used by 'ArgumentNode', 'AtomDotNode', etc.;
%%   str(_) is used by 'Class', 'Func', etc.)

%% assign/2 facts are made up of a left-hand-side (assigned-to) and a
%% right-hand-side (expression). These correspond to the LHS and RHS
%% of an expression, and have a few variants (not that var(...) on
%% the RHS will typically be reduced to a type by evaluation):
%%   assign([var_binds(a)], [var(b)]) corresponds to the statement `a = b`
%%   assign([var_binds(a)], []) corresponds to the definition of a
%%       name, e.g. the `a` in `def foo(a)`
%%   assign([dot_op_binds(var(a), b)], [var(c)]) corresponds to
%%       `a.b = c`
%%   assign([subscr_op_binds(var(a)], [var(b)] corresponds to
%%       `a[i] = c` (the `i` isn't used in deriving type information)
%% expr/1 facts are like assign/2 but with nothing to assign to.
%% expr([]) is a no-op.

%% See comments at the top of this file on union and single types.

%% The following are handled by the container (e.g., ImportFromStmt):
%%   AsNameNode
%%   NameRawNode  (from DottedNameNode, ImportFromStmt, etc.)
%%   NameNode

%% All the clauses have cuts become some are complex enough that the
%% Prolog compiler can't tell that they're disjoint. Also, there's a
%% catch-all at the end for in case one has been missed.
kynode('AnnAssignStmt'{left_annotation: LeftAnnotation, expr: Expr, left: Left},
       [stmt(annassign)]) -->> !,
    %% Corresponds to `expr_stmt: testlist_star_expr annassign`.
    expr_normalized(Expr),
    assign_normalized(Left, LeftAnnotation).
kynode('ArgumentNode'{name: NameAstn, arg: Arg},
       [todo_arg(Name, ArgType)]) -->> !,
    %% Corresponds to `argument: test '=' test`.  ast_raw creates
    %% ArgumentNode only for `test '=' test`; all other cases just
    %% generate the expr (or similar)
    %% TODO: match Name to func def param
    { node_astn(NameAstn, _, _, Name) },
    kynode(Arg, ArgType).
kynode('AssertStmt'{items: Items},
       [stmt(assert)]) -->> !,
     %% Corresponds to `assert_stmt`.
     maplist_kyfact_expr(expr_normalized, Items).
kynode('AssignExprStmt'{expr: Expr, left: Left},
       [stmt(assign)]) -->> !,
    assign_normalized(Left, Expr).
kynode('AtomCallNode'{args: Args, atom: Atom},
       [call(AtomType, ArgsType)]) -->> !,
    kynode(Atom, AtomType),
    maplist_kynode(Args, ArgsType).
kynode('AtomDotNode'{atom: Atom, binds: bool('False'), attr_name: AttrNameAstn},
       [dot_op(AtomType, astn(Start, End, AttrName))]) -->> !,
    %% TODO: eval_atom_dot_op_single//3 creates /kythe/edge/ref ...
    %%       the edge probably should be created here and added to the
    %%       dot_op term.
    { node_astn(AttrNameAstn, Start, End, AttrName) },
    kynode(Atom, AtomType).
kynode('AtomDotNode'{atom: Atom, binds: bool('True'), attr_name: AttrNameAstn},
       [dot_op_binds(AtomType, astn(Start,End,AttrName))]) -->> !,
    %% TODO: eval_atom_dot_op_binds_single//3 creates
    %%       /kythe/edge/defines/binding ...  the edge probably should
    %%       be created here and added to the dot_op_binds term.
    { node_astn(AttrNameAstn, Start, End, AttrName) },
    kynode(Atom, AtomType).
kynode('AtomSubscriptNode'{atom: Atom,
                           binds: bool('False'),
                           subscripts: Subscripts},
       [subscr_op(AtomType)]) -->> !,
    kynode(Atom, AtomType),
    maplist_kynode(Subscripts, _).
kynode('AtomSubscriptNode'{atom: Atom,
                           binds: bool('True'),
                           subscripts: Subscripts},
       [subscr_op_binds(AtomType)]) -->> !,
    kynode(Atom, AtomType),
    maplist_kynode(Subscripts, _).
kynode('AugAssignStmt'{augassign: _OpAstn, expr: Expr, left: Left},
       [stmt(augassign)]) -->> !,
    %% { node_astn(OpAstn, _, _, _Op) },
    expr_normalized(Left),
    expr_normalized(Expr).
kynode('BreakStmt'{},
       [stmt(break)]) -->> !, [ ].
kynode('Class'{bases: Bases, fqn: Fqn, name: NameAstn},
       [class_type(Fqn, BasesType)]) -->> !,
    kyanchor_node_kyedge_fqn(NameAstn, '/kythe/edge/defines/binding', Fqn),
    kyfacts_signature_node(Fqn,
                           ['/kythe/node/kind'-'record',
                            '/kythe/subkind'-'class']),
    maplist_kynode(Bases, BasesType),
    [ class_type(Fqn, BasesType) ]:expr.
kynode('CompFor'{for_astn: _ForAstn,
                 for_exprlist: ForExprlist,
                 in_testlist: InTestlist,
                 comp_iter: CompIter},
       [todo_compfor(iter:CompIterType,
                     for:ForExprlistType,
                     in:InTestlistType)]) -->> !,
    kynode(ForExprlist, ForExprlistType),
    kynode(InTestlist, InTestlistType),
    kynode(CompIter, CompIterType).
kynode('CompIfCompIterNode'{value_expr: ValueExpr, comp_iter: CompIter},
       [todo_compifcompiter(ValueExprType, CompIterType)]) -->> !,
    kynode(ValueExpr, ValueExprType),
    kynode(CompIter, CompIterType).
kynode('ContinueStmt'{},
       [stmt(continue)]) -->> !, [ ].
kynode('DecoratedStmt'{items: Items},
       [todo_decorated(ItemsType)]) -->> !,
    maplist_kynode(Items, ItemsType).
kynode('DecoratorDottedNameNode'{items: Items},
       [todo_decorator_dottedname(ItemsType)]) -->> !,
    maplist('NameRawNode_astn_and_name', Items, _, ItemsType).
kynode('DecoratorsNode'{items: Items},
       [todo_decorators(ItemsType)]) -->> !,
    maplist_kynode(Items, ItemsType).
kynode('DelStmt'{items: Items},
       [stmt(del)]) -->> !,
    maplist_kyfact_expr(expr_normalized, Items).
kynode('DictGenListSetMakerCompFor'{value_expr: ValueExpr, comp_for: CompFor},
       [todo_dictgen(ValueExprType, CompForType)]) -->> !,
    kynode(ValueExpr, ValueExprType),
    kynode(CompFor, CompForType).
kynode('DictKeyValue'{items: Items},
       [todo_dictkeyvaluelist(ItemsType)]) -->> !,
    maplist_kynode(Items, ItemsType).
kynode('DictSetMakerNode'{items: Items},
       [todo_dictset(ItemsType)]) -->> !,
    maplist_kynode(Items, ItemsType).
kynode('EllipsisNode'{},
       [ellipsis]) -->> !, [ ].
kynode('ExceptClauseNode'{expr: Expr, as_item: AsItem},
       [stmt(except)]) -->> !,
    kynode(Expr, ExprType),
    kynode(AsItem, AsItemType),
    (  { AsItemType = omitted }
    -> [ expr(ExprType) ]:expr
    ;  [ assign(AsItemType, ExprType) ]:expr
    ).
kynode('ExprListNode'{items: Items},
       [todo_exprlist(ItemsType)]) -->> !,
    maplist_kynode(Items, ItemsType).
kynode('ExprStmt'{expr: Expr},
       [stmt(assign)]) -->> !,
    kynode(Expr, ExprType),
    [ expr(ExprType) ]:expr.
kynode('FileInput'{scope_bindings: _ScopeBindings, stmts: Stmts, path: _Path},
       [stmt(file)]) -->> !,
    %% kynode(ScopeBindings, _),
    maplist_kynode(Stmts, _).
kynode('ForStmt'{for_exprlist: ForExprlist,
                 in_testlist: InTestlist,
                 suite: Suite,
                 else_suite: ElseSuite},
       [stmt(for)]) -->> !,
    kynode(ElseSuite, _),       % kynode(ElseSuite, [stmt(_)])
    kynode(ForExprlist, _),
    kynode(InTestlist, _),
    kynode(Suite, _).
kynode('Func'{fqn: Fqn,
              name: NameAstn,
              parameters: Parameters,
              return_type: ReturnType},
       [function_type(Fqn, [ReturnTypeType])]) -->> !,
    kyanchor_node_kyedge_fqn(NameAstn, '/kythe/edge/defines/binding', Fqn),
    kyfact_signature_node(Fqn, '/kythe/node/kind', 'function'),
    maplist_kynode(Parameters, _),
    kynode(ReturnType, ReturnTypeType),
    [ function_type(Fqn, ReturnTypeType) ]:expr.
kynode('GlobalStmt'{items: Items},
       [stmt(global)]) -->> !,
    maplist_kyfact_expr(expr_normalized, Items).
kynode('IfStmt'{eval_results: EvalResults, items: Items},
       [stmt(if)]) -->> !,
    kynode_if_stmt(EvalResults, Items).
kynode('ImportFromStmt'{from_dots: FromDots,
                        import_part: ImportPart}, Type) -->> !,
    %% The parser doesn't output a field if it's None, so add
    %% from_name and recurse.
    kynode('ImportFromStmt'{from_dots: FromDots,
                            from_name: 'DottedNameNode'{items:[]},
                            import_part: ImportPart},
           Type).
kynode('ImportFromStmt'{from_dots: FromDots,
                        from_name: FromName,
                        import_part: 'ImportAsNamesNode'{items: ImportPartItems}},
       [unused_import_from(Types)]) -->> !,
    maplist_kyfact_expr(kyImportFromStmt(FromDots, FromName), ImportPartItems, Types).
kynode('ImportFromStmt'{from_dots: FromDots,
                        from_name: FromName,
                        import_part: 'StarFqn'{star:StarAstn, fqn:StarFqn}},
       Type) -->> !,
    %% TODO: process this properly
    ImportPartItems = [
        'AsNameNode'{as_name:'NameBindsFqn'{fqn:StarFqn, name:StarAstn},
                     name:'NameRawNode'{name:StarAstn}}],
    ImportPart = 'ImportAsNamesNode'{items: ImportPartItems},
    kynode('ImportFromStmt'{from_dots: FromDots, from_name: FromName, import_part: ImportPart}, Type).
kynode('ImportNameFqn'{dotted_as_names: 'ImportDottedAsNamesFqn'{items: DottedAsNames}},
       [unused_import(DottedAsNamesType)]) -->> !,
    maplist_kyfact_expr(kyImportDottedAsNamesFqn, DottedAsNames, DottedAsNamesType).
kynode('ListMakerNode'{items: Items},
       [list_make(ItemsType)]) -->> !,
    maplist_kynode(Items, ItemsType).
%% 'NameBindsFqn' is only for 'AssignExprStmt' -- for import statements,
%% it's handled separately.
%% TODO: special case this within processing of AssignExprStmt?
kynode('NameBindsFqn'{fqn: Fqn, name: NameAstn},
       [var_binds(Fqn)]) -->> !,  %% result is same as NameRefFqn
    kyanchor_node_kyedge_fqn(NameAstn, '/kythe/edge/defines/binding', Fqn), % only difference from NameRef
    kyfact_signature_node(Fqn, '/kythe/node/kind', 'variable').
kynode('NameBindsGlobalFqn'{fqn: Fqn, name: NameAstn},
       [var_binds(Fqn)]) -->> !,  %% result is same as NameRefFqn
    kyanchor_node_kyedge_fqn(NameAstn, '/kythe/edge/defines/binding', Fqn), % only difference from NameRef
    kyfact_signature_node(Fqn, '/kythe/node/kind', 'variable').
kynode('NameBindsGlobalUnknown'{fqn_scope: FqnScope, name: NameAstn},
       [var_binds_lookup(FqnScope, NameAstn)]) -->> !,
    [ ].  % The defines/binding edge is added in eval_single_type//2.
kynode('NameRefFqn'{fqn: Fqn, name: NameAstn},
       [var(Fqn)]) -->> !, % result is same as NameBinds
    kyanchor_node_kyedge_fqn(NameAstn, '/kythe/edge/ref', Fqn). % only difference from NameBindsFqn
kynode('NameRefGenerated'{fqn: Fqn},
       [var(Fqn)]) -->> !, %% result is same as NameBinds
    %% TODO: This occurs inside TypedArgNode, which needs to be implemented.
    [ ].  % E.g., the implicit type for `self`.
kynode('NameRefUnknown'{fqn_scope: FqnScope, name: NameAstn},
       [var_lookup(FqnScope, NameAstn)]) -->> !,
    [ ].  % The ref edge is added in eval_single_typeeval//2.
kynode('NonLocalStmt'{items: Items},
       [stmt(nonlocal)]) -->> !,
    maplist_kyfact_expr(expr_normalized, Items).
kynode('NumberComplexNode'{astn: _Astn}, ComplexType) -->> !,
    { builtins_symtab_primitive(complex, ComplexType) }.
kynode('NumberFloatNode'{astn: _Astn}, FloatType) -->> !,
    { builtins_symtab_primitive(float, FloatType) }.
kynode('NumberIntNode'{astn: _Astn}, IntType) -->> !,
    { builtins_symtab_primitive(int, IntType) }.
kynode('OmittedNode'{},
       [omitted]) -->> !, [ ].
kynode('OpNode'{args: Args, op_astns: OpAstns},
       [call_op(OpAstns, ArgsType)]) -->> !,
    maplist_kynode(Args, ArgsType).
kynode('PassStmt'{},
       [stmt(break)]) -->> !, [ ].
kynode('RaiseStmt'{items: Items},
       [stmt(raise)]) -->> !,
    maplist_kynode(Items, _).
kynode('StarNode'{},
       [star]) -->> !, [ ]. % TODO: can we get rid of this in ast_cooked?
kynode('Stmts'{items: Items},
       [todo_expr(stmts)]) -->> !,
    maplist_kynode(Items, _).
kynode('StringNode'{astns: _Astns}, StrType) -->> !,
    { builtins_symtab_primitive(str, StrType) }.
kynode('StringBytesNode'{astns: _Astns}, BytesType) -->> !,
    { builtins_symtab_primitive(bytes, BytesType) }.
%% subscript only occurs inside subscr_op or subscr_op_binds and is
%% ignored (we only care about the resulting type)
kynode('SubscriptNode'{expr1: Expr1, expr2: Expr2, expr3: Expr3},
       [subscript(Expr1Type, Expr2Type, Expr3Type)]) -->> !,
    kynode(Expr1, Expr1Type),
    kynode(Expr2, Expr2Type),
    kynode(Expr3, Expr3Type).
%% kynode('TnameNode'{name: Name, type_expr: TypeType} isn't needed
%% because it's always inside TypedArgNode.
kynode('TryStmt'{items: Items},
       [stmt(try)]) -->> !,
    maplist_kynode(Items, _).
kynode('TypedArgNode'{tname: 'TnameNode'{name: Name, type_expr: TypeExpr},
                      expr: Expr},
       [todo_typedarg()]) -->> !,
    assign_normalized(Name, TypeExpr),
    expr_normalized(Expr).  %% assign_normalized(Name, Expr) would cause duplicate facts
kynode('WhileStmt'{else_suite: ElseSuite,
                   suite: Suite,
                   test: Test},
       [stmt(while)]) -->> !,
    kynode(ElseSuite, _),
    kynode(Suite, _),
    kynode(Test, _).
kynode('WithItemNode'{item: Item, as_item: AsItem},
       [stmt(with_item)]) -->> !,
    kynode(Item, ItemType),
    kynode(AsItem, AsItemType),
    (  { AsItemType = [omitted] }
    -> [ expr(ItemType) ]:expr
    ;  [ assign(AsItemType, ItemType) ]:expr
    ).
kynode('WithStmt'{items: Items, suite: Suite},
       [stmt(with)]) -->> !,
    maplist_kynode(Items, _),   % handled by WithItemNode
    kynode(Suite, _).
kynode('YieldNode'{items: Items},
       [stmt(yield)]) -->> !,
    maplist_kynode(Items, _).
kynode(X, Ty) -->>              % TODO: delete this catch-all clause
    { type_error(kynode, [X,Ty]) }.

%! kynode_if_stmt(+Results:list, +Items:list)//[kyfact,expr,file_meta] is det.
kynode_if_stmt([], []) -->> [ ]. % No 'else'
kynode_if_stmt([], [ElseItem]) -->>
    kynode(ElseItem, _).
kynode_if_stmt(['EvalResult'{result:bool('True')}|_], [Cond,ThenItem|ElseItems]) -->> % if/elif True
    if_stmt_elses(ElseItems, ElseItemsConds),
    maplist_kynode([Cond, ThenItem | ElseItemsConds], _).
kynode_if_stmt(['EvalResult'{result:bool('False')}|Results], [Cond,_Then|Items]) -->> % if/elif False
    kynode(Cond, _),
    kynode_if_stmt(Results, Items).
kynode_if_stmt(['EvalResult'{exception:_Exc}|Results], [Cond,Item|Items]) -->> % if/elif Exception
    %% We don't know if this is true or not, so assume true and continue with the result
    maplist_kynode([Cond,Item], _),
    kynode_if_stmt(Results, Items).

%! if_stmt_elses(+Items, -ElseItems) is det.
%% Extract the "conds" from an IfStmt (removing the "then"s and "else"s)
if_stmt_elses([], []).
if_stmt_elses([_ElseItem], []).
if_stmt_elses([Cond,_ThenItem|ElseItems], [Cond|ElseItemsConds]) :-
    if_stmt_elses(ElseItems, ElseItemsConds).

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

%! kyImportDottedAsNamesFqn(+DottedName, -DottedAsNamesType)//[kyfact,expr,file_meta] is det.
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
                             top_name: 'NameBindsFqn'{fqn: BindsFqn, name: BindsNameAstn}},
                         unused_import_module_1(BindsFqn, ModuleAndMaybeToken)) -->>
    %% Note that BindsFqn is just the "top name" (e.g., "${FQN}.os" for "os.path")
    %% so we don't need to do anything special for it.
    kyImportDottedAsNamesFqn_comb([], % FromDots
                                  DottedNameItems, BindsFqn, BindsNameAstn,
                                  ModuleAndMaybeToken).
kyImportDottedAsNamesFqn('ImportDottedAsNameFqn'{
                             dotted_name: 'DottedNameNode'{items:DottedNameItems},
                             as_name: 'NameBindsFqn'{fqn: BindsFqn, name: BindsNameAstn}},
                         unused_import_module_2(BindsFqn, ModuleAndMaybeToken)) -->>
    kyImportDottedAsNamesFqn_comb([], % FromDots
                                  DottedNameItems, BindsFqn, BindsNameAstn,
                                  ModuleAndMaybeToken).

%%! kyImportDottedAsNamesFqn_comb(+FromDots, +DottedNameItems:list, +BindsFqn:string, +BindsNameAstn:astn, -ModuleAndMaybeToken)//[kyfact,expr,file_meta] is det.
%%  Combined code for ImportDottedFqn, ImportDottedAsNameFqn.
%% FromDots is list of ImportDotNode{dot:ASTN(Start,End,'.')}
%% DottedNameItems is list of NameRawNode{name:ASTN(Start,End,Name)}
%%  Adds anchors and binding facts for an imported FQN.
%%   for "import os.path.sep as os_path_sep" (which is not valid Python, unless
%%   os.path.sep happens to be a module):
%%      FromDots = []   % Always [] for "import"; "from ... import" can have non-[]
%%      DottedNameItems = [NameRawNode{name:ASTN(os)}, NameRawNode{name:ASTN(path){, NameRawNode{name:ASTN(sep)}}]
%%      BindsNameAstn = ASTN(os_path_sep)  % 'os' if there's no "as" part to the import
%%      BindsFqn = '$FQN.os_path-sep'  % '$FQN.os' if there's no "as" part to the import
%%      ModuleAndMaybeToken = module_and_token('$FQN.os.path', '$DIR/os/path/__init__.pyi', 'sep')
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
                              ModuleAndMaybeToken) -->>
    Meta/file_meta,
    { kyImportDottedAsNamesFqn_dots(FromDots, DottedNameItems, FromDotAstns, DottedNameAstns, FromImportPath) },
    { full_path(FromDots, FromImportPath, Meta.pythonpaths, Meta.path, ModuleAndMaybeToken, FullModulePieces) },
    (  FullModulePieces = ['<unknown>'|FullModulePieces2]
    -> [ImportsEdgeKind, ImportsSep] = ['/kythe/edge/ref/file', '/']
    ;  [ImportsEdgeKind, ImportsSep] = ['/kythe/edge/ref/imports', '.'],
       FullModulePieces2 = FullModulePieces
    ),
    { append(FromDotAstns, DottedNameAstns, DotNameAstns) },
    { reverse(DotNameAstns, ReversedDotNameAstns) },
    dotted_name_imports(ReversedDotNameAstns, FullModulePieces, ImportsEdgeKind, ImportsSep),
    kyanchor_node_kyedge_fqn_pieces(BindsNameAstn, ImportsEdgeKind, ImportsSep, FullModulePieces2),
    kyfact_signature_node(BindsFqn, '/kythe/node/kind', 'variable'),
    kyanchor_node_kyedge_fqn(BindsNameAstn, '/kythe/edge/defines/binding', BindsFqn),
    [ assign_import_module(BindsFqn, ModuleAndMaybeToken) ]:expr.

%! dotted_name_imports(+ReversedDotsAndNames:list, +ModulePieces:list, +ImportsEdgeKind:atom, +ImportsSep:atom)//kyfact,file_meta] is det.
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

%! kyImportFromStmt(+FromDots:list, +FromName, +AsNameNode, +ImportPart)//[kyfact,expr,file_meta] is det.
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
                 'AsNameNode'{name:RawNameAstn, % 'NameRawNode'{name:NameAstn},
                              as_name:'NameBindsFqn'{
                                          fqn: BindsFqn,
                                          name: AsNameAstn}},
                 unused_import_module_3(BindsFqn, ModuleAndMaybeToken)) -->>
    { append(DottedNameItems, [RawNameAstn], DottedNameItemsComb) },
    kyImportDottedAsNamesFqn_comb(FromDots, DottedNameItemsComb, BindsFqn, AsNameAstn,
                                  ModuleAndMaybeToken).

%! 'NameRawNode_astn_and_name'(+DottedNameItem, -DottedName) is det.
%% Process a NameRawNode node into a name
'NameRawNode_astn_and_name'('NameRawNode'{name: NameAstn}, NameAstn, Name) :-
    node_astn(NameAstn, _, _, Name).

%! kyNameRawNode(+Node, -Astn, -Name:atom) is det.
%  Used by DottedNameNode to process a list of NameRawNode into a list of atoms.
kyNameRawNode('NameRawNode'{name: NameAstn}, astn(Start, End, Name), Name) :-
    node_astn(NameAstn, Start, End, Name).

%! kyImportDotNode(+Node, -Astn, -Name:atom) is det.
kyImportDotNode('ImportDotNode'{dot:DotAstn}, astn(Start, End, Dot), Dot) :-
    node_astn(DotAstn, Start, End, Dot).

%! maplist_kynode(+Nodes:list(json_dict), -NodeTypes:list)//[kyfact,expr,file_meta] is det.
%% equivalent to: maplist_kyfact_expr(kynode, Nodes, NodeTypes)
%% TODO: for some reason this fails when maplist meta-predicate is used
%%       (maybe due to handling of _? in a meta-call?)
maplist_kynode([], []) -->> [ ].
maplist_kynode([Node|Nodes], [NodeType|NodeTypes]) -->>
    kynode(Node, NodeType),
    maplist_kynode(Nodes, NodeTypes).

%! assign_normalized(+Left, +Right)//[kyfact,expr,file_meta] is det.
%% Process the Left and Right parts of an assign/2 term, handling
%% things like `omitted` and `ellipsis`.
assign_normalized(Left, Right) -->>
    kynode(Left, LeftType),
    kynode(Right, RightType),
    (  { LeftType = [omitted] }
    -> [ ]
    ;  { RightType = [omitted] ; RightType = [ellipsis] }
    -> [ assign(LeftType, []) ]:expr
    ;  [ assign(LeftType, RightType) ]:expr
    ).

%! expr_normalized(+Right)//[kyfact,expr,file_meta] is det.
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
%%  Access the inner parts of an Astn node.
node_astn(Astn, Start, End, Value) :-
    node_astn0(Astn, Start, End, Value).

%! node_astn0(+AstnNode, -Start, -End, -Value) is semidet.
%! node_astn0(-AstnNode, +Start, +End, +Value) is det.
%% Access the inner parts of an Astn node.
%% See also portray/1 rule for 'Astn' (uses node_astn0/4).
node_astn0('Astn'{start: Start, end: End, value: Value},
           Start, End, Value).

%! kyanchor_kyedge_fqn(+Start:int, +End:int, +EdgeKind:atom, +Fqn:atom)//kyfact,file_meta is det.
kyanchor_kyedge_fqn(Start, End, EdgeKind, Fqn) -->>
    kyanchor(Start, End, Source),
    kyedge_fqn(Source, EdgeKind, Fqn).

%! kyanchor_kyedge_fqn(+Start:int, +End:int, +EdgeKind:atom, +Sep:atom, +FqnPices:list)//kyfact,file_meta is det.
kyanchor_kyedge_fqn_pieces(Start, End, EdgeKind, Sep, FqnPieces) -->>
    { atomic_list_concat(FqnPieces, Sep, Fqn) },
    kyanchor_kyedge_fqn(Start, End, EdgeKind, Fqn).

%! kyanchor_node_kyedge_fqn(+Astn, +EdgeKind:atom, +Fqn:atom, -Token)//[kyfact,file_meta] is det.
kyanchor_node_kyedge_fqn(Astn, EdgeKind, Fqn, Token) -->>
    kyanchor_node(Astn, Source, Token),
    kyedge_fqn(Source, EdgeKind, Fqn).

%! kyanchor_node_kyedge_fqn(+Astn, +EdgeKind:atom, +Fqn:atom)//[kyfact,file_meta] is det.
kyanchor_node_kyedge_fqn(Astn, EdgeKind, Fqn) -->>
    kyanchor_node(Astn, Source),
    kyedge_fqn(Source, EdgeKind, Fqn).

%! kyanchor_node_kyedge_fqn_pieces(+Astn, +EdgeKind:atom, +Sep:atom +FqnPieces:atom)//[kyfact,file_meta] is det.
kyanchor_node_kyedge_fqn_pieces(Astn, EdgeKind, Sep, FqnPieces) -->>
    { atomic_list_concat(FqnPieces, Sep, Fqn)},
    kyanchor_node_kyedge_fqn(Astn, EdgeKind, Fqn).

%! kyanchor_node(+Astn, -Source)/[kyfact,file_meta] is det.
kyanchor_node(Astn, Source) -->>
    kyanchor_node(Astn, Source, _Token).

%! kyanchor_node(+Astn, -Source, -Token)/[kyfact,file_meta] is det.
kyanchor_node(Astn, Source, Token) -->>
    { node_astn(Astn, Start, End, Token) },
    kyanchor(Start, End, Source).

%! kyanchor(+Start, +End, -Source)//[kyfact,file_meta] is det.
%% Create the Kythe facts for an anchor. Source gets the source signature.
kyanchor(Start, End, Source) -->>
    { format(string(Signature), '@~d:~d', [Start, End]) },
    signature_source(Signature, Source),
    %% https://github.com/kythe/kythe/issues/1725
    %% - there is no need to generate kyedge(Source, '/kythe/edge/childof', json{path: Meta.path})
    kyfact(Source, '/kythe/node/kind', 'anchor'),
    kyfact(Source, '/kythe/loc/start', Start),
    kyfact(Source, '/kythe/loc/end', End).

%! kyedge_fqn(+Source, +EdgeKind:atom, +Fqn:atom)//[kyfact,file_meta] is det.
%% High-level create a Kythe edge fact to a target identified by an FQN.
kyedge_fqn(Source, EdgeKind, Fqn) -->>
    signature_node(Fqn, Target),
    kyedge(Source, EdgeKind, Target).

%! kyedge(+Source, +EdgeKind:atom, +Target:atom)//{kyfact,file_meta] is det.
%% Low-level create a Kythe edge fact -- for both Source and Target,
%% corpus and root are filled in from file_meta.
kyedge(Source, EdgeKind, Target) -->>
    Meta/file_meta,
    [ json{source: Source.put(corpus, Meta.kythe_corpus).put(root, Meta.kythe_root),
           edge_kind: EdgeKind,
           target: Target.put(corpus, Meta.kythe_corpus).put(root, Meta.kythe_root),
           fact_name: '/'} ]:kyfact.

%! kyfacts(+Vname, FactValues:list)//[kyfact,file_meta] is det.
%% kyfact over a list of FactName-FactValue
kyfacts(_Vname, []) -->> [ ].
kyfacts(Vname, [FactName-FactValue|FactValues]) -->>
    kyfact(Vname, FactName, FactValue),
    kyfacts(Vname, FactValues).

%! kyfact(+Source, +FactName, +FactValue)//[kyfact,file_meta] is det.
%% Low-level create a Kythe fact or edge -- for Source, corpus and root
%% are filled in from file_meta.
kyfact(Source, FactName, FactValue) -->>
    { base64(FactValue, FactBase64) },
    kyfact_base64(Source, FactName, FactBase64).

%! kyfact_64(+Source, +FactName, +FactBase64)//[kyfact,file_meta] is det.
%% Low-level create a Kythe fact or edge inputting the base64 of the
%% fact value -- for Source, corpus and root are filled in from file_meta.
%% The accumulator takes care of duplicate removal.
kyfact_base64(Source, FactName, FactBase64) -->>
    Meta/file_meta,
    { put_dict([corpus=Meta.kythe_corpus, root=Meta.kythe_root],
               Source, Source2) },
    [ json{source: Source2, fact_name: FactName, fact_value: FactBase64} ]:kyfact.

%! signature_source(+Signature:string, -Source)//[file_meta] is det.
%% Create a Kythe "source" tuple from a Signature string.
signature_source(Signature, Source) -->>
    Meta/file_meta,
    { Source = json{signature: Signature, path: Meta.path, language: Meta.language} }.

%! kyfact_signature_node(+Signature:string, +FactName, +FactValue)//[kyfact,file_meta is det.
kyfact_signature_node(Signature, FactName, FactValue) -->>
    signature_node(Signature, Vname),
    kyfact(Vname, FactName, FactValue).

%! kyfacts_signature_node(+Signature:string, +FactValues:list)//[kyfact,file_meta] is det.
kyfacts_signature_node(Signature, FactValues) -->>
    signature_node(Signature, Vname),
    kyfacts(Vname, FactValues).

%! signature_node(+Signature:string, -Vname)//[file_meta] is det.
%% Create a Kythe "vname" from a Signature string
signature_node(Signature, Vname) -->>
    Meta/file_meta,
    { Vname = json{signature: Signature, language: Meta.language} }.

%%%%%%        %%%%%%%
%%%%%% Pass 2 %%%%%%%
%%%%%%        %%%%%%%

%! assign_exprs(+Exprs:list, +Meta:dict, +Symtab0:dict, -Symtab:dict, -KytheFacts:list) is det.
%% Process a list of Exprs, generating a Symtab (by adding to initial
%% Symtab0) and a list of KytheFacts.
assign_exprs(Exprs, Meta, Symtab0, Symtab, KytheFacts) :-
    assign_exprs_count(1, Exprs, Meta, Symtab0, Symtab, KytheFacts).

%! assign_exprs_count(+Count, +Exprs:list, +Meta:dict, +Symtab0:dict, -Symtab:dict, -KytheFacts:list) is det.
%% Process a list of Exprs, generating a Symtab (by adding to initial
%% Symtab0) and a list of KytheFacts.
%% `Count` tracks the number of passes over Exprs; if too large, the
%% processing stops. In most cases, three passes suffice.
%% TODO: Improved output when too many passes are needed.
%% TODO: Parameterize max number of passes.
assign_exprs_count(Count, Exprs, Meta, Symtab0, Symtab, KytheFacts) :-
    assign_exprs_count_impl(Exprs, Meta, Symtab0, Symtab1, Rej, KytheFacts1), % phrase(assign_exprs_count(...))
    length(Rej, RejLen),
    log_if(true, % RejLen > 0, % TODO: Output Pass# with RejLen = 0 for performance profiling.
           'Process exprs: Pass ~q (rej=~q) for ~q', [Count, RejLen, Meta.path]),
    CountIncr is Count + 1,
    (  (Rej = [] ; CountIncr > 5) % TODO: parameterize.
    -> Symtab = Symtab1,
       KytheFacts = KytheFacts1,
       pairs_keys(Rej, RejKeys),
       log_if(Rej \= [], 'Max pass count exceeded: ~d leaving ~d unprocessed: ~q', [CountIncr, RejLen, RejKeys])
       %% log_if(Rej \= [], 'Rejected: ~q', [Rej])
    ;  assign_exprs_count(CountIncr, Exprs, Meta, Symtab1, Symtab, KytheFacts)
    ).

%! assign_exprs_count_impl(+Exprs, +Meta:dict, +Symtab0:dict, -SymtabWithRej:dict, -Rej:dict, -KytheFacts) is det.
%% Helper for assign_exprs_count, which does the actual processing.
assign_exprs_count_impl(Exprs, Meta, Symtab0, SymtabWithRej, Rej, KytheFacts) :-
    maplist_eval_assign_expr(Exprs, KytheFactsList, [],
                             sym_rej(Symtab0,[]), sym_rej(SymtabAfterEval,Rej), Meta),
    list_to_set(KytheFactsList, KytheFacts),
    %% TODO: is the following needed? The accumulator should have
    %%       already added the types to the symtab.
    foldl(add_rej_to_symtab, Rej, SymtabAfterEval, SymtabWithRej),
    must_once(SymtabAfterEval == SymtabWithRej). % TODO: delete if this is always true.

%! maplist_assign_exprs_eval(+Assign:list)//[kyfact,symrej,file_meta] is det.
%% Process a list of assign or eval nodes.
maplist_eval_assign_expr([]) -->> [ ].
maplist_eval_assign_expr([Assign|Assigns]) -->>
    do_if_file(dump_term('(EVAL_ASSIGN_EXPR)', Assign)),
    eval_assign_expr(Assign),
    symtab_if_file('SYMTAB'),
    maplist_eval_assign_expr(Assigns).

%! assign_expr_eval(+Node)//[kyfact,symrej,file_meta] is det.
%% Process a signle assign/2 or expr/1 node.
eval_assign_expr(assign(Left, Right)) -->> !,
    %% e.g.:
    %% _S = TypeVar('_S')
    %% => assign([var_binds('.home.peter.src.typeshed.stdlib.3.collections._S')], [call([var('.home.peter.src.typeshed.stdlib.3.collections.TypeVar')],[['.home.peter.src.typeshed.stdlib.2and3.builtins.str']])])
    eval_union_type(Right, RightEval),
    eval_union_type(Left, LeftEval),
    log_if_file('ASSIGN(~q=>~q, ~q=>~q', [Left, LeftEval, Right, RightEval]),
    maplist_kyfact_symrej(eval_assign_single(RightEval), LeftEval).
eval_assign_expr(expr(Right)) -->> !,
    eval_union_type(Right, _RightEval).
eval_assign_expr(class_type(Fqn, Bases)) -->> !,
    maplist_kyfact_symrej(eval_union_type, Bases, BasesEval0),
    { clean_class(Fqn, BasesEval0, BasesEval) },
    [ Fqn-[class_type(Fqn, BasesEval)] ]:symrej.
eval_assign_expr(function_type(Fqn, ReturnType)) -->> !,
    eval_union_type(ReturnType, ReturnTypeEval),
    [ Fqn-[function_type(Fqn, ReturnTypeEval)] ]:symrej.
eval_assign_expr(assign_import_module(Fqn, ModuleAndMaybeToken)) -->> !,
    %% Add the module to symtab, and the item it binds to
    { full_module_part(ModuleAndMaybeToken, FullModule) },
    { path_part(ModuleAndMaybeToken, Path) },
    [ FullModule-[module_type(module_alone(FullModule,Path))] ]:symrej,
    [ Fqn-[module_type(ModuleAndMaybeToken)] ]:symrej.
eval_assign_expr(Expr) -->> % TODO: delete this catch-all clause and the cuts above.
    { type_error(assign_expr_eval, Expr) }.

%! eval_assign_single(+Right, +Left)//[kyfact,symrej,file_meta] is det.
%% Helper for a single assignment. The order of args is because of how maplist works.
eval_assign_single(RightEval, var_binds(Fqn)) -->> !,
    %% Anchor and defines/binding edge have already been done by kynode//2.
    [ Fqn-RightEval ]:symrej.
eval_assign_single(RightEval, var(Fqn)) -->> !,
    %% Can occur from subscr_op_binds, e.g. foo[i] = bar
    %% because foo is not in a binding context (it's a ref)_
    %% There should already be a ref edge from kynode//2.
    [ Fqn-RightEval ]:symrej.
eval_assign_single(RightEval, dot_op_binds(AtomType, AttrAstn)) -->> !,
    maplist_kyfact_symrej(eval_assign_dot_op_binds_single(RightEval, AttrAstn), AtomType).
eval_assign_single(RightEval, subscr_op_binds(AtomType)) -->> !,
    maplist_kyfact_symrej(eval_assign_subscr_op_binds_single(RightEval), AtomType).
eval_assign_single(_RightEval, Left) -->>
    memberchk(Left, [var_binds(_), dot_op_binds(_, _), subscr_op_binds(_)]),
    %% l.h.s. is of a form that we can't process.
    !.
eval_assign_single(RightEval, LeftEval) -->> % TODO: delete this catch-all clause and the cuts above.
    { type_error(eval_assign_single, [left=LeftEval, right=RightEval]) }.

%! eval_assign_subscr_op_binds_single(+RightEval, +AtomType) is det.
eval_assign_subscr_op_binds_single(RightEval, var(Fqn)) -->> !,
    [ Fqn-[list_of_type(RightEval)] ]:symrej.
eval_assign_subscr_op_binds_single(_RightEval, _) -->> !.

%! eval_assign_dot_op_binds_single(+RightEval, +AttrAstn, +AtomType) is det.
eval_assign_dot_op_binds_single(RightEval, astn(Start,End,AttrName), class_type(ClassName,_Bases)) -->> !,
    %% TODO: should subclasses that don't override this get anything?
    { atomic_list_concat([ClassName, AttrName], '.', Fqn) },
    [ Fqn-RightEval ]:symrej,
    kyanchor_kyedge_fqn(Start, End, '/kythe/edge/defines/binding', Fqn).
eval_assign_dot_op_binds_single(RightEval, astn(Start,End,AttrName), function_type(FunctionName, _ReturnType)) -->> !,
    { atomic_list_concat([FunctionName, AttrName], ',', Fqn) },
    [ Fqn-RightEval ]:symrej,
    kyanchor_kyedge_fqn(Start, End, '/kythe/edge/defines/binding', Fqn).
eval_assign_dot_op_binds_single(RightEval, astn(Start,End,AttrName), module_type(module_alone(Module,_Path))) -->> !,
    { atomic_list_concat([Module, AttrName], '.', Fqn) },
    [ Fqn-RightEval ]:symrej,
    kyanchor_kyedge_fqn(Start, End, '/kythe/edge/defines/binding', Fqn).
eval_assign_dot_op_binds_single(RightEval, astn(Start,End,AttrName), module_type(module_and_token(Module,_Path,Token))) -->> !,
    { atomic_list_concat([Module, Token, AttrName], '.', Fqn) },
    [ Fqn-RightEval ]:symrej,
    kyanchor_kyedge_fqn(Start, End, '/kythe/edge/defines/binding', Fqn).
%% TODO: should have a catch-all here (see eval_atom_dot_single(astn(Start,End,Attr), AtomSingleType, EvalType))
eval_assign_dot_op_binds_single(RightEval, Astn, LeftEval) -->> % TODO: delete this catch-all clause and the cuts above.
    { type_error(eval_assign_dot_op_binds_single, [left=LeftEval, astn=Astn, right=RightEval]) }.


%! eval_union_type(+Expr, -UnionEvalType)//[kyfact,symrej,file_meta] is det.
%% Evaluate (union) Expr and look it up in the symtab.
eval_union_type(Expr, UnionEvalType) -->>
    maplist_kyfact_symrej_combine(eval_single_type, Expr, UnionEvalType).

%! eval_single_type((+Expr, -UnionEvalType)//[kyfact,symrej,file_meta] is det.
%% Evaluate (non-union) Expr and look it up in the symtab.
eval_single_type(var(Fqn), Type) -->> !,
    %% TODO: could call symtab_lookup(Fqn, UnionEvalType)
    %%       to avoid weird code in symrej_accum: ( Type = [] -> true ; true )
    [ Fqn-Type ]:symrej.
eval_single_type(var_binds_lookup(FqnScope, NameAstn), Type) -->> !,
    resolve_unknown_fqn(FqnScope, NameAstn, ResolvedFqn, Type),
    [ ResolvedFqn-Type ]:symrej,
    kyanchor_node_kyedge_fqn(NameAstn, '/kythe/edge/defines/binding', ResolvedFqn).
eval_single_type(var_lookup(FqnScope, NameAstn), Type) -->> !,
    resolve_unknown_fqn(FqnScope, NameAstn, ResolvedFqn, Type),
    [ ResolvedFqn-Type ]:symrej,
    kyanchor_node_kyedge_fqn(NameAstn, '/kythe/edge/ref', ResolvedFqn).
eval_single_type(var_binds(Fqn), [var_binds(Fqn)]) -->> !,
    [ ].
eval_single_type(class_type(ClassName, Bases0),
                 [class_type(ClassName, Bases)]) -->> !,
    maplist_kyfact_symrej(eval_union_type, Bases0, Bases1),
    { clean_class(ClassName, Bases1, Bases) }.
eval_single_type(function_type(FuncName, ReturnType0),
                 [function_type(FuncName, ReturnType)]) -->> !,
    eval_union_type(ReturnType0, ReturnType).
eval_single_type(module_type(ModuleAndMaybeToken),
                 [module_type(ModuleAndMaybeToken)]) -->> !,
    [ ].
eval_single_type(dot_op(Atom, Astn), EvalType) -->> !,
    eval_union_type(Atom, AtomEval0),
    (  { AtomEval0 = [] }
    -> %% don't know what the atom is, so the best we can do is 'object':
       { builtins_symtab_primitive(object, AtomEval) }
    ;  { AtomEval = AtomEval0 }
    ),
    maplist_kyfact_symrej_combine(eval_atom_dot_single(Astn), AtomEval, EvalType).
eval_single_type(dot_op_binds(Atom, Astn), [dot_op_binds(AtomEval, Astn)]) -->> !,
    eval_union_type(Atom, AtomEval).
eval_single_type(subscr_op_binds(Atom), [subscr_op_binds(AtomEval)]) -->> !,
    %% This is used by eval_asssign_expr, which further processes it.
    maplist_kyfact_symrej_combine(eval_atom_subscr_binds_single, Atom, AtomEval).
eval_single_type(subscr_op(Atom), EvalType) -->> !,
    eval_union_type(Atom, AtomEval0),
    (  { AtomEval0 = [] }
    -> %% don't know what the atom is, so the best we can do is 'object':
       { builtins_symtab_primitive(object, AtomEval) }
    ; { AtomEval = AtomEval0 }
    ),
    maplist_kyfact_symrej_combine(eval_atom_subscr_single, AtomEval, EvalType).
eval_single_type(call(Atom, Parms), EvalType) -->> !,
    eval_union_type(Atom, AtomEval),
    maplist_kyfact_symrej(eval_union_type, Parms, ParmsEval),
    maplist_kyfact_symrej_combine(eval_atom_call_single(ParmsEval), AtomEval, EvalType).
eval_single_type(call_op(_OpAstns, ArgsType), EvalType) -->> !,
    maplist_kyfact_symrej(eval_union_type, ArgsType, _ArgsTypeEval),
    %% See typeshed/stdlib/2and3/operator.pyi
    EvalType = [].
eval_single_type(function_type(Name, ReturnType), [function_type(Name, ReturnTypeEval)]) -->> !,
    eval_union_type(ReturnType, ReturnTypeEval).
eval_single_type(ellipsis, []) -->> !, [ ].
eval_single_type(module(Fqn, Path), [module(Fqn,Path)]) -->> !, [ ].
eval_single_type(omitted, []) -->> !, [ ].

%% TODO: implement the following:
eval_single_type(todo_compfor(iter:_CompIterType, for:_ForExprlistType, in:_InTestlistType), []) -->> !, [ ].
eval_single_type(todo_compifcompiter(_ValueExprType, _CompIterType), []) -->> !, [ ].
eval_single_type(todo_decorated(_ItemsType), []) -->> !, [ ].
eval_single_type(todo_decorator_dottedname(_ItemsType), []) -->> !, [ ].
eval_single_type(todo_decorators(_ItemsType), []) -->> !, [ ].
eval_single_type(todo_dictgen(_ValueExprType, _CompForType), []) -->> !, [ ].
eval_single_type(todo_dictkeyvaluelist(_ItemsType), []) -->> !, [ ].
eval_single_type(todo_dictset(_ItemsType), []) -->> !, [ ].
eval_single_type(todo_dottedname(_ItemsType), []) -->> !, [ ].
eval_single_type(todo_expr(stmts), []) -->> !, [ ].
eval_single_type(todo_typedarg(), []) -->> !, [ ].
eval_single_type(subscript(X1,X2,X3), [subscript(E1,E2,E3)]) -->> !,
    eval_union_type(X1, E1),
    eval_union_type(X2, E2),
    eval_union_type(X3, E3).
eval_single_type(todo_arg(_, _), []) -->> !, [ ].
eval_single_type(list_make(Xs), [list_of_type(Combined)]) -->> !,
    maplist_kyfact_symrej(eval_union_type, Xs, XEs),
    { combine_types(XEs, Combined) }.
eval_single_type(todo_exprlist(_), []) -->> !, [ ].
eval_single_type(Expr, EvalType) -->> % TODO: delete this catch-all clause.
    { type_error(eval_single_type, ['Expr'=Expr, 'EvalType'=EvalType]) }.

%! eval_atom_dot_single(+AttrAstn, +AtomSingleType:ordset, -EvalType:ordset)//[kyfact,symrej,file_meta] is det.
%% Helper for single type-dot-attr.
%% See also https://github.com/python/typeshed/issues/2726
%% TODO: list, set, etc. (from builtins)
%%          but need to handle MutableSequence(Sequence[_T], Generic[_T]), etc.
%%          if their methods arent' @overload-ed in builtins.
%%   -- for all else, use builtins.object as MRO
eval_atom_dot_single(AttrAstn, class_type(ClassName, Bases), EvalType) -->>
    (  { setof(Mro, c3:mro(class_type(ClassName, Bases), Mro), Mros0) }
    -> [ ]
    ;  Mros0 = [[ClassName]]
    ),
    { builtins_symtab_primitive(object, [class_type(Object, _)]) },
    { maplist(ensure_class_mro_object(Object), Mros0, Mros) },
    maplist_kyfact_symrej(resolve_mro_dot(ClassName, AttrAstn), Mros, EvalTypes),
    { combine_types(EvalTypes, EvalType) }.
eval_atom_dot_single(astn(Start,End,Attr),  module_type(module_alone(Module,_Path)), EvalType) -->> !,
    { atomic_list_concat([Module, Attr], '.', FqnAttr) },
    [ FqnAttr-EvalType ]:symrej,
    kyanchor_kyedge_fqn(Start, End, '/kythe/edge/ref', FqnAttr).
eval_atom_dot_single(astn(Start,End,Attr), module_type(module_and_token(Module,_Path,Token)), EvalType) -->> !,
    %% TODO: test case -- see i1.py (III().x)
    { atomic_list_concat([Module, Token, Attr], '.', FqnAttr) },
    [ FqnAttr-EvalType ]:symrej,
    kyanchor_kyedge_fqn(Start, End, '/kythe/edge/ref', FqnAttr).
eval_atom_dot_single(astn(Start,End,Attr), function_type(FunctionName, _ReturnType), EvalType) -->> !,
    { atomic_list_concat([FunctionName, Attr], '.', FqnAttr) },
    [ FqnAttr-EvalType ]:symrej,
    kyanchor_kyedge_fqn(Start, End, '/kythe/edge/ref', FqnAttr).
eval_atom_dot_single(astn(Start,End,Attr), AtomSingleType, EvalType) -->>
    (  atom(AtomSingleType)
    -> { atomic_list_concat([AtomSingleType, Attr], '.', FqnAttr) },
       [ FqnAttr-EvalType ]:symrej,
       %% TODO: if AtomSingleType = function_type(Name, ReturnType)
       %%          builtins_symtab_primitive(function, FunctionType)
       %%          apply dot operator
       kyanchor_kyedge_fqn(Start, End, '/kythe/edge/ref', FqnAttr)
    ;  [ ]
    ).

%! eval_atom_subscr_single(+Expr, -EvalType)//[kyfact,symrej,file_meta] is det. is det.
%% Get the type of applying a subscript operator to a type.
eval_atom_subscr_single(list_of_type(Class), Class) -->> !, [ ].
eval_atom_subscr_single(_, []) -->> [ ].

%! eval_atom_subscr_binds_single(+Expr, -EvalType)//[kyfact,symrej,file_meta] is det.
%% eval_single_type, for binding context of subscr_op_binds
%% This special-cases for a var or '.' and doesn't evaluate it further
%% (eval_single_type does a lookup).
eval_atom_subscr_binds_single(var(Name), [var(Name)]) -->> !.
eval_atom_subscr_binds_single(dot_op(Atom,AttrAstn), DotEvals) -->> !,
    eval_union_type(Atom, AtomEval),
    maplist_kyfact_symrej_combine(subscr_resolve_dot_binds(AttrAstn), AtomEval, DotEvals).
eval_atom_subscr_binds_single(Expr, ExprEval) -->>
    eval_single_type(Expr, ExprEval).

%! subscr_resolve_dot_binds(+AttrAstn, +AtomType, -EvalType) is det.
subscr_resolve_dot_binds(astn(Start,End,Attr), class_type(ClassName,_Bases), [var(DotEval)]) -->> !,
    { atomic_list_concat([ClassName, Attr], '.', DotEval) },
    %% This is within a subscr operator, so it's a ref, not binds:
    kyanchor_kyedge_fqn(Start, End, '/kythe/edge/ref', DotEval).
subscr_resolve_dot_binds(_Astn, _AtomEval, []) -->> [ ].

%! ensure_class_mro_object(+Object, +Mro0, -Mro) is det.
%% If class 'object' (passed in as Object) not in Mro0, then add it
%% at the end. Mro is Mr0 'object' in it.
ensure_class_mro_object(Object, Mro0, Mro) :-
    (  memberchk(Object, Mro0)
    -> Mro = Mro0
    ;  append(Mro0, [Object], Mro)
    ).

%! resolve_mro_dot(+ClassName:atom, +AttrAstn, +Mro:list(atom), -EvalType:ordset(atom))//[kyfact,symrej,file_meta] is det.
%% Using Mro, resolve Attr and add "ref" edge. If it couldn't be
%% resolved, do best guess using ClassName.  EvalType is the resulting
%% type. This never adds to symtab, so symtab lookup can be used to
%% check if an attr exists for a class in the MRO.
resolve_mro_dot(ClassName, astn(Start,End,Attr), Mro, EvalType) -->>
    (  maybe_resolve_mro_dot(Mro, Start, End, Attr, EvalType)
    -> [ ]
    ;  %% Couldn't find it, so assume it's part of ClassName
       { atomic_list_concat([ClassName, Attr], '.', FqnAttr) },
       EvalType = [],
       kyanchor_kyedge_fqn(Start, End, '/kythe/edge/ref', FqnAttr)
    ).

%! maybe_resolve_mro_dot(+Mro:list(atom), +Start:int, +End:int, +Attr:atom, -EvalType:ordset(atom)/[kyfact,symrej,file_meta] is semidet.
%% Using Mro, resolve Attr and add "ref" edge, setting EvalType as the
%% symtab entry. Fail if Attr can't be resolved. We're guaranteed that
%% dot-resolution won't put an entry into symtab, so symtab lookup can
%% be used to check if an attr exists for a class in the MRO.
maybe_resolve_mro_dot([MroBaseName|Mros], Start, End, Attr, EvalType) -->>
    {  atomic_list_concat([MroBaseName, Attr], '.', FqnAttr) },
    (  symtab_lookup(FqnAttr, EvalType)
    -> kyanchor_kyedge_fqn(Start, End, '/kythe/edge/ref', FqnAttr)
    ;  maybe_resolve_mro_dot(Mros, Start, End, Attr, EvalType)
    ).

%! eval_atom_call_single(+Parms, +AtomSingleType, -EvalType:ordset)//[kyfact,symrej,file_meta] is det.
%% Helper for single type-call.
eval_atom_call_single(_Parms, class_type(Fqn,Bases), EvalType) -->>  !,
    %% TODO: MRO for__init__ and output ref to it
    { EvalType = [class_type(Fqn,Bases)] }.
eval_atom_call_single(_Parms, function_type(_, ReturnType), ReturnType) -->>  !,
    [ ].
eval_atom_call_single(_Parms, _AtomSingleType, []) -->> [ ]. % Don't know how to call anything else.

%! resolve_unknown_fqn(+FqnScope, +NameAstn, -ResolvedFqn, -Type)//[symrej,file_meta] is det.
%% Do a "dynamic" lookup of a name, given its "scope" (see
%% NameBindsGlobalUnknown in ast_cooked.py)
resolve_unknown_fqn(FqnScope, NameAstn, ResolvedFqn, Type) -->>
    Meta/file_meta,
    { node_astn(NameAstn, _, _, Name) },
    { atomic_list_concat([FqnScope, Name], '.', Fqn) },
    (  symtab_lookup(Fqn, Type)
    -> { ResolvedFqn = Fqn }
    ;  { FqnScope = Meta.src_fqn }
    -> [ Fqn-Type ]: symrej, % Not found, so add it at module scope
       { ResolvedFqn = Fqn }
    ;  { split_atom(FqnScope, '.', '', FqnScope0) },
       { append(FqnScope1, [_], FqnScope0) },
       { atomic_list_concat(FqnScope1, '.', FqnScope2) },
       resolve_unknown_fqn(FqnScope2, NameAstn, ResolvedFqn, Type)
    ).

%! clean_class(+ClassName:atom, -Bases:list, +BasesCleaned:list) is det.
%% Remove cycles and [] ("Any)" types from a class' of base types.
clean_class(ClassName, Bases, BasesCleaned) :-
    remove_class_cycles(Bases, seen{}.put(ClassName, ''), Bases2),
    exclude(is_empty_list, Bases2, BasesCleaned).

is_empty_list([]).

%! remove_class_cycles(+Bases:list(list), +Seen:dict, +BasesCleaned:List(list)) is det.
%% Ensure that there are no cycles in the base classes of a class (see
%% test cases for examples). Bases0 is the original list of bases;
%% Bases gets the cycle-free bases. Seen is a dict of class names that
%% have been seen so far; it should have an initial value of the class
%% name (the dict values are ignored).
%% The resulting BasesCleaned may have some empty types, which equate to "Any";
%% the assumption is that the caller will remove them.
%% (Hint for understanding this code -- each Base is a union (list) of types.)
remove_class_cycles([], _Seen, []).
remove_class_cycles([Base|Bases], Seen, [Base2|Bases2]) :-
    remove_class_cycles_one(Base, Base2, Seen, Seen2),
    remove_class_cycles(Bases, Seen2, Bases2).

%! remove_class_cycles_one(+Types:list, -TypesOut:list, +Seen:list(atom), -SeenOut:list(atom)) is det.
%% Remove cycles for a single base.
%% Seen is a set of already seen class names.
remove_class_cycles_one([], [], Seen, Seen).
remove_class_cycles_one([Type|Types], TypesOut, Seen, SeenOut) :-
    (  Type = class_type(ClassName, _)
    -> (  get_dict(ClassName, Seen, _)
       -> remove_class_cycles_one(Types, TypesOut, Seen, SeenOut)
       ;  TypesOut = [Type|Types2],
          put_dict(ClassName, Seen, '', Seen2),
          remove_class_cycles_one(Types, Types2, Seen2, SeenOut)
       )
    ;  TypesOut = [Type|Types2],
       remove_class_cycles_one(Types, Types2, Seen, SeenOut)
    ).

%%%%%%              %%%%%%%
%%%%%% Accumulators %%%%%%%
%%%%%%              %%%%%%%

%! add_rej_to_symtab(+FqnRejType:pair, +Symtab0, -Symtab) is det.
%% For Fqn-RejType pairs in FqnRejTypes, add to symtab.
add_rej_to_symtab(Fqn-RejType, Symtab0, Symtab) :-
    get_dict(Fqn, Symtab0, FqnType),
    type_union(FqnType, RejType, CombinedType),
    put_dict(Fqn, Symtab0, CombinedType, Symtab).

%! symrej_accum(+FqnType:pair, +Symtab0Rej0Mod0, +SymtabRejMod) is det.
%% The accumulator for 'symrej'.
%% Tries to unify Key-Type with what's already in symtab; if that
%% fails because it's not in the symtab, adds it to symtab; otherwise
%% adds it Rej.
%% See table of actions in the top-level documentation.
%% Symtab0Rej0Mod0 and SymtabRejMod are sym_rej/2 functors.
%% If Type is uninstantiated it gets set to []
%% TODO: can we eliminate the "(Type=[]->true;true)" ?
%%       One way would be to do an initial pass that
%%       enters all the identifiers into symtab (with type=[]).
%% TODO: use library(assoc) or library(rbtrees) or trie or hash
%%       instead of dict for Symtab (performance)
%%       symrej_accum(Fqn-Type.  (Probably rbtrees, because we do a
%%       lot of insertions when reading in a cached symtab compared to
%%       the number of lookups that will be done ... also might want
%%       to create a merge_ord_list_to_rbtree for making the update
%%       faster.)
symrej_accum(Fqn-Type, sym_rej(Symtab0,Rej0), sym_rej(Symtab,Rej)) :-
    (  get_dict(Fqn, Symtab0, TypeSymtab)
    -> symrej_accum_found(Fqn, Type, TypeSymtab, Symtab0, Symtab, Rej0, Rej)
    ;  Rej = Rej0,
       %% ensure Type is instantiated (defaults to []), if this is a lookup
       ( Type = [] -> true ; true ),  % see comment in eval_single_type//1
       put_dict(Fqn, Symtab0, Type, Symtab)
    ).

%! symtab_lookup(+Fqn, ?Type)//[symrej] is semidet.
%% Succeeds if FQN is in symtab With Type.
%% Currently used only by resolve_unknown_fqn//5, but could be used to
%% make symrej_accum/3 more logical (see comments there and
%% eval_single_type//1).
symtab_lookup(Fqn, Type) -->>
    SymRej/symrej/SymRej,
    { SymRej = sym_rej(Symtab,_) },
    get_dict(Fqn, Symtab, Type).

%! symrej_accum_found(+Fqn, +Type, +TypeSymtab, +Symtab0, -Symtab, +Rej0, -Rej).
%% Helper for symrej_accum/3 for when Fqn is in Symtab with value
%% TypeSymtab (Type is the new type).
%% Symtab gets updated type information for Fqn, and Rej is added to
%% if there was any change to the entry in Symtab.
symrej_accum_found(Fqn, Type, TypeSymtab, Symtab0, Symtab, Rej0, Rej) :-
    (  Type = TypeSymtab % also handles Type is uninstantiated (i.e., a lookup)
    -> Symtab = Symtab0,
       Rej = Rej0
    ;  type_union(TypeSymtab, Type, TypeComb),
       (  TypeComb = TypeSymtab
       -> Symtab = Symtab0,
          Rej = Rej0
       ;  put_dict(Fqn, Symtab0, TypeComb, Symtab),
          Rej = [Fqn-Type|Rej0]
       )
    ).


%%%%%%           %%%%%%%
%%%%%% Utilities %%%%%%%
%%%%%%           %%%%%%%

%% The predicates for handling types are a thin wrapper around
%% "ordsets". It is anticipated that some normalization will be
%% needed, but currently there isn't any.

%! type_union(+Set1, +Set2, -Set) is det.
type_union(Type1, Type2, UnionType) :-
    ord_union(Type1, Type2, UnionType0),
    normalize_type(UnionType0, UnionType).
%! type_add_element(+Set1, +Element, ?Set2) is det.
type_add_element(Type0, Element, Type) :-
    ord_add_element(Type0, Element, Type1),
    normalize_type(Type1, Type).
%! list_to_union_type(+List, -OrdSet) is det.
list_to_union_type(List, Type) :-
    list_to_ord_set(List, Type0),
    normalize_type(Type0, Type).

%! combine_types(+ListOfOrdSets, -Set) is det.
combine_types(ListOfTypes, Type) :-
    ord_union(ListOfTypes, Type).

normalize_type(Type, Type).  % TODO: this probably is unsufficient.

%! maplist_kyfact_symrej_combine(:Pred, L:list, EvalType:ordset)//[kyfact,symrej,file_meta] is det.
%% maplist/3 for EDCG [kyfact,symrej,file_meta] + combine_types
maplist_kyfact_symrej_combine(Pred, L, EvalType) -->>
    maplist_kyfact_symrej(Pred, L, EvalType0),
    { combine_types(EvalType0, EvalType) }.

user:portray(Term) :-
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
my_portray(str(S)) :- !,
    format('str(~q)', [S]).
my_portray(bool(B)) :- !,
    format('bool(~q)', [B]).
my_portray('StringNode'{astns: [Astn]}) :- !, % doesn't handle "foo" "bar"
    format("'StringNode'{astns:[~p]}", [Astn]).
my_portray('StringBytesNode'{astns: [Astn]}) :- !, % doesn't handle "foo" "bar"
    format("'StringBytesNode'{astns:[~p]}", [Astn]).
my_portray('NumberComplexNode'{astn: Astn}) :-
    format("'NumberComplexNode'{astn:~p}", [Astn]).
my_portray('NumberFloatNode'{astn: Astn}) :-
    format("'NumberFloatNode'{astn:~p}", [Astn]).
my_portray('NumberIntNode'{astn: Astn}) :-
    format("'NumberIntNode'{astn:~p}", [Astn]).
my_portray(op([Astn])) :-
    node_astn(Astn, _, _, _), !,
    format('op([~p])', [Astn]).
my_portray(var(X)) :- !,
    format('var(~p)', [X]).
my_portray(var_binds(X)) :- !,
    format('var_binds(~p)', [X]).
my_portray(var_lookup(X)) :- !,
    format('var_lookup(~p)', [X]).
my_portray(function_type(F, R)) :- !,
    format('function_type(~p, ~p)', [F, R]).
my_portray(class_type(F, R)) :- !,
    format('class_type(~p, ~p)', [F, R]).
my_portray(union(U)) :- !,
    format('union(~p)', [U]).
my_portray(astn(Start, End, String)) :- !,
    format('astn(~p,~p, ~p)', [Start, End, String]).
my_portray('*list*'(List)) :- !, % To make print_term output more compact
    format('~p', [List]).
my_portray(Meta) :-
    is_dict(Meta, MetaTag),
    MetaTag == meta,        % ==/2 in case dict has uninstantiated tag
    get_dict(kythe_corpus, Meta, KytheCorpus),
    get_dict(kythe_root, Meta, KytheRoot),
    get_dict(path, Meta, Path),
    get_dict(src_fqn, Meta, SrcFqn),
    !,
    format('meta{~q, ~q, ~q, ~q, ...}', [KytheCorpus, KytheRoot, Path, SrcFqn]).
my_portray('$VAR'('_')) :- !, % work around a bug in print_term
    format('_', []).          % (numbervars(true) should handle this)
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
    is_dict(Symtab, Tag),
    Tag == symtab,          % ==/2 in case dict has uninstantiated tag
    !,
    dict_pairs(Symtab, Tag, Entries),
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

%% Variants on maplist, foldl (and combinations of them) for EDCGs

%! include(:Goal, List)//[kyfact,file_meta] is det.
%% Like apply:include/3, but the list is subsumed by "-->>".
include_kyfact(_Pred, []) -->> [ ].
include_kyfact(Pred, [X|Xs]) -->>
    (  call(Pred, X):[kyfact,file_meta]
    -> true
    ;  true
    ),
    include_kyfact(Pred, Xs).

%! maplist_kyfact(:Pred, +L:list)//[kyfact,file_meta] is det.
%% maplist/2 for EDCG [kyfact,file_meta]
maplist_kyfact(_Pred, []) -->> [ ].
maplist_kyfact(Pred, [X|Xs]) -->>
    call(Pred, X):[kyfact,file_meta],
    maplist_kyfact(Pred, Xs).

%! maplist_kyfact(:Pred, +L0:list, -L:list)//[kyfact,file_meta] is det.
%% maplist/3 for EDCG [kyfact,file_meta]
maplist_kyfact(_Pred, [], []) -->> [ ].
maplist_kyfact(Pred, [X|Xs], [Y|Ys]) -->>
    call(Pred, X, Y):[kyfact,file_meta],
    maplist_kyfact(Pred, Xs, Ys).

%! maplist_kyfact_symrej(:Pred, +L:list)//[kyfact,symrej,file_meta] is det.
%% maplist/2 for EDCG [kyfact,symrej,file_meta]
maplist_kyfact_symrej(_Pred, []) -->> [ ].
maplist_kyfact_symrej(Pred, [X|Xs]) -->>
    call(Pred, X):[kyfact,symrej,file_meta],
    maplist_kyfact_symrej(Pred, Xs).

%! maplist_kyfact_symrej(:Pred, +L0:list, -L:list)//[kyfact,symrej,file_meta] is det.
%% maplist/3 for EDCG [kyfact,symrej,file_meta]
maplist_kyfact_symrej(_Pred, [], []) -->> [ ].
maplist_kyfact_symrej(Pred, [X|Xs], [Y|Ys]) -->>
    call(Pred, X, Y):[kyfact,symrej,file_meta],
    maplist_kyfact_symrej(Pred, Xs, Ys).

%! maplist_kyfact_expr(:Pred, +L0:list)//[kyfact,expr,file_meta] is det.
%% maplist/2 for EDCG [kyfact,expr,file_meta]
maplist_kyfact_expr(_Pred, []) -->> [ ].
maplist_kyfact_expr(Pred, [X|Xs]) -->>
    call(Pred, X):[kyfact,expr,file_meta],
    maplist_kyfact_expr(Pred, Xs).

%! maplist_kyfact_expr(:Pred, +L0:list, -L:list)//[kyfact,expr,file_meta] is det.
%% maplist/3 for EDCG [kyfact,expr,file_meta]
maplist_kyfact_expr(_Pred, [], []) -->> [ ].
maplist_kyfact_expr(Pred, [X|Xs], [Y|Ys]) -->>
    call(Pred, X, Y):[kyfact,expr,file_meta],
    maplist_kyfact_expr(Pred, Xs, Ys).

trace_file(_) :- fail.
%% trace_file('/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/t2.py'). % TODO: delete

log_if_file(Fmt, Args) -->>
    Meta/file_meta,
    { log_if(trace_file(Meta.path), Fmt, Args) }.

:- meta_predicate do_if_file(0, +).

%! do_if_file(:Goal) is det.
%% Do the Goal if trace_file/1 matches Meta.path. (for debugging)
do_if_file(Goal) -->>
    Meta/file_meta,
    { do_if(trace_file(Meta.path), Goal) }.

%! Dump the symtab if trace_file/1 matches Meta.path. (for debugging)
symtab_if_file(Msg) -->>
    Meta/file_meta,
    SymRej/symrej/SymRej,
    (  { trace_file(Meta.path) }
    -> { SymRej = sym_rej(Symtab, _) },
       { atomic_list_concat([Meta.src_fqn, '.'], SrcFqnDot) },
       { dict_pairs(Symtab, SymtabTag, SymtabPairs) },
       { convlist(starts_with_fqn_type(SrcFqnDot), SymtabPairs, SymtabPairs2) },
       { dict_pairs(Symtab2, SymtabTag, SymtabPairs2) },
       { log_if(true, '~w: ~q', [Msg, Symtab2]) }
    ;  [ ]
    ).

%! starts_with_fqn(+Prefix:atom, +Fqn-Type:pair(atom), -Fqn2-Type2:pair(atom)) is semidet.
%% If Fqn has Prefix as a prefix, and the result isn't in the builtins,
%% return the de-prefixed FQN with its type.
starts_with_fqn_type(Prefix, Fqn-Type, Fqn2-Type) :-
    atom_concat(Prefix, Fqn2, Fqn),
    builtins_pairs(BuiltinsPairs), % TODO: this is inefficient.
    \+ memberchk(Fqn2-_, BuiltinsPairs).
