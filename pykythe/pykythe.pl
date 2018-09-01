% -*- mode: Prolog -*-

% Post-processor for the simplified nodes with FQNs that is generated
% by pykythe (see ast_cooked.Base.add_fqns).  The post-processing is
% mainly:
% - add Kythe anchor facts
% - add facts/edges for attributes (e.g., "foo" in self.foo)
% - resolve and process imports
% - in future, things like function call references

% TODO: :- use_module(library(protobufs)).  % instead of input/output JSON
%        ... handling JSON seems to be the most expensive thing,
%            according to profile/1 (it also seems to be the main
%            contributor to garbage collection; base64 manipulation is
%            also expensive).

% There are two passes:

% 0. [run_parse_cmd] Create a "cooked" AST (Python program specified
%    by --parsecmd which generates a JSON data structures of the AST
% 1. [process_nodes] Read in the "cooked" AST (see ast_cooked.Base),
%    transform them into a simpler form, and produce (using
%    accumulators):
%    - Kythe facts (e.g., anchors) for all the variables and
%      parameters in the code (but not the attributes)
%    - references and assignments (in the assign/2 facts).
%    Each predicate also returns a "type" result that is used to
%    populate the right-hand-side of assign/2 facts (for statements,
%    a expr/1 term is returned for completeness.  For more details,
%    see kynode//2.

% 2. [assign_exprs] Process the assign/2 and expr/1 facts, by
%    interpreting the expressions and recording the results in a
%    symtab (symbol table), then outputting Kythe facts for the
%    attributes, calls, etc.  Note that expr([]) is a no-op.

% Names:
%  'astn' is an AST (Abstract Syntax Tree) node.
%  'fqn' is fully qualified name
%  'ky' as prefix means 'kythe' (e.g. kyfact instead of kythe_fact)
%  'symtab' is symbol table
%  'symrej' is symbol table (symtab) + rejects

% "types" and "eval" ...

% The input consists of a list of simplified items from the AST.
% For example, this Python line (in class C2's __init__):
%   self.x = 'C2_x'
% is turned into the following (in portray-output format):
%    'AssignExprStmt'{
%        expr: 'StringNode'{astn: [ASTN(1160:1166, "'C2_x'")]},
%        left: 'AtomDotNode'{
%               atom: 'NameRefFqn'{
%                         fqn: str("test_data.simple.C2.__init__.<local>.self"),
%                         name: 'ASTN'(1151:1155, "self") },
%               attr_name: 'ASTN'(1156:1157, "x"),
%               binds: bool("True") } }
%
% When this is read in, it is simplified to something like this:
%   assign([dot(fqn('test_data.simple.C2.__init__.<local>.self')],
%              astn(1156,1157, "x"),
%              '/kythe/edge/defines/binding'),
%          [class('typeshed.stdlib.3.builtin.str', [])])
%                 % (Py2.7 would be __builtin__.str)
%
% To process this, we need to resolve the FQNs (in this case,
% fqn('test_data.simple.C2.__init__.<local>.self') by looking up in
% the symtab, eventually resulting in the dot(...) expression being
% reduced to fqn('test_data.simple.C2.x')).

% The symtab mappings are an ord_union (possibly empty) of:
%     fqn(Fqn) - a global or local name (fully qualified)
%     class(Fqn, Bases)  % Bases is a list of union types
%     func(Fqn, ReturnType)  % ReturnType is a union type
%     import(Fqn, Path)  % TODO: this will probably change somewhat

% There are two flavors of the "eval" predicates, depending on the
% behavior with fqn(Fqn): those ending with ..._and_lookup are for the
% right-hand-side of assignments; they use the symtab to look up any
% resulting Fqn and return the associated value (or add it to symtab
% and return []).  For the left-hand-side of an assignment, the lookup
% isn't done.
%   (Implementation detail: this is done using
%        [ Fqn-Result ]:symrej
%   which calls symrej_accum/3.
%
% All types are unions (represented as an ordset); [] means that
% there's no information and is effectively "Any". Many of the
% predicates come in two versions: one that works with a type union
% (ordset), and one that works on single "types", such as fqn(...),
% class(...), func(...), etc. -- typically the predicate that works
% with a type union iterates over the single items, calling the
% predicate for single "types", and uses ord_union/3 to combine them
% all. (This use of ord_union ensures that there's no need to
% "flatten" the list and that the single types are kept in a canonical
% order).

% The list of assign(Left, Right) and expr(Right) terms is repeatedly
% reprocessed until no changes occur. When a FQN is first encountered,
% it is put into the symtab with its type (the type is [] if it can't
% be determined) -- when subsequently encountered, any inconsistency
% in type is added to a "reject" list. After a pass is complete, the
% rejects are unioned with the symtab and if there were changes,
% another pass is done. In this way, each expression is repeatedly
% reprocessed until no more changes (in practice, only one or two
% passes are needed).

% TODO: can we remove the kyfact accumulator from the first pass
%       and generate all the Kythe information from the second pass?

% TODO: Use QLF: http://www.swi-prolog.org/pldoc/man?section=qlf

%% :- module(pykythe, [pykythe_main/0]).  %% TODO

:- use_module(library(aggregate), [aggregate_all/3, foreach/2]).
:- use_module(library(apply), [maplist/2, maplist/3, maplist/4, foldl/4, convlist/3]).
:- use_module(library(assoc), [is_assoc/1]).
:- use_module(library(base64), [base64/2]).
:- use_module(library(debug), [assertion/1, debug/3]).
:- use_module(library(edcg)).  % requires: ?- pack_install(edcg).
:- use_module(library(error), [type_error/2]).
:- use_module(library(filesex), [relative_file_name/3, make_directory_path/1]).
:- use_module(library(http/json), [json_read_dict/2, json_write_dict/3]).
:- use_module(library(lists), [append/3, list_to_set/2, member/2, select/3]).
:- use_module(library(optparse), [opt_arguments/3]).
:- use_module(library(ordsets), [list_to_ord_set/2, ord_empty/1, ord_union/3, ord_add_element/3]).
:- use_module(library(pcre), [re_replace/4]).
:- use_module(library(pprint), [print_term/2]).
:- use_module(library(yall)).
%% :- use_module(library(apply_macros).  % TODO: for performance
:- use_module(must_once, [must_once/1,
                          must_once_msg/3
                         ]).

:- meta_predicate
       maplist_kyfact(2, +, +, -, +),
       maplist_kyfact(3, +, -, +, -, +),
       maplist_kyfact_symrej(5, +, -, +, -, +, -, +),
       maplist_kyfact_expr(5, +, +, -, +, -, +),
       maplist_kyfact_expr(5, +, -, +, -, +, -, +),
       maplist_foldl_kyfact_expr(5, +, -, +, -, +, -, +),
       maplist_foldl_kyfact_symrej(5, +, +, -, +, -, +, -, +).

:- style_check(+singleton).
:- style_check(+no_effect).
:- style_check(+discontiguous).
:- set_prolog_flag(autoload, false).  % See below json:term_to_dict/3
:- set_prolog_flag(report_error, true).          % TODO: remove
:- set_prolog_flag(backtrace, true).             % TODO: remove
:- set_prolog_flag(backtrace_show_lines, true).  % TODO: remove
:- set_prolog_flag(backtrace_show_lines, true).  % TODO: remove
% :- set_prolog_flag(generate_debug_info, false).

:- use_module(library(rdet), [rdet/1]).
%% Note: library(rdet) expands a call to
%%       ( Call -> true ; throw(...) )
%%       which causes a warning about variable not introduced in all branches
%%       if the Call instantiates something that's used later.
%%       Therefore, we don't enable style_check(+var_branches)
:- style_check(-var_branches).

% TODO: there are too many rdet declarations, and they slow things down
%       (although this might be only compilation).

% Higher-level predicates that we use deterministically:
:- maplist(rdet, [maplist/2, maplist/3, maplist/4, foldl/4, convlist/3]).

% Other imported predicates:
:- maplist(rdet, [aggregate_all/3, foreach/2, base64/2,
                  json_read_dict/2, json_write_dict/3,
                  list_to_ord_set/2, list_to_set/2,
                  opt_arguments/3]).

% Deterministic predicates in this module
% You can generate an approximation of these by:
%     forall(current_predicate(user:Pred), format('>>> ~q~n', [Pred])).

:- maplist(rdet, ['NameRawNode_astn_and_name'/3,
                  absolute_dir/2,
                  add_rej_to_symtab/3,
                  assign_expr_eval/6,
                  assign_exprs/5,
                  assign_exprs_count/6,
                  assign_exprs_count_impl/6,
                  assign_normalized/7,
                  %% builtin_name/1,
                  builtin_names/1,
                  %% canonical_path/2,
                  do_if/2,
                  dot_edge_name/2,
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
                  %% expand_filepath/2,
                  %% expr_from_symtab/2,
                  expr_normalized/6,
                  %% file_and_token/3,
                  file_search_path/2,
                  kyImportFromStmt_import_part/7,
                  full_path/3,
                  full_path_prefixed/4,
                  import_from/6,
                  initial_symtab/1,
                  json_read_dict/2,
                  json_write_dict/3,
                  kyImportDottedAsNamesFqn/7,
                  kyImportDottedAsNamesFqn_comb/8,
                  kyImportFromStmt/7,
                  kyImportFromStmt_dots_file/7,
                  kyImportFromStmt_dots_file_as_single_node/5,
                  kyImportFromStmt_from_name/6,
                  kyImportFromStmt_from_name_pieces/7,
                  kyImportFromStmt_import_part/7,
                  kyNameRawNode/3,
                  kyanchor/6,
                  kyedge/6,
                  kyedge_fqn/6,
                  kyfact/6,
                  kyfact_b64/6,
                  kyfile/4,
                  kynode/7,
                  lookup_module/2,
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
                  %% module_name_as_path/2,
                  %% must_once/1,
                  %% must_once_msg/3,
                  %% node_astn/4,
                  output_kyfact/2,
                  parse_and_process_module/3,
                  %% path_to_python_module/2,
                  path_to_python_module_or_unknown/2,
                  print_term_cleaned/3,
                  process_nodes/5,
                  process_nodes/7,
                  %% py_ext/2,
                  %% py_ext_ext/1,
                  %% pykythe_main/0,
                  pykythe_opts/2,
                  %% pythonpath_prefix/2,
                  read_nodes/3,
                  ref_import/4,
                  relative_file_name/3,
                  %% remove_last_component/3,
                  remove_suffix_star/3,
                  resolve_file_comb_import/3,
                  run_parse_cmd/4,
                  signature_node/3,
                  signature_source/3,
                  simplify_json/2,
                  simplify_json_slot_pair/2,
                  simplify_meta/2,
                  split_atom/4,
                  split_path_string/3,
                  symrej_accum/3,
                  symrej_accum_found/5,
                  symtab_as_kyfact/3,
                  zip_merge/3]).

% "kyfact" accumulator gets FQN anchor facts, in an ordinary list
% with each value being a dict to be output in JSON. The list may
% contain duplicates, which are removed before output. Duplicates can
% arise if a variable is redefined in the Python source; our de-dup
% keeps the first such definition.
% TODO: check for duplicate edge facts, which indicate a bug.
edcg:acc_info(kyfact, T, Out, In, Out=[T|In]).

% "expr" accumulator gets expressions that need interpreting.
edcg:acc_info(expr, T, Out, In, Out=[T|In]).

% "symrej" accumulator is for symtab + items that need reprocessing.
edcg:acc_info(symrej, FqnType, In, Out, symrej_accum(FqnType, In, Out)).

% "file_meta" passed arg contains meta-info about the current file.
edcg:pass_info(file_meta).

edcg:pred_info(maplist_kyfact, 2,                [kyfact, file_meta]).
edcg:pred_info(maplist_kyfact, 3,                [kyfact, file_meta]).

edcg:pred_info(kyImportDottedAsNamesFqn_comb, 5, [kyfact, file_meta]).
edcg:pred_info(kyImportFromStmt, 4,              [kyfact, file_meta]).
edcg:pred_info(kyImportFromStmt_dots_file, 4,    [kyfact, file_meta]).
edcg:pred_info(kyImportFromStmt_import_part, 4,  [kyfact, file_meta]).
edcg:pred_info(kyanchor, 3,                      [kyfact, file_meta]).
edcg:pred_info(kyedge_fqn, 3,                    [kyfact, file_meta]).
edcg:pred_info(kyfact, 3,                        [kyfact, file_meta]).
edcg:pred_info(kyfact_b64, 3,                    [kyfact, file_meta]).
edcg:pred_info(kyfile, 1,                        [kyfact, file_meta]).
edcg:pred_info(ref_import, 1,                    [kyfact, file_meta]).
edcg:pred_info(ref_imports, 1,                   [kyfact, file_meta]).

edcg:pred_info(maplist_foldl_kyfact_expr, 4,     [kyfact, expr, file_meta]).
edcg:pred_info(maplist_kyfact_expr, 2,           [kyfact, expr, file_meta]).
edcg:pred_info(maplist_kyfact_expr, 3,           [kyfact, expr, file_meta]).

edcg:pred_info(assign_normalized, 2,             [kyfact, expr, file_meta]).
edcg:pred_info(expr_normalized, 1,               [kyfact, expr, file_meta]).
edcg:pred_info(import_from, 1,                   [kyfact, expr, file_meta]).
edcg:pred_info(kyImportDottedAsNamesFqn, 2,      [kyfact, expr, file_meta]).
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

edcg:pred_info(kyedge, 3,                        [kyfact, file_meta]).

edcg:pred_info(signature_node, 2,                [file_meta]).
edcg:pred_info(signature_source, 2,              [file_meta]).

% TODO: process typeshed builtins.
%       also module special attributes:
%           '__build_class__', '__debug__', '__doc__', '__import__',
%           '__loader__', '__name__', '__package__', '__spec__'.

% builtin_names(['ArithmeticError', 'AssertionError', 'AttributeError',
%     'BaseException', 'BlockingIOError', 'BrokenPipeError',
%     'BufferError', 'BytesWarning', 'ChildProcessError',
%     'ConnectionAbortedError', 'ConnectionError',
%     'ConnectionRefusedError', 'ConnectionResetError',
%     'DeprecationWarning', 'EOFError', 'Ellipsis', 'EnvironmentError',
%     'Exception', 'False', 'FileExistsError', 'FileNotFoundError',
%     'FloatingPointError', 'FutureWarning', 'GeneratorExit', 'IOError',
%     'ImportError', 'ImportWarning', 'IndentationError', 'IndexError',
%     'InterruptedError', 'IsADirectoryError', 'KeyError',
%     'KeyboardInterrupt', 'LookupError', 'MemoryError',
%     'ModuleNotFoundError', 'NameError', 'None', 'NotADirectoryError',
%     'NotImplemented', 'NotImplementedError', 'OSError',
%     'OverflowError', 'PendingDeprecationWarning', 'PermissionError',
%     'ProcessLookupError', 'RecursionError', 'ReferenceError',
%     'ResourceWarning', 'RuntimeError', 'RuntimeWarning',
%     'StopAsyncIteration', 'StopIteration', 'SyntaxError',
%     'SyntaxWarning', 'SystemError', 'SystemExit', 'TabError',
%     'TimeoutError', 'True', 'TypeError', 'UnboundLocalError',
%     'UnicodeDecodeError', 'UnicodeEncodeError', 'UnicodeError',
%     'UnicodeTranslateError', 'UnicodeWarning', 'UserWarning',
%     'ValueError', 'Warning', 'ZeroDivisionError', 'abs', 'all', 'any',
%     'ascii', 'bin', 'bool', 'bytearray', 'bytes', 'callable', 'chr',
%     'classmethod', 'compile', 'complex', 'copyright', 'credits',
%     'delattr', 'dict', 'dir', 'divmod', 'enumerate', 'eval', 'exec',
%     'exit', 'filter', 'float', 'format', 'frozenset', 'getattr',
%     'globals', 'hasattr', 'hash', 'help', 'hex', 'id', 'input', 'int',
%     'isinstance', 'issubclass', 'iter', 'len', 'license', 'list',
%     'locals', 'map', 'max', 'memoryview', 'min', 'next', 'object',
%     'oct', 'open', 'ord', 'pow', 'print', 'property', 'quit', 'range',
%     'repr', 'reversed', 'round', 'set', 'setattr', 'slice', 'sorted',
%     'staticmethod', 'str', 'sum', 'super', 'tuple', 'type', 'vars',
%     'zip']).

%! builtin_names(-BuiltinNames:list) is det.
builtin_names([]).  % TODO: use the commented-out list above

%! builtin_name(+BuiltinName:atom) is det.
%! builtin_name(-BuiltinName:atom) is nondet.
%  True if BuiltinName is in the initial Python (builtins) symbol table.
builtin_name(Name) :-
    builtin_names(Names),
    member(Name, Names).

%! initial_symtab(-Symtab:dict) is det.
%  creates a symtab with the contents of typeshed/stdlib/3/builtins.pyi
% TODO: implement this fully
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

:- initialization(pykythe_main, main).

%! main is det.
%  The main predicate, run during initialization.
%  See also library(main)'s definition of main
pykythe_main :-
    % set_prolog_flag(gc, true),  % TODO: tune GC for performance
    % set_prolog_flag(agc_margin, 0),  % TODO: tune GC for performance
    on_signal(int, _, interrupt),
    pykythe_opts(Src, Opts),
    path_to_python_module_or_unknown(Src, SrcFqn),
    parse_and_process_module(Src, SrcFqn, Opts).

%! pykythe_opts(-Src:atom, -Opts:list(pair)) is det.
%  Process the command line, getting the source file and options.
pykythe_opts(Src, Opts) :-
    current_prolog_flag(version, PrologVersion),
    must_once_msg(PrologVersion >= 70713, 'SWI-Prolog version is too old', []),  % Sync this with README.md
    % TODO: optparse has a bug if a longflags contains '_', so using '-' for now.
    OptsSpec = [
        [opt(parsecmd), type(atom), longflags([parsecmd]),
         help('Command for running parser than generates fqn.json file')],
        [opt(kythe_corpus), type(atom), default(''), longflags(['kythe-corpus']),
        help('Value of "corpus" in Kythe facts')],
        [opt(kythe_root), type(atom), default(''), longflags(['kythe-root']),
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
    must_once_msg(PositionalArgs = [Src0], 'Missing/extra positional args', []),
    absolute_file_name(Src0, Src),
    split_path_string(pythonpath, Opts0, Opts).

%! split_path_string0(+OptName:atom, +Opts0:list, -Opts:list) is det.
%  Find the option given by OptName in Opts0, split the value into
%  components in a list, add back into Opts (can be in a different
%  position in the list).
split_path_string(OptName, Opts0, [NewOpt|Opts1]) :-
    Opt =.. [OptName, PathStr],
    select(Opt, Opts0, Opts1),
    split_atom(PathStr, ':', '', PathList0),
    maplist(absolute_dir, PathList0, PathList),
    NewOpt =.. [OptName, PathList].

%! absolute_dir(+Path0:atom, -AbsPath:atom) is det.
%  Apply absolute_file_name to Path0, giving AbsPath, ensuring it's a directory
%  and appending '/' to the name.
absolute_dir(/, /) :- !.  % Special case for root dir, which otherwise would become '//'
absolute_dir(Path0, AbsPath) :-
    remove_suffix_star(Path0, '/', Path),
    absolute_file_name(Path, AbsPath0, [access(read), file_type(directory), file_errors(fail)]),
    atom_concat(AbsPath0, '/', AbsPath).

%! split_atom(+Atom:atom, +SepChars:atom, +PadChars:atom, -SubAtoms:list(atom)) is det.
%  Like split_string, but result is a list of atoms.
split_atom(Atom, SepChars, PadChars, SubAtoms) :-
    split_string(Atom, SepChars, PadChars, SubStrings),
    maplist([S,A]>>atom_string(A,S), SubStrings, SubAtoms).

%! remove_suffix_star(+Full:atom, +Suffix:atom, -NoSuffix:atom) is det.
%  Repeatedly removes suffix if present.
remove_suffix_star(Full, Suffix, NoSuffix) :-
    (  atom_concat(Full1, Suffix, Full)
    -> remove_suffix_star(Full1, Suffix, NoSuffix)
    ;  NoSuffix = Full
    ).

%! lookup_module(+Module:atom, -FullPath:atom) is det.
% TODO: document this.
lookup_module(Module, FullPath) :-
    module_name_as_path(Module, CanonicalPath),
    atom_string(FullPath, CanonicalPath),  % TODO - use string instead of atom
    path_to_python_module(FullPath, Fqn),  % TODO - if path_to_python_module_or_unknown, fails consistency check below
    must_once_msg(
        Module == Fqn,
        'Derived FQN differs from Module name ... canonical(~q) full(~q)',
        [CanonicalPath, FullPath]).

%! module_name_as_path(+Module:atom, -Path:atom) is nondet.
%! module_name_as_path(-Module:atom, +Path:atom) is nondet.
%  Convert a module ('path.to.module') to a path
%  ('path/to/module.py').  Does not check for existence of the file,
%  nor apply any PythonPath addition. Backtracks through all
%  solutions. At least one of Module and Path must be instantiated.
module_name_as_path(Module, Path) :-
    (  var(Module)
    -> py_ext(Path0, Path2),
       split_atom(Path0, '/', '', ModuleParts),
       atomic_list_concat(ModuleParts, '.', Module)
    ;  split_atom(Module, '.', '', ModuleParts),
       atomic_list_concat(ModuleParts, '/', Path1),
       py_ext(Path1, Path2)
    ),
    canonical_path(Path2, Path).

%! path_to_python_module_or_unknown(+Path, -Fqn) is det.
%  Get the Fqn for the Python module corresponding to Path or
%  a '<unknown>...' atom.
path_to_python_module_or_unknown(Path, Fqn) :-
    (  path_to_python_module(Path, Fqn)
    -> true
    ;  atomic_list_concat(['<unknown>', Path], ':', Fqn)
    ).

%! path_to_python_module_or_unknown(+Path, -Fqn) is semidet.
%  Get the Fqn for the Python module corresponding to Path or fail.
path_to_python_module(Path, Fqn) :-
    canonical_path(Path, CanonicalPath),
    py_ext(CanonicalPath0, CanonicalPath),
    split_atom(CanonicalPath0, '/', '', FqnParts),
    atomic_list_concat(FqnParts, '.', Fqn).

%! canonical_path(+Path, -CanonicalPath) is semidet.
%  Get a Path into a canonical (absolute) form.
%  Fails if the file doesn't exist.
canonical_path(Path, CanonicalPath) :-
    % TODO: besides being slightly less efficient, this doesn't do
    %       quite what we want -- probably want to change
    %       expand_filepath/2 to use py_ext/2 for files and to use
    %       the given file name for directories. However, it's
    %       unlikely that anyone would notice the subtlely different
    %       semantics -- e.g., we allow a directory whose name ends in
    %       '.py' or a file without a '.py' extension, but these
    %       wouldn't be allowed by the Python interpreter, so the
    %       extra permissivity is shouldn't be a problem.
    (  absolute_file_name(Path, AbsPath, [access(read), file_errors(fail)])
    -> true
    ;  absolute_file_name(Path, AbsPath, [access(read), file_type(directory), file_errors(fail)])
    ),
    atom_string(CanonicalPath, AbsPath).  % TODO: use string

%! parse_and_process_module(+Src:atom, +SrcFqn:atom, +Opts) is det.
%  Read in a single file (JSON output from pykythe module, which
%  encodes the AST nodes with FQNs), output Kythe JSON to current
%  output stream. Src is assumed to be in absolute form (leading '/').
parse_and_process_module(Src, SrcFqn, Opts) :-
    must_once(is_absolute_file_name(Src)),
    memberchk(pythonpath(Pythonpaths), Opts),
    memberchk(kytheout(KytheOutDir), Opts),
    memberchk(kytheout_suffix(KytheOutSuffix), Opts),
    do_if(false, dump_term('PYTHONPATHS', Pythonpaths)),  % TODO: delete
    lookup_module(SrcFqn, Src),
    run_parse_cmd(Opts, Src, SrcFqn, ParsedFile),
    current_prolog_flag(autoload, AutoloadFlag),
    % TODO: fix library(http/json): use_module(library(lists)).
    set_prolog_flag(autoload, true),  % TODO: Otherwise gets error: json:term_to_dict/3 - undefined select/3
    read_nodes(ParsedFile, Nodes, Meta0),
    set_prolog_flag(autoload, AutoloadFlag),
    do_if(false,
          dump_term('NODES', Nodes)),
    put_dict(pythonpaths, Meta0, Pythonpaths, Meta),
    process_nodes(Nodes, src{src_fqn: SrcFqn, src: Src},
                  KytheFacts, Exprs, Meta),
    do_if(false,
          dump_term('EXPRS', Exprs, [indent_arguments(auto),
                                     right_margin(72)])),
    assign_exprs(Exprs, Meta, SrcFqn, Symtab, KytheFacts2),
    do_if(false,
          dump_term('SYMTAB', Symtab)),
    (  atom_concat(SrcBase, '.py', Src)
    -> true
    ;  atom_concat(SrcBase, '.pyi', Src)
    -> true
    ;  type_error(file_name_not_ending_in_py_or_pyi, Src)
    ),
    atomic_list_concat([KytheOutDir, SrcBase, KytheOutSuffix], KytheFile),
    open(KytheFile, write, KytheStream),
    % write(KytheStream, "%% === Kythe ==="), nl(KytheStream),
    symtab_as_kyfact(Symtab, Meta, SymtabKytheFact),
    output_kyfact(SymtabKytheFact, KytheStream),
    maplist({KytheStream}/[KytheFact]>>output_kyfact(KytheFact, KytheStream), KytheFacts),
    maplist({KytheStream}/[KytheFact]>>output_kyfact(KytheFact, KytheStream), KytheFacts2),
    close(KytheStream).

%! run_parse_cmd(+Opts, +Src, +SrcFqn, -OutFile) is det.
%  Run the parse command into a temporary file. (The temp file is
%  automatically deleted on graceful termination.)
%  An alternative would be to run the parse command as a process, into
%  a a pipe. This needs more memory, is more complicated to manage,
%  and is a bit more difficult to debug.
run_parse_cmd(Opts, Src, SrcFqn, OutFile) :-
    must_once_msg(ground(Opts), 'Invalid command line options', []),
    maplist({Opts}/[X]>>memberchk(X, Opts),
            [python_version(PythonVersion), parsecmd(ParseCmd),
             kythe_corpus(KytheCorpus), kythe_root(KytheRoot)]),
    must_once_msg(memberchk(PythonVersion, [2, 3]), 'Invalid Python version: ~q', [PythonVersion]),
    tmp_file_stream(OutFile, OutFileStream, [encoding(binary), extension('fqn-json')]),
    close(OutFileStream),
    atomic_list_concat(
            [ParseCmd,
             " --kythe-corpus='", KytheCorpus, "'",
             " --kythe-root='", KytheRoot, "'",
             " --python_version='", PythonVersion, "'",
             " --src='", Src, "'",
             " --module='", SrcFqn, "'",
             " --out_fqn_expr='", OutFile, "'"],
            Cmd),
    do_if(false, dump_term('CMD', Cmd)),
    must_once_msg(shell(Cmd, 0), 'Parse failed', []).

%! symtab_as_kyfact(+Symtab, +Meta, -KytheFactAsJsonDict) is det.
%  Convert the symtab into a Kythe fact.
symtab_as_kyfact(Symtab, Meta,
                 json{source: Source,
                      fact_name: '/kythe/x-symtab',
                      fact_value: SymtabStr64}) :-
    print_term_cleaned(Symtab,
                       [tab_width(0),
                        indent_arguments(2),
                        right_margin(100)],
                       SymtabStr),
    % TODO: the following is dup-ed from kyfile//0 but
    %       with Language specified
    base64(SymtabStr, SymtabStr64),
    Source = json{path: Meta.path, language: Meta.language}.

%! read_nodes(+FqnExprPath:atom, -Nodes, -Meta:dict) is det.
%  Read the JSON node tree (with FQNs) into Nodes and file meta-data into Meta.
read_nodes(FqnExprPath, Nodes, Meta) :-
    open(FqnExprPath, read, FqnExprStream),
    json_read_dict(FqnExprStream, MetaDict),
    simplify_meta(MetaDict, Meta),
    json_read_dict(FqnExprStream, JsonDict),
    must_once(
        at_end_of_stream(FqnExprStream)),
    do_if(false,
        dump_term('JSON_DICT', JsonDict)),
    simplify_json(JsonDict, Nodes).

%! simplify_meta(+MetaDictJson:dict, -Meta:dict) is det.
%  Simplify the file meta-data. The argument is the Prolog dict form
%  of the first JSON item (see ast_cooked.Meta).
simplify_meta(MetaDictJson, Meta) :-
    MetaDictJson = _{kind: "Meta",
        slots: _{
            kythe_corpus: _{kind: "str", value: KytheCorpus},
            kythe_root: _{kind: "str", value: KytheRoot},
            path: _{kind: "str", value: Path},
            language: _{kind: "str", value: Language},
            contents_b64: _{kind: "str", value: ContentsB64},
            encoding: _{kind: "str", value: Encoding}}},
    canonical_path(Path, CanonicalPath),
    Meta = meta{
        kythe_corpus: KytheCorpus,
        kythe_root: KytheRoot,
        path: CanonicalPath,
        language: Language,
        encoding: Encoding,
        file_contents_b64: ContentsB64}.

%! simplify_json(+Json, -Prolog) is det.
%  Simplify the JSON term into more specific dicts, each one
%  distinguished by its tag. The input dicts for base types (str, int,
%  etc.) are turned into simpler functors.
simplify_json([], []).
simplify_json([V|Vs], Values) :-
    maplist(simplify_json, [V|Vs], Values).
simplify_json(_{kind: "str", value: Value}, str(Value)).
simplify_json(_{kind: "int", value: Value}, int(Value)).
simplify_json(_{kind: "bool", value: Value}, bool(Value)).
simplify_json(_{kind: "None"}, none).  % Shouldn't be generated by pod.PlainOldDataExtended.make_json_dict
simplify_json(_{kind: "dict", items: Items}, Value) :-
    dict_pairs(Items, _, ItemPairs),
    maplist(simplify_json_slot_pair, ItemPairs, ItemPairs2),
    dict_pairs(Value, dict, ItemPairs2).
simplify_json(_{kind: Kind, slots: Slots}, Value) :-
    dict_pairs(Slots, _, SlotPairs),
    maplist(simplify_json_slot_pair, SlotPairs, SlotPairs2),
    atom_string(KindAtom, Kind),
    dict_pairs(Value, KindAtom, SlotPairs2).

%! simplify_json_slot_pair(+KeyValue:pair, -KeyValue2:pair) is det.
simplify_json_slot_pair(Key-Value, Key-Value2) :-
    simplify_json(Value, Value2).

%! process_nodes(+Nodes, +SrcInfo:dict, -KytheFacts:list, -Exprs:list, +Meta:dict) is det.
%  Wrapper for process_nodes//[kyfact, expr, file_meta].
% TODO: separate KytheFacts into those that require de-duping and
%  those that can be simply appended, to minimize the final de-dup.
process_nodes(Node, SrcInfo, KytheFacts, Exprs, Meta) :-
    process_nodes(Node, SrcInfo, KytheFacts1, [], Exprs, [], Meta),  % phrase(process_nodes(Node), KytheFacts, Exprs, Meta)
    % TODO: don't preserve order (for debugging) - use sort/2 to dedup:
    list_to_set(KytheFacts1, KytheFacts).

%! process_nodes(+Nodes)//[kyfact, expr, file_meta] is det.
%  Traverse the Nodes, accumulating in KytheFacts (mostly anchors)
%  and Expr (which will be traversed later, to fill in dynamically
%  created attribtes (e.g., self.foo).
process_nodes(Node, SrcInfo) -->>
    kyfile(SrcInfo),
    kynode(Node, _Expr).

%! kyfile//[kyfact, file_meta] is det.
%  Generate the KytheFacts at the file level.
kyfile(SrcInfo) -->>
    % TODO: output x-numlines, x-html ?
    Meta/file_meta,
    { must_once(Meta.path == SrcInfo.src) },
    { Source = json{path: Meta.path} },
    kyfact(Source, '/kythe/node/kind', 'file'),
    kyfact_b64(Source, '/kythe/text', Meta.file_contents_b64),
    signature_node(SrcInfo.src_fqn, Module),
    kyfact(Module, '/kythe/node/kind', 'package'),
    kyedge_fqn(Source, '/kythe/edge/childof', SrcInfo.src_fqn),
    kyfact(Source, '/kythe/text/encoding', Meta.encoding).

%! kynode(+Node:json_dict, -Type)//[kyfact, expr, file_meta] is det.
%  Extract anchors (with FQNs) from the the AST nodes.  The anchors go
%  into accumulator 'kyfact' and the expressions (for further
%  processing) go into accumulator 'expr'. The predicate returns a
%  "type", which is used to populate the right-hand-sides of assign/2
%  terms in the 'expr' accumulator.

%   For descriptions of the various types of Node, and how they relate
%   to the raw AST, see ast_cooked.py.

%   [], [_|_], bool(_), dict(_), int(_), none, str(_), 'Astn'{...}'
%   are all handled by higher-level nodes.
%     (e.g., 'Astn'{start: int(Start), end: int(End), value: str(Value)}}
%     in node_astn/4, which is uesd by 'ArgumentNode', 'AtomDotNode', etc.;
%     str(_) is used by 'Class', 'Func', etc.)

%   assign/2 facts are made up of a left-hand-side (assigned-to) and a
%   right-hand-side (expression. These correspond to the LHS and RHS
%   of an expression, and have a few variants:
%     assign([a], [b]) corresponds to the statement `a = b`
%     assign([a], []) corresponds to the definition of a name, e.g. `def foo(a)`
%   expr/1 are like assign/2 but with nothing to assign to (expr([]) is a no-op).

%   See comments at the top of this file on union and single types.

%   The following are handled by the container (e.g., ImportFromStmt):
%     AsNameNode
%     NameRawNode  (from DottedNameNode, ImportFromStmt, etc.)
%     NameNode
kynode('AnnAssignStmt'{left_annotation: LeftAnnotation,
                       expr: Expr,
                       left: Left},
             [stmt(annassign)]) -->>
    % Corresponds to `expr_stmt: testlist_star_expr annassign`.
    expr_normalized(Expr),
    assign_normalized(Left, LeftAnnotation).
kynode('ArgumentNode'{name: NameAstn, arg: Arg},
       [todo_arg(Name, ArgType)]) -->>
    % Corresponds to `argument: test '=' test`.
    % ast_raw creates ArgumentNode only for `test '=' test`; all other cases
    % just generate the expr (or similar)
    % TODO: match Name to func def param
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
kynode('AtomDotNode'{atom: Atom, binds: bool(Binds),
                           attr_name: AttrNameAstn},
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
    { node_astn(NameAstn, Start, End, _Token) },
    signature_node(FqnAtom, Vname),
    kyanchor(Start, End, Source),
    kyedge_fqn(Source, '/kythe/edge/defines/binding', FqnAtom),
    kyfact(Vname, '/kythe/node/kind', 'record'),
    kyfact(Vname, '/kythe/subkind', 'class'),
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
    { node_astn(NameAstn, Start, End, _Token) },
    signature_node(FqnAtom, Vname),
    kyanchor(Start, End, Source),
    kyedge_fqn(Source, '/kythe/edge/defines/binding', FqnAtom),
    kyfact(Vname, '/kythe/node/kind', 'function'),
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
    % The parser doesn't output a field if it's None, so add
    % from_name:none node and recurse:
    kynode('ImportFromStmt'{from_dots: FromDots,
                            from_name: none,
                            import_part: ImportPart},
           Type).
kynode('ImportFromStmt'{from_dots: FromDots,
                        from_name: FromName,
                        import_part: 'ImportAsNamesNode'{items: ImportPartItems}},
       [unused_importfrom(CombImportPart)]) -->>
    kyImportFromStmt(FromDots, FromName, ImportPartItems, CombImportPart),
    maplist_kyfact_expr(import_from, CombImportPart).
kynode('ImportFromStmt'{from_dots: FromDots,
                        from_name: FromName,
                        import_part: 'StarNode'{}},
       [unused_importfrom_star]) -->>
    kyImportFromStmt(FromDots, FromName, '*', _CombImportPart),
    % TODO: expand '*'
    [ ].
kynode('ImportNameFqn'{dotted_as_names: 'ImportDottedAsNamesFqn'{items: DottedAsNames}},
       [unused_import(DottedAsNamesType)]) -->>
    maplist_kyfact_expr(kyImportDottedAsNamesFqn, DottedAsNames, DottedAsNamesType).
kynode('ListMakerNode'{items: Items},
       [todo_list(ItemsType)]) -->>
    maplist_kynode(Items, ItemsType).
% 'NameBindsFqn' is only for 'AssignExprStmt' -- for import statements,
% it's handled separately.
% TODO: special case this within processing of AssignExprStmt?  IMPORTANT
kynode('NameBindsFqn'{fqn: str(Fqn), name: NameAstn},
       [fqn(FqnAtom)]) -->>  %% result is same as NameRefFqn
    { atom_string(FqnAtom, Fqn) },
    { node_astn(NameAstn, Start, End, _Token) },
    signature_node(FqnAtom, Vname),
    kyanchor(Start, End, Source),
    kyedge_fqn(Source, '/kythe/edge/defines/binding', FqnAtom),  %% only difference from NameRef
    kyfact(Vname, '/kythe/node/kind', 'variable').
kynode('NameRefFqn'{fqn: str(Fqn), name: NameAstn},
       [fqn(FqnAtom)]) -->>  %% result is same as NameBinds
    { atom_string(FqnAtom, Fqn) },
    { node_astn(NameAstn, Start, End, _Token) },
    kyanchor(Start, End, Source),
    kyedge_fqn(Source, '/kythe/edge/ref', FqnAtom).  %% only difference from NameBindsFqn
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
kynode(X, _) -->>  % TODO: remove this "catchall" clause
    { type_error(kynode, X) }.

%! kyImportDottedAsNameFqn(+DottedName, -DottedAsNamesType)//[kyfact, expr, file_meta] is det.
%  Corresponds to "import" and "import ... as ...".
%  The Fqn is either the top-level of the import (e.g., "os" in "import os.path")
%  or the "as" name (e.g., "os_path" in "import os.path as os_path").
%  This code is similar to kyImportFromStmt, but deviates in a few places:
%  - Leading dots (relative imports) are not allowed by the grammar.
%  - "import foo.bar" adds "foo" to the symtab; but "import foo.bar as foo_bar" adds
%    "foo_bar".
% TODO: this code is a mess -- factor out common code for the two cases,
%                              combine with kyImportFromStmt
kyImportDottedAsNamesFqn('ImportDottedFqn'{
                             dotted_name: 'DottedNameNode'{items: Items},
                             top_name: 'NameBindsFqn'{fqn: str(Fqn), name: NameAstn}},
                         unused_dottedName_top(DottedNameAstns, FqnAtom)) -->>
    { maplist(kyNameRawNode, Items, DottedNameAstns, Names) },
    kyImportDottedAsNamesFqn_comb(Fqn, NameAstn, Names, FqnAtom, file(ResolvedPath)),
    { path_to_python_module_or_unknown(ResolvedPath, Module) },  % TODO: delete
    { atomic_list_concat(Names, '.', ImportToken) },
    { maplist(get_dict(name), Items, ItemAstns) },
    { kyImportFromStmt_dots_file_as_single_node(ImportToken, ItemAstns,
                                                _ImportAstnDotsAndName, ImportStart, ImportEnd) },
    { append(_, [ItemLast], Items) },
    { node_astn(ItemLast.name, ItemLastStart, ItemLastEnd, _) },
    kyanchor(ItemLastStart, ItemLastEnd, ItemLastVname),
    kyedge_fqn(ItemLastVname, '/kythe/edge/ref/imports', Module),
    kyanchor(ImportStart, ImportEnd, ImportSource),
    kyedge(ImportSource, '/kythe/edge/ref/file',
           json{path:ResolvedPath}),
    [ import_top(DottedNameAstns, FqnAtom, Names, ResolvedPath) ]:expr.
kyImportDottedAsNamesFqn('ImportDottedAsNameFqn'{
                             dotted_name: 'DottedNameNode'{items:Items},
                             as_name: 'NameBindsFqn'{fqn: str(Fqn), name: NameAstn}},
                         unused_importDotted_as(DottedNameAstns, FqnAtom)) -->>
    { maplist(kyNameRawNode, Items, DottedNameAstns, Names) },
    kyImportDottedAsNamesFqn_comb(Fqn, NameAstn, Names, FqnAtom, file(ResolvedPath)),
    { path_to_python_module_or_unknown(ResolvedPath, Module) },  % TODO: delete
    { atomic_list_concat(Names, '.', ImportToken) },
    { maplist(get_dict(name), Items, ItemAstns) },
    { kyImportFromStmt_dots_file_as_single_node(ImportToken, ItemAstns,
                                                _ImportAstnDotsAndName, ImportStart, ImportEnd) },
    { node_astn(NameAstn, NameStart, NameEnd, _) },
    kyanchor(NameStart, NameEnd, NameVname),
    kyedge_fqn(NameVname, '/kythe/edge/ref/imports', Module),
    kyanchor(ImportStart, ImportEnd, ImportSource),
    kyedge(ImportSource, '/kythe/edge/ref/file',
           json{path:ResolvedPath}),
    [ import_as(DottedNameAstns, FqnAtom, ResolvedPath) ]:expr.

%! kyImportDottedAsNameFqn_comb(+Fqn:string, +NameAstn:astn, +Names:list(atom), -FqnAtom:atom, -ResolvedPath:atom)//[kyfact, file_meta] is det.
%  Combined code for ImportDottedFqn, ImportDottedAsNameFqn.
%  Adds anchors and binding facts for an imported FQN.
kyImportDottedAsNamesFqn_comb(Fqn, NameAstn, Names, FqnAtom, ResolvedPath) -->>
    { atom_string(FqnAtom, Fqn) },
    { node_astn(NameAstn, Start, End, _Token) },
    { atomic_list_concat(['$PYTHONPATH'|Names], '/', ImportFile) },
    Meta/file_meta,
    { full_path(ImportFile, Meta.pythonpaths, ResolvedPath) },
    signature_node(FqnAtom, Vname),
    kyanchor(Start, End, Source),
    kyedge_fqn(Source, '/kythe/edge/defines/binding', FqnAtom),
    kyfact(Vname, '/kythe/node/kind', 'variable').

%! kyImportFromStmt(+FromDots:list, +FromName, +ImportPart, -CombImportPart)//[kyfact, file_meta] is det.
%  Corresponds to import_from: 'from' ... 'import' ...
%  The FromDots is zero or more ImportDotNode's, FromName is none or
%  DottedNameNode. If there are no ImportDotNode's, then the result is
%  $PYTHONPATH/Path/To/From/Pat/ImportPart. If there are
%  ImportDotNode's, then the result is FilePath/ImportPart, where
%  FilePath is derived from the Meta information for the file,
%  followed by '/..' as needed.
kyImportFromStmt(FromDots, FromName, ImportPart, ResolvedImportParts) -->>
    Meta/file_meta,
    { must_once(
          py_ext(PathBase0, Meta.path)) },
    { file_directory_name(PathBase0, PathBase) },
    { kyImportFromStmt_from_name(FromDots, FromName, PathBase, FromFileOrDir, FromAstns, FromToken) },
    kyImportFromStmt_dots_file(FromToken, FromAstns, FromFileOrDir, FromAstnDotsAndName),
    kyImportFromStmt_import_part(ImportPart, FromFileOrDir, FromAstnDotsAndName, CombImportParts),
    { Pythonpaths = Meta.pythonpaths },
    { maplist(resolve_file_comb_import(Pythonpaths), CombImportParts, ResolvedImportParts) },
    maplist_kyfact(ref_import, ResolvedImportParts).

%! kyImportFromStmt_from_name(+FromDots:list, +FromName, +PathBase:atom, -FromFileOrDir:atom, -FromAstns:list(atom), -FromToken:atom) :-
%  Given a list of dots and names (that is, a list of 'ImportDotNode's
%  followed by an optional 'DottedNameNode', generate the file (or
%  directory) and the compressed module name of the file or directory
%  (including leading dots), and the ASTNs for the components.
%  PathBase is used as the "base" for relative names and replaced by
%  $PYTHONPATH if this isn't a relative name (that is, no leading
%  '.'s).
kyImportFromStmt_from_name(FromDots, FromName, PathBase, FromFileOrDir, FromAstns, FromToken) :-
    kyImportFromStmt_from_name_pieces(FromDots, FromName, PathBase, Dots, DottedNames, FromAstns, FromTokens),
    (  Dots = []
    -> FromFileOrDirPieces = ['$PYTHONPATH'|DottedNames]
    ;  append(Dots, DottedNames, FromFileOrDirPieces)
    ),
    atomic_list_concat(FromFileOrDirPieces, '/', FromFileOrDir),
    atomic_list_concat(FromTokens, FromToken).

%! kyImportFromStmt_from_name_pieces(+FromDots: list, +FromName, +PathBase:atom, -Dots:list, -DottedNames:list(atom), -FromAstns:list(astn), -FromFileOrDirPieces:list(atom)) is det.
%  Handle the 'from' part of the import_from rule in the Grammar:
%      import_from: ('from' ('.'* dotted_name | '.'+) 'import' ...
%  (the stuff after the 'import' is handled elsewhere). Generates a
%  list of leading "dots" (for relative paths), pieces that are then
%  concatenated to form the FromFileOrDirPiece; also generate the ASTNs for the
%  pieces.
kyImportFromStmt_from_name_pieces([], none, _PathBase, [], [], [], []).
kyImportFromStmt_from_name_pieces([], 'DottedNameNode'{items: Items}, _PathBase, [], DottedNames, Astns, [FromFileOrDirPiece]) :-
    maplist('NameRawNode_astn_and_name', Items, Astns, DottedNames),
    atomic_list_concat(DottedNames, '.', FromFileOrDirPiece).
kyImportFromStmt_from_name_pieces(['ImportDotNode'{dot:Astn}|Ds], FromName, PathBase, [PathBase|Dots], DottedNames, [Astn|Astns], [FromFileOrDirPiece|FromFileOrDirPieces]) :-
    node_astn(Astn, _, _, FromFileOrDirPiece),
    kyImportFromStmt_from_name_pieces(Ds, FromName, '..', Dots, DottedNames, Astns, FromFileOrDirPieces).

%! 'NameRawNode_astn_and_name'(+DottedNameItem, -DottedName) is det.
%  Process a NameRawNode node into a name
'NameRawNode_astn_and_name'('NameRawNode'{name: NameAstn}, NameAstn, Name) :-
    node_astn(NameAstn, _, _, Name).

%! kyImportFromStmt_dots_file(+FromToken, +FromAstns, +FromFileOrDir, -FromAstnDotsAndName)//[kyfact, file_meta] is det.
%  Generate the Kythe ref/file fact for the "from" part of an import.
kyImportFromStmt_dots_file(FromToken, FromAstns, FromFileOrDir, FromAstnDotsAndName) -->>
    Meta/file_meta,
    { full_path(FromFileOrDir, Meta.pythonpaths, file(ResolvedFromFileOrDir)) },
    { kyImportFromStmt_dots_file_as_single_node(FromToken, FromAstns, FromAstnDotsAndName,
                                                FromStart, FromEnd) },
    kyanchor(FromStart, FromEnd, FromSource),
    kyedge(FromSource, '/kythe/edge/ref/file',
               json{path: ResolvedFromFileOrDir}).

%! kyImportFromStmt_dots_file_as_single_node(+FromToken, +FromAstns, -FromAstnDotsAndName, -FromStart, -FromEnd) is det.
%  For the "from" part of an import, generate a ASTNs plus the
%  start end end positions of the ASTN.
kyImportFromStmt_dots_file_as_single_node(FromToken, FromAstns,
                                          FromAstnDotsAndName, FromStart, FromEnd) :-
    FromAstns = [FromAstnFirst|_],
    append(_, [FromAstnLast], FromAstns),
    node_astn(FromAstnFirst, FromStart, _, _),
    node_astn(FromAstnLast, _, FromEnd, _),
    % The FromToken has had excess blanks squeezed out, so it doesn't
    % necessarily match what the file has in FromStart:FromEnd.
    node_astn(FromAstnDotsAndName, FromStart, FromEnd, FromToken).

%! ref_import(+RefImport)//[kyfact, file_meta] is det.
% TODO: rename and document
ref_import(path_fqn_vname(file_and_token(File,Token),_Var,VarVname)) -->>
    { atomic_list_concat([File, Token], '.', FileAndToken) },
    kyedge_fqn(VarVname, '/kythe/edge/ref/imports', FileAndToken).
ref_import(path_fqn_vname(file(File),_Var,VarVname)) -->>
    { path_to_python_module_or_unknown(File, Module) },  % TODO: delete
    kyedge_fqn(VarVname, '/kythe/edge/ref/imports', Module).
% TODO: handle refs for 'from ... import *'
ref_import(path_star(_File)) -->> [ ].
% TODO: the following should have been converted to path_star, but
%       somehow that was skipped -- the following is a quick fix:
ref_import(from_star(_File)) -->> [ ].

% TODO: rename and document
resolve_file_comb_import(_Pythonpaths, from_star(Import), from_star(Import)).  % TODO: implement
resolve_file_comb_import(Pythonpaths, path_fqn_vname(Import,Var,VarVname), path_fqn_vname(ResolvedAndToken,Var,VarVname)) :-
    full_path(Import, Pythonpaths, ResolvedAndToken).

% TODO: rename and document  is det
full_path(Path, Prefixes, ResolvedAndToken) :-
    (  pythonpath_prefix(Path, Deprefix)
    -> full_path_prefixed(Path, Deprefix, Prefixes, ResolvedAndToken)
    ;  expand_filepath(Path, ResolvedAndToken)
    -> true
    ;  canonical_path(Path, ExpandedPath)
    -> ResolvedAndToken = file(ExpandedPath)
    ;  ResolvedAndToken = file(Path)
    ).

% TODO: rename and document    is det
full_path_prefixed(Path, Deprefix, Prefixes, ResolvedAndToken) :-
    (  member(Prefix, Prefixes),
       atom_concat(Prefix, Deprefix, Path0),
       expand_filepath(Path0, ResolvedAndToken)
    -> true
    ;  ResolvedAndToken = file(Path)
    ).

% TODO: rename and document  is semidet.
expand_filepath(Path0, ResolvedAndToken) :-
    (  ResolvedAndToken = file(Expanded),
       Path1 = Path0
    ;  remove_last_component(Path0, Path1, LookupToken),
       file_and_token(Expanded, LookupToken, ResolvedAndToken)
    ),
    py_ext(Path1, Path),
    canonical_path(Path, Expanded).

% TODO: rename and document   is det
file_and_token(Expanded, '*', path_star(Expanded)) :- !.
file_and_token(Expanded, LookupToken, file_and_token(Expanded, LookupToken)).

% TODO: rename and document   is det.
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
%  Path unifies with all permutations of PathBase plus {.py,.pyi} and
%  __init__ equivalents and does not check for existence.
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
%  "extensions" to append to a module to get a file
%  file_name_extension/3 adds a '.', so can't use for /__init__.*
% TODO: allow more than .py and .pyi as extensions?
py_ext_ext('/__init__.py').
py_ext_ext('/__init__.pyi').
py_ext_ext('.py').
py_ext_ext('.pyi').
py_ext_ext('').  % For directories

%! kyImportFromStmt_import_part(+ImportPart, +FullFromName:atom, +AstnDotsAndName:atom, -CombImportParts)//[kyfact, file_meta] is det.
%  Used by kyImportFromStmt: extracts from individual AsNameNode
%  items and combines it with the FullFromName and outputs a list of
%  name-name pairs.
kyImportFromStmt_import_part('*', FullFromName, _AstnDotsAndName, [from_star(ConcatName)]) -->>
    % TODO: anchor for '*', with defines/bindings for all imported names
    { atomic_list_concat([FullFromName, '/*'], ConcatName) }.
kyImportFromStmt_import_part([], _, _, []) -->> [ ].
kyImportFromStmt_import_part(['AsNameNode'{as_name: 'NameBindsFqn'{fqn: str(AsName), name: AsNameAstn},
                                           name: 'NameRawNode'{name: NameAstn}}|Ns],
                 FullFromName,
                 AstnDotsAndName,
                 [path_fqn_vname(ConcatName,AsNameAtom,Source)|NANs]) -->>
    { node_astn(NameAstn, _, _, Name) },
    { node_astn(AsNameAstn, Start, End, _) },
    { atomic_list_concat([FullFromName, '/', Name], ConcatName) },
    { atom_string(AsNameAtom, AsName) },
    kyanchor(Start, End, Source),
    kyedge_fqn(Source, '/kythe/edge/defines/binding', AsName),
    kyImportFromStmt_import_part(Ns, FullFromName, AstnDotsAndName, NANs).

%! import_from(+PathFqn)//[kyfact, expr, file_meta] is det.
%  Used by ImportFromStmt to process the Path-Fqn pairs generated by
%  kyImportFromStmt_import_part.
import_from(path_fqn_vname(Path,Fqn,_VarVname)) -->>
    [ import_from(Path, Fqn) ]:expr.

%! kyNameRawNode(+Node, -Astn, -Name:atom) is det.
%  Used by DottedNameNode to process a list of NameRawNode.
% TODO: needs some file resolution
kyNameRawNode('NameRawNode'{name: NameAstn}, astn(Start, End, Name), NameAtom) :-
    node_astn(NameAstn, Start, End, Name),
    atom_string(NameAtom, Name).

%! maplist_kynode(+Nodes:list, -NodeTypes:list)//[kyfact, expr, file_meta] is det.
%  maplist_kyfact_expr(kynode, Nodes, NodeTypes)
% TODO: for some reason this fails when maplist meta-predicate is used
%        (maybe due to handling of _? in a meta-call?)
maplist_kynode([], []) -->> [ ].
maplist_kynode([Node|Nodes], [NodeTypeWrap|NodeTypes]) -->>
    kynode(Node, NodeTypeWrap),
    maplist_kynode(Nodes, NodeTypes).

%! assign_normalized(+Left, +Right)//[kyfact, expr, file_meta] is det.
%  Process the Left and Right parts of an assign/2 term, handling
%  things like `omitted` and `ellipsis`.
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
%  Process the Right parts of an expr/1 term, handling
%  things like `omitted` and `ellipsis`.
expr_normalized(Right) -->>
    kynode(Right, [RightType]),
    (  { RightType = omitted ; RightType = ellipsis }
    -> [ ]
    ;  [ expr([RightType]) ]:expr
    ).

%! node_astn(+AstnNode, -Start, -End, -Value) is semidet.
%! node_astn(-AstnNode, +Start, +End, +Value) is det.
%  Access the inner parts of an Astn node.
%  See also portray/1 rule for 'Astn' (uses node_astn/4).
node_astn('Astn'{start: int(Start), end: int(End), value: str(Value)},
          Start, End, Value).

%! dot_edge_name(+TrueFalse:string, -KytheEdge:atom) is det.
%  Translate True/False to Kythe ref or binding edge type
dot_edge_name("False", '/kythe/edge/ref').
dot_edge_name("True", '/kythe/edge/defines/binding').

%! kyanchor(+Start, +End, -Source)//[kyfact, file_meta] is det.
%  Create the Kythe facts for an anchor. Source gets the source signature.
kyanchor(Start, End, Source) -->>
    { format(string(Signature), '@~d:~d', [Start, End]) },
    signature_source(Signature, Source),
    kyfact(Source, '/kythe/node/kind', 'anchor'),
    kyfact(Source, '/kythe/loc/start', Start),
    kyfact(Source, '/kythe/loc/end', End).

%! kyedge_fqn(+Source, +EdgeKind:atom, +Fqn:atom)//[kyfact, file_meta] is det.
%  High-level create a Kythe edge fact to a target identified by an FQN.
kyedge_fqn(Source, EdgeKind, Fqn) -->>
    signature_node(Fqn, Target),
    kyedge(Source, EdgeKind, Target).

%! kyedge(+Source, +EdgeKind:atom, +Target:atom)//{kyfact, file_meta] is det.
%  Low-level create a Kythe edge fact -- for both Source and Target,
%  corpus and root are filled in from file_meta.
kyedge(Source, EdgeKind, Target) -->>
    Meta/file_meta,
    [ json{source: Source.put(corpus, Meta.kythe_corpus).put(root, Meta.kythe_root),
           edge_kind: EdgeKind,
           target: Target.put(corpus, Meta.kythe_corpus).put(root, Meta.kythe_root),
           fact_name: '/'} ]:kyfact.

%! kyfact(+Source, FactName, FactValue//[kyfact, file_meta] is det.
%  Low-level create a Kythe fact or edge -- for Source, corpus and root
%  are filled in from file_meta.
kyfact(Source, FactName, FactValue) -->>
    { base64(FactValue, FactBase64) },
    kyfact_b64(Source, FactName, FactBase64).

%! kyfact_64(+Source, +FactName, +FactBase64)//[kyfact, file_meta] is det.
%  Low-level create a Kythe fact or edge inputting the base64 of the
%  fact value -- for Source, corpus and root are filled in from file_meta.
%  The accumulator takes care of duplicate removal.
kyfact_b64(Source, FactName, FactBase64) -->>
    Meta/file_meta,
    [ json{source: Source.put(corpus, Meta.kythe_corpus).put(root, Meta.kythe_root),
           fact_name: FactName, fact_value: FactBase64} ]:kyfact.

%! signature_source(+Signature:string, -Source)//[file_meta] is det.
%  Create a Kythe "source" tuple from a Signature string.
signature_source(Signature, Source) -->>
    Meta/file_meta,
    { Source = json{signature: Signature, path: Meta.path} }.

%! signature_node(+Signature:string, -Vname)//[file_meta] is det.
%  Create a Kythe "vname" from a Signature string
signature_node(Signature, Vname) -->>
    Meta/file_meta,
    { Vname = json{signature: Signature, language: Meta.language} }.

%! output_kyfact(+AnchorAsDict:json_dict, +KytheStream:stream) is det.
%  Output a single Kythe fact.
output_kyfact(AnchorAsDict, KytheStream) :-
    % The tags are ignored unless option tag(type) is specified (which
    % it isn't). All dicts should have the tag 'json', for simplicity.
    json_write_dict(KytheStream, AnchorAsDict, [width(0)]),
    nl(KytheStream).

%%%%%%        %%%%%%%
%%%%%% Pass 2 %%%%%%%
%%%%%%        %%%%%%%

%! assign_exprs(+Exprs:list, +Meta: dict, +ModuleFqn:atom, -Symtab:dict, -KytheFacts:list) is det.
%  Process a list of Exprs, generating a Symtab and list of KytheFacts.
assign_exprs(Exprs, Meta, ModuleFqn, Symtab, KytheFacts) :-
    initial_symtab(Symtab0),
    put_dict(ModuleFqn, Symtab0, [module(ModuleFqn, Meta.path)], Symtab1),
    assign_exprs_count(1, Exprs, Meta, Symtab1, Symtab, KytheFacts).

%! assign_exprs(+Count, +Exprs:list, +Meta:dict, +Symtab0:dict, -Symtab:dict, -KytheFacts:list) is det.
%  Process a list of Exprs, generating a Symtab and list of KytheFacts.
%  Count tracks the number of passes over Exprs; if too large, the
%  processing stops.
% TODO: Improved output when too many passes are needed.
% TODO: Parameterize max number of passes.
assign_exprs_count(Count, Exprs, Meta, Symtab0, Symtab, KytheFacts) :-
    do_if(false,  % TODO: delete
          format(user_error, '% === EXPRS === ~q~n~n', [Count])),
    assign_exprs_count_impl(Exprs, Meta, Symtab0, Symtab1, Rej, KytheFacts1),  % phrase(assign_exprs_count(...))
    length(Rej, RejLen),
    do_if(RejLen > 0,
          format(user_error, 'Pass ~q (rej=~q)~n', [Count, RejLen])),
    CountIncr is Count + 1,
    (  (Rej = [] ; CountIncr > 5)  % TODO: parameterize.
    -> Symtab = Symtab1,
       KytheFacts = KytheFacts1
    ;  assign_exprs_count(CountIncr, Exprs, Meta, Symtab1, Symtab, KytheFacts)
    ).

%! assign_exprs_count_impl(+Exprs, +Meta:dict, +Symtab0:dict, -SymtabWithRej:dict, -Rej:dict, -KytheFacts) :-
%  Helper for assign_exprs_count, which does the actual processing.
assign_exprs_count_impl(Exprs, Meta, Symtab0, SymtabWithRej, Rej, KytheFacts) :-
    dict_pairs(Symtab0, symtab, SymtabPairs0),
    convlist(expr_from_symtab, SymtabPairs0, ExprsFromSymtab1),
    sort(ExprsFromSymtab1, ExprsFromSymtab),  % remove dups
    append(ExprsFromSymtab, Exprs, ExprsCombined),  %% TODO: difference list
    maplist_assign_expr_eval(ExprsCombined, KytheFacts1, [], Symtab0-[], SymtabAfterEval-Rej, Meta),  % phrase(assign_exprs_eval_list(...))
    % TODO: if we don't care about the order (for debugging), can use sort/2 for dedup:
    list_to_set(KytheFacts1, KytheFacts),
    do_if(false,
          dump_term('REJ', Rej)),
    foldl(add_rej_to_symtab, Rej, SymtabAfterEval, SymtabWithRej).

%! maplist_assign_exprs_eval_list(+Assign:list)//[kyfact, symrej, file_meta] is det.
%  Process a list of assign or eval nodes.
maplist_assign_expr_eval([]) -->> [ ].
maplist_assign_expr_eval([Assign|Assigns]) -->>
    SymtabRej/symrej,  %% TODO: delete (it's only used for debug logging)
    { do_if(false,
            dump_term('', SymtabRej)) },
    { do_if(false,
            dump_term('', Assign, [indent_arguments(auto),
                                   right_margin(60)])) },
    assign_expr_eval(Assign),
    maplist_assign_expr_eval(Assigns).

%! assign_expr_eval(+Node)//[kyfact, symrej, file_meta] is det.
%  Process a single assign/2 or expr/1 node.
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
assign_expr_eval(import_from(Path, Fqn)) -->>
    { do_if(false, dump_term('IMPORT_FROM', [Path, Fqn])) },  % TODO: delete
    [ Fqn-[import(Fqn, Path)] ]: symrej.
assign_expr_eval(import_top(_DottedNameAstns, Fqn, _Names, Path)) -->>
    { do_if(false, dump_term('IMPORT_TOP', [Path, Fqn])) },  % TODO: delete
    [ Fqn-[import(Fqn, Path)] ]: symrej.
assign_expr_eval(import_as(_DottedNameAstns, Fqn, Path)) -->>
    { do_if(false, dump_term('IMPORT_AS', [Path, Fqn])) },  % TODO: delete
    [ Fqn-[import(Fqn, Path)] ]: symrej.
assign_expr_eval(Expr) -->>  % TODO: remove this "catchall" clause
    { type_error(assign_expr_eval, Expr) }.

%! eval_union_type(+Type:ordset, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
%  Evaluate a Type, generating a new (union) EvalType.
eval_union_type(Type, EvalType) -->>
    { ord_empty(EvalType0) },
    maplist_foldl_eval_union_type(Type, EvalType0, EvalType).

%! eval_union_type(+Type:ordset, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
%  Evaluate a Type, generating a new (union) EvalType, using an explicit
%  accumulator (UnionSoFar).
maplist_foldl_eval_union_type([], UnionSofar, UnionSofar) -->> [ ].
maplist_foldl_eval_union_type([T|Ts], UnionSoFar, EvalTypes) -->>
    eval_single_type_and_lookup(T, ET),
    { ord_union(UnionSoFar, ET, UnionSoFar2) },
    maplist_foldl_eval_union_type(Ts, UnionSoFar2, EvalTypes).

%! eval_union_type_and_lookup(+Expr, -UnionEvalType)//[kyfact, symrej, file_meta] is det.
%  Evaluate (union) Expr and look it up in the symtab.
eval_union_type_and_lookup(Expr, UnionEvalType) -->>
    eval_union_type(Expr, UnionEvalType0),
    eval_lookup(UnionEvalType0, UnionEvalType).

%! eval_single_type_and_lookup(+Expr, -UnionEvalType)//[kyfact, symrej, file_meta] is det.
%  Evaluate (non-union) Expr and look it up in the symtab.
eval_single_type_and_lookup(Expr, UnionEvalType) -->>
    eval_single_type(Expr, UnionEvalType0),
    eval_lookup(UnionEvalType0, UnionEvalType).

%! eval_lookup(+UnionType, -UnionEvalType)//[kyfact, symrej, file_meta] is det.
%  Look up an evaluated union type, generating a union UnionEvalType.
% TODO: handle [string], [number], etc.
%       (this is a nice-to-do, for when we add more support for Kythe's
%       type annotations; but for now, we really only need lookups for
%       functions (calls) and classes/imports (',' operation))
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
eval_lookup_single(import(Fqn, Path),
                   [import(Fqn, Path)]) -->> !,
    [ ].
eval_lookup_single(var(Fqn),
                   [var(Fqn)]) -->> !, [ ].
eval_lookup_single(_EvalType, []) -->> [ ].

%! eval_single_type(+Type, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
eval_single_type(fqn(Fqn), [fqn(Fqn)]) -->> [ ].
eval_single_type(dot(Atom, Astn, DotEdgeName), EvalType) -->>
    eval_union_type_and_lookup(Atom, AtomEval),
    % TODO: MRO for class -- watch out for Bases containing Unions!
    eval_atom_dot_union(AtomEval, Astn, DotEdgeName, EvalType).
eval_single_type(call(Atom, Parms), EvalType) -->>
    eval_union_type_and_lookup(Atom, AtomEval),
    maplist_kyfact_symrej(eval_union_type, Parms, ParmsEval),
    eval_atom_call_union(ParmsEval, AtomEval, EvalType).
eval_single_type(call_op(OpAstns, ArgsType), [call_op(OpAstns, ArgsTypeEval)]) -->>
    maplist_kyfact_symrej(eval_union_type, ArgsType, ArgsTypeEval).
eval_single_type(class(Name, Bases), [class(Name, BasesEval)]) -->>
    maplist_kyfact_symrej(eval_union_type, Bases, BasesEval).
eval_single_type(import(Fqn, Path), [import(Fqn, Path)]) -->>
    [ ].  % TODO: look-up
eval_single_type(func(Name, ReturnType), [func(Name, ReturnTypeEval)]) -->>
    eval_union_type_and_lookup(ReturnType, ReturnTypeEval).
eval_single_type(ellipsis, []) -->> [ ].
eval_single_type(module(Fqn, Path), [module(Fqn, Path)]) -->> [ ].
eval_single_type(omitted, []) -->> [ ].

% TODO: implement the following:
eval_single_type(todo_compfor(iter:_CompIterType, for:_ForExprlistType, in:_InTestlistType), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_compifcompiter(_ValueExprType, _CompIterType), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_decorated(_ItemsType), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_decorator_dottedname(_ItemsType), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_decorators(_ItemsType), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_dictgen(_ValueExprType, _CompForType), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_dictkeyvaluelist(_ItemsType), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_dictset(_ItemsType), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_dottedname(_ItemsType), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_expr(stmts), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_typedarg(), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_subscr(_), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_arg(_, _), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_list(_), []) -->> [ ].  % [kyfact, symrej, file_meta]
eval_single_type(todo_exprlist(_), []) -->> [ ].  % [kyfact, symrej, file_meta]

%! eval_atom_dot_union(+AtomEval:ordset, +Astn, +DotEdgeName:atom, -EvalType:ordset)//[kyfact, symrej, file_meta]
%  Helper for eval(dot(Atom, Astn, DotEdgeName)), which loops over the
%  individual types in the (union) AtomEval and creates a union type
%  of all the possibilities.
eval_atom_dot_union(AtomEval, Astn, DotEdgeName, EvalType) -->>
    { ord_empty(EvalType0) },
    maplist_foldl_kyfact_symrej(
        eval_atom_dot_union_of_type(Astn, DotEdgeName), AtomEval, EvalType0, EvalType).

eval_atom_dot_union_of_type(Astn, DotEdgeName, T, EvalType0, EvalType) -->>
    eval_single_type(T, ET0),
    maplist_foldl_kyfact_symrej(
            eval_atom_dot_single(Astn, DotEdgeName), ET0, EvalType0, EvalType).

%! eval_atom_dot_single(+Astn, +DotEdgeName:atom, +Type, +EvalType0:ordset, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
%  Process a single type-dot-attr, adding to EvalType
% TODO: also allow func(...).attr (currently only allows class(...).attr
eval_atom_dot_single(astn(Start, End, Attr), DotEdgeName, class(ClassName, _), EvalType0, EvalType) -->> !,
    { atomic_list_concat([ClassName, '.', Attr], FqnAttr) },
    { ord_add_element(EvalType0, fqn(FqnAttr), EvalType) },
    kyanchor(Start, End, Source),
    kyedge_fqn(Source, DotEdgeName, FqnAttr).
eval_atom_dot_single(astn(Start, End, Attr), DotEdgeName, import(_Fqn, file(Path)), EvalType0, EvalType) -->> !,
    kyanchor(Start, End, Source),
    { atomic_list_concat([Path, '::', Attr], FqnAttr) },  % TODO: need to resolve path
    kyedge_fqn(Source, DotEdgeName, FqnAttr),  % TODO: does this belong here?
    { EvalType = EvalType0 }.
eval_atom_dot_single(astn(Start, End, Attr), DotEdgeName, import(_Fqn, file_and_token(Path, Token)), EvalType0, EvalType) -->> !,
    kyanchor(Start, End, Source),
    { atomic_list_concat([Path, '::', Token, '::', Attr], FqnAttr) },  % TODO: need to resolve path
    kyedge_fqn(Source, DotEdgeName, FqnAttr),  % TODO: does this belong here?
    { EvalType = EvalType0 }.
eval_atom_dot_single(_Astn, _DotEdgeName, _Type, EvalType, EvalType) -->> [ ].

%! eval_atom_call_union(+Parms, +AtomEval:ordset, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
%  Helper for eval_single_type(call(Atom, Parms)), which loops over
%  the individual types in the (union) AtomEval and creates a union
%  type of all the possibilities.
eval_atom_call_union(Parms, AtomEval, EvalType) -->>
    { ord_empty(EvalType0) },
    maplist_foldl_kyfact_symrej(
        eval_atom_call_single_of_type(Parms), AtomEval, EvalType0, EvalType).

%! eval_atom_call_single_of_type(+Parms, +Type, +EvalType0, -EvalType) is det.
%  Helper for eval_atom_call_union
eval_atom_call_single_of_type(Parms, Type, EvalType0, EvalType) -->>
    eval_single_type(Type, TypeEval),
    maplist_foldl_kyfact_symrej(
        eval_atom_call_single(Parms), TypeEval, EvalType0, EvalType).

%! eval_atom_call_single(+Parms, +Type, +EvalType0:ordset, -EvalType:ordset)//[kyfact, symrej, file_meta] is det.
%  Process a single call, adding to EvalType
eval_atom_call_single(_Parms, class(Fqn, Bases), EvalType0, EvalType) -->>  !,
    % TODO: MRO for__init__ and output ref to it
    { ord_add_element(EvalType0, class(Fqn, Bases), EvalType) }.
eval_atom_call_single(_Parms, func(_, ReturnType), EvalType0, EvalType) -->>  !,
    % ord_union because ReturnTYpe is a union type
    { ord_union(EvalType0, ReturnType, EvalType) }.
eval_atom_call_single(Parms, T, EvalType0, EvalType) -->>
    { ord_add_element(EvalType0, func(T, Parms), EvalType) }.

%! exprs_from_symtab(+SymtabPair:pair, -Exprs) is semidet.
%  Using the Fqn-Type pairs dict_pairs, get expr if it has a non-[] type.
expr_from_symtab(_Fqn-Type, expr(Type)) :-
    Type = [_|_].

%! add_rej_to_symtab(+FqnRejType:pair, +Symtab0, -Symtab) is det.
%  For Fqn-RejType pairs in FqnRejTypes, add to symtab.
add_rej_to_symtab(Fqn-RejType, Symtab0, Symtab) :-
    get_dict(Fqn, Symtab0, FqnType),
    ord_union(FqnType, RejType, CombinedType),
    put_dict(Fqn, Symtab0, CombinedType, Symtab).

%! symrej_accum(+FqnType:pair, +Symtab0Rej0:pair, +SymtabRej:pair) is det.
%  The accumulator for 'symrej'.
%  Tries to unify Key-Type with what's already in symtab; if that
%  fails because it's not in the symtab, adds it to symtab; otherwise
%  adds it Rej.
%  If Type is uninstantiated it gets set to []
%  TODO: can we eliminate the "(Type=[]->true;true)" ?
% TODO: use library(assoc) or library(rbtrees) or trie or hash
%       instead of dict for Symtab (performance)
symrej_accum(Fqn-Type, Symtab0-Rej0, Symtab-Rej) :-
    (  get_dict(Fqn, Symtab0, TypeSymtab)
    -> Symtab = Symtab0,
       symrej_accum_found(Fqn, Type, TypeSymtab, Rej0, Rej)
    ;  Rej = Rej0,
       ( Type = [] -> true ; true ),  %% in case Type is not instantiated (i.e., a lookup)
       put_dict(Fqn, Symtab0, Type, Symtab)
    ).

%! symrej_accum_found(+Fqn, +Type, +TypeSymtab, +Rej0, -Rej).
%  Helper for symrej_accum/3 for when Fqn is in Symtab with value
%  TypeSymtab (Type is the new type).
symrej_accum_found(Fqn, Type, TypeSymtab, Rej0, Rej) :-
   (  Type = TypeSymtab  %% in case Type is not instantiated (i.e., a lookup)
   -> Rej = Rej0
   ;  ord_union(TypeSymtab, Type, TypeComb),  % TODO: ord_intersect(TypeSymtab, Type) ?
      TypeComb = TypeSymtab
   -> Rej = Rej0
   ;  Rej = [Fqn-Type|Rej0]
   ).

%! dict_values(+Dict, -Values) is det.
%     True when Values is an ordered set of the values appearing in Dict.
% TODO: this should be in library(dicts).
% TODO: this isn't used?
dict_values(Dict, Values) :-
    dict_pairs(Dict, _Tag, Pairs),
    pairs_values(Pairs, Values).

%! portray is det.
%  For more compact output (debugging).
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
portray(meta{kythe_corpus: KytheCorpus,
             kythe_root: KytheRoot,
             path: Path,
             language: _Language,
             encoding: _Encoding,
             file_contents_b64: _ContentsB64,
             pythonpaths: _Pythonpaths}) :-
    format('meta{~q, ~q, ~q}',
           [KytheCorpus, KytheRoot, Path]).
portray('$VAR'('_')) :- !,  % work around a bug in print_term
    format('_', []).        % (numbervars(true) should handle this):
portray('$VAR'(N)) :- !,
    Chr is N + 65,
    format('~c', [Chr]).
portray(Assoc) :-
    is_assoc(Assoc), !,
    aggregate_all(count, gen_assoc(_, Assoc, _), Length),
    min_assoc(Assoc, MinKey, MinValue),
    max_assoc(Assoc, MaxKey, MaxValue),
    format('<assoc:~d, ~p: ~p ... ~p: ~p>', [Length, MinKey, MinValue, MaxKey, MaxValue]).
% portray(Symtab) :-
%     is_dict(Symtab, Tag), !,
%     ground(Tag),
%     Tag = symtab, !,
%     format('symtab{...}').

%! do_if(:Cond, :Pred) is det.
%  A handy meta-predicate for turning debug stuff on/off, according to Cond
do_if(Cond, Pred) :-
    (  call(Cond)
    -> call(Pred)
    ;  true
    ).

%! dump_term(+Msg:atom, +Term) is det.
% TODO: Delete this debugging code
dump_term(Msg, Term) :-
    dump_term(Msg, Term, [tab_width(0),
                          indent_arguments(2),
                          right_margin(100)]).
%! dump_term(+Msg:atom, +Term, +Options:list) is det.
% TODO: Delete this debugging code
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
%  print_term, cleaned up
print_term_cleaned(Term, Options, TermStr) :-
    % print_term leaves trailing whitespace, so remove it
    with_output_to(
            string(TermStr0),
            (current_output(TermStream),
             print_term(Term, [output(TermStream)|Options]))),
    re_replace(" *\n"/g, "\n", TermStr0, TermStr).

%! zip_merge(+Xs:list, Ys:list, -XYs:list) is det
%  zip_merge([a,b], [1,2,], [a-1, b-2])
zip_merge([], [], []).
zip_merge([X|Xs], [Y|Ys], [X-Y, XYs]) :-
    zip_merge(Xs, Ys, XYs).

% Variants on maplist, foldl (and combinations of them) for EDCGs

%! maplist_kyfact(:Pred, +L:list)//[kyfact, file_meta] is det.
% maplist/2 for EDCG [kyfact, file_meta]
maplist_kyfact(_Pred, []) -->> [ ].
maplist_kyfact(Pred, [X|Xs]) -->>
    call(Pred, X):[kyfact,file_meta],
    maplist_kyfact(Pred, Xs).

%! maplist_kyfact(:Pred, +L0:list, -L:list)//[kyfact, file_meta] is det.
% maplist/3 for EDCG [kyfact, file_meta]
maplist_kyfact(_Pred, [], []) -->> [ ].
maplist_kyfact(Pred, [X|Xs], [Y|Ys]) -->>
    call(Pred, X, Y):[kyfact,file_meta],
    maplist_kyfact(Pred, Xs, Ys).

%! maplist_kyfact_symrej(:Pred, +L0:list, -L:list)//[kyfact, symrej, file_meta] is det.
%  maplist/3 for EDCG [kyfact, symrej, file_meta]
maplist_kyfact_symrej(_Pred, [], []) -->> [ ].
maplist_kyfact_symrej(Pred, [X|Xs], [Y|Ys]) -->>
    call(Pred, X, Y):[kyfact,symrej,file_meta],
    maplist_kyfact_symrej(Pred, Xs, Ys).

%! maplist_kyfact_expr(:Pred, +L0:list)//[kyfact, expr, file_meta] is det.
%  maplist/2 for EDCG [kyfact, expr, file_meta]
maplist_kyfact_expr(_Pred, []) -->> [ ].
maplist_kyfact_expr(Pred, [X|Xs]) -->>
    call(Pred, X):[kyfact,expr,file_meta],
    maplist_kyfact_expr(Pred, Xs).

%! maplist_kyfact_expr(:Pred, +L0:list, -L:list)//[kyfact, expr, file_meta] is det.
%  maplist/3 for EDCG [kyfact, expr, file_meta]
maplist_kyfact_expr(_Pred, [], []) -->> [ ].
maplist_kyfact_expr(Pred, [X|Xs], [Y|Ys]) -->>
    call(Pred, X, Y):[kyfact,expr,file_meta],
    maplist_kyfact_expr(Pred, Xs, Ys).

%! maplist_kyfact_expr(:Pred, +L0:list, -L:list)//[kyfact, expr, file_meta] is det.
%  maplist/2 plus fold/4 for EDCG [kyfact, expr, file_meta]
maplist_foldl_kyfact_expr(_Pred, [], V, V) -->> [ ].
maplist_foldl_kyfact_expr(Pred, [X|Xs], V0, V) -->>
    call(Pred, X, V0, V1):[kyfact,expr,file_meta],
    maplist_foldl_kyfact_expr(Pred, Xs, V1, V).

%! maplist_foldl_kyfact_symrej(:Pred, -L0:list, +L:list, -V0, +V) is det.
%  maplist/2 plus foldl/4 for EDCG [kyfact, symrej, file_meta]
maplist_foldl_kyfact_symrej(_Pred, [], V, V) -->> [ ].
maplist_foldl_kyfact_symrej(Pred, [X|Xs], V0, V) -->>
    call(Pred, X, V0, V1):[kyfact,symrej,file_meta],
    maplist_foldl_kyfact_symrej(Pred, Xs, V1, V).
