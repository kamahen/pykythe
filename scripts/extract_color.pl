% -*- mode: Prolog -*-

%% Read in Kythe JSON facts and extract the color-related data,
%% outputting to a directory, for use by src_browser.pl

%% TODO: performance
%%  pykythe_utils:base64_utf8/2 (and b64_to_utf8_to_atom/2) take 57% of the CPU.
%%  json:json_read_dict/3 takes 41%
%%  json:json_string_codes/3 takes 15% (most of it in utf8_codes/3).

:- use_module(library(http/json), [json_read_dict/3, json_write_dict/3]).
:- use_module(library(pairs)).

%% swipl -g get_and_print_color_text -t halt extract_color.pl </tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/t10.kythe.json --filesdir=/tmp/pykte_test/browser/files
%% debugging: get_and_print_color_text('/tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/t10.kythe.json').

%% See also test predicates at end.

%! kythe_node(Source: vname, FactName:atom, FactValue:atom).
%! kythe_edge(Source: vname, EdgeKind:atom, Target:vname).
%% Kythe facts are of one of these forms:
%%   (source, edge, target, /, "")
%%   (source, edge, target, /kythe/ordinal, base10string)
%%   (source, "", "", string, _)

%% Kythe vname is Signature,Corpus,Root,Path,Language
%% We also define vname0:   Corpus,Root,Path,Language

:- use_module(library(optparse), [opt_arguments/3]).
:- use_module(library(thread), [concurrent_maplist/3]).
:- use_module('../pykythe/pykythe_utils.pl', [base64_utf8/2]).
:- use_module('../pykythe/must_once.pl').

:- dynamic kythe_node/7, kythe_edge/11.

get_and_print_color_text :-
    get_and_print_color_text(user_input).

get_and_print_color_text(InStream) :-
    % TODO: remove with swipl 8.1.21
    set_stream(InStream, tty(false)), % try to ensure no prompting
    prompt(_, ''),                    % really ensure no prompting
    log('Start'),
    extract_opts(Opts),
    retract_kythe_facts,
    get_and_assert_kythe_facts(InStream),
    log('get_and_assert_kythe_facts-done'),
    get_color_data(ColorData),
    log('get_color_data-done'),
    /* show_profile([cumulative(true)]), */
    maplist(file_name, ColorData, FileNames),
    maplist(file_path, ColorData, PathNames),
    files_to_tree(PathNames, PathTree),
    tree_to_json(PathTree, PathTreeJson),
    do_output_stream(Opts, 'FILES.json', '', [],
                     pykythe_json_write_dict(FileNames),
                     '~n', []),
    do_output_stream(Opts, 'FILETREE.json', '', [],
                     pykythe_json_write_dict(PathTreeJson),
                     '~n', []),
    do_output_stream(Opts, 'kythe_facts.pl', '', [],
                     write_kythe_facts,
                     '', []),
    concurrent_maplist(write_color_data(Opts), ColorData, FileNames),  % concurrent gives a slight speed-up
    log('json_write_dict-done'),
    nl.

pykythe_json_write_dict(Data, OutStream) :-
    json_write_dict(OutStream, Data,
                    [width(0),
                     true(#(true)),false(#(false)),null(#(null))]).

write_kythe_facts(KytheFactsOutStream) :-
    %% TODO: use fast_serialize?
    forall(kythe_node(Signature,Corpus,Root,Path,Language, FactName, FactValue),
           format(KytheFactsOutStream, '~q.~n',
                  [kythe_node(Signature,Corpus,Root,Path,Language, FactName, FactValue)])),
    forall(kythe_edge(Signature1,Corpus1,Root1,Path1,Language1, EdgeName,
                      Signature2,Corpus2,Root2,Path2,Language2),
           format(KytheFactsOutStream, '~q.~n',
                  [kythe_edge(Signature1,Corpus1,Root1,Path1,Language1, EdgeName,
                              Signature2,Corpus2,Root2,Path2,Language2)])).

file_name(json{corpus:Corpus, root:Root, path:Path, language:Language,
               lines:_ColorText},
          json{corpus: Corpus, root: Root, path: Path, language: Language,
               filename: FileName}) :-
    format(atom(CombinedName), '~w:~w:~w', [Corpus, Root, Path]),
    base64(CombinedName, FileName0),
    atomic_list_concat(['file-', FileName0, '.json'], FileName).

file_path(json{corpus:Corpus, root:Root, path:Path, language:_Language,
               lines:_ColorText},
          CombinedPath) :-
    format(atom(CombinedPath), '~w/~w/~w', [Corpus, Root, Path]).

write_color_data(Opts,
                 json{corpus:Corpus, root:Root, path:Path, language:Language,
                      lines:ColorText},
                 json{corpus: Corpus, root: Root, path: Path, language: Language,
                      filename: FileName}) :-
    open_output_stream(Opts, FileName,
                       '', [],
                       OutStream),
    json_write_dict(OutStream,
                    json{corpus: Corpus, root: Root, path: Path, language: Language,
                         lines: ColorText},
                    [width(0),
                     true(#(true)),false(#(false)),null(#(null))]),
    close_output_stream(OutStream,
                        '~n', []). % '~n;~n', []).

get_color_data(ColorData) :-
    (  setof(vname0(Corpus,Root,Path,Language), kythe_file(Corpus,Root,Path,Language), Files)
    -> true
    ;  Files = []
    ),
    assertion(Files \== []),
    concurrent_maplist(get_color_data_one_file, Files, ColorData). % concurrent gives slight speed-up

kythe_file(Corpus, Root, Path, Language) :-
    kythe_node(vname('',Corpus,Root,Path,_), '/kythe/node/kind', file),
    (  kythe_node(vname('',Corpus,Root,Path,_), '/kythe/language', Language)
    -> true
    ;  file_name_extension(_, Extension, Path),
       guess_language(Extension, Language)
    ).

guess_language(py, python).
guess_language(Extension, _) :-
    domain_error(file_name_extension, Extension).

get_color_data_one_file(Vname0,
                        json{corpus:Corpus, root:Root, path:Path, language:Language,
                             lines:ColorText}) :-
    statistics(thread_cputime, T0),
    Vname0 = vname0(Corpus,Root,Path,Language),
    get_color_text_one_file(Vname0, ColorText),
    statistics(thread_cputime, T1),
    T is T1 - T0,
    log('get_color-dict_keys-done ~q (~3f sec)', [Vname0, T]).

get_color_text_one_file(Vname0, ColorTextLines) :-
    Vname0 = vname0(Corpus,Root,Path,Language),
    setof(ColorTextStr,
          ( kythe_node(vname('',Corpus,Root,Path,Lang), '/pykythe/color', ColorTextStr),
            ( Lang = Language; Lang = '')),
          ColorTextStrs),
    (  ColorTextStrs = [ColorTextStr]
    -> true
    ;  throw('Multiple /pykythe/color':Vname0)
    ),
    catch(term_string(ColorText, ColorTextStr),
           Err,
           ( ColorText = [],
             log('ERROR: ~q in ~q', [Err, ColorTextStr]))),
    maplist(lineno_and_chunk, ColorText, LineNoAndChunk0),
    keysort(LineNoAndChunk0, LineNoAndChunk),
    group_pairs_by_key(LineNoAndChunk, ColorTextLines0),
    maplist(add_links(Vname0), ColorTextLines0, ColorTextLines1), % concurrent gives slight slow-down
    pairs_values(ColorTextLines1, ColorTextLines).

lineno_and_chunk(Chunk, LineNo-Chunk) :-
    LineNo = Chunk.lineno.

add_links(Vname0, LineNo-Items, LineNo-AppendedItems) :-
    maplist(add_link(Vname0), Items, AppendedItems).

add_link(Vname0, Item, ItemWithEdges) :-
    Signature = Item.signature,
    %% There can be multiple edges with the same label (but different
    %% targets, so leave as a list and don't combine into a dict.
    (  setof(json{edge:Edge,target:TargetJson},
             node_and_edge_json(Signature, Vname0, Edge, TargetJson),
             Edges)
    -> true
    ;  Edges = []
    ),
    put_dict(edges, Item, Edges, ItemWithEdges).

get_and_assert_kythe_facts(InStream) :-
    get_kythe_facts(InStream, KytheDicts),
    log('get_kythe_facts-done'),
    maplist(kythe_fact_pred, KytheDicts, Preds0),
    sort(Preds0, Preds), % remove dups, although there shouldn't be any
    assertion(ground(Preds)),
    maplist(assert_pred, Preds).

retract_kythe_facts :-
    functor(KytheNode, kythe_node, 7),
    functor(KytheEdge, kythe_edge, 11),
    retractall(KytheNode),
    retractall(KytheEdge).

get_kythe_facts(InStream, KytheDicts) :-
    json_read_dict(InStream, KytheDict,
                   [value_string_as(atom), end_of_file(@(end)), default_tag(json)]),
    (  KytheDict == @(end)
    -> KytheDicts = []
    ;  KytheDicts = [KytheDict|KytheDicts2],
       get_kythe_facts(InStream, KytheDicts2)
    ).

%! kythe_fact_pred(+JsonFact:dict, -Pred) is det.
%% See transform_kythe_fact/3 in pykythe.pl
kythe_fact_pred(json{source:Source0, fact_name:FactName, fact_value:FactValueB64},
                kythe_node(Source1, FactName, FactValue)) :- !,
    vname_fix(Source0, Source1),
    (  FactName == '/pykythe/symtab'
    -> FactValue = FactValueB64  % It's not b64-encoded for performance
    ;  base64_utf8(FactValue, FactValueB64)
    ).
kythe_fact_pred(json{source:Source0, fact_name:'/', edge_kind:EdgeKind, target:Target0},
                kythe_edge(Source1, EdgeKind, Target1)) :- !,
    vname_fix(Source0, Source1),
    vname_fix(Target0, Target1).
kythe_fact_pred(Fact, Fact) :-
    domain_error(json, Fact).

node_and_edge_json(Signature, Vname0, Edge, TargetJson) :-
    node_and_edge(Signature, Vname0, Edge, Target),
    vname_json(Target, TargetJson).

node_and_edge(Signature, Vname0, Edge, Target) :-
    vname_vname0(Vname, Signature, Vname0),
    node_and_edge(Vname, Edge, Target).

node_and_edge(Vname, Edge, Target) :-
    kythe_edge(Vname, Edge, Target).
node_and_edge(Vname, Edge, Target) :-
    kythe_edge(Target, ReverseEdge, Vname),
    reverse_edge_name(ReverseEdge, Edge).

reverse_edge_name(Edge, ReverseEdge) :-
    atom_concat('%', Edge, ReverseEdge).

%! vname_fix(+Json:json, -Vname:vname) is det.
%% vname is same as verifier:
%%    vname(Signature, Corpus, Root, Path, Language)
vname_fix(json{corpus:Corpus, root:Root},
          vname('', Corpus, Root, '', '')) :- !.
vname_fix(json{corpus:Corpus, root:Root, path:Path},
          vname('', Corpus, Root, Path, '')) :- !.
vname_fix(json{corpus:Corpus, root:Root, language:Language, path:Path},
          vname('', Corpus, Root, Path, Language)) :- !.
vname_fix(json{corpus:Corpus, root:Root, language:Language, signature:Signature},
          vname(Signature, Corpus, Root, '', Language)) :- !.
vname_fix(json{corpus:Corpus, root:Root, language:Language, signature:Signature, path:Path},
          vname(Signature, Corpus, Root, Path, Language)) :- !.
vname_fix(Source, Source) :-
    domain_error(json-source, Source).

assert_pred(kythe_node(vname(Signature,Corpus,Root,Path,Language), FactName, FactValue)) :- !,
    assertz(kythe_node(Signature,Corpus,Root,Path,Language, FactName, FactValue)).
assert_pred(kythe_edge(vname(Signature1,Corpus1,Root1,Path1,Language1), EdgeName,
                       vname(Signature2,Corpus2,Root2,Path2,Language2))) :- !,
    assertz(kythe_edge(Signature1,Corpus1,Root1,Path1,Language1, EdgeName,
                       Signature2,Corpus2,Root2,Path2,Language2)).
assert_pred(Pred) :-
    domain_error(kythe_node_or_edge, Pred).

utf8_bytes_to_term_doesnt_work(Bytes, Term) :-
    %% TODO: delete this
    %% Doesn't work (Error: "No permission to encoding stream")
    %% because a string is a sequence of unicode chars. Python 2 made
    %% the mistake of mixing bytes and strings.
    open_string(Bytes, Stream),
    set_stream(Stream, encoding(utf8)),
    read(Stream, Term).

extract_opts(Opts) :-
    current_prolog_flag(version, PrologVersion),
    must_once_msg(PrologVersion >= 80121, 'SWI-Prolog version is too old'), % Sync this with README.md
    OptsSpec =
    [[opt(filesdir), type(atom), default('filesdir-must-be-specified'), longflags([filesdir]),
      help('Directory for putting the files\'s contents')
     ]],
    opt_arguments(OptsSpec, Opts, PositionalArgs),
    must_once_msg(PositionalArgs = [], 'Unknown positional arg(s)').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Convenience predicates for vnames

kythe_node(vname(Signature,Corpus,Root,Path,Language), FactName, FactValue) :-
    kythe_node(Signature,Corpus,Root,Path,Language, FactName, FactValue).
kythe_node(Signature, vname0(Corpus,Root,Path,Language), FactName, FactValue) :-
    kythe_node(Signature,Corpus,Root,Path,Language, FactName, FactValue).

kythe_edge(vname(Signature1,Corpus1,Root1,Path1,Language1), EdgeName,
           vname(Signature2,Corpus2,Root2,Path2,Language2)) :-
    kythe_edge(Signature1,Corpus1,Root1,Path1,Language1, EdgeName,
               Signature2,Corpus2,Root2,Path2,Language2).
kythe_edge(Signature1,vname0(Corpus1,Root1,Path1,Language1), EdgeName,
           Signature2,vname0(Corpus2,Root2,Path2,Language2)) :-
    kythe_edge(Signature1,Corpus1,Root1,Path1,Language1, EdgeName,
               Signature2,Corpus2,Root2,Path2,Language2).

vname_vname0(vname(Signature,Corpus,Root,Path,Language),
             Signature,
             vname0(Corpus,Root,Path,Language)).

vname_json(vname(Signature,Corpus,Root,Path,Language),
           json{signature:Signature,corpus:Corpus,root:Root,path:Path,language:Language}).

do_output_stream(Opts, FileName, Fmt1, Args1, Goal, Fmt2, Args2) :-
    open_output_stream(Opts, FileName, Fmt1, Args1, OutStream),
    call(Goal, OutStream),
    close_output_stream(OutStream, Fmt2, Args2).

open_output_stream(Opts, FileName, Fmt, Args, OutStream) :-
    memberchk(filesdir(FilesDir), Opts),
    make_directory_path(FilesDir),
    atomic_list_concat([FilesDir, FileName], '/', FullPath),
    open(FullPath, write, OutStream, [encoding(utf8)]),
    format(OutStream, Fmt, Args).

close_output_stream(OutStream, Fmt, Args) :-
    format(OutStream, Fmt, Args),
    close(OutStream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% user:portray(Term) :-
%     is_dict(Term, TermTag),
%     format('~w{...}', [TermTag]).

%% For non-graphical profiling:
:- unload_file('/usr/lib/swi-prolog/xpce/prolog/lib/swi/pce_profile.pl').
pce_profile:pce_show_profile :- fail.

log(Fmt) :- log(Fmt, []).
log(Fmt, Args) :-
    atomic_list_concat(['~` t~3f~8|: ', Fmt, '~n'], '', Fmt2),
    statistics(process_cputime, Time),
    format(user_error, Fmt2, [Time|Args]).

kythe_anchor(Vname, Start, End, Token) :-
    kythe_node(Vname, '/kythe/node/kind', anchor),
    kythe_node(Vname, '/kythe/loc/start', StartStr),
    kythe_node(Vname, '/kythe/loc/end', EndStr),
    term_string(Start, StartStr),
    term_string(End, EndStr),
    Len is End - Start,
    Vname = vname(_, Corpus, Root, Path, _),
    kythe_node(vname('', Corpus, Root, Path, _), '/kythe/text', SourceText),
    sub_string(SourceText, Start, Len, _, TokenStr),
    atom_string(Token, TokenStr).


%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Transform a list of file names to a tree for browser.

files_to_tree(Files, Tree) :-
    maplist(split_on_slash, Files, SplitFiles),
    list_to_tree(SplitFiles, [], Tree).

split_on_slash(Str, Split) :-
    split_string(Str, '/', '', SplitStr),
    maplist(string_atom, SplitStr, Split0),
    once(append(First, [Last], Split0)), % like last/2 but also gets first part.
    maplist(wrap_dir, First, FirstWrapped),
    append(FirstWrapped, [file(Last)], Split).

wrap_dir(Dir, dir(Dir)).

string_atom(Str, Atom) :- atom_string(Atom, Str).

list_to_tree(List, Prefix, Tree) :-
    maplist(head_tail_pair, List, HeadTail),
    keysort(HeadTail, HeadTailSorted),
    group_pairs_by_key(HeadTailSorted, HeadTailGroup),
    maplist(subtree(Prefix), HeadTailGroup, Tree).

subtree(Prefix, Item-Sublist, Result) :-
    subtree_(Item, Sublist, Prefix, Result).

subtree_(dir(Dir), Sublist, Prefix, dir(Dir,Path,Children)) :-
    append(Prefix, [Dir], Prefix2),
    atomic_list_concat(Prefix2, '/', Path),
    list_to_tree(Sublist, Prefix2, Children).
subtree_(file(File), [[]], Prefix, file(File,Path)) :-
    append(Prefix, [File], Prefix2),
    atomic_list_concat(Prefix2, '/', Path).

head_tail_pair([Hd|Tl], Hd-Tl).

tree_to_json([X|Xs], Ys) :-
    maplist(tree_to_json, [X|Xs], Ys).
tree_to_json(file(N,Path), json([type=file, name=N, path=Path])).
tree_to_json(dir(N,Path,Children), json([type=dir, name=N, path=Path, children=ChildrenDict])) :-
    tree_to_json(Children, ChildrenDict).

%%%%%%%%%%%%%%%%%%%%%%%%%%

% For manual testing:

test :-
    open('/tmp/i7.kythe.json', read, InStream),
    get_and_print_color_text(InStream).

test_get_facts :-
    open('/tmp/t10.kythe.json', read, InStream),
    get_and_assert_kythe_facts(InStream).

test_anchor_and_edges :-
    test_get_facts,
    forall((kythe_anchor(Vname, Start, End, Token),
            setof(Edge-Target, node_and_edge(Vname, Edge, Target), Edges),
            Edges = [_,_|_]),
           format('~q~n', [Vname:Start:End:Token:Edges])).

%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(plunit)).

:- begin_tests(file_tree).

t1(Ftree) :-
    F = ['x',
         'a/b/x1',
         'a/b/x2',
         'a/c',
         'a/d/e/x3'
        ],
    files_to_tree(F, Ftree),
    tree_to_json(Ftree, FtreeJson),
    with_output_to(atom(JsonAtom),
                   (current_output(JsonStream),
                    json_write_dict(JsonStream, FtreeJson,
                                    [width(0),
                                     true(#(true)),false(#(false)),null(#(null))]))),
    %% format('~w~n', [JsonAtom]),
    assertion(JsonAtom == '[ {"type":"dir", "name":"a", "path":"a", "children": [ {"type":"dir", "name":"b", "path":"a/b", "children": [ {"type":"file", "name":"x1", "path":"a/b/x1"},  {"type":"file", "name":"x2", "path":"a/b/x2"} ]},  {"type":"dir", "name":"d", "path":"a/d", "children": [ {"type":"dir", "name":"e", "path":"a/d/e", "children": [ {"type":"file", "name":"x3", "path":"a/d/e/x3"} ]} ]},  {"type":"file", "name":"c", "path":"a/c"} ]},  {"type":"file", "name":"x", "path":"x"} ]').

test(f1, [true]) :-
    t1(Ftree),
    assertion(
              [dir(a, 'a',
                   [dir(b, 'a/b',
                        [file(x1, 'a/b/x1'), file(x2, 'a/b/x2')]
                       ),
                    dir(d, 'a/d',
                        [
                         dir(e, 'a/d/e',
                             [file(x3, 'a/d/e/x3')]
                            )
                        ]
                       ),
                    file(c, 'a/c')
                   ]
                  ),
               file(x, 'x')
              ]
             = Ftree).

:- end_tests(file_tree).
