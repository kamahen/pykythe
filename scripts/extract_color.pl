:- use_module(library(http/json), [json_read_dict/3, json_write_dict/3]).

%% See test predicates at end.

%% swipl -g get_and_print_color_text -t halt extract_color.pl </tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/t10.kythe.json --filesdir=/tmp/pykte_test/browser/files
%% debugging: get_and_print_color_text('/tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/t10.kythe.json').

%! kythe_node(Source: vname, FactName:atom, FactValue:atom).
%! kythe_edge(Source: vname, EdgeKind:atom, Target:vname).
%% Kythe facts are of one of these forms:
%%   (source, edge, target, /, "")
%%   (source, edge, target, /kythe/ordinal, base10string)
%%   (source, "", "", string, _)

%% Kythe vname is Signature,Corpus,Root,Path,Language
%% We also define vname0:   Corpus,Root,Path,Language

:- use_module(library(optparse), [opt_arguments/3]).
:- use_module('../pykythe/pykythe_utils.pl', [base64_utf8/2]).
:- use_module('../pykythe/must_once.pl').

:- dynamic kythe_node/7, kythe_edge/11.

get_and_print_color_text :-
    get_and_print_color_text(user_input).

get_and_print_color_text(InStream) :-
    log('Start'),
    extract_opts(Opts),
    retract_kythe_facts,
    get_and_assert_kythe_facts(InStream),
    log('get_and_assert_kythe_facts-done'),
        /* profile( */
    get_color_data(ColorData)
        /* ) */ ,
    log('get_color_data-done'),
    /* show_profile([cumulative(true)]), */
    maplist(file_name, ColorData, FileNames),
    open_output_stream(Opts, 'FILES.js',
                       '', [],
                       FilesOutStream),
    json_write_dict(FilesOutStream, FileNames,
                    [width(0),
                     true(#(true)),false(#(false)),null(#(null))]),
    close_output_stream(FilesOutStream,
                        '~n', []),
    maplist(write_color_data(Opts), ColorData, FileNames),
    log('json_write_dict-done'),
    nl.

file_name(json{corpus:Corpus, root:Root, path:Path, language:Language,
               line_keys:_LineKeys,
               lines:_ColorText},
          json{corpus: Corpus, root: Root, path: Path, language: Language,
               filename: FileName}) :-
    format(atom(CombinedName), '~w', [Corpus:Root:Path]),
    base64(CombinedName, FileName0),
    atomic_list_concat(['file-', FileName0, '.js'], FileName).

write_color_data(Opts,
                 json{corpus:Corpus, root:Root, path:Path, language:Language,
                      line_keys:LineKeys,
                      lines:ColorText},
                 json{corpus: Corpus, root: Root, path: Path, language: Language,
                      filename: FileName}) :-
    memberchk(filesdir(FilesDir), Opts),
    log('outputting to directory ~q: ~q', [FilesDir/FileName, Path]),
    open_output_stream(Opts, FileName,
                       '', [],
                       OutStream),
    json_write_dict(OutStream,
                    json{corpus: Corpus, root: Root, path: Path, language: Language,
                         line_keys:LineKeys,
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
    maplist(get_color_data1, Files, ColorData).

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

get_color_data1(Vname0,
                json{corpus:Corpus, root:Root, path:Path, language:Language,
                     line_keys:LineKeys,
                     lines:ColorText}) :-
    Vname0 = vname0(Corpus,Root,Path,Language),
    get_color_text_file(Vname0, ColorText),
    dict_keys(ColorText, LineKeys),
    log('get_color-dict_keys-done ~q', [Vname0]).

get_color_text_file(Vname0, ColorTextLines) :-
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
    maplist(lineno_and_line, ColorText, LineNoAndLines0, LineNos0),
    sort(LineNoAndLines0, LineNoAndLines),
    sort(LineNos0, LineNos),
    (  setof(LineNoStr-LineItems,
             single_line_items(LineNoAndLines, LineNos, LineNoStr, LineItems),
             ColorTextLines0)
    -> true
    ;  ColorTextLines0 = []
    ),
    maplist(add_links(Vname0), ColorTextLines0, ColorTextLines1),
    dict_pairs(ColorTextLines, json, ColorTextLines1).

single_line_items(LineNoAndLines, LineNos, LineNoStr, LineItems) :-
    member(LineNo, LineNos),
    (  setof(Item, not_newline(LineNo, LineNoAndLines, Item), LineItems)
    -> true
    ;  LineItems = []
    ),
    %% Make the keys sort nicely:
    %% Javascript: ('00000000' + lineno).slice(-8)
    %%             or (''+lineno).PadStart(8, '0')
    format(atom(LineNoStr), '~|~`0t~d~8+', [LineNo]).

lineno_and_line(Line, LineNo-Line, LineNo) :-
    LineNo = Line.lineno.

not_newline(LineNo, Items, Item) :-
    member(LineNo-Item, Items),
    Item.token_color \== '<NEWLINE>'.

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
    must_once_msg(PrologVersion >= 80120, 'SWI-Prolog version is too old'), % Sync this with README.md
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

user:portray(Term) :-
    is_dict(Term, TermTag),
    format('~w{...}', [TermTag]).

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

test :-
    open('/tmp/t10.kythe.json', read, InStream),
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
