% -*- mode: Prolog -*-

%% Read in Kythe JSON facts and convert them to Prolog facts for use
%% by src_browser.pl
%% Input is from stdin.
%% For historical reasons, output is specified as a dir and the
%% facts are in kythe_facts.pl
%% TODO: change how output is specified.

%% TODO: Change processing -- currently, a list of facts is created,
%%       then asserted, then the asserted facts are output. This cause
%%       code duplication ... better to just generate a list of facts
%%       to be output.

% TODO: performance
%  pykythe_utils:base64_utf8/2 (and b64_to_utf8_to_atom/2) take 57% of the CPU.
%  json:json_read_dict/3 takes 41%
%  json:json_string_codes/3 takes 15% (most of it in utf8_codes/3).

:- use_module(library(http/json), [atom_json_dict/3, json_read_dict/3]).
:- use_module(library(base64), [base64/2]).
:- use_module(library(pairs)).

% debugging: main('/tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/t10.kythe.json').

% Kythe vname is Signature,Corpus,Root,Path,Language
% We also define vname0:   Corpus,Root,Path,Language

% kythe_node(Source: vname, FactName:atom, FactValue:atom).
% kythe_edge(Source: vname, EdgeKind:atom, Target:vname).
% Kythe facts are of one of these forms:
%   (source, edge, target, /, "")
%   (source, edge, target, /kythe/ordinal, base10string)
%   (source, "", "", string, _)

:- use_module(library(optparse), [opt_arguments/3]).
:- use_module('../pykythe/pykythe_utils.pl', [base64_utf8/2, log_if/2, log_if/3, validate_prolog_version/0]).
:- use_module('../pykythe/must_once.pl').

:- dynamic kythe_node/7, kythe_edge/11, kythe_color_line/6.

main :-
    main(user_input).

main(InStream) :-
    log_if(true, 'Start'),
    extract_opts(Opts),
    must_once(retract_kythe_facts),
    must_once(read_lines(InStream, Files)),
    length(Files, NumFiles),
    log_if(true, 'Processing ~d files.', [NumFiles]),
    concurrent_maplist(get_and_assert_kythe_facts, Files),
    must_once(
        do_output_stream(Opts, 'kythe_facts.pl', '', [],
                         write_kythe_facts,
                         '', [])),
    log_if(true, 'End').

read_lines(InStream, Files) :-
    read_lines(InStream, [], Files).

read_lines(InStream, FilesAcc, Files) :-
    read_line_to_string(InStream, FileStr),
    (  FileStr == end_of_file
    -> reverse(FilesAcc, Files)
    ;  read_lines(InStream, [FileStr|FilesAcc], Files)
    ).

write_kythe_facts(KytheFactsOutStream) :-
    % TODO: use fast_serialize?
    log_if(false, 'Write_kythe_facts: start'),
    forall(kythe_node(Signature,Corpus,Root,Path,Language, FactName, FactValue),
           format(KytheFactsOutStream, '~q.~n',
                  [kythe_node(Signature,Corpus,Root,Path,Language, FactName, FactValue)])),
    log_if(false, 'Write_kythe_facts: kythe_node-done'),
    forall(kythe_edge(Signature1,Corpus1,Root1,Path1,Language1, EdgeName,
                      Signature2,Corpus2,Root2,Path2,Language2),
           format(KytheFactsOutStream, '~q.~n',
                  [kythe_edge(Signature1,Corpus1,Root1,Path1,Language1, EdgeName,
                              Signature2,Corpus2,Root2,Path2,Language2)])),
    forall(kythe_color_line(Corpus,Root,Path,Language,LineNo,LineChunks),
           format(KytheFactsOutStream, '~q.~n',
                  [kythe_color_line(Corpus,Root,Path,Language,LineNo,LineChunks)])),
    log_if(false, 'Write_kythe_facts: kythe_edge-done').

get_and_assert_kythe_facts(File) :-
    must_once(get_and_assert_kythe_facts_(File)).

get_and_assert_kythe_facts_(File) :-
    setup_call_cleanup(open(File, read, InStream, [encoding(utf8)]),
                       read_kythe_json_facts(InStream, KytheDicts),
                       close(InStream)),
    log_if(false, 'read_kythe_json_facts-done ~q', [File]),
    get_and_assert_kythe_facts_2(KytheDicts, File).

get_and_assert_kythe_facts_2(KytheDicts, File) :-
    phrase(kythe_fact_preds(KytheDicts, File), Preds0),
    log_if(false, 'kythe_fact_preds-done ~q', [File]),
    sort(Preds0, Preds), % remove dups, although there shouldn't be any
    % log_if(false, 'Sort preds-done ~q', [File]),
    assertion(ground(Preds)),
    % log_if(false, 'Assert ground-done ~q', [File]),
    maplist(assert_pred, Preds),
    log_if(false, 'Assert_kythe_facts-done ~q', [File]).

retract_kythe_facts :-
    functor(KytheNode, kythe_node, 7),
    functor(KytheEdge, kythe_edge, 11),
    retractall(KytheNode),
    retractall(KytheEdge).

read_kythe_json_facts(InStream, KytheDicts) :-
    json_read_dict(InStream, KytheDict,
                   [value_string_as(atom), end_of_file(@(end)), default_tag(json)]),
    (  KytheDict == @(end)
    -> KytheDicts = []
    ;  KytheDicts = [KytheDict|KytheDicts2],
       read_kythe_json_facts(InStream, KytheDicts2)
    ).

%! kythe_fact_preds(+JsonFacts:list(dict), +File:atom, -Pred)// is det.
kythe_fact_preds([], _File) --> [ ].
kythe_fact_preds([JsonFact|JFs], File) -->
    (  kythe_fact_pred_(JsonFact, File)
    -> { true }
    ;  { throw(error(must_once_failed(kythe_fact_pred_(JsonFact, File)), _)) }
    ),
    kythe_fact_preds(JFs, File).

% See transform_kythe_fact/3 in pykythe.pl
kythe_fact_pred_(json{source:Source0, fact_name:FactName, fact_value:FactValueB64}, File) -->
    !,
    { vname_fix(Source0, Source) },
    (  { FactName == '/pykythe/color_all' }
    -> kythe_fact_pred_color(Source, FactValueB64) % Not b64-encoded for performance
    ;  (  { base64_utf8(FactValue, FactValueB64) }
       -> [ ]
       ;  % if base64_utf8/2 fails, assume it's invalid UTF-8 encoding
          % and leave it as-is.
          % TODO: Look for '# -*- coding: ...' and use that (see
          %       lib2to3/pgen2/tokenize.py detect_encoding())
          { log_if(true, 'Fact not UTF-8: ~q in ~q', [FactName, File]) },
          { base64(FactValue, FactValueB64) }
       ),
       (  { numeric_fact(FactName) }
       -> { atom_number(FactValue, FactNumber) },
          [ kythe_node(Source, FactName, FactNumber) ]
       ;  [ kythe_node(Source, FactName, FactValue) ]
       )
    ).
kythe_fact_pred_(json{source:Source0, fact_name:'/', edge_kind:EdgeKind, target:Target0}, _File) -->
    !,
    { vname_fix(Source0, Source) },
    { vname_fix(Target0, Target) },
    [ kythe_edge(Source, EdgeKind, Target) ].
kythe_fact_pred_(Fact, File) -->
    [ Fact ],
    { domain_error(json, File:Fact) }.

numeric_fact('/kythe/loc/start').
numeric_fact('/kythe/loc/end').
numeric_fact('/kythe/snippet/start').
numeric_fact('/kythe/snippet/end').
% TODO: other numeric facts?

kythe_fact_pred_color(Source, FactStr) -->
    { atom_json_dict(FactStr,
                     FactValue,
                     [width(0),true(#(true)),false(#(false)),null(#(null)),
                      value_string_as(atom), default_tag(color), end_of_file(@(end))]) },
    % The following double-bagof groups chunks by line#:
    % TODO: is this the same as maplist(lineno_chunk), sort, group_pairs_by_key?
    { bagof_or_empty(LineNo-LineChunks,
            bagof(LineChunk, lineno_chunk(LineNo, LineChunk, FactValue), LineChunks),
            Lines) },
    kythe_color_lines(Lines, Source).

:- meta_predicate bagof_or_empty(?, ^, ?).
bagof_or_empty(Template, Goal, Bag) :-
    (   bagof(Template, Goal, Bag)
    *-> true
    ;   Bag = []
    ).

lineno_chunk(LineNo, LineChunk, Chunks) :-
    member(LineChunk, Chunks),
    get_dict(lineno, LineChunk, LineNo).

kythe_color_lines([], _Source) --> [ ].
kythe_color_lines([LineNo-Line|Lines], Source) -->
    [ kythe_color_line(Source, LineNo, Line) ],
    kythe_color_lines(Lines, Source).

%! vname_fix(+Json:json, -Vname:vname) is det.
% vname is same as verifier:
%    vname(Signature, Corpus, Root, Path, Language)
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

assert_pred(kythe_node(vname(Signature,Corpus,Root,Path,Language), FactName, FactValue)) :-
    !,
    assertz(kythe_node(Signature,Corpus,Root,Path,Language, FactName, FactValue)).
assert_pred(kythe_edge(vname(Signature1,Corpus1,Root1,Path1,Language1), EdgeName,
                       vname(Signature2,Corpus2,Root2,Path2,Language2))) :-
    !,
    assertz(kythe_edge(Signature1,Corpus1,Root1,Path1,Language1, EdgeName,
                       Signature2,Corpus2,Root2,Path2,Language2)).
assert_pred(kythe_color_line(vname('',Corpus,Root,Path,Language), LineNo, Line)) :-
    !,
    assertz(kythe_color_line(Corpus,Root,Path,Language, LineNo, Line)).
assert_pred(Pred) :-
    domain_error(kythe_node_or_edge, Pred).

utf8_bytes_to_term_doesnt_work(Bytes, Term) :-
    % TODO: delete this
    % Doesn't work (Error: "No permission to encoding stream")
    % because a string is a sequence of unicode chars. Python 2 made
    % the mistake of mixing bytes and strings.
    open_string(Bytes, Stream),
    set_stream(Stream, encoding(utf8)),
    read(Stream, Term).

extract_opts(Opts) :-
    validate_prolog_version,
    OptsSpec =
    [[opt(filesdir), type(atom), default('filesdir-must-be-specified'), longflags([filesdir]),
      help('Directory for putting the files\'s contents')
     ]],
    opt_arguments(OptsSpec, Opts, PositionalArgs),
    must_once_msg(PositionalArgs = [], 'Unknown positional arg(s)').

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

do_output_stream(Opts, FileName, Fmt1, Args1, Goal, Fmt2, Args2) :-
    must_once(memberchk(filesdir(FilesDir), Opts)),
    make_directory_path(FilesDir),
    atomic_list_concat([FilesDir, FileName], '/', FullPath),
    setup_call_cleanup(open(FullPath, write, OutStream, [encoding(utf8)]),
                       (   format(OutStream, Fmt1, Args1),
                           call(Goal, OutStream)
                       ),
                       (   format(OutStream, Fmt2, Args2),
                           close(OutStream)
                       )).

end_of_file.
