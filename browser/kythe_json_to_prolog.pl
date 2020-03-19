% -*- mode: Prolog -*-

%% Read in Kythe JSON facts and convert them to Prolog facts for use
%% by src_browser.pl
%% Input is from stdin.
%% For historical reasons, output is specified as a dir and the
%% facts are in kythe_facts.pl
%% TODO: change how output is specified.

%% TODO: performance
%%  pykythe_utils:base64_utf8/2 (and b64_to_utf8_to_atom/2) take 57% of the CPU.
%%  json:json_read_dict/3 takes 41%
%%  json:json_string_codes/3 takes 15% (most of it in utf8_codes/3).

:- use_module(library(http/json), [json_read_dict/3]).
:- use_module(library(pairs)).

%% swipl -g main -t halt extract_color.pl </tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/t10.kythe.json --filesdir=/tmp/pykte_test/browser/files
%% debugging: main('/tmp/pykythe_test/KYTHE/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/t10.kythe.json').

%% Kythe vname is Signature,Corpus,Root,Path,Language
%% We also define vname0:   Corpus,Root,Path,Language

%% kythe_node(Source: vname, FactName:atom, FactValue:atom).
%% kythe_edge(Source: vname, EdgeKind:atom, Target:vname).
%% Kythe facts are of one of these forms:
%%   (source, edge, target, /, "")
%%   (source, edge, target, /kythe/ordinal, base10string)
%%   (source, "", "", string, _)

:- use_module(library(optparse), [opt_arguments/3]).
:- use_module('../pykythe/pykythe_utils.pl', [base64_utf8/2, log_if/2, log_if/3, validate_prolog_version/0]).
:- use_module('../pykythe/must_once.pl').

:- dynamic kythe_node/7, kythe_edge/11.

main :-
    main(user_input).

main(InStream) :-
    % TODO: remove with swipl 8.1.21
    set_stream(InStream, tty(false)), % try to ensure no prompting
    prompt(_, ''),                    % really ensure no prompting
    log_if(true, 'Start'),
    extract_opts(Opts),
    retract_kythe_facts,
    read_lines(InStream, Files),
    concurrent_maplist(get_and_assert_kythe_facts, Files),
    do_output_stream(Opts, 'kythe_facts.pl', '', [],
                     write_kythe_facts,
                     '', []),
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
    %% TODO: use fast_serialize?
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
    log_if(false, 'Write_kythe_facts: kythe_edge-done').

get_and_assert_kythe_facts(File) :-
    open(File, read, InStream, [encoding(utf8)]),
    read_kythe_json_facts(InStream, KytheDicts),
    log_if(false, 'Read_kythe_json_facts-done ~q', [File]),
    %% TODO: base64/2 takes most of the CPU time (from b64_to_utf8_to_atom/2)
    %% TODO: slightly more efficient if kythe_fact_pred/2 is
    %%       moved into read_kythe_json_facts/2.
    maplist(kythe_fact_pred, KytheDicts, Preds0),
    log_if(false, 'Kythe_fact_pred-done ~q', [File]),
    sort(Preds0, Preds), % remove dups, although there shouldn't be any
    %% log_if(false, 'Sort preds-done ~q', [File]),
    assertion(ground(Preds)),
    %% log_if(false, 'Assert ground-done ~q', [File]),
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

%! kythe_fact_pred(+JsonFact:dict, -Pred) is det.
%% See transform_kythe_fact/3 in pykythe.pl
kythe_fact_pred(json{source:Source0, fact_name:FactName, fact_value:FactValueB64},
                kythe_node(Source1, FactName, FactValue)) :-
    !,
    vname_fix(Source0, Source1),
    (  FactName == '/pykythe/symtab'
    -> FactValue = FactValueB64  % It's not b64-encoded for performance
    ;  base64_utf8(FactValue0, FactValueB64),
       post_process_fact(FactName, FactValue0, FactValue)
    ).
kythe_fact_pred(json{source:Source0, fact_name:'/', edge_kind:EdgeKind, target:Target0},
                kythe_edge(Source1, EdgeKind, Target1)) :-
    !,
    vname_fix(Source0, Source1),
    vname_fix(Target0, Target1).
kythe_fact_pred(Fact, Fact) :-
    domain_error(json, Fact).

post_process_fact('/kythe/loc/start', Value0, Value) :- !, atom_number(Value0, Value).
post_process_fact('/kythe/loc/end',   Value0, Value) :- !, atom_number(Value0, Value).
post_process_fact('/kythe/snippet/start', Value0, Value) :- !, atom_number(Value0, Value).
post_process_fact('/kythe/snippet/end',   Value0, Value) :- !, atom_number(Value0, Value).
%% TODO: other numeric facts to convert?
post_process_fact(_, Value, Value).


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

assert_pred(kythe_node(vname(Signature,Corpus,Root,Path,Language), FactName, FactValue)) :-
    !,
    assertz(kythe_node(Signature,Corpus,Root,Path,Language, FactName, FactValue)).
assert_pred(kythe_edge(vname(Signature1,Corpus1,Root1,Path1,Language1), EdgeName,
                       vname(Signature2,Corpus2,Root2,Path2,Language2))) :-
    !,
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

:- use_module(library(check)).  %% DO NOT SUBMIT
%% TODO: trap print_message(informational,check(pass(Message)))
?- check. %% DO NOT SUBMIT

end_of_file.
