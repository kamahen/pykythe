#!/usr/bin/env swipl
% -*- Mode: Prolog -*-

               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %% TODO: THIS IS OUT OF DATE and doesn't work!!! %%
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is a simple HTTP server for displaying the the results of
% running pykythe (JSON output).  There is one window for the program
% source and one window for the xref info.

% The "top" window sets up two iframe's:
%   "content": the source content
%   (colorized) "xref": for the xref (Kythe) information

% The content has links (target="xref") that cause the xref
% information to be computed and displayed. The xref information has
% links (target="content") into the source.

% TODO: add a third iframe (called "toc" will be added for navigating
% through the source directory.

:- use_module(library(base64)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).  % TODO: needed?
:- use_module(library(optparse)).
:- use_module(library(option)).

:- dynamic kythe_fact/3.  % Added by load_kythe
:- dynamic kythe_edge/3.  % Added by load_kythe

:- initialization main.

:- http_handler(root(.), top, []).
:- http_handler(root('index.html'), top, []).
:- http_handler(root('xref'), xref, []).


% The top window, which creates the iframe's.
top(Request) :-
    ( member(search(Search), Request) -> true ; Search = ''),
    format(atom(SearchAtom), '~q', [Search]),
    format(atom(RequestAtom), '~q', [Request]),
    reply_html_page(
            title("Top"),
            [h1('Hello World!'),
             p(code(SearchAtom)),
             p(code(RequestAtom))]).


% The "content" iframe's contents.
content(_Request) :-
    true.


% The "xref" iframe's contents.
xref(_Request) :-
    true.


% main: parse args, read in files, start HTTP server.
main :-
    OptsSpec = [
        [opt(port), type(integer), longflags([port]), shortflags([p]),
         help(['The port that the HTTP server listens on'])],
        % [opt(src), type(atom), longflags([src]), shortflags([s]),
        %  help(['The HTML-ized source file'])],
        [opt(kythe), type(atom), longflags([kythe]), shortflags([k]),
         help(['The Kythe facts, in JSON form'])]
        ],
    opt_arguments(OptsSpec, Opts, _PositionalArgs),
    % format('Opts: ~q~n', [Opts]),
    % format('PostionalArgs ~q~n', [_PositionalArgs]),
    % opt_help(OptsSpec, Help),
    % format('Help:~n~w~n======~n', [Help]),
    option(port(Port), Opts),
    ensure_instantiated_arg(Port, '--port'),
    % option(src(Src), Opts),
    % ensure_instantiated_arg(Src, '--src'),
    option(kythe(Kythe), Opts),
    ensure_instantiated_arg(Kythe, '--kythe'),
    load_kythe(Kythe),
    print_all(file/3),
    % print_all(anchor/6),
    % print_all(node_kind/5),
    % print_all(node_subkind/5),
    % print_all(edge_defines_binding/6),
    % print_all(kythe_fact/3),
    % print_all(kythe_edge/3),
    % http_server(http_dispatch, [port(Port)]),
    % thread_get_message(quit).
    halt.

% Throw an exception if Arg isn't instantiated
ensure_instantiated_arg(Arg, Flag) :-
    (   var(Arg)
     -> throw(error(instantiation_error,
                    context(optparse, Flag)))
     ; true
    ).

% assert kythe facts from a file
load_kythe(Kythe) :-
    open(Kythe, read, KytheStream),
    writeln('Reading': Kythe),
    load_json(KytheStream),
    close(KytheStream).

load_json(KytheStream) :-
    ( at_end_of_stream(KytheStream)
     -> true
     ;  json_read_dict(KytheStream, JsonDict),
        process_json_dict(JsonDict),
        load_json(KytheStream)
    ).

% From https://kythe.io/docs/kythe-verifier.html -
%    Facts must be one of the following forms:
%       (source, edge, target, /, "")
%       (source, edge, target, /kythe/ordinal, base10string)
%       (source, "", "", string, _)
process_json_dict(_{source: Source, fact_name: Name, fact_value: ValueBase64}) :-
    base64(Value, ValueBase64),
    vname(Source, base64_fact{source: Source, fact_name: Name, fact_value: Value}),
    % format('JSON-fact: ~q~n', [kythe_fact{name: Name, value: Value, source: Source}]),
    assertz(kythe_fact(Name, Source, Value)),
    !.
process_json_dict(_{source: Source, edge_kind: Kind, fact_name: "/", target: Target}) :-
    vname(Source, edge_source{source: Source, edge_kind: Kind, fact_name: "/", target: Target}),
    vname(Target, edge_target{source: Source, edge_kind: Kind, fact_name: "/", target: Target}),
    % format('JSON-edge: ~q~n', [kythe_edge{kind: Kind, source: Source, target: Target}]),
    assertz(kythe_edge(Kind, Source, Target)),
    !.
process_json_dict(Fact) :-
    throw(error(bad_json, context(json_dict, Fact))).

% Verify vname dict is in the form we expect and set tag.
% TODO: remove the Context param (it's for debugging)
vname(vname{corpus: _, language: _, root: _, signature: _}, _) :- !.
vname(vname_signature{corpus: _, path: _, root: _, signature: _}, _) :- !.
vname(vname_path{corpus: _, path: _, root: _}, _) :- !.
vname(Dict, Context) :-
    throw(error(unknown_json_vname, context(fact, Dict-Context))).

% Debugging: print Kythe facts
print_all(Pred/N) :-
    length(Args, N),
    Call =.. [Pred|Args],
    Call,
    format('~q~n', [Call]),
    fail
    ; true.

% Convenience predicates for accessing the facts

file(Corpus, Root, Path) :-
    kythe_fact("/kythe/node/kind",
               vname_path{corpus: Corpus, root: Root, path: Path},
               file).

file_content(Corpus, Root, Path, Content) :-
    kythe_fact("/kythe/text",
               vname_path{corpus: Corpus, root: Root, path: Path},
               Content).

file_numlines(Corpus, Root, Path, NumLines) :-
    kythe_fact("/kythe/x-numlines",
               vname_path{corpus: Corpus, root: Root, path: Path},
               NumLinesAtom),
    atom_number(NumLinesAtom, NumLines).

file_html(Corpus, Root, Path, Html) :-
    kythe_fact("/kythe/x-numlines",
               vname_path{corpus: Corpus, root: Root, path: Path},
               Html).

anchor(Signature, Corpus, Root, Path, Start, End) :-
    kythe_fact("/kythe/node/kind",
               vname_signature{corpus: Corpus, root: Root, path: Path, signature: Signature},
               anchor),
    kythe_fact("/kythe/loc/start",
               vname_signature{corpus: Corpus, root: Root, path: Path, signature: Signature},
               StartAtom),
    kythe_fact("/kythe/loc/start",
               vname_signature{corpus: Corpus, root: Root, path: Path, signature: Signature},
               EndAtom),
    atom_number(StartAtom, Start),
    atom_number(EndAtom, End).

node_kind(Kind, Signature, Corpus, Root, Language) :-
    kythe_fact("/kythe/node/kind",
               vname{corpus: Corpus, root: Root, language: Language, signature: Signature},
               Kind).

node_subkind(Subkind, Signature, Corpus, Root, Language) :-
    kythe_fact("/kythe/subkind",
               vname{corpus: Corpus, root: Root, langage: Language, signature: Signature},
               Subkind).

edge_defines_binding(AnchorSignature, TargetSignature, Corpus, Root, Path, Language) :-
    kythe_edge("/kythe/edge/defines/binding",
               vname_signature{corpus: Corpus, root: Root, path: Path, signature: AnchorSignature},
               vname{corpus: Corpus, root: Root, language: Language, signature: TargetSignature}).
