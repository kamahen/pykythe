#!/usr/bin/env swipl
% -*- mode: Prolog -*-

%% Start this with
%%   http://localhost:9999/static/src_browser.html

%% See also:
%%    http://www.pathwayslms.com/swipltuts/html/index.html
%%    https://swi-prolog.discourse.group/t/yet-another-web-applications-tutorial/566
%%    https://www.swi-prolog.org/howto/http/

:- use_module(library(http/http_server), [http_server/1,
                                          reply_html_page/2,
                                          http_read_json_dict/3,
                                          reply_json_dict/1,
                                          reply_json_dict/2, % TODO: Options=[status(201)]
                                          http_redirect/3 % TODO: commented out below
                                          ]).
:- use_module(library(http/http_files), [http_reply_from_files/3]).
%% TODO: if using daemon, then: swipl src_browser.pl --port=.... --pidfile=/var/run/src_browser.pid
%%       and kill $(cat /var/run/src_browser.pid)
%% TODO: Support HTTPS: https://www.swi-prolog.org/pldoc/man?section=ssl-https-server
%%                      /usr/share/doc/openssl/HOWTO/certificates.txt.gz
%%                          openssl genrsa -out privkey.pem
%%                          openssl req -new -x509 -key privkey.pem -out cacert.pem -days 1095
%%                          ? openssl req -new -key privkey.pem -out cert.csr
%%                      https://www.openssl.org/docs/manmaster/man1/CA.pl.html
%% :- use_module(library(http/http_unix_daemon)).
%% :- use_module(library(http_log)).
%% :- set_setting(http:logfile, '/tmp/httpd.log').
:- use_module(library(http/http_path)).
:- use_module(library(http/http_error)). % TODO: remove - this decorates uncaught HTTP exceptions with stack-trace
:- use_module(library(debug)).
:- use_module(library(optparse), [opt_arguments/3]).
:- use_module(library(readutil), [read_file_to_string/3]).
:- use_module('../pykythe/must_once.pl').
:- use_module('../pykythe/pykythe_utils.pl').

%% The "base" Kythe facts, which are dynamically loaded at start-up.
:- dynamic kythe_node/7, kythe_edge/11.

%% Convenience predicates for access the base Kythe facts,
%% using vname(Signature, Corpus, Root, Path, Language).

kythe_node(vname(Signature, Corpus, Root, Path, Language),
           FactName, FactValue) :-
    kythe_node(Signature, Corpus, Root, Path, Language,
               FactName, FactValue).

kythe_edge(vname(Signature1, Corpus1, Root1, Path1, Language1),
           Edge,
           vname(Signature2, Corpus2, Root2, Path2, Language2)) :-
    kythe_edge(Signature1, Corpus1, Root1, Path1, Language1,
               Edge,
               Signature2, Corpus2, Root2, Path2, Language2).
kythe_edge(vname(Signature1, Corpus1, Root1, Path1, Language1),
           ReverseEdge,
           vname(Signature2, Corpus2, Root2, Path2, Language2)) :-
    (  var(ReverseEdge)
    -> kythe_edge(Signature2, Corpus2, Root2, Path2, Language2,
                  Edge,
                  Signature1, Corpus1, Root1, Path1, Language1),
       atom_concat('%', Edge, ReverseEdge)
    ;  atom_concat('%', Edge, ReverseEdge),
       kythe_edge(Signature2, Corpus2, Root2, Path2, Language2,
                  Edge,
                  Signature1, Corpus1, Root1, Path1, Language1)
    ).

:- debug(log).    % enable log messages with debug(log, '...', [...]).
% :- debug(http_path).            % TODO: remove
% :- debug(http(request)).        % TODO: remove
% :- debug(http(hook)).           % TODO: remove
:- debug(http_session).         % TODO: remove
:- debug(http(error)).          % TODO: remove
% :- debug(http(header)).         % TODO: remove
:- debug(http(send_request)).   % TODO: remove
:- debug(websocket).            % TODO: remove
:- debug(websocket(open)).      % TODO: remove
:- debug(websocket(close)).     % TODO: remove

% :- debug.                       % TODO: remove

%% :- initialization main.  % TODO: restore this.

:- multifile http:location/3.
:- multifile user:file_search_path/2.

:- dynamic http:location/3.
:- dynamic user:file_search_path/2.

%% See https://www.swi-prolog.org/howto/http/HTTPFile.html
%% and library(http/mimetype).
%% e.g.: file_content_type('foo.js', text/javascript, 'text/javascript; charset=UTF-8').
%! http:location(+Alias, -Expansion, -Options) is nondet.
%%   The only current option is priority(...).

http:location(static, root(static), []).
http:location(files, static(files), []).
http:location(json, root(json), []).  %% DO NOT SUBMIT - remove?

main :-
    browser_opts(Opts),
    %% set_prolog_flag(verbose_file_search, true),
    assert_server_locations(Opts),
    read_and_assert_kythe_facts,
    server(Opts).

assert_server_locations(Opts) :-
    debug(log, 'files dir:  ~q', [Opts.filesdir]),
    debug(log, 'static dir: ~q', [Opts.staticdir]),
    asserta(user:file_search_path(files,  Opts.filesdir)),
    asserta(user:file_search_path(static, Opts.staticdir)).

read_and_assert_kythe_facts :-
    unload_file(files('kythe_facts.pl')),
    load_files([files('kythe_facts.pl')],
               [silent(false),
                imports([kythe_node/7,
                         kythe_edge/11])]).

server(Opts) :-
    %% See comments with "Support HTTPS" above.
    http_server([port(Opts.port),
                 %% TODO: enable ssl (https):
                 %% ssl([certificate_file('cacert.pem'), %% or cert.csr?
                 %%      key_file('privkey.pem')]),
                 workers(5)]).

browser_opts(Opts) :-
    current_prolog_flag(version, PrologVersion),
    must_once_msg(PrologVersion >= 80119, 'SWI-Prolog version is too old'), % Sync this with README.md
    OptsSpec =
    [[opt(port), type(integer), default(9999), longflags([port]),
      help('Server port')],
     [opt(filesdir), type(atom), default('filesdir-must-be-specified'), longflags([filesdir]),
      help('Directory for the files\'s contents (for "files" URL)')],
     [opt(staticdir), type(atom), default('staticdir-must-be-specified'), longflags([staticdir]),
      help('Directory for the static files (for "static" URL)')]
    ],
    opt_arguments(OptsSpec, Opts0, PositionalArgs),
    opts_dict(Opts0, Opts),
    must_once_msg(PositionalArgs = [], 'Unknown positional arg(s)').


%% localhost:9999/ ... redirects to /static/src_browser.htmle
%%      - for debugging, 'moved' can be cleared by chrome://settings/clearBrowserData
%%        (Cached images and files)
:- http_handler(root(.),
                http_redirect(
                    moved_temporary, % or moved - but that makes debugging a bit more difficult
                    static('src_browser.html')),
                []).

% Serve localhost:9999/static/ from 'static' directory (See also facts for http:location/3)
:- http_handler(static(.),
                my_http_reply_from_files(static(.), []),
                [prefix]).
:- http_handler(files(.),
                my_http_reply_from_files(files(.), []),
                [prefix]).

:- http_handler('/json',  %% json(.),     % localhost:9999/json  -- DO NOT SUBMIT - json(.) is better?
                reply_with_json, [priority(0)]).

my_http_reply_from_files(Dir, Opts, Request0) :-
    opts_dict(Request0, request, Request),
    %% debug(log, '~q', [my_http_reply_from_files(Dir, Request)]),
    debug(log, 'Request (~q): ~q', [Dir, [method:Request.method,
                                          path_info:Request.path_info]]),
    %% This is a hack because I can't seem to get static/files to work.
    (  Dir = static(.),
       split_string(Request.path_info, '/', '', SplitList),
       SplitList = ["files", FileStr]
    -> atom_string(FileAtom, FileStr),
       select(path_info(_), Request0, Request0a),
       http_reply_from_files(files(.), Opts, [path_info(FileAtom)|Request0a])
    ;  http_reply_from_files(Dir, Opts, Request0)
    ).

reply_with_json(Request) :-
    %% print_term_cleaned(Request, [], RequestPretty),
    memberchk(method(post), Request),
    % debug(log, 'Request-JSON(handle)0: ~q', [Request]),
    my_http_read_json_dict(Request, DictIn), %% [content_type("application/json")]),
    % debug(log, 'Request-JSON(handle): ~q', [DictIn]),
    json_response(DictIn, DictOut),
    % debug(log, 'Request(handle): ~q => ~q', [DictIn, DictOut]),
    reply_json_dict(DictOut).

json_response(json{fetch:FileName},
              json_result{file:FileName,
                          contents:Contents}) :-
    !,
    % TODO: catch error(existence_error(source_sink,...),_)
    read_file_to_string(files(FileName), Contents, []).
    % string_length(Contents, ContentsLen),
    % debug(log, 'json_response: ~q => ~d chars', [FileName, ContentsLen]).

json_response(json{anchor: json{signature: Signature,
                                corpus: Corpus, root: Root,
                                path: Path,
                                language: Language}},
              json{signature: Signature,
                   corpus: Corpus, root: Root,
                   path: Path, language: Language,
                   semantic: SemanticJson,
                   semantic_node_values: SemanticNodeValuesJson,
                   edge_links: EdgeLinksJson}) :-
    !,
    anchor_links_grouped(vname(Signature, Corpus, Root, Path, Language),
                         Semantic, SemanticNodeValues, EdgeLinks),
    link_to_dict(Semantic, SemanticJson),
    maplist(pair_to_json(kind, value), SemanticNodeValues, SemanticNodeValuesJson),
    maplist(group_to_dict, EdgeLinks, EdgeLinksJson).
json_response(Request) :-
    debug(log, 'JSON_RESPONSE fail: ~q~n', [Request]),
    fail.

group_to_dict(Edge-Links, json{edge: Edge, links: LinksJson}) :-
    maplist(link_to_dict, Links, LinksJson).

link_to_dict(vname(Signature, Corpus, Root, Path, Language),
             json{signature: Signature,
                    corpus: Corpus, root: Root,
                  path: Path, language: Language}).
link_to_dict(vname_flip(Corpus, Root, Path, Signature, Language),
             json{signature: Signature,
                    corpus: Corpus, root: Root,
                  path: Path, language: Language}).

pair_to_json(KeyTag, ValueTag, Key-Value, Json) :-
    dict_create(Json, json, [KeyTag-Key, ValueTag-Value]).

my_http_read_json_dict(Request, DictIn) :-
    http_read_json_dict(Request, DictIn,
                        [value_string_as(atom), end_of_file(@(end)), default_tag(json),
                         true(#(true)),false(#(false)),null(#(null))]).

orphan_semantic(AnchorVname1, Semantic, Edge) :-
    kythe_node(AnchorVname1, '/kythe/node/kind', 'anchor'),
    kythe_edge(AnchorVname1, Edge, Semantic),
    \+ (kythe_edge(AnchorVname2, _Edge2, Semantic),
        kythe_node(AnchorVname2, '/kythe/node/kind', 'anchor')).

%% TODO: orphan semantics for non-anchors

anchor_link_anchor(AnchorVname1, Edge1, Semantic, Edge2, AnchorVname2) :-
    kythe_node(AnchorVname1, '/kythe/node/kind', 'anchor'),
    kythe_edge(AnchorVname1, Edge1, Semantic),
    kythe_edge(AnchorVname2, Edge2, Semantic),
    kythe_node(AnchorVname2, '/kythe/node/kind', 'anchor').


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

anchor_links(AnchorVname1, Semantic, SemanticNodeValues, SortedLinks) :-
    %% TODO: filter Edge1 by anchor_out_edge?
    (  setof(Edge2-AnchorVname2flipped,
             anchor_link_anchor_sort_order(AnchorVname1, Semantic, Edge2, AnchorVname2flipped),
             SortedLinks)
    -> true
    ;  SortedLinks = []
    ),
    (  setof(NodeKind-NodeValue, kythe_node(Semantic, NodeKind, NodeValue), SemanticNodeValues)
    -> true
    ;  SemanticNodeValues = []
    ).

anchor_link_anchor_sort_order(AnchorVname1, Semantic, Edge2, AnchorVname2flipped) :-
    anchor_link_anchor(AnchorVname1, _Edge1, Semantic, Edge2, AnchorVname2),
    vname_flip(AnchorVname2, AnchorVname2flipped).

vname_flip(vname(Signature, Corpus, Root, Path, Language),
           vname_flip(Corpus, Root, Path, Signature, Language)).

%% TODO: Can we use library(solution_sequences) group_by/4?
%%       It has bagof rather than setof at bottom, which is not
%%       necessarily a bad thing, as it might show errors in the Kythe
%%       facts (except that duplicates should have been removed by the
%%       processing pipeline).
anchor_links_grouped(AnchorVname1, Semantic, SemanticNodeValues, GroupedLinks) :-
    anchor_links(AnchorVname1, Semantic, SemanticNodeValues, SortedLinks),
    group_pairs_by_key(SortedLinks, GroupedLinks).

anchor_out_edge('/kythe/edge/defines').
anchor_out_edge('/kythe/edge/defines/binding').
anchor_out_edge('/kythe/edge/ref').
anchor_out_edge('/kythe/edge/ref/call').
anchor_out_edge('%/kythe/edge/defines').
anchor_out_edge('%/kythe/edge/defines/binding').
anchor_out_edge('%/kythe/edge/ref').
anchor_out_edge('%/kythe/edge/ref/call').
