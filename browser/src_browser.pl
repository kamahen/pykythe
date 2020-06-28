#!/usr/bin/env swipl
% -*- mode: Prolog -*-

%% Start this with
%%   http://localhost:9999
%%     which does a redirect to:
%%   http://localhost:9999/static/src_browser.html

%% See also:
%%    http://www.pathwayslms.com/swipltuts/html/index.html
%%    https://swi-prolog.discourse.group/t/yet-another-web-applications-tutorial/566
%%    https://www.swi-prolog.org/howto/http/

:- module(src_browser, [src_browser_main/0, src_browser_main2/0]).

% :- set_prolog_flag(autoload, false).  % TODO: Seems to break plunit, qsave

:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(error), [must_be/2, domain_error/2]).
:- use_module(library(pairs), [group_pairs_by_key/2, pairs_values/2]).
:- use_module(library(prolog_jiti), [jiti_list/1]).
:- use_module(library(apply), [maplist/2, maplist/3, maplist/4, maplist/5]).
:- use_module(library(thread), [concurrent_maplist/2]).
:- use_module(library(http/http_server), [http_server/1,
                                          http_read_json_dict/3,
                                          reply_json_dict/2, % TODO: Options=[status(201)]
                                          http_redirect/3
                                         ]).
:- use_module(library(http/http_files), [http_reply_from_files/3]).
% TODO: if using daemon, then: swipl src_browser.pl --port=.... --pidfile=/var/run/src_browser.pid
%       and kill $(cat /var/run/src_browser.pid)
% TODO: Support HTTPS: https://www.swi-prolog.org/pldoc/man?section=ssl-https-server
%                      /usr/share/doc/openssl/HOWTO/certificates.txt.gz
%                          openssl genrsa -out privkey.pem
%                          openssl req -new -x509 -key privkey.pem -out cacert.pem -days 1095
%                          ? openssl req -new -key privkey.pem -out cert.csr
%                      https://www.openssl.org/docs/manmaster/man1/CA.pl.html
%       ==> See ~/src/swipl-devel/packages/ssl/mkcerts.pl.in
%           - look for server-key.pem etc.
%             https://swi-prolog.discourse.group/t/debugging-failing-ssl-test/2073
% :- use_module(library(http/http_unix_daemon)).
% :- use_module(library(http_log)).
% :- set_setting(http:logfile, '/tmp/httpd.log').
:- use_module(library(http/http_path)).
:- use_module(library(http/http_error)). % TODO: remove - this decorates uncaught HTTP exceptions with stack-trace
:- use_module(library(uri)).
:- use_module(library(debug)).
:- use_module(library(optparse), [opt_arguments/3]).
:- use_module(library(readutil), [read_file_to_string/3]).
:- use_module(library(aggregate)). % TODO: do we use all of these?
:- use_module(library(solution_sequences), [distinct/1, distinct/2, order_by/2, group_by/4]). % TODO: do we use all of these?
:- use_module(library(yall)).   % For [S,A]>>atom_string(A,S) etc.
:- use_module('../pykythe/must_once.pl').
:- use_module('../pykythe/pykythe_utils.pl').

% The "base" Kythe facts, which are dynamically loaded at start-up.
:- dynamic kythe_node/7, kythe_edge/11.

% Convenience predicates for accessing the base Kythe facts,
% using vname(Signature, Corpus, Root, Path, Language).
% We also define vname0: Corpus, Root, Path, Language

%! kythe_node(Source: vname, FactName:atom, FactValue:atom) is nondet.
kythe_node(vname(Signature, Corpus, Root, Path, Language),
           FactName, FactValue) :-
    kythe_node(Signature, Corpus, Root, Path, Language,
               FactName, FactValue).

%! kythe_edge(Source: vname, EdgeKind:atom, Target:vname).
% Kythe facts are of one of these forms:
%   (source, edge, target, /, "")
%   (source, edge, target, /kythe/ordinal, base10string)
%   (source, "", "", string, _)
kythe_edge(vname(Signature1, Corpus1, Root1, Path1, Language1),
           Edge,
           vname(Signature2, Corpus2, Root2, Path2, Language2)) :-
    kythe_edge(Signature1, Corpus1, Root1, Path1, Language1,
               Edge,
               Signature2, Corpus2, Root2, Path2, Language2).
% TODO: instead of using atom_concat/3 to create reverse edges,
%       initialize reverse_edge/2 from
%       src_browser:findall(E, kythe_edge(_,_,_,_,_,E,_,_,_,_,_), Edges0), sort(Edges0, Edges)
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
% :- debug(redirect_log).
% :- debug(request_json_log).

% TODO: remove this list
% List of all http debug flags (grep /usr/lib/swi-prolog/library/http/):
% :- debug(calc).
% :- debug(cookie).
% :- debug(daemon).
% :- debug(daemon(socket)).
% :- debug(false).
% :- debug(html(mailman)).
% :- debug(html(script)).
% :- debug(http_authenticate).
% :- debug(http(authenticate)).
% :- debug(http(cgi)).
% :- debug(http(connection)).
% :- debug(http(cookie)).
% :- debug(http(error)).
% :- debug(http(header)).
% :- debug(http(hook)).
% :- debug(http(keep_alive)).
% :- debug(http(nonce)).
% :- debug(http(open)).
% :- debug(http_path).
% :- debug(http(proxy)).
% :- debug(http(redirect)).
% :- debug(http(request)).
% :- debug(http(scheduler)).
% :- debug(http(send_request)).
% :- debug(http(server)).
% :- debug(http_session).
% :- debug(http_session(gc)).
% :- debug(http(spawn)).
% :- debug(http(transfer_encoding)).
% :- debug(http(worker)).
% :- debug(hub(broadcast)).
% :- debug(hub(event)).
% :- debug(hub(gate)).
% :- debug(hub(ready)).
% :- debug(hub(wait)).
% :- debug(json_arg).
% :- debug(logrotate).
% :- debug(multipart(bom)).
% :- debug(multipart(content)).
% :- debug(obsolete).
% :- debug(openid(associate)).
% :- debug(openid(authenticate)).
% :- debug(openid(ax)).
% :- debug(openid(check_authentication)).
% :- debug(openid(crypt)).
% :- debug(openid(resolve)).
% :- debug(openid(test)).
% :- debug(openid(verify)).
% :- debug(openid(yadis)).
% :- debug(post).
% :- debug(post_request).
% :- debug(proxy).
% :- debug(sgml_plugin).
% :- debug(websocket).
% :- debug(websocket(close)).
% :- debug(websocket(open)).

% :- debug(http(post_request)).   % TODO: remove
:- debug(http(request)).        % TODO: remove % TODO: figure out why If-modified-since and friends don't work
% :- debug(http_session).         % TODO: remove
:- debug(http(error)).          % TODO: remove
% :- debug(http_path).            % TODO: remove
% :- debug(http(header)).         % TODO: remove
% :- debug(http(hook)).           % TODO: remove

:- initialization(src_browser_main, main).

:- multifile http:location/3.
:- multifile user:file_search_path/2.

:- dynamic http:location/3.
:- dynamic user:file_search_path/2.

% See https://www.swi-prolog.org/howto/http/HTTPFile.html
% and library(http/mimetype).
% e.g.: file_content_type('foo.js', text/javascript, 'text/javascript; charset=UTF-8').

%! http:location(+Alias, -Expansion, -Options) is nondet.
%   The only current option is priority(...).
http:location(static, root(static), []).
http:location(files, root(files), []).
http:location(json, root(json), []). % TODO: remove?

src_browser_main :-
    run_tests, % TODO: remove
    src_browser_main2,
    % When started via initialization, need to handle inputs via REPL
    % TODO: start as daemon (see library(http/http_unix_daemon)).
    % See also library(main):main/0
    debug(log, 'Starting REPL ...', []),
    prolog.  % REPL

src_browser_main2 :-
    browser_opts(Opts),
    % set_prolog_flag(verbose_file_search, true),
    assert_server_locations(Opts),
    read_and_assert_kythe_facts,
    server(Opts).

% Not used -- a trivial REPL, in case prolog/0 or
% '$toplevel':'$toplevel'/0 doesn't work.

:- if(false).

repl :-
    read(Goal),
    (  Goal == end_of_file
    -> format('*** exiting src_browser~n', [])
    ;  E = error(_, _),         % avoid trapping abort, timeout, etc.
       (  catch_with_backtrace(Goal, E, handle_repl_throw(Goal, E))
       -> format('Result: ~q~n', [Goal])
       ;  format('*** fail ***~n', [])
       ),
       repl
    ).

handle_repl_throw(_Goal, E) :-
    print_message(error, E).

:- endif.

assert_server_locations(Opts) :-
    debug(log, 'files dir:  ~q', [Opts.filesdir]),
    debug(log, 'static dir: ~q', [Opts.staticdir]),
    asserta(user:file_search_path(files,  Opts.filesdir)),
    asserta(user:file_search_path(static, Opts.staticdir)).

% DO NOT SUBMIT:
% TODO: move the read/assert stuff to a separate module,
%       also the convenience preds (kythe_node/3, etc.)
read_and_assert_kythe_facts :-
    % TODO: open an issue about this (need to have an empty .qlf file to force
    %       saving the compiled facts)
    % TODO: files('kythe_facts') results in message:
    %       "recompiling QLF file (incompatible with current Prolog version)"
    %       even if silent(true) is specified.
    unload_file(files('kythe_facts')),
    garbage_collect,
    % loading speeds for consult / read-assertz / qlf: 25 : 7 : 1
    %          (consult generates source locations, etc.)
    %       https://swi-prolog.discourse.group/t/quick-load-files/1239/2
    %       https://swi-prolog.discourse.group/t/quick-load-files/1239/8
    load_files([files('kythe_facts')],
               [silent(false),
                optimise(true),
                imports([kythe_node/7,
                         kythe_edge/11])]),
    index_kythe_facts,
    forall(retract(kythe_node(Signature, Corpus,Root,Path,Language, '/pykythe/color', ColorTermStr)),
           assert_color_items(Signature, Corpus,Root,Path,Language, ColorTermStr)),
    thread_create(validate_kythe_facts, _, [detached(true)]).

assert_color_items(Signature, Corpus,Root,Path,Language, ColorTermStr) :-
    term_string(ColorTerm, ColorTermStr),
    dict_pairs(ColorTerm, color, ColorPairs0),
    adjust_color(Corpus,Root,Path,Language, ColorPairs0, ColorPairs),
    % TODO: There are more color items than anything else -- need to
    %       use a more compact representation, especially as there is
    %       no need for lookup on most of the fields.
    maplist(assert_color_item(Signature, Corpus,Root,Path,Language), ColorPairs).

adjust_color(Corpus,Root,Path,Language, ColorPairs0, ColorPairs) :-
    % NOTE: assumes that kythe_node/7 facts have already been asserted
    % [column-0,end-3,lineno-1,start-0,token_color-'<VAR_BINDING>',value-loc]
    (  select(token_color-'<PUNCTUATION>', ColorPairs0, ColorPairs1),
       memberchk(start-Start, ColorPairs1),
       memberchk(end-End, ColorPairs1),
       AnchorPunctuation = vname(_, Corpus,Root,Path,Language),
       AnchorSemantic = vname(_, Corpus,Root,Path,Language),
       kythe_node(AnchorPunctuation, '/kythe/loc/start', Start),
       kythe_node(AnchorPunctuation, '/kythe/loc/end', End),
       ( kythe_node(AnchorSemantic, '/kythe/node/kind', 'anchor')
       ; kythe_edge(AnchorSemantic, '/kythe/edge/tagged', _)
       )
    -> ColorPairs = [token_color-'<PUNCTUATION_REF>'|ColorPairs1]
    ;  ColorPairs = ColorPairs0
    ).

assert_color_item(Signature, Corpus,Root,Path,Language, Key-Value) :-
    % Key: lineno, column, start, end, token_color, value
    atomic_list_concat(['/pykythe/color/', Key], FactName),
    assertz(kythe_node(Signature, Corpus,Root,Path,Language, FactName, Value)).

% index_kythe_facts/0 takes a bit of time because it builds the
% JIT indexes; if you run it a second time, it's fast. (The indexes
% are used elsewhere, so no harm.)
% e.g.: Building the JIT indexes takes 8-10 seconds, validation takes 10 seconds
index_kythe_facts :-
    % Without the call to index_pred (kythe_edge/11, kythe_edge/3 had no change):
    %   Predicate                                     Indexed  Buckets Speedup Flags
    %   ============================================================================
    %   src_browser:kythe_node/7                        1+7   2,097,152  536399.3
    %                                                   1+4     524,288  299008.9
    %                                                   6+7     262,144    4821.3
    % With the call to index_pred:
    %   src_browser:kythe_node/7                        1+4     524,288  298977.2
    %                                                   6+7     262,144    4821.1
    %
    % kythe_node/7 1+7 would be generated by orphan_semantic/3,
    % but seems to not help anything.
    concurrent_maplist(index_pred,
                       [kythe_node(sig,corpus,root,path,lang,_,_),         % kythe_node/7 1+4
                        kythe_node(_,_,_,_,_,fact,value),                  % kythe_node/7 6+7
                        % kythe_node(sig,_,_,_,_,_,value),                 % kythe_node/7 1+7
                        kythe_edge(sig,corpus,root,path,lang,_,_,_,_,_,_), % kythe_edge/11 1
                        kythe_edge(_,_,_,_,_,_,sig,corpus,root,path,lang), % kythe_edge/11 7
                        kythe_edge(vname(sig,corpus,root,path,lang),_,_),  % kythe_edge/3 1
                        kythe_edge(_,_,vname(sig,corpus,root,path,lang)),  % kythe_edge/3 1
                        kythe_edge(vname(sig,corpus,root,path,lang),edge,_),
                        kythe_edge(_,edge,vname(sig,corpus,root,path,lang)),
                        kythe_edge(_,edge,_)
                       ]),
    garbage_collect,
    show_jiti,
    debug(log, 'JIT index done.', []).

validate_kythe_facts :-
    statistics(cputime, T0),
    must_fail(orphan_semantic(_AnchorVname, _SemanticVname, _Edge)),
    % Ensure that every token anchor has an associated color anchor
    % (the inverse isn't true).
    forall(kythe_node(Vname, Name, Value),
           must_once( ground(kythe_node(Vname, Name, Value)) )),
    forall(kythe_edge(V1, Edge, V2),
           must_once( ground(kythe_edge(V1, Edge, V2)) )),
    forall(( kythe_node(Anchor, '/kythe/node/kind', 'anchor'),
             Anchor=(_,Corpus,Root,Path,Language),
             ColorAnchor=(_,Corpus,Root,Path,Language)
           ),
           must_once(( kythe_node(Anchor, '/kythe/loc/start', Start),
                       kythe_node(Anchor, '/kythe/loc/end', End),
                       kythe_node(ColorAnchor, '/pykythe/color/start', Start),
                       kythe_node(ColorAnchor, '/pykythe/color/end', End),
                       semantic_or_tagged(Anchor)
                     ))
          ),
    forall(kythe_node(Anchor, '/kythe/node/kind', 'anchor'),
           must_once( anchor_to_lineno(Anchor, _LineNo) )),
    forall(kythe_node(Anchor, '/kythe/node/kind', 'anchor'),
           must_once(( anchor_to_line_chunks(Anchor, _, Chunks),
                       Chunks \== [] ))),
    forall(kythe_node(Vname, _, _),
           must_once( ( kythe_node(Vname, Name , _),
                        memberchk(Name, ['/kythe/node/kind',
                                         '/pykythe/type',
                                         '/pykythe/color/token_color']) ) )),
    % show_jiti,    % Not needed - should be the same as the first one
    % validate_anchor_link_anchor, % DO NOT SUBMIT: fix this test, which is also slow
    statistics(cputime, T2),
    Tvalid is T2 - T0,
    debug(log, 'Validation done: ~3f sec)', [Tvalid]),
    debug(log, '',[]),
    debug(log, 'Server started: to stop, enter ctrl-D or "halt." (including the ".")', []).

semantic_or_tagged(Anchor) :-
    anchor_semantic(Anchor, _Semantic).
semantic_or_tagged(Anchor) :-
    kythe_edge(Anchor, '/kythe/edge/tagged', _).

index_pred(Goal) :-
    statistics(cputime, T0),
    ( Goal -> true ; true ),
    statistics(cputime, T1),
    T is T1 - T0,
    debug(log, 'Indexed ~q in ~3f sec', [Goal, T]).

% For some reason jiti_list/1 didn't do what I wanted, so extracting
% its core logic here, but needs better formatting:
show_jiti :-
    strip_module(kythe_node(_,_,_), Module, _),
    jiti_list(Module:_),
    findall(S, kythe_node(S,_,_,_,_,_,_), Sigs),
    length(Sigs, LenSigs),
    sort(Sigs, SigsSorted),
    length(SigsSorted, LenSigsSorted),
    debug(log, 'kythe_node(Signature): ~d entries, ~d unique.', [LenSigs, LenSigsSorted]).

% TODO: validate_anchor_link_anchor is incorrect:
% Semantic links have unique signatures ... this isn't true. For example
% if we can't resolve an attr to a single item (ast_raw.py at line 267: "ch0.type")
validate_anchor_link_anchor :-
    forall(anchor_link_anchor(AnchorVname1, Edge1, _SemanticVname, Edge2, AnchorVname2),
           must_once(setof(anchor_link_anchor(AnchorVname1, Edge1, SemanticVname2, Edge2, AnchorVname2),
                           anchor_link_anchor(AnchorVname1, Edge1, SemanticVname2, Edge2, AnchorVname2),
                           [_]))).

server(Opts) :-
    % See comments with "Support HTTPS" above.
    http_server([port(Opts.port),
                 % TODO: enable ssl (https):
                 % ssl([certificate_file('cacert.pem'), % or cert.csr?
                 %      key_file('privkey.pem')]),
                 workers(5)]).

browser_opts(Opts) :-
    validate_prolog_version,
    OptsSpec =
    [[opt(port), type(integer), default(9999), longflags([port]),
      help('Server port')],
     [opt(filesdir), type(atom), default('filesdir-must-be-specified'), longflags([filesdir]),
      help('Directory for the files\'s contents (for "files" URL)')],
     [opt(staticdir), type(atom), default('staticdir-must-be-specified'), longflags([staticdir]),
      help('Directory for the static files (for "static" URL)')]
    ],
    opt_arguments(OptsSpec, Opts0, PositionalArgs),
    dict_create(Opts, opts, Opts0),
    must_once_msg(PositionalArgs = [], 'Unknown positional arg(s)').


% localhost:9999/ ... redirects to /static/src_browser.html
%      - for debugging, 'moved' can be cleared by chrome://settings/clearBrowserData
%        (Cached images and files)
:- http_handler(root(.),
                http_handler_redirect(
                    moved_temporary, % or moved - but that makes debugging a bit more difficult
                    static('src_browser.html')),
                []).

% Serve localhost:9999/static/ from 'static' directory (See also facts for http:location/3)
% TODO: remove the cache(false) options
:- http_handler(static(.),
                pykythe_http_reply_from_files(static(.), [cache(true)]),
                [prefix]).
:- http_handler(files(.),
                pykythe_http_reply_from_files(files(.), [cache(true)]),
                [prefix]).

:- http_handler('/json',  % json(.),     % localhost:9999/json  -- DO NOT SUBMIT - json(.) is better?
                reply_with_json, [priority(0)]).

pykythe_http_reply_from_files(Dir, Options, Request) :-
    (  false
    -> % TODO: remove the following code, for debugging file caching.
       %       See https://swi-prolog.discourse.group/t/how-to-debug-if-modified-since-with-http-reply-from-files/1892/3
       ( member(path_info(PathInfo), Request) -> true ; PathInfo = '' ),
       ( member(cache_control(CacheControl), Request) -> true ; CacheControl = '' ),
       http_files:locate_file(Dir, PathInfo, Path, _IsFile, Options), % TODO: fragile
       time_file(Path, FileTimeStamp),
       format_time(string(FileTime), '%FT%T%z', FileTimeStamp),
       get_time(NowTimeStamp),
       format_time(string(NowTime), '%FT%T%z', NowTimeStamp),
       debug(log, 'File modified [~w]: ~s (now: ~s) ~q', [CacheControl, FileTime, NowTime, PathInfo])
    ;  true
    ),
    http_reply_from_files(Dir, Options, Request).

http_handler_redirect(How, To, Request) :-
    memberchk(path(Base), Request),
    memberchk(request_uri(RequestURI), Request),
    http_absolute_location(To, ToURL, [relative_to(Base)]),
    uri_components(RequestURI, URI),
    uri_data(path, URI, ToURL, ToURI),
    uri_components(NewTo, ToURI),
    debug(redirect_log, 'Redirect: ~q', [[how:How, to:To, toURL:ToURL, requestURI:RequestURI, uri:URI, toURI:ToURI, newTo: NewTo]]),
    http_redirect(How, NewTo, Request).

reply_with_json(Request) :-
    % print_term_cleaned(Request, [], RequestPretty),
    % TODO: why doesn't thead_cputime give non-zero value?
    statistics(cputime, T0),
    memberchk(method(post), Request),
    % Doesn't show the 302 (or 301) redirect:
    %    request_uri('/json'), path('/json'),
    %    origin('http://localhost:9999'), content_type('application/json')
    http_handler_read_json_dict(Request, JsonIn), % [content_type("application/json")]),
    debug(request_json_log, 'Request(json): ~q', [JsonIn]),
    statistics(cputime, T1),
    Tdelta1 is T1 - T0,
    debug(timing, 'Request-JSON: ~q [~3f sec]', [JsonIn, Tdelta1]),
    must_once(
              json_response(JsonIn, JsonOut)), % TODO: improve this error handling
    statistics(cputime, T2),
    Tdelta2 is T2 - T1,
    debug(timing, 'Request-response: ~q [~3f sec]', [JsonIn, Tdelta2]),
    reply_json_dict(JsonOut, [width(0)]), % TODO: this is the most expensive part
    statistics(cputime, T3),
    Tdelta3 is T3 - T2,
    debug(timing, 'Request-reply: ~q [~3f sec]', [JsonIn, Tdelta3]).

json_response(json{fetch:FileName},
              json_result{file:FileName,
                          contents:Contents}) :-
    !,
    % TODO: catch error(existence_error(source_sink,...),_)
    read_file_to_string(files(FileName), Contents, []).
json_response(json{anchor_xref: json{signature: Signature,
                                     corpus: Corpus,
                                     root: Root,
                                     path: Path,
                                     language: Language}},
              json{signature: Signature,
                   lineno: LineNo,
                   line: LineChunks,
                   corpus: Corpus, root: Root,
                   path: Path, language: Language,
                   semantics: SemanticVnamesJson,
                   semantic_node_values: SemanticNodeValuesJson,
                   edge_links: EdgeLinksJson}) :-
    !,
    AnchorVname = vname(Signature, Corpus, Root, Path, Language),
    % TODO: probably better to use nested setof/bagof, but with
    %       library(solution_sequences) for ordering, grouping
    anchor_to_line_chunks(AnchorVname, LineNo, LineChunks),
    debug(log, 'Xref ~q lineno: ~q', [[signature: Signature, corpus=Corpus, root: Root, path: Path, language: Language], LineNo]),
    anchor_links_grouped(AnchorVname, SemanticNodeValues, EdgeLinks0),
    maplist(expand_edge_link, EdgeLinks0, EdgeLinks),
    setof_or_empty(S, anchor_semantic_json(AnchorVname, S), SemanticVnamesJson),
    maplist(pair_to_json(kind, value), SemanticNodeValues, SemanticNodeValuesJson),
    maplist(edge_links_group_to_dict, EdgeLinks, EdgeLinksJson).
json_response(json{src_browser_file:
                  json{corpus:Corpus, root:Root, path:Path}},
              Contents) :-
    !,
    color_data_one_file(Corpus, Root, Path, Contents).
json_response(json{src_file_tree: _}, PathTreeJson) :-
    !,
    setof(Path, file_path(Path), PathNames),
    files_to_tree(PathNames, PathTree),
    tree_to_json(PathTree, PathTreeJson).

% TODO: Can we use library(solution_sequences) group_by/4?  It has
%       bagof rather than setof at bottom, which is not necessarily a
%       bad thing, as it might show errors in the Kythe facts
%       (although probably not -- duplicates should have been removed
%       by the processing pipeline).
%! anchor_links_grouped(+AnchorVname, -SemanticNodeValues, -GroupedLinks) is det.
anchor_links_grouped(AnchorVname, SemanticNodeValues, GroupedLinks) :-
    anchor_links(AnchorVname, SemanticNodeValues, SortedLinks),
    group_pairs_by_key(SortedLinks, GroupedLinks0),
    maplist(group_edge_by_files, GroupedLinks0, GroupedLinks).

group_edge_by_files(Edge-Vnames, Edge-GroupVnamePaths) :-
    maplist(path_pair_vname, Vnames, VnamePaths),
    group_pairs_by_key(VnamePaths, GroupVnamePaths0),
    maplist(path_vname, GroupVnamePaths0, GroupVnamePaths).

path_vname(Vname0-Signatures, Vname0-Vnames) :-
    maplist(vname0_join_signature(Vname0), Signatures, Vnames).

%! anchor_links(+AnchorVname, -SemanticNodeValues, -SortedLinks) is det.
anchor_links(AnchorVname, SemanticNodeValues, SortedLinks) :-
    % TODO: filter Edge1 by anchor_out_edge?
    setof_or_empty(Edge2-AnchorVname2flipped,
                   anchor_link_anchor_sort_order(AnchorVname, Edge2, AnchorVname2flipped),
                   SortedLinks0),
    maplist(pair_vname_remove_start, SortedLinks0, SortedLinks),
    setof_or_empty(NodeKind-NodeValue,
                   node_link_node_value(AnchorVname, NodeKind, NodeValue), SemanticNodeValues).

expand_edge_link(Edge-PathAnchors, Edge-PathLines) :-
    maplist(path_anchors_to_line_chunks, PathAnchors, PathLines).

path_anchors_to_line_chunks(vname0(Corpus, Root, Path, Language)-Anchors,
                            path{corpus:Corpus, root:Root, path:Path, language:Language,
                                 lines:Lines}) :-
    maplist(anchor_to_line_chunks, Anchors, LineNos, LinesLineChunks),
    maplist(zip_lines, Anchors, LineNos, LinesLineChunks, Lines).

zip_lines(vname(Signature, Corpus, Root, Path, Language),
          LineNo, LineChunks,
          line{lineno:LineNo, line:LineChunks,
               signature: Signature,
               corpus: Corpus, root: Root,
               path: Path, language: Language}).

edge_links_group_to_dict(Edge-Links, json{edge: Edge, links: LinksJson}) :-
    maplist(link_to_dict, Links, LinksJson).

link_to_dict(vname(Signature, Corpus, Root, Path, Language),
             json{signature: Signature,
                  corpus: Corpus, root: Root,
                  path: Path, language: Language}) :- !.
link_to_dict(vname_flip(Corpus, Root, Path, Signature, Language),
             json{signature: Signature,
                  corpus: Corpus, root: Root,
                  path: Path, language: Language}) :- !.
link_to_dict(Json, Json) :-
    must_once((is_dict(Json, Tag), (Tag == line; Tag == path))).


pair_to_json(KeyTag, ValueTag, Key-Value, Json) :-
    dict_create(Json, json, [KeyTag-Key, ValueTag-Value]).

% TODO: combine with pykythe_utils:pykythe_json_read_dict?
http_handler_read_json_dict(Request, JsonIn) :-
    http_read_json_dict(Request, JsonIn,
                        [value_string_as(atom), end_of_file(@(end)), default_tag(json),
                         true(#(true)),false(#(false)),null(#(null))]).

%! anchor_link_anchor_sort_order(+AnchorVname, -Edge2, -AnchorVname2flipped) is det.
anchor_link_anchor_sort_order(AnchorVname, Edge2, AnchorVnameSort) :-
    anchor_link_anchor(AnchorVname, _Edge1, _SemanticVname, Edge2, AnchorVname2),
    vname_sort(AnchorVname2, AnchorVnameSort).

% An orphan semantic doesn't have an associated anchor
orphan_semantic(AnchorVname1, SemanticVname, Edge) :-
    kythe_node(AnchorVname1, '/kythe/node/kind', 'anchor'),
    kythe_edge(AnchorVname1, Edge, SemanticVname),
    \+ (kythe_edge(AnchorVname2, _Edge2, SemanticVname),
        kythe_node(AnchorVname2, '/kythe/node/kind', 'anchor')).

% TODO: orphan semantics for non-anchors

anchor_link_anchor(AnchorVname1, Edge1, SemanticVname, Edge2, AnchorVname2) :-
    kythe_node(AnchorVname1, '/kythe/node/kind', 'anchor'),
    node_link_node(AnchorVname1, Edge1, SemanticVname, Edge2, AnchorVname2),
    kythe_node(AnchorVname2, '/kythe/node/kind', 'anchor').

% Note that because the graph is bidirectional (e.g.,
% Edge1='/kythe/edge/ref', Edge2='%/kythe/edge/ref'), it is possible
% that Vname1 = Vname2.
node_link_node(Vname1, Edge1, SemanticVname, Edge2, Vname2) :-
    kythe_edge(Vname1, Edge1, SemanticVname),
    kythe_edge(Vname2, Edge2, SemanticVname).

node_link_node_value(Vname, EdgeNodeKind, Value) :-
    node_link_node_value(Vname, Edge, _NodeVname, Name, Value),
    format(atom(EdgeNodeKind), '(~w)~w', [Edge, Name]).

node_link_node_value(Vname, Edge, Name, Value) :-
    node_link_node_value(Vname, Edge, _NodeVname, Name, Value).

node_link_node_value(Vname, Edge, NodeVname, Name, Value) :-
    kythe_edge(Vname, Edge, NodeVname),
    \+ kythe_node(NodeVname, '/kythe/node/kind', 'anchor'),
    \+ kythe_node(NodeVname, '/pykythe/color/token_color', _),
    kythe_node(NodeVname, Name, Value).

pair_vname_remove_start(Key-VnameSort, Key-Vname) :-
    vname_sort(Vname, VnameSort).

% Change the ordering of items in a vname, for sorting
vname_flip(vname(Signature, Corpus, Root, Path, Language),
           vname_flip(Corpus, Root, Path, Signature, Language)).

anchor_semantic_json(AnchorVname, SemanticJson) :-
    anchor_semantic(AnchorVname, Semantic),
    link_to_dict(Semantic, SemanticJson).


% For testing check/0:  DO NOT SUBMIT
do_not_submit0(AnchorVname, SemanticJsonSet) :-
    setof_or_empty(SemanticJson, Semantic^( anchor_semantic(AnchorVname, Semantic),
                                            link_to_dict(Semantic, SemanticJson) ),
                   SemanticJsonSet).
do_not_submit1(AnchorVname, SemanticJsonSet) :-
    setof(SemanticJson, Semantic^( anchor_semantic(AnchorVname, Semantic),
                                   link_to_dict(Semantic, SemanticJson) ),
          SemanticJsonSet).

do_not_submit2(SemanticJsonSet) :-
    setof_or_empty(SemanticJson, Semantic^link_to_dict(Semantic, SemanticJson),
                   SemanticJsonSet).


anchor_semantic(AnchorVname, Semantic) :-
    kythe_node(AnchorVname, '/kythe/node/kind', 'anchor'),
    kythe_edge(AnchorVname, Edge, Semantic),
    semantic_edge(Edge).

% TODO: add all other appropriate edges from https://kythe.io/docs/schema/
% TODO: consider defining this by what is *not* a semantic edge
% semantic_edge/1 is for anchor->semantic edges that define semantics
% so, for example, a diagnostic (/kythe/edge/tagged) would not be a
% semantic edge.
semantic_edge('/kythe/edge/defines').
semantic_edge('/kythe/edge/defines/binding').
semantic_edge('/kythe/edge/ref').
semantic_edge('/kythe/edge/ref/call').
semantic_edge('/kythe/edge/ref/call/implicit').
semantic_edge('/kythe/edge/ref/doc'). % TODO: should this be a semantic edge?
semantic_edge('/kythe/edge/ref/file').
semantic_edge('/kythe/edge/ref/implicit').
semantic_edge('/kythe/edge/ref/imports').
semantic_edge('/kythe/edge/ref/includes').
semantic_edge('/kythe/edge/ref/init').

% anchor/out_edge/1 is generated manually by scanning pykythe.pl
%    for /kythe/edge/
% TODO: add all other edges from https://kythe.io/docs/schema/
anchor_out_edge('/kythe/edge/childof').
anchor_out_edge('/kythe/edge/defines').
anchor_out_edge('/kythe/edge/defines/binding').
anchor_out_edge('/kythe/edge/ref').
anchor_out_edge('/kythe/edge/ref/call').
anchor_out_edge('/kythe/edge/ref/file').
anchor_out_edge('/kythe/edge/ref/imports').
anchor_out_edge('/kythe/edge/tagged').
anchor_out_edge('%/kythe/edge/childof').
anchor_out_edge('%/kythe/edge/defines').
anchor_out_edge('%/kythe/edge/defines/binding').
anchor_out_edge('%/kythe/edge/ref').
anchor_out_edge('%/kythe/edge/ref/call').
anchor_out_edge('%/kythe/edge/ref/file').
anchor_out_edge('%/kythe/edge/ref/imports').
anchor_out_edge('%/kythe/edge/tagged').

% TODO: currently unused
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

% TODO: Escape '/' in Corpus, Root
file_path(CombinedPath) :-
    kythe_file(Corpus, Root, Path, _Language),
    % TODO: escape '/' inside Corpus, Root
    format(atom(CombinedPath), '~w/~w/~w', [Corpus, Root, Path]).

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

tree_to_json([X|Xs], Ys) :-
    maplist(tree_to_json, [X|Xs], Ys).
tree_to_json(file(N,Path), json([type=file, name=N, path=Path])).
tree_to_json(dir(N,Path,Children), json([type=dir, name=N, path=Path, children=ChildrenDict])) :-
    tree_to_json(Children, ChildrenDict).

color_data_one_file(Corpus, Root, Path,
                    json{corpus:Corpus, root:Root, path:Path, language:Language,
                         lines:ColorTextLines}) :-
    kythe_file(Corpus,Root,Path,Language),
    Vname0 = vname0(Corpus,Root,Path,Language),
    setof(KeyedColorText, keyed_color_fact(Corpus, Root, Path, Language, KeyedColorText),
          LineNoAndChunks0),
    pairs_values(LineNoAndChunks0, LineNoAndChunks),
    group_pairs_by_key(LineNoAndChunks, ColorTextLines0),
    maplist(add_links(Vname0), ColorTextLines0, ColorTextLines1), % concurrent gives slight slow-down
    pairs_values(ColorTextLines1, ColorTextLines).

keyed_color_fact(Corpus, Root, Path, Language, Start-(LineNo-ColorChunk)) :-
    line_chunk(vname(_ColorAnchor, Corpus,Root,Path,Language), LineNo, ColorChunk),
    Start = ColorChunk.start.

add_links(Vname0, LineNo-Items, LineNo-AppendedItems) :-
    maplist(add_link(Vname0), Items, AppendedItems).

add_link(Vname0, Item, ItemWithEdges) :-
    Start = Item.start,
    % Note the use of Signature -- it gets instantiated by a lookup to
    % /kythe/loc/start and then edges are found. The lookup gives
    % either 0 or 1 result (Item.start might not have any edges
    % associated with it).
    vname_vname0(Vname, Signature, Vname0),
    (  kythe_node(Vname, '/kythe/loc/start', Start),
       % There can be multiple edges with the same label (but
       % different targets, so leave as a list and don't combine into
       % a dict.
       setof(json{edge:Edge,target:TargetJson},
             node_and_edge_json(Signature, Vname0, Edge, TargetJson),
             Edges)
    -> true
    ;  Edges = []
    ),
    put_dict(edges, Item, Edges, ItemWithEdges).

node_and_edge_json(Signature, Vname0, Edge, TargetJson) :-
    node_and_edge(Signature, Vname0, Edge, Target),
    vname_json(Target, TargetJson).

node_and_edge(Signature, Vname0, Edge, Target) :-
    vname_vname0(Vname, Signature, Vname0),
    node_and_edge(Vname, Edge, Target).

node_and_edge(Vname, Edge, Target) :-
    kythe_edge(Vname, Edge, Target).

vname_json(vname(Signature,Corpus,Root,Path,Language),
           json{signature:Signature,corpus:Corpus,root:Root,path:Path,language:Language}).

vname_signature(vname(Signature,_Corpus,_Root,_Path,_Language), Signature).

vname_vname0(vname(Signature,Corpus,Root,Path,Language),
             Signature,
             vname0(Corpus,Root,Path,Language)).

vname_vname0(vname(_Signature,Corpus,Root,Path,Language),
             vname0(Corpus,Root,Path,Language)).

path_pair_vname(vname(Signature,Corpus,Root,Path,Language),
                vname0(Corpus,Root,Path,Language)-Signature).

vname0_join_signature(vname0(Corpus,Root,Path,Language), Signature,
                      vname(Signature,Corpus,Root,Path,Language)).

vname_sort(vname(Signature, Corpus, Root, Path, Language),
           vname_sort(Corpus, Root, Path, Start, Language, Signature)) :-
    (  var(Start)               % No need to do the lookup if nonvar
    -> kythe_node(Signature, Corpus, Root, Path, Language, '/kythe/loc/start', Start)
    ;  true
    ).

vname_strip_signature(vname(_Signature1,Corpus,Root,Path,Language),
                      vname(_Signature2,Corpus,Root,Path,Language)).

%! anchor_to_line_chunks(+AnchorVname:vname, -LineNo:int, -Chunks:list(dict)) is semidet.
% Given an AnchorVname, get all the color chunks (in order) for the
% line that anchor is in. Can fail if the anchor is invalid (and if
% there isn't a color anchor that matches the token anchor).
% TODO: refactor with color_data_one_file
anchor_to_line_chunks(AnchorVname, LineNo, Chunks) :-
    anchor_to_lineno(AnchorVname, LineNo),
    vname_vname0(AnchorVname, Vname0),
    setof(KeyedChunk, keyed_color_chunk(Vname0, LineNo, KeyedChunk), KeyedChunks),
    pairs_values(KeyedChunks, Chunks).

anchor_to_lineno(AnchorVname, LineNo) :-
    kythe_node(AnchorVname, '/kythe/loc/start', Start),
    vname_strip_signature(AnchorVname, ColorVname),
    kythe_node(ColorVname, '/pykythe/color/start', Start),
    % There can be multiple chunks for a color anchor, but all have
    % the same line#.
    once(kythe_node(ColorVname, '/pykythe/color/lineno', LineNo)). % TODO: once(...)

keyed_color_chunk(Vname0, LineNo, Start-Chunk) :-
    vname_vname0(ColorVname, Vname0),
    kythe_node(ColorVname, '/pykythe/color/lineno', LineNo),
    line_chunk(ColorVname, LineNo, Chunk),
    Start = Chunk.start.

line_chunk(ColorVname, LineNo, color{lineno:LineNo, column:Column,
                                     start:Start, end:End,
                                     signature:Signature,
                                     semantic_signature:'***', % DO NOT SUBMIT fixme
                                     token_color:TokenColor, value:Value}) :-
    % DO NOT SUBMIT - needs semantic_signature (above)
    kythe_node(ColorVname, '/pykythe/color/lineno',      LineNo),
    kythe_node(ColorVname, '/pykythe/color/column',      Column),
    kythe_node(ColorVname, '/pykythe/color/start',       Start),
    kythe_node(ColorVname, '/pykythe/color/end',         End),
    kythe_node(ColorVname, '/pykythe/color/token_color', TokenColor),
    kythe_node(ColorVname, '/pykythe/color/value',       Value),
    vname_vname0(ColorVname, Vname0),
    vname_vname0(AnchorVname, Signature, Vname0),
    (  kythe_node(AnchorVname, '/kythe/loc/start', Start)
    -> true
    ;  Signature = ''
    ).

vname_neg_num_edges(Vname, MinNumEdges, NegNumEdges) :-
    setof(Edge-Vname2, kythe_edge(Vname, Edge, Vname2), Edges),
    length(Edges, NumEdges),
    NumEdges >= MinNumEdges,
    NegNumEdges is -NumEdges.

most_edges :-
    aggregate(set(N-V), vname_neg_num_edges(V, 2, N), L),
    aggregate(count, V^N^vname_neg_num_edges(V, 2, N), Len),
    aggregate(count, V^N^vname_neg_num_edges(V, 0, N), LenAll),
    format('# vnames: ~w (~w including 1-edge)~n', [Len, LenAll]),
    pykythe_utils:at_most(L, 20, L0),
    forall(member(A, L0), format('~q~n', [A])).

:- meta_predicate setof_or_empty(?, 0, ?).
setof_or_empty(Template, Goal, Set) :-
    (   setof(Template, Goal, Set)
    *-> true
    ;   Set = []
    ).


% Add a timestamp to thread messages (which are typically from the http server).
:- multifile prolog:message_prefix_hook/2.
prolog:message_prefix_hook(thread, Prefix) :-
    thread_self(Me),
    Me \== main,
    thread_property(Me, id(Id)),
    get_time(NowTimeStamp),
    % %FT adds 2020-02-23T; % %z adds '-0800'
    format_time(string(NowTime), '%T', NowTimeStamp),
    format(atom(Prefix), '[Thread ~w ~w] ', [Id, NowTime]).


%%%%%%%%%%%%%%%%%%%%%%%%%%

% Transform a list of file names to a tree for browser.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- use_module(library(check)).  % DO NOT SUBMIT
% % TODO: trap print_message(informational,check(pass(Message)))
% ?- check.                       % DO NOT SUBMIT

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
                    pykythe_json_write_dict_nl(JsonStream, FtreeJson))),
    % format('~w~n', [JsonAtom]),
    assertion(JsonAtom == '[ {"type":"dir", "name":"a", "path":"a", "children": [ {"type":"dir", "name":"b", "path":"a/b", "children": [ {"type":"file", "name":"x1", "path":"a/b/x1"},  {"type":"file", "name":"x2", "path":"a/b/x2"} ]},  {"type":"dir", "name":"d", "path":"a/d", "children": [ {"type":"dir", "name":"e", "path":"a/d/e", "children": [ {"type":"file", "name":"x3", "path":"a/d/e/x3"} ]} ]},  {"type":"file", "name":"c", "path":"a/c"} ]},  {"type":"file", "name":"x", "path":"x"} ]\n').

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

end_of_file.
