#!/usr/bin/env swipl
% -*- mode: Prolog -*-

%% For the overall structure of this, see
%% https://github.com/kamahen/swipl-server-js-client/blob/master/simple_server.pl

%% Start this with
%%   http://localhost:9999
%%     which does a redirect to:
%%   http://localhost:9999/static/src_browser.html

%% See also:
%%    http://www.pathwayslms.com/swipltuts/html/index.html
%%    https://swi-prolog.discourse.group/t/yet-another-web-applications-tutorial/566
%%    https://www.swi-prolog.org/howto/http/

%% This server handles 3 requests (see json_response/2),
%  envoked from scripts/src_browser.js (see calls to fetchFromServer()):
%%    src_file_tree
%%    src_browser_file:PartialVname
%%    anchor_xref:Vname

%% When this file is loaded, it calls src_browser_main/0, which loads
%% the Kythe facts (from files(kythe_facts), whose lookup is defined
%% by assert_server_locations/1, from parameter --filesdir). It then
%% starts a thread that validates the facts (validate_kythe_facts/0)
%% and starts the server. For performance, the facts are pre-indexed
%% (index_kythe_facts/0).

% Memory usage notes (ran gc before each one):
% loading kythe_facts.pl 151M (1 058 422 lines), qlf=77M ... bzip2 compresses .pl to 8.5MB (qlf doesn't compress)
%  before/after
%     VSZ    RSS  (in Kb)
%   40636  17728          9,219 atoms, 7,288 functors, 6,495 predicates, 109 modules,    295,076 VM-codes
%  589456 462216        320,773 atoms, 7,309 functors, 6,527 predicates, 110 modules, 29,409,360 VM-codes
%    delta VSZ = 548 820
%    delta RSS = 444 488
% So, roughly 450-550MB for the facts

:- module(src_browser, [src_browser_main/0, src_browser_main2/0]).

% :- set_prolog_flag(autoload, false).  % TODO: Seems to break plunit, qsave

:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(error), [must_be/2, domain_error/2]).
:- use_module(library(pairs), [group_pairs_by_key/2, pairs_values/2]).
:- use_module(library(prolog_jiti), [jiti_list/1]).
:- use_module(library(apply), [maplist/2, maplist/3, maplist/4, maplist/5, exclude/3]).
:- use_module(library(thread), [concurrent_maplist/2, concurrent_forall/2]).
:- use_module(library(http/http_server), [http_server/1,
                                          http_read_json_dict/3,
                                          reply_json_dict/2, % TODO: Options=[status(201)]
                                          http_redirect/3
                                         ]).
:- use_module(library(http/http_files), [http_reply_from_files/3]).
:- use_module(library(http/http_ssl_plugin)). % don't forget this, otherwise it's only an http server
% TODO: if using daemon, then: swipl src_browser.pl --port=.... --pidfile=/var/run/src_browser.pid
%       and kill $(cat /var/run/src_browser.pid)
% TODO: Support HTTPS: https://www.swi-prolog.org/pldoc/man?section=ssl-https-server
%                      /usr/share/doc/openssl/HOWTO/certificates.txt.gz
%                          openssl genrsa -out privkey.pem
%                          openssl req -new -x509 -key privkey.pem -out cacert.pem -days 1095
%                          ? openssl req -new -key privkey.pem -out cert.csr
%                      https://www.openssl.org/docs/manmaster/man1/CA.pl.html
%       ==> See swipl-devel/packages/ssl/mkcerts.pl.in
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
:- use_module('../pykythe/must_once').
:- use_module('../pykythe/pykythe_utils').

% For the kythe_edge_kind_/1 facts, adding the reverse edge:
term_expansion(kythe_edge_kind_(Edge),
                    [kythe_edge_kind(Edge),
                     kythe_edge_kind(ReverseEdge)]) :-
    atom_concat('%', Edge, ReverseEdge).

% This expands kythe_edge/11 to also have the "reverse" edges:
% (See read_and_assert_kythe_facts/0 for where the facts are read in).
term_expansion(kythe_edge(Signature1, Corpus1, Root1, Path1, Language1,
                          Edge,
                          Signature2, Corpus2, Root2, Path2, Language2),
               [kythe_edge(Signature1, Corpus1, Root1, Path1, Language1,
                           Edge,
                           Signature2, Corpus2, Root2, Path2, Language2),
                kythe_edge(Signature2, Corpus2, Root2, Path2, Language2,
                           ReverseEdge,
                           Signature1, Corpus1, Root1, Path1, Language1)]) :-
    atom_concat('%', Edge, ReverseEdge).

% The "base" Kythe facts, which are dynamically loaded at start-up.
:- dynamic kythe_node/7, kythe_edge/11, kythe_color_line/6.

% Simplistic cache for json_response/2
% TODO: LRU cache
:- dynamic json_response_cache/2.
:- retractall(json_response_cache(_, _)). % when reloading, ensure cache is cleared.

% Convenience predicates for accessing the base Kythe facts, using
% vname(Signature, Corpus, Root, Path, Language).
% Some predicates use vname0, which omits the Signature:
% vname0(Corpus, Root, Path, Language).

%! kythe_node(?Source:vname, ?FactName:atom, ?FactValue:atom) is nondet.
%% TODO: Consider storing nodes as Signature-Corpus-Root-Path-Language
%%       and a dict of FactName:ValueValue (so, a single fact for a
%%       node).
kythe_node(vname(Signature, Corpus, Root, Path, Language), FactName, FactValue) :-
    kythe_node(Signature, Corpus, Root, Path, Language, FactName, FactValue).

%! kythe_edge(?Source:vname, ?EdgeKind:atom, ?Target:vname) is nondet.
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

% For validation, get all corpus and roots:
kythe_corpus(Corpus) :- kythe_node(_Signature, Corpus, _Root, _Path, _Language, _FactName, _FactValue).
kythe_corpus(Corpus) :- kythe_edge(_Signature1, Corpus, _Root1, _Path1, _Language1,
                                   _Edge,
                                   _Signature2, _Corpus2, _Root2, _Path2, _Language2).
kythe_corpus(Corpus) :- kythe_edge(_Signature1, _Corpus1, _Root1, _Path1, _Language1,
                                   _Edge,
                                   _Signature2, Corpus, _Root2, _Path2, _Language2).
kythe_root(Root) :- kythe_node(_Signature, _Corpus, Root, _Path, _Language, _FactName, _FactValue).
kythe_root(Root) :- kythe_edge(_Signature1, _Corpus1, Root, _Path1, _Language1,
                               _Edge,
                               _Signature2, _Corpus2, _Root2, _Path2, _Language2).
kythe_root(Root) :- kythe_edge(_Signature1, _Corpus1, _Root1, _Path1, _Language1,
                               _Edge,
                               _Signature2, _Corpus, Root, _Path2, _Language2).
kythe_language(Language) :- kythe_node(_Signature, _Corpus, _Root, _Path, Language, _FactName, _FactValue).
kythe_language(Language) :- kythe_edge(_Signature1, _Corpus, _Root1, _Path1, Language,
                                       _Edge,
                                       _Signature2, _Corpus2, _Root2, _Path2, _Language2).
kythe_language(Language) :- kythe_edge(_Signature1, _Corpus1, _Root1, _Path1, _Language1,
                                       _Edge,
                                       _Signature2, _Corpus2, _Root2, _Path2, Language).
kythe_path(Path) :- kythe_node(_Signature, _Corpus, _Root, Path, _Language, _FactName, _FactValue).
kythe_path(Path) :- kythe_edge(_Signature1, _Corpus, _Root1, Path, _Language1,
                               _Edge,
                               _Signature2, _Corpus2, _Root2, _Path2, _Language2).
kythe_path(Path) :- kythe_edge(_Signature1, _Corpus1, _Root1, _Path1, _Language1,
                               _Edge,
                               _Signature2, _Corpus2, _Root2, Path, _Language2).
kythe_signature(Signature) :- kythe_node(Signature, _Corpus, _Root, _Path, _Language, _FactName, _FactValue).
kythe_signature(Signature) :- kythe_edge(Signature, _Corpus1, _Root1, _Path1, _Language1,
                                         _Edge,
                                         _Signature2, _Corpus2, _Root2, _Path2, _Language2).
kythe_signature(Signature) :- kythe_edge(_Signature1, _Corpus1, _Root1, _Path1, _Language1,
                                         _Edge,
                                         Signature, _Corpus2, _Root2, _Path2, _Language2).

:- debug(log).    % enable log messages with debug(log, '...', [...]).
:- debug(timing).
% :- debug(redirect_log).
% :- debug(request_json_log).

% List of all http debug flags:
% find /usr/lib/swi-prolog/library/http/ -type f  ! -name '*~' -print0 | xargs -0 fgrep -nH 'debug(' |sed 's/^.* debug//' | egrep -v '^/' | sed 's/,.*$//' | sort -u

% :- debug(http(post_request)).   % TODO: remove
:- debug(http(request)).        % TODO: remove % TODO: figure out why If-modified-since and friends don't always work
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
http:location(files,  root(files),  []).
http:location(json,   root(json),   []).

%! src_browser_main is det.
% Called by initialization/2 directive at startup
src_browser_main :-
    run_tests, % TODO: remove
    src_browser_main2,
    % TODO: start as daemon (see library(http/http_unix_daemon)).
    % See also library(main):main/0
    debug(log, 'Starting REPL ...', []),
    prolog.  % REPL
    % TODO: use at_halt/1 to schedule http_stop_server(Opts.port, [])
    %       after a short time-out.

%! src_browser_main2 is det.
% For interactive testing - loads facts and starts server.
src_browser_main2 :-
    browser_opts(Opts),
    % set_prolog_flag(verbose_file_search, true),
    assert_server_locations(Opts),
    read_and_assert_kythe_facts,
    % See comments with "Support HTTPS" above.
    http_server([port(Opts.port),
                 % TODO: enable ssl (https):
                 % ssl([certificate_file('cacert.pem'), % or cert.csr?
                 %      key_file('privkey.pem')]),
                 workers(5)]).

                % ?? sudo apt install certbot  (letsencrypt.org points to this)
                % ssl([  certificate_file('/tmp/fullchain.pem'), % letsencrypt gives you this file
                %        key_file('/tmp/privkey.pem'), % letsencrypt gives you this file
                %        cacerts([file('/etc/ssl/certs/ca-certificates.crt'),

%! browser_opts(-Opts:dict) is det.
% Use opt_arguments/3 to get the command-line options, andput them into a dict.
browser_opts(Opts) :-
    validate_prolog_version,
    OptsSpec =
    [[opt(port), type(integer), default(9999), longflags([port]),
      help('Server port')],
     [opt(filesdir), type(atom), default('filesdir-must-be-specified'), longflags([filesdir]),
      help('Directory for the files\'s contents (for "files" URL) and for load_files(files(kythe_facts))')],
     [opt(staticdir), type(atom), default('staticdir-must-be-specified'), longflags([staticdir]),
      help('Directory for the static files (for "static" URL)')]
    ],
    opt_arguments(OptsSpec, Opts0, PositionalArgs),
    dict_create(Opts, opts, Opts0),
    must_once_msg(PositionalArgs = [], 'Unknown positional arg(s)').

:- if(false).

% Not used -- a trivial REPL, in case prolog/0 or
% '$toplevel':'$toplevel'/0 doesn't work.

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

%! assert_server_locations(+Opts:dict) is det.
% Add file_search_path/2 facts for files(...) and static(...) file
% specifications (see absolute_file_name/3).
assert_server_locations(Opts) :-
    debug(log, 'files dir:  ~q', [Opts.filesdir]),
    debug(log, 'static dir: ~q', [Opts.staticdir]),
    asserta(user:file_search_path(files,  Opts.filesdir)),
    asserta(user:file_search_path(static, Opts.staticdir)).

%! read_and_assert_kythe_facts is det.
% Load the kythe_facts file, pre-index it and start a separate
% thread for validating the facts.
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
    % Note that term_expansion/2 takes effect on the kythe_edge/11 facts
    % that are loaded here:
    catch(load_files([files('kythe_facts')],
                     [silent(false),
                      optimise(false), % Doesn't seem to have any effect
                      imports([kythe_node/7,
                               kythe_edge/11])]),
          E,
          (   format('Could not load Kythe facts: ~q~n', [E]),
              halt(1)
          )),
    index_kythe_facts,
    thread_create(validate_kythe_facts, _, [detached(true)]).

%! index_kythe_facts is det.
% Build the indexes in parallel.
index_kythe_facts :-
    concurrent_count(10 * Cores, Cores, % will reduce to length of goals
        concurrent_maplist(index_pred,
                       [kythe_node(sig,_,_,_,_,_,_),                       % kythe_node/7 1
                      % kythe_node(sig,corpus,root,path,lang,_,_),         % kythe_node/7 1+4
                      % kythe_node(sig,_,_,_,_,_,value),                   % kythe_node/7 1+7
                        kythe_node(_,_,_,path,_,_,value),                  % kythe_node/7 4+7
                        kythe_node(_,_,_,_,_,fact,value),                  % kythe_node/7 6+7
                        kythe_edge(sig,corpus,root,path,lang,_,_,_,_,_,_), % kythe_edge/11 1
                        kythe_edge(_,_,_,_,_,_,sig,corpus,root,path,lang), % kythe_edge/11 7
                        kythe_edge(_,_,_,_,_,lang,sig,corpus,root,path,lang), % kythe_edge/11 6
                        kythe_edge(vname(sig,corpus,root,path,lang),_,_),  % kythe_edge/3 1
                        kythe_edge(_,_,vname(sig,corpus,root,path,lang)),  % kythe_edge/3 1
                        kythe_edge(vname(sig,corpus,root,path,lang),edge,_),
                        kythe_edge(_,edge,vname(sig,corpus,root,path,lang)),
                        kythe_edge(_,edge,_),
                        kythe_color_line(corpus,root,path,lang,_,_),       % kythe_color_line/6 3
                        kythe_color_line(corpus,root,path,lang,0,_)        % kythe_color_line/6 3+5
                       ])),
    garbage_collect,
    show_jiti,
    debug(log, 'JIT index done.', []).

%! validate_kythe_facts is det.
% Run some consistency/validity checks on the Kythe facts. Throws
% an exception on failure.
% DO NOT SUBMIT - add check that all edges, facts conform to
%      kythe_edge_kind/1, kythe_fact_name/1
validate_kythe_facts :-
    statistics(walltime, [T0_ms_valid, _]),
    statistics(process_cputime, T0),
    must_fail(orphan_semantic(_AnchorVname, _SemanticVname, _Edge)),
    forall(distinct(Corpus, kythe_corpus(Corpus)),
           must_once(atom(Corpus))),
    forall(distinct(Root, kythe_root(Root)),
           must_once(atom(Root))),
    forall(distinct(Language, kythe_language(Language)),
           must_once(atom(Language))),
    forall(kythe_path(Path),
           must_once(atom(Path))),
    forall(kythe_signature(Signature),
           must_once(atom(Signature))),
    forall(kythe_node(Vname, Name, Value),
           must_be(ground, kythe_node(Vname, Name, Value))),
    forall(kythe_edge(V1, Edge, V2),
           must_be(ground, kythe_edge(V1, Edge, V2))),
    forall(distinct(Vname, kythe_node(Vname, _, _)),
           must_once( ( kythe_node(Vname, Name , _),
                        memberchk(Name, ['/kythe/node/kind',
                                         '/pykythe/type']) ) )),
    forall(distinct(Vname, kythe_node_kind_kythe(Vname)),
           must_once(valid_kythe_node(Vname))),
    % TODO: The following tests (anchor_to_lineno, anchor_to_line_chunks)
    %       aren't very useful because both of these predicates have
    %       fallback code (for situations where the source didn't parse).
    %       Need better tests.
    % concurrent_forall(kythe_anchor(Anchor),
    %        must_once( anchor_to_lineno(Anchor, _LineNo) )),
    % concurrent_forall(kythe_anchor(Anchor),
    %        must_once(( anchor_to_line_chunks(Anchor, _, Chunks),
    %                    Chunks \== [] ))),
    % show_jiti,    % Not needed - should be the same as the first one
    % validate_anchor_link_anchor, % DO NOT SUBMIT: fix this test, which is also slow
    statistics(walltime, [T2_ms_valid, _]),
    statistics(process_cputime, T2),
    Tvalid is T2 - T0,
    T_ms_valid is (T2_ms_valid - T0_ms_valid) * 0.001,
    debug(log, 'Validation done: ~3f sec. (real: ~3f sec.).', [Tvalid, T_ms_valid]),
    debug(log, '', []),
    debug(log, 'Server started: to stop, enter ctrl-D or "halt." (including the ".")', []).

kythe_node_kind_kythe(Vname) :-
    kythe_node(Vname, Kind, _),
    sub_atom(Kind, 0, _, _, '/kythe/').

valid_kythe_node(Vname) :-
    % In most cases, there's just one "kind" per node, but there are
    % cases where a node can be both a "package" and a "variable"
    % (e.g., if there's some kind of "if" around definining one).
    % distinct(Vname, kythe_node(Vname, _, _)),
    must_once(bagof(K, kythe_node(Vname, '/kythe/node/kind', K), Ks)),
    must_once(Ks = [_|_]), % Always true for bagof/3; not true for findall/3
    maplist(valid_node_kind(Vname), Ks).

valid_node_kind(Vname, Kind) :-
    valid_node_kind_(Kind, Vname).

valid_node_kind_('anchor',     Vname) :- valid_anchor_vname(Vname).
valid_node_kind_('diagnostic', Vname) :- valid_anchor_vname(Vname).
valid_node_kind_('file',       Vname) :-
    kythe_node(Vname, '/kythe/language', Language),
    Language \== '',
    kythe_node(Vname, '/kythe/text', _),
    kythe_node(Vname, '/kythe/text/encoding', _).
valid_node_kind_('function',   Vname) :- valid_semantic_vname(Vname).
valid_node_kind_('package',    Vname) :- valid_semantic_vname(Vname).
valid_node_kind_('record',     Vname) :- valid_semantic_vname(Vname).
valid_node_kind_('variable',   Vname) :- valid_semantic_vname(Vname).

valid_semantic_vname(vname(Signature, _Corpus, _Root, Path, Language)) :-
    Signature \== '',
    Path == '',
    Language \= ''.

valid_anchor_vname(vname(Signature, _Corpus, _Root, Path, Language)) :-
    Signature \== '',
    Path \== '',
    Language \== ''.

valid_path_vname(vname(Signature, _Corpus, _Root, Path, Language)) :-
    Signature \== '',
    Path \== '',
    Language == ''.

v_n(Vname) :-
    kythe_node(Vname, '/pykythe/type', _),
    \+ kythe_node(Vname, '/kythe/node/kind', _).

% TODO: validate_anchor_link_anchor is incorrect:
% Semantic links have unique signatures ... this isn't true. For example
% if we can't resolve an attr to a single item (ast_raw.py at line 267: "ch0.type")
validate_anchor_link_anchor :-
    forall(anchor_link_anchor(AnchorVname1, Edge1, _SemanticVname, Edge2, AnchorVname2),
           must_once(setof(anchor_link_anchor(AnchorVname1, Edge1, SemanticVname2, Edge2, AnchorVname2),
                           anchor_link_anchor(AnchorVname1, Edge1, SemanticVname2, Edge2, AnchorVname2),
                           [_]))).

%! index_pred(:Goal) is nondet.
% Call a single Goal and ignore its result - this forces a building the index.
index_pred(Goal) :-
    statistics(process_cputime, T0),
    ( Goal -> true ; true ),
    statistics(process_cputime, T1),
    T is T1 - T0,
    debug(log, 'Indexed ~k in ~3f sec', [Goal, T]).

%! show_jiti is det.
show_jiti :-
    strip_module(kythe_node(_,_,_), Module, _),
    jiti_list(Module:_),
    aggregate_all(count, kythe_node(_A1,_A2,_A3,_A4,_A5,_A6,_A7), LenSigs),
    aggregate_all(count, kythe_node( A1, A2, A3, A4, A5, A6, A7),
                         kythe_node( A1, A2, A3, A4, A5, A6, A7), LenSigsUnique),
    aggregate_all(count, kythe_edge(_B1,_B2,_B3,_B4,_B5,_B6,_B7,_B8,_B9,_B10,_B11), LenEdges),
    aggregate_all(count, kythe_edge( B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11),
                         kythe_edge( B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11), LenEdgesUnique),
    aggregate_all(count, kythe_color_line(_C1,_C2,_C3,_C4,_C5,_C6), LenColorLines),
    aggregate_all(count, kythe_color_line( C1, C2, C3, C4, C5, C6),
                         kythe_color_line( C1, C2, C3, C4, C5, C6), LenColorLinesUnique),
    debug(log, 'kythe_node(Signature) in ~q: ~d (~d unique).', [Module, LenSigs, LenSigsUnique]),
    debug(log, 'kythe_edge in ~q: ~d (~d unique), color lines: ~d (~d unique).', [Module, LenEdges, LenEdgesUnique, LenColorLines, LenColorLinesUnique]).


%%%%%% HTTP handlers %%%%%%%%

%! http_handler(+Path, :Closure, +Options is det.
% See library(http/http_dispatch) http_handler/3.
% localhost:9999/ ... redirects to /static/src_browser.html
%      - for debugging, 'moved' can be cleared by chrome://settings/clearBrowserData
%        (Cached images and files)
:- http_handler(root(.),
                http_handler_redirect(
                    moved_temporary, % or moved - but that makes debugging a bit more difficult
                    static('src_browser.html')),
                []).

% Serve localhost:9999/static/ from 'static' directory (See also facts for http:location/3)
:- http_handler(static(.),
                pykythe_http_reply_from_files(static(.), [cache(true)]),
                [prefix]).
:- http_handler(files(.),
                pykythe_http_reply_from_files(files(.), [cache(true)]),
                [prefix]).

:- http_handler(root(json),     % localhost:9999/json
                reply_with_json, [priority(0)]).

%! pykythe_http_reply_from_files(+Dir, +Options, +Request) is det.
% Callback from http_handler/3 for a file - calls http_reply_form_files(Dir, Options, Request).
pykythe_http_reply_from_files(Dir, Options, Request) :-
    (  false % Change this to true to debug file caching
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

%! http_handler_redirect(+How, +To, +Request) is det.
% Handle a redirect callback.
http_handler_redirect(How, To, Request) :-
    memberchk(path(Base), Request),
    memberchk(request_uri(RequestURI), Request),
    http_absolute_location(To, ToURL, [relative_to(Base)]),
    uri_components(RequestURI, URI),
    uri_data(path, URI, ToURL, ToURI),
    uri_components(NewTo, ToURI),
    debug(redirect_log, 'Redirect: ~q', [[how:How, to:To, toURL:ToURL, requestURI:RequestURI, uri:URI, toURI:ToURI, newTo: NewTo]]),
    http_redirect(How, NewTo, Request).

%! reply_with_json(+Request) is det.
% Handle a request from static/src_browser.js in browser.
% See the calls to fetchFromServer() - these are handled by json_response/2.
reply_with_json(Request) :-
    % print_term_cleaned(Request, [], RequestPretty),
    % TODO: why doesn't thead_cputime give non-zero value?
    statistics(cputime, T0),
    must_once(memberchk(method(post), Request)),
    % Doesn't show the 302 (or 301) redirect:
    %    request_uri('/json'), path('/json'),
    %    origin('http://localhost:9999'), content_type('application/json')
    http_handler_read_json_dict(Request, JsonIn), % [content_type("application/json")]),
    debug(request_json_log, 'Request(json): ~q', [JsonIn]),
    (   json_response_cache(JsonIn, JsonOut)
    ->  statistics(cputime, T2),
        debug(timing, 'Request-Json:  ~q found in cache', [JsonIn])
   ;    statistics(cputime, T1),
        Tdelta1 is T1 - T0,
        debug(timing, 'Request-JSON: ~q [~3f sec]', [JsonIn, Tdelta1]),
        must_once(
                  json_response(JsonIn, JsonOut)), % TODO: improve this error handling
        % debug(request_json_log, 'Request(json): ~q => ~q', [JsonIn, JsonOut]), % DO NOT SUBMIT
        asserta(json_response_cache(JsonIn, JsonOut)),
        statistics(cputime, T2),
        Tdelta2 is T2 - T1,
        debug(timing, 'Request-response: ~q [~3f sec]', [JsonIn, Tdelta2])
    ),
    % TODO: cache the JSON form rather than the JsonOut form
    reply_json_dict(JsonOut, [width(0)]), % TODO: is this the most expensive part?
    statistics(cputime, T3),
    Tdelta3 is T3 - T2,
    debug(timing, 'Request-reply: ~q [~3f sec]', [JsonIn, Tdelta3]).

%! json_response(+Request:dict, -Response:dict) is det.
% Handle a specific request using fetchFromSever() in static/src_browser.js (browser).
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
                   semantic_links: SemanticLinksJson,
                   edge_links: EdgeLinksJson}) :-
    !,
    AnchorVname = vname(Signature, Corpus, Root, Path, Language),
    % TODO: probably better to use nested setof/bagof, but with
    %       library(solution_sequences) for ordering, grouping
    anchor_to_line_chunks(AnchorVname, LineNo, LineChunks),
    debug(log, 'Xref ~q lineno: ~q', [[signature:Signature, corpus:Corpus, root:Root, path:Path, language:Language], LineNo]),
    anchor_links_grouped(AnchorVname, SemanticNodeValues, SemanticLinks, EdgeLinks0),
    maplist([Edge-PathAnchors, Edge-PathLines]>>
                maplist(path_anchors_to_line_chunks, PathAnchors, PathLines),
            EdgeLinks0, EdgeLinks),
    setof_or_empty(S, anchor_semantic_json(AnchorVname, S), SemanticVnamesJson),
    maplist([Key-Value, json{kind:Key, value:Value}]>>true,
            SemanticNodeValues, SemanticNodeValuesJson),
    % debug(log, 'Xref-SemanticLinks: ~q', [SemanticLinks]),
    maplist([Key-Value, json{kind:Key, value:Value}]>>true,
            SemanticLinks, SemanticLinksJson),
    maplist([Edge-Links, json{edge: Edge, links: LinksJson}]>>
                maplist(link_to_dict, Links, LinksJson),
            EdgeLinks, EdgeLinksJson).
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

%! file_path(-CombinedPath) is nondet.
% Combine the Corpus/Root/Path into a single string for the browser's file tree
file_path(Corpus:Root:Path) :-
    kythe_file(Corpus, Root, Path, _Language).

%! anchor_links_grouped(+AnchorVname, -SemanticNodeValues, -SemanticLinks, -GroupedLinks) is det.
% Get the anchor links (grouped for an AnchorVname.
% SemanticNodeValues gets the semantic nodes as a list of the form NodeName-Value
%     (e.g., NodeName='(/kythe/edge/ref)/kythe/node/kind', Value=record)
% SemanticLinks - TODO: document
% GroupedLinks gets a list of links in the form NodeName-list(vname)
%     (e.g., NodeName='/kythe/edge/ref').
% TODO: Can we use library(solution_sequences) group_by/4?  It has
%       bagof rather than setof at bottom, which is not necessarily a
%       bad thing, as it might show errors in the Kythe facts
%       (although probably not -- duplicates should have been removed
%       by the processing pipeline).
anchor_links_grouped(AnchorVname, SemanticNodeValues, SemanticLinks, GroupedLinks) :-
    setof_or_empty(NodeKind-NodeValue,
                   node_link_node_value(AnchorVname, NodeKind, NodeValue), SemanticNodeValues),
    setof_or_empty(NodeKind-NodeValue,
                   node_link_semantic(AnchorVname, NodeKind, NodeValue), SemanticLinks),

    % TODO: filter Edge1 by type of link, e.g. semantic_edge_def/1?
    setof_or_empty(Edge2-AnchorVname2flipped,
                   anchor_link_anchor_sort_order(AnchorVname, Edge2, AnchorVname2flipped),
                   SortedLinks0),
    maplist(pair_vname_remove_start, SortedLinks0, SortedLinks),
    group_pairs_by_key(SortedLinks, GroupedLinks0),
    maplist(group_edge_by_files, GroupedLinks0, GroupedLinks).

group_edge_by_files(Edge-Vnames, Edge-GroupVnamePaths) :-
    maplist(path_pair_vname, Vnames, VnamePaths),
    group_pairs_by_key(VnamePaths, GroupVnamePaths0),
    maplist(path_vname, GroupVnamePaths0, GroupVnamePaths).

path_vname(Vname0-Signatures, Vname0-Vnames) :-
    maplist(vname0_join_signature(Vname0), Signatures, Vnames).

pair_vname_remove_start(Key-VnameSort, Key-Vname) :-
    vname_sort(Vname, VnameSort).

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

link_to_dict(vname(Signature, Corpus, Root, Path, Language),
             json{signature: Signature,
                  corpus: Corpus, root: Root,
                  path: Path, language: Language}) :- !.
link_to_dict(vname_flip(Corpus, Root, Path, Signature, Language),
             json{signature: Signature,
                  corpus: Corpus, root: Root,
                  path: Path, language: Language}) :- !.
link_to_dict(Json, Json) :-
    must_be(dict, Json),
    is_dict(Json, Tag),
    must_be(oneof([line,path]), Tag).

% TODO: combine with pykythe_utils:pykythe_json_read_dict?
http_handler_read_json_dict(Request, JsonIn) :-
    http_read_json_dict(Request, JsonIn,
                        [value_string_as(atom), end_of_file(@(end)), default_tag(json),
                         true(#(true)),false(#(false)),null(#(null))]).

%! anchor_link_anchor_sort_order(+AnchorVname, -Edge2, -AnchorVname2flipped) is det.
% Get the edge1->edge2 for a link and put the AnchorVname into the order for sorting,
% used by setof(Edge2-AnchorVname2flipped, ...)
anchor_link_anchor_sort_order(AnchorVname, Edge2, AnchorVnameSort) :-
    anchor_link_anchor(AnchorVname, _Edge1, _SemanticVname, Edge2, AnchorVname2),
    vname_sort(AnchorVname2, AnchorVnameSort).

%! anchor_link_anchor(?AnchorVname1, ?Edge1, ?SemanticVname, ?Edge2, ?AnchorVname2) is nondet.
% Lookup AnchorVname1 (Edge1)-> SemanticVname (Edge2)-> AnchorVname2
anchor_link_anchor(AnchorVname1, Edge1, SemanticVname, Edge2, AnchorVname2) :-
    kythe_anchor(AnchorVname1),
    node_link_node(AnchorVname1, Edge1, SemanticVname, Edge2, AnchorVname2),
    kythe_anchor(AnchorVname2).

anchor_link_semantic(AnchorVname, Edge, SemanticVname) :-
    (   var(AnchorVname)
    ->  kythe_edge(AnchorVname, Edge, SemanticVname),
        kythe_anchor(AnchorVname)
    ;   kythe_anchor(AnchorVname),
        kythe_edge(AnchorVname, Edge, SemanticVname)
    ).

%! orphan_semantic(?AnchorVname1, ?SemanticVname, ?Edge) is nondet.
% An orphan semantic doesn't have an associated anchor.
% Used for validation of Kythe facts.
% TODO: orphan semantics for non-anchors
orphan_semantic(AnchorVname1, SemanticVname, Edge) :-
    kythe_anchor(AnchorVname1),
    kythe_edge(AnchorVname1, Edge, SemanticVname),
    \+ (kythe_edge(AnchorVname2, _Edge2, SemanticVname),
        kythe_anchor(AnchorVname2)).

% Note that because the graph is bidirectional (e.g.,
% Edge1='/kythe/edge/ref', Edge2='%/kythe/edge/ref'), it is possible
% that Vname1 = Vname2.
node_link_node(Vname1, Edge1, SemanticVname, Edge2, Vname2) :-
    kythe_edge(Vname1, Edge1, SemanticVname),
    kythe_edge(SemanticVname, Edge2, Vname2).

node_link_node_value(Vname, EdgeNodeKind, Value) :-
    kythe_edge(Vname, Edge, NodeVname),
    \+ kythe_anchor(NodeVname),
    kythe_node(NodeVname, Name, Value),
    shorten_edge(Edge, ShortEdge),
    format(atom(EdgeNodeKind), '(~w)~w', [ShortEdge, Name]).

% DO NOT SUBMIT -- this doesn't work at outer level, because there's
%  no anchor for a module.
node_link_semantic(Anchor1, EdgeNodeKind, Semantic2Sig) :-
    anchor_def_link(Anchor1, Edge1, _Semantic1, Edge2, Semantic2),
    % TODO: look up Semantic2->Anchor2 line (but watch out for node/kind package)
    Semantic2 = vname(Semantic2Sig, _, _, _, _),
    % Remove the edges that are handled elsewhere:
    Edge2 \= '%/kythe/edge/defines/binding',
    Edge2 \= '%/kythe/edge/ref',
    shorten_edge(Edge1, ShortEdge1),
    shorten_edge(Edge2, ShortEdge2),
    format(atom(EdgeNodeKind), '(~w)~w', [ShortEdge1, ShortEdge2]).

anchor_def_link(Anchor1, Edge1, Semantic1, Edge2, Semantic2) :-
    semantic_edge_def(Edge1),
    anchor_semantic_link_semantic(Anchor1, Edge1, Semantic1, Edge2, Semantic2).

anchor_semantic_link_semantic(Anchor1, Edge1, Semantic1, Edge2, Semantic2) :-
    kythe_anchor(Anchor1),
    kythe_edge(Anchor1, Edge1, Semantic1),
    kythe_edge(Semantic1, Edge2, Semantic2).

shorten_edge(Edge, ShortEdge) :-
    (   atom_concat('/kythe/edge/', ShortEdge, Edge)
    ->  true
    ;   atom_concat('%/kythe/edge/', ShortEdge0, Edge)
    ->  atom_concat('%', ShortEdge0, ShortEdge)
    ;   ShortEdge = Edge
    ).

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
    kythe_anchor(AnchorVname),
    kythe_edge(AnchorVname, _Edge, Semantic).

% The following are derived from kythe/kythe/go/util/schema/indexdata.go

kythe_node_kind('abs').
kythe_node_kind('absvar').
kythe_node_kind('anchor').
kythe_node_kind('constant').
kythe_node_kind('diagnostic').
kythe_node_kind('doc').
kythe_node_kind('file').
kythe_node_kind('function').
kythe_node_kind('interface').
kythe_node_kind('lookup').
kythe_node_kind('macro').
kythe_node_kind('meta').
kythe_node_kind('name').
kythe_node_kind('package').
kythe_node_kind('process').
kythe_node_kind('record').
kythe_node_kind('sum').
kythe_node_kind('symbol').
kythe_node_kind('talias').
kythe_node_kind('tapp').
kythe_node_kind('tbuiltin').
kythe_node_kind('tnominal').
kythe_node_kind('tsigma').
kythe_node_kind('variable').
kythe_node_kind('vcs').

kythe_node_subkind('category').
kythe_node_subkind('class').
kythe_node_subkind('constructor').
kythe_node_subkind('destructor').
kythe_node_subkind('enum').
kythe_node_subkind('enumClass').
kythe_node_subkind('field').
kythe_node_subkind('implicit').
kythe_node_subkind('import').
kythe_node_subkind('initializer').
kythe_node_subkind('local').
kythe_node_subkind('local/parameter').
kythe_node_subkind('method').
kythe_node_subkind('namespace').
kythe_node_subkind('struct').
kythe_node_subkind('type').
kythe_node_subkind('union').

kythe_fact_name('/kythe/build/config').
kythe_fact_name('/kythe/code').
kythe_fact_name('/kythe/complete').
kythe_fact_name('/kythe/context/url').
kythe_fact_name('/kythe/details').
kythe_fact_name('/kythe/doc/uri').
kythe_fact_name('/kythe/label').
kythe_fact_name('/kythe/loc/end').
kythe_fact_name('/kythe/loc/start').
kythe_fact_name('/kythe/message').
kythe_fact_name('/kythe/node/kind').
kythe_fact_name('/kythe/param/default').
kythe_fact_name('/kythe/ruleclass').
kythe_fact_name('/kythe/snippet/end').
kythe_fact_name('/kythe/snippet/start').
kythe_fact_name('/kythe/subkind').
kythe_fact_name('/kythe/tag/deprecated').
kythe_fact_name('/kythe/text').
kythe_fact_name('/kythe/text/encoding').
kythe_fact_name('/kythe/visibility').

kythe_edge_kind_('/kythe/edge/aliases').
kythe_edge_kind_('/kythe/edge/aliases/root').
kythe_edge_kind_('/kythe/edge/annotatedby').
kythe_edge_kind_('/kythe/edge/bounded/lower').
kythe_edge_kind_('/kythe/edge/bounded/upper').
kythe_edge_kind_('/kythe/edge/childof').
kythe_edge_kind_('/kythe/edge/childof/context').
kythe_edge_kind_('/kythe/edge/completes').
kythe_edge_kind_('/kythe/edge/completes/uniquely').
kythe_edge_kind_('/kythe/edge/defines').
kythe_edge_kind_('/kythe/edge/defines/binding').
kythe_edge_kind_('/kythe/edge/depends').
kythe_edge_kind_('/kythe/edge/documents').
kythe_edge_kind_('/kythe/edge/exports').
kythe_edge_kind_('/kythe/edge/extends').
kythe_edge_kind_('/kythe/edge/generates').
kythe_edge_kind_('/kythe/edge/imputes').
kythe_edge_kind_('/kythe/edge/instantiates').
kythe_edge_kind_('/kythe/edge/instantiates/speculative').
kythe_edge_kind_('/kythe/edge/named').
kythe_edge_kind_('/kythe/edge/overrides').
kythe_edge_kind_('/kythe/edge/overrides/root').
kythe_edge_kind_('/kythe/edge/overrides/transitive').
kythe_edge_kind_('/kythe/edge/param').
kythe_edge_kind_('/kythe/edge/property/reads').
kythe_edge_kind_('/kythe/edge/property/writes').
kythe_edge_kind_('/kythe/edge/ref').
kythe_edge_kind_('/kythe/edge/ref/call').
kythe_edge_kind_('/kythe/edge/ref/call/implicit').
kythe_edge_kind_('/kythe/edge/ref/doc').
kythe_edge_kind_('/kythe/edge/ref/expands').
kythe_edge_kind_('/kythe/edge/ref/expands/transitive').
kythe_edge_kind_('/kythe/edge/ref/file').
kythe_edge_kind_('/kythe/edge/ref/id').
kythe_edge_kind_('/kythe/edge/ref/implicit').
kythe_edge_kind_('/kythe/edge/ref/imports').
kythe_edge_kind_('/kythe/edge/ref/includes').
kythe_edge_kind_('/kythe/edge/ref/init').
kythe_edge_kind_('/kythe/edge/ref/init/implicit').
kythe_edge_kind_('/kythe/edge/ref/queries').
kythe_edge_kind_('/kythe/edge/satisfies').
kythe_edge_kind_('/kythe/edge/specializes').
kythe_edge_kind_('/kythe/edge/specializes/speculative').
kythe_edge_kind_('/kythe/edge/tagged').
kythe_edge_kind_('/kythe/edge/typed').
kythe_edge_kind_('/kythe/edge/undefines').

% Added separately:

semantic_edge_def('/kythe/edge/defines').
semantic_edge_def('/kythe/edge/defines/binding').
semantic_edge_def('%/kythe/edge/defines').
semantic_edge_def('%/kythe/edge/defines/binding').

%! kythe_anchor(?Vname) is nondet.
kythe_anchor(Vname) :-
    kythe_node(Vname, '/kythe/node/kind', 'anchor').

%! kythe_anchor(?Vname, ?Start, ?End, -Token) is nondet.
% Lookup an anchor, and extract its "token" from the /kythe/text fact.
% TODO: currently unused
kythe_anchor(Vname, Start, End, Token) :-
    kythe_anchor(Vname),
    kythe_node(Vname, '/kythe/loc/start', Start),
    kythe_node(Vname, '/kythe/loc/end',   End),
    Len is End - Start,
    Vname = vname(_, Corpus, Root, Path, _),
    kythe_node(vname('', Corpus, Root, Path, _), '/kythe/text', SourceText),
    sub_string(SourceText, Start, Len, _, TokenStr),
    atom_string(Token, TokenStr).

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
tree_to_json(file(Name,Corpus,Root,Path),
             json{type:file, name:Name,
                  corpus:Corpus, root:Root, path:Path}).
tree_to_json(dir(Name,Children,Corpus,Root,Path),
             json{type:dir, name:Name, children:ChildrenDict,
                  corpus:Corpus, root:Root, path:Path}) :-
    tree_to_json(Children, ChildrenDict).

color_data_one_file(Corpus, Root, Path,
                    json{corpus:Corpus, root:Root, path:Path, language:Language,
                         lines:ColorTextLines,
                         anchor_to_anchors:AnchorToAnchorsDict,
                         anchor_to_semantics:AnchorToSemanticsDict,
                         semantic_to_anchors:SemanticToAnchorsDict}) :-
    kythe_file(Corpus,Root,Path,Language),
    kythe_color_all(Corpus,Root,Path,Language, ColorAll0),
    maplist(verify_color_items, ColorAll0), % TODO: delete
    maplist(maplist(add_color_edges(Corpus,Root,Path,Language)), ColorAll0, ColorTextLines),
    conv_conv_dict(anchor_to_anchors_same_file(Corpus,Root,Path,Language), ColorAll0, AnchorToAnchorsDict),
    % TODO: the code for AnchorToSemanticsDict and SemanticToAnchorsDict is too
    %       complicated; better to just generate all pairs of anchor-semantic and
    %       then group by anchor or semantic
    conv_conv_dict(anchor_to_semantics(Corpus,Root,Path,Language), ColorAll0, AnchorToSemanticsDict),
    semantic_to_anchors_same_file(Corpus,Root,Path,Language, ColorAll0, SemanticToAnchorsDict),
    % debug(log, 'color_data- ~q', [[Corpus, Root, Path, Language]]), % DO NOT SUBMIT
    % debug(log, 'color_data- anchor_to_anchors ~q', [AnchorToAnchorsDict]), % DO NOT SUBMIT
    % debug(log, 'color_data- anchor_to_semantics ~q', [AnchorToSemanticsDict]), % DO NOT SUBMIT
    % debug(log, 'color_data- semantic_to_anchors ~q', [SemanticToAnchorsDict]), % DO NOT SUBMIT
    true.

conv_conv_dict(Pred, ColorAll0, Dict) :-
    convlist(convlist(Pred), ColorAll0, PairsList),
    append(PairsList, Pairs),
    dict_pairs(Dict, json, Pairs).

kythe_color_all(Corpus, Root, Path, Language, ColorTerm) :-
    % Depends on ordering of the facts; if we can't depend on it,
    % then use setof/3:
    findall(Line,
            kythe_color_line(Corpus, Root, Path, Language, _LineNo, Line),
            ColorTerm).

verify_color_items(ColorItems) :-
    % TODO: delete this validation
    exclude(has_start, ColorItems, WithoutStart),
    must_once([] == WithoutStart),
    maplist([Item, Key-Item]>>get_dict(start, Item, Key), ColorItems, KeyedColorItems),
    keysort(KeyedColorItems, KeyedColorItemSorted),
    must_once(KeyedColorItems == KeyedColorItemSorted).

has_start(Item) :- get_dict(start, Item, _).

add_color_edges(Corpus,Root,Path,Language, ColorAll0, ColorAll) :-
    (   ColorAll0.token_color == '<PUNCTUATION>'
    ->  (   kythe_node_punctuation_linkable(ColorAll0.signature,
                                            Corpus, Root, Path, Language,
                                            ColorAll0.start, ColorAll0.end)
        ->  % TODO: assert exactly one AnchorPunctuation
            %       setof(_, ...linkable(...), [AnchorPunctuation])
            put_dict(token_color, ColorAll0, '<PUNCTUATION_REF>', ColorAll1)
        ;   put_dict(signature, ColorAll0, '', ColorAll1)
        )
    ;   ColorAll1 = ColorAll0
    ),
    get_link_edges(Corpus, Root, Path, Language, ColorAll1, ColorAll).

kythe_node_punctuation_linkable(Signature, Corpus,Root,Path,Language, Start, End) :-
    Signature \== '',
    AnchorPunctuation = vname(Signature, Corpus,Root,Path,Language),
    kythe_node(AnchorPunctuation, '/kythe/loc/start', Start),
    kythe_node(AnchorPunctuation, '/kythe/loc/end', End),
    (   kythe_anchor(AnchorPunctuation)
    ;   kythe_edge(AnchorPunctuation, '/kythe/edge/tagged', _)
    ).

get_link_edges(Corpus, Root, Path, Language, ColorAll1, ColorAll) :-
    % We ignore ColorAll1.signature - it's '' for '<BARE>'
    % Note the use of Signature -- it gets instantiated by a lookup to
    % /kythe/loc/start and then edges are found. The lookup gives
    % either 0 or 1 restuls (Item.start might not have any edges
    % associated with it).
    (   Vname = vname(Signature, Corpus, Root, Path, Language),
        kythe_node(Vname, '/kythe/loc/start', ColorAll1.start),
        kythe_node(Vname, '/kythe/loc/end', ColorAll1.end), % TODO: not needed?
        must_once(kythe_anchor(Vname)), % TODO: remove this check
        vname_vname0(Vname, Signature, Vname0),
        % There can be multiple edges with the same label (but
        % different targets), so leave as a list and don't combine
        % into a dict.
        setof(json{edge:Edge,target:TargetJson},
              node_and_edge_json(Signature, Vname0, Edge, TargetJson),
              Edges)
    ->  put_dict(edges, ColorAll1, Edges, ColorAll2),
        put_dict(signature, ColorAll2, Signature, ColorAll)
    ;   put_dict(edges, ColorAll1, [], ColorAll)
    ).

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

anchor_to_anchors_same_file(Corpus,Root,Path,Language, ColorToken, AnchorSignature-Signatures) :-
    var_token(ColorToken.token_color),
    AnchorSignature = ColorToken.signature,
    % The following constrains the anchors to all be in the same file;
    % it will fail if Signature isn't for an anchor.
    setof(S,
          anchor_link_anchor(vname(AnchorSignature,Corpus,Root,Path,Language),
                             vname(S,              Corpus,Root,Path,Language)),
          Signatures).

% anchor_link_anchor always succeeds at least once with anchor_link_anchor(A, A).
anchor_link_anchor(AnchorVname1, AnchorVname2) :-
    anchor_link_anchor(AnchorVname1, _Edge1, _SemanticVname, _Edg2, AnchorVname2).

anchor_to_semantics(Corpus,Root,Path,Language, ColorToken, AnchorSignature-Signatures) :-
    var_token(ColorToken.token_color),
    AnchorSignature = ColorToken.signature,
    % TODO: vname(S,Corpus,Root,'',Language2) - might be a different language
    setof(S,
          anchor_link_semantic_name(vname(AnchorSignature,Corpus,Root,Path,Language), S),
          Signatures).

semantic_to_anchors_same_file(Corpus,Root,Path,Language, ColorAll0, SemanticAnchorsDict) :-
    convlist(convlist(anchor_token_semantics(Corpus,Root,Path,Language)), ColorAll0, Semantics0),
    append(Semantics0, Semantics1),
    append(Semantics1, Semantics2),
    sort(Semantics2, Semantics),
    maplist(semantic_anchor(Corpus,Root,Path,Language), Semantics, SemanticAnchorsPairs),
    dict_pairs(SemanticAnchorsDict, json, SemanticAnchorsPairs).

anchor_token_semantics(Corpus,Root,Path,Language, ColorToken, SemanticNames) :-
    var_token(ColorToken.token_color),
    AnchorSignature = ColorToken.signature,
    setof(S, anchor_link_semantic(vname(AnchorSignature,Corpus,Root,Path,Language), S),
          SemanticNames).

anchor_link_semantic_name(AnchorVname, SemanticName) :-
    anchor_link_semantic(AnchorVname, _Edge, vname(S, Corpus,Root,'',Language)),
    atomic_list_concat([Corpus,Root,Language,S], '</>', SemanticName).

semantic_anchor(Corpus,Root,Path,Language, SemanticVname, SemanticName-Anchors) :-
    must_once(SemanticVname = vname(SemanticSignature, Corpus,Root,_,Language)), % TODO: Path vs '' ?
    atomic_list_concat([Corpus,Root,Language,SemanticSignature], '</>', SemanticName),
    setof_or_empty(A, anchor_link_semantic(vname(A, Corpus,Root,Path,Language), SemanticVname),
                   Anchors).

anchor_link_semantic(AnchorVname, SemanticVname) :-
    anchor_link_semantic(AnchorVname, _Edge, SemanticVname).

% See pykythe:var_token/1.

var_token('<ARG_KEYWORD>').
var_token('<ATTR_BINDING>').
var_token('<ATTR_REF>').
var_token('<BARE>').
% var_token('<PUNCTUATION>').   % if exists semantic => '<PUNCTUATION_REF>'
var_token('<PUNCTUATION_REF>'). % This is generated add_color_edges/6
var_token('<VAR_BINDING>').
var_token('<VAR_BINDING_GLOBAL>').
var_token('<VAR_REF>').

%! anchor_to_line_chunks(+AnchorVname:vname, +LineNo:int, -Chunks:list(dict)) is semidet.
% Given an AnchorVname, get all the color chunks (in order) for the
% line that anchor is in. Can fail if the anchor is invalid (and if
% there isn't a color anchor that matches the token anchor).
% TODO: refactor with color_data_one_file
anchor_to_line_chunks(AnchorVname, LineNo, Chunks) :-
    anchor_to_lineno(AnchorVname, LineNo),
    vname_vname0(AnchorVname, Vname0),
    keyed_color_chunks(Vname0, LineNo, Chunks).

anchor_to_lineno(AnchorVname, LineNo) :-
    % Some tokens don't have a signature in the color data.
    % Fix kythe_color_all/5 facts to contain signatures for bare tokens.
    AnchorVname = vname(_Signature,Corpus,Root,Path,Language),
    must_once(kythe_node(AnchorVname, '/kythe/loc/start', Start)),
    (   kythe_color_line(Corpus, Root, Path, Language, LineNo, LineChunks),
        member(Chunk, LineChunks),
        Chunk.start = Start
    ->  true
    ;   LineNo = 1,
        debug(log, 'No color ( anchor_to_lineno) for ~q', [AnchorVname])
    ).

keyed_color_chunks(Vname0, LineNo, Chunks) :-
    must_be(integer, LineNo),
    % TODO: This can be simplified DO NOT SUBMIT
    must_once(Vname0 = vname0(Corpus,Root,Path,Language)),
    (   kythe_color_line(Corpus, Root, Path, Language, LineNo, Chunks)
    *-> true
    ;   % TODO: this code doesn't get executed? - FIXME(34)
        vname_vname0(AnchorVname, Signature, Vname0),
        kythe_node(AnchorVname, '/kythe/loc/start', Start),
        kythe_node(AnchorVname, '/kythe/loc/end', End),
        Column = 0,
        TokenColor = '<BARE>',  % DO NOT SUBMIT
        Value = 'xxxxxx', % DO NOT SUBMIT
        debug(log, 'No color (keyed_color_chunk) for ~q ~q:(~q:~q)', [AnchorVname, LineNo, Start, End]),
        Chunks = [color{start:Start, end:End,
                        lineno:LineNo, column:Column,
                        signature:Signature,
                        token_color:TokenColor, value:Value}]
    ).

vname_neg_num_edges(Vname, MinNumEdges, NegNumEdges) :-
    setof(Edge-Vname2, kythe_edge(Vname, Edge, Vname2), Edges),
    length(Edges, NumEdges),
    NumEdges >= MinNumEdges,
    NegNumEdges is -NumEdges.

%! most_edges is det.
% Calculate the most edges for some common nodes
%  e.g., int:4129, str:2968, None:1781, bytes:1204, ...
most_edges :-
    aggregate(set(N-V), vname_neg_num_edges(V, 2, N), L),
    aggregate(count, V^N^vname_neg_num_edges(V, 2, N), Len),
    aggregate(count, V^N^vname_neg_num_edges(V, 0, N), LenAll),
    format('# vnames: ~w (~w including 1-edge)~n', [Len, LenAll]),
    pykythe_utils:at_most(L, 20, L0),
    forall(member(A, L0), format('~q~n', [A])).

:- meta_predicate setof_or_empty(?, ^, ?).
setof_or_empty(Template, Goal, Set) :-
    (   setof(Template, Goal, Set)
    *-> true
    ;   Set = []
    ).


% Add a timestamp to thread messages (which are typically from the http server).
:- multifile prolog:message_prefix_hook/2.
prolog:message_prefix_hook(thread, Prefix) :-
    thread_self(Me),
    % Me \== main,
    thread_property(Me, id(Id)),
    get_time(NowTimeStamp),
    % %FT adds 2020-02-23T; % %z adds '-0800'
    format_time(string(NowTime), '%T', NowTimeStamp),
    format(atom(Prefix), '[Thread ~w ~w] ', [Id, NowTime]).


%! concurrent_count(+Max, -Cores, :Goal) is nondet.
% A utility for controlling the number of concurrent threads.
% Max can be an expression allowed  on the r.h.s. of is/2;
%   it can include the number of cores by using the Cores logical variable, e.g.:
%     concurrent_count(2 * Cores, Cores, my_goal(...)).
?- meta_predicate concurrent_count(+, ?, 0).
concurrent_count(Max, Cores, Goal) :-
    current_prolog_flag(cpu_count, Cores),
    MaxValue is Max,
    setup_call_cleanup(set_prolog_flag(cpu_count, MaxValue),
                       Goal,
                       set_prolog_flag(cpu_count, Cores)).


%%%%%%%%%%%%%%%%%%%%%%%%%%

% Transform a list of file names to a tree for browser.

files_to_tree(Files, Tree) :-
    maplist(split_on_slash, Files, SplitFiles),
    list_to_tree(SplitFiles, [], Tree).

split_on_slash(Corpus:Root:Str, Split) :-
    split_string(Str, '/', '', SplitStr),
    maplist(string_atom, SplitStr, Split0),
    once(append(First, [Last], Split0)), % like last/2 but also gets first part.
    % wrap all but last in dir(_), last in file(_)
    maplist(wrap_dir, [Corpus,Root|First], FirstWrapped),
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

subtree_(dir(Dir), Sublist, Prefix, dir(Dir,Children,Corpus,Root,Path)) :-
    append(Prefix, [Dir], Prefix2), % extract Dir as last element
    corpus_root_path_to_str(Prefix2, Corpus, Root, Path),
    list_to_tree(Sublist, Prefix2, Children).
subtree_(file(File), [[]], Prefix, file(File,Corpus,Root,Path)) :-
    append(Prefix, [File], Prefix2), % extract File as last element
    corpus_root_path_to_str(Prefix2, Corpus, Root, Path).

head_tail_pair([Hd|Tl], Hd-Tl).

corpus_root_path_to_str([Corpus], Corpus, '', '') :- !.
corpus_root_path_to_str([Corpus,Root], Corpus, Root, '') :- !.
corpus_root_path_to_str([Corpus,Root|Path], Corpus, Root, PathStr) :-
    atomic_list_concat(Path, '/', PathStr).

%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- use_module(library(check)).  % DO NOT SUBMIT
% % TODO: trap print_message(informational,check(pass(Message)))
% ?- check.                       % DO NOT SUBMIT

:- if(false). % TODO: redo the unit test for the new file/dir structures

:- use_module(library(plunit)).

% TODO: set_test_options([load(never)])
%       so that these tests aren't in the normal runtime.

:- begin_tests(file_tree).

t1(Ftree) :-
    F = ['CORPUS':'ROOT':'x',
         'CORPUS':'ROOT':'a/b/x1',
         'CORPUS':'ROOT':'a/b/x2',
         'CORPUS':'ROOT':'a/c',
         'CORPUS':'ROOT':'a/d/e/x3'
        ],
    files_to_tree(F, Ftree),
    tree_to_json(Ftree, FtreeJson),
    with_output_to(atom(JsonAtom),
                   (current_output(JsonStream),
                    pykythe_json_write_dict_nl(JsonStream, FtreeJson))),
    % format('~w~n', [JsonAtom]),
    assertion(JsonAtom == '[ {"children": [ {"children": [ {"children": [ {"children": [ {"name":"x1", "path":"CORPUS/ROOT/a/b/x1", "type":"file"},  {"name":"x2", "path":"CORPUS/ROOT/a/b/x2", "type":"file"} ], "name":"b", "path":"CORPUS/ROOT/a/b", "type":"dir"},  {"children": [ {"children": [ {"name":"x3", "path":"CORPUS/ROOT/a/d/e/x3", "type":"file"} ], "name":"e", "path":"CORPUS/ROOT/a/d/e", "type":"dir"} ], "name":"d", "path":"CORPUS/ROOT/a/d", "type":"dir"},  {"name":"c", "path":"CORPUS/ROOT/a/c", "type":"file"} ], "name":"a", "path":"CORPUS/ROOT/a", "type":"dir"},  {"name":"x", "path":"CORPUS/ROOT/x", "type":"file"} ], "name":"ROOT", "path":"CORPUS/ROOT", "type":"dir"} ], "name":"CORPUS", "path":"CORPUS", "type":"dir"} ]\n').

test(f1, [true]) :-
    t1(Ftree),
    assertion(
              [dir('CORPUS','CORPUS',
                   [dir('ROOT','CORPUS/ROOT',
                        [dir(a,'CORPUS/ROOT/a',
                             [dir(b,'CORPUS/ROOT/a/b',
                                  [file(x1,'CORPUS/ROOT/a/b/x1'),
                                   file(x2,'CORPUS/ROOT/a/b/x2')]),
                              dir(d,'CORPUS/ROOT/a/d',
                                  [dir(e,'CORPUS/ROOT/a/d/e',
                                       [file(x3,'CORPUS/ROOT/a/d/e/x3')])]),
                              file(c,'CORPUS/ROOT/a/c')]),
                         file(x,'CORPUS/ROOT/x')])])]
             == Ftree).

:- end_tests(file_tree).

:- endif.

-end_of_file.
