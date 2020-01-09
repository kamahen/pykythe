                                % -*- mode: Prolog -*-

%% Start this with
%%   http://localhost:9999/static/src_browser.html

%% See also:
%%    http://www.pathwayslms.com/swipltuts/html/index.html
%%    https://swi-prolog.discourse.group/t/yet-another-web-applications-tutorial/566
%%    https://www.swi-prolog.org/howto/http/

:- use_module(library(http/http_server), [http_server/1,
                                          reply_html_page/2,
                                          http_read_json_dict/2,
                                          http_read_json_dict/3,
                                          reply_json_dict/1,
                                          reply_json_dict/2 % TODO: Options=[status(201)]
                                          % http_redirect/3 % TODO: commented out below
                                          ]).
:- use_module(library(http/http_files), [http_reply_from_files/3]).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_error)). % TODO: remove - this decorates uncaught HTTP exceptions with stack-trace
:- use_module(library(debug)).
:- use_module(library(optparse), [opt_arguments/3]).
:- use_module('../pykythe/must_once.pl').
:- use_module('../pykythe/pykythe_utils.pl').


:- debug(log).    % enable log messages with debug(log, '...', [...]).
%% :- debug(http_path).            % TODO: remove
:- debug(http(request)).        % TODO: remove
:- debug(http(hook)).           % TODO: remove
:- debug(http_session).         % TODO: remove
:- debug(http(error)).          % TODO: remove
:- debug(http(header)).         % TODO: remove
:- debug(http(send_request)).   % TODO: remove
:- debug(websocket).            % TODO: remove
:- debug(websocket(open)).      % TODO: remove
:- debug(websocket(close)).     % TODO: remove

:- debug.                       % TODO: remove

:- initialization main.

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

main :-
    browser_opts(Opts),
    %% set_prolog_flag(verbose_file_search, true),
    assert_server_locations(Opts),
    server(Opts).

assert_server_locations(Opts) :-
    debug(log, 'files dir:  ~q', [Opts.filesdir]),
    debug(log, 'static dir: ~q', [Opts.staticdir]),
    asserta(user:file_search_path(files,  Opts.filesdir)),
    asserta(user:file_search_path(static, Opts.staticdir)).

server(Opts) :-
    http_server([port(Opts.port), workers(5)]).

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


%% localhost:9999/ ... redirects to /home
%%      - for debugging, 'moved' can be cleared by chrome://settings/clearBrowserData
%%        (Cached images and files)
%% :- http_handler(root(.),
%%                 http_redirect(
%%                     moved, % or moved_temporary
%%                     location_by_id(home_page)), % a term, not http_dispatch:location_by_id/5
%%                 []).

:- http_handler(root(.),        % localhost:9999/
                home_page, []).

% Serve localhost:9999/static/ from 'static' directory (See also facts for http:location/3)
:- http_handler(static(.),
                my_http_reply_from_files(static(.), []),
                [prefix]).
:- http_handler(files(.),
                my_http_reply_from_files(files(.), []),
                [prefix]).

:- http_handler(root(json),     % localhost:9999/json
                json, []).

my_http_reply_from_files(Dir, Opts, Request0) :-
    opts_dict(Request0, request, Request),
    %% debug(log, '~q', [my_http_reply_from_files(Dir, Request)]),
    debug(log, 'Request (~q): ~q', [Dir, [method:Request.method,
                                          path_info:Request.path_info]]),
    %% This is a hack because I can't seem to get static/files to work
    %% otherwise, and JS import('files/FILES.js') ends up as
    %% static/files/FILES.js ... No doubt there are more elgant ways of
    %% doing this, such as locate-relative-to but eventually I'll be
    %% getting rid of this anyway, and using AJAX.
    (  Dir = static(.),
       split_string(Request.path_info, '/', '', SplitList),
       SplitList = ["files", FileStr]
    -> atom_string(FileAtom, FileStr),
       select(path_info(_), Request0, Request0a),
       http_reply_from_files(files(.), Opts, [path_info(FileAtom)|Request0a])
    ;  http_reply_from_files(Dir, Opts, Request0)
    ).

home_page(Request0) :-
    opts_dict(Request0, request, Request),
    debug(log, 'Request(home_page):~n~q', [Request]),
    reply_html_page(
        title('Demo server'),
        [ h1('Hello world!')
        ]).

json(Request) :-
    print_term_cleaned(Request, [], RequestPretty),
    debug(log, 'Request(handle)0: ~s', [RequestPretty]),
    http_read_json_dict(Request, DictIn, [default_tag(json)]), %% [content_type("application/json")]),
    debug(log, 'Request(handle): ~q', [DictIn]),
    json_compute(DictIn, DictOut),
    debug(log, 'Request(handle): ~q => ~q', [DictIn, DictOut]),
    reply_json_dict(DictOut).

json_compute(json{userName:UserName}, json_result{computedString:ReversedUserName}) :-
    string_chars(UserName, UserNameChars),
    reverse(UserNameChars, UserNameCharsReversed),
    string_chars(ReversedUserName, UserNameCharsReversed).
