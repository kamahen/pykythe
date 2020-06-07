%% Use logtalk to do some lint-ing
%% See https://swi-prolog.discourse.group/t/linter/1160/11

pack_install(logtalk, [interactive(false), upgrade(true)]).
use_module(library(logtalk)).
set_logtalk_flag(optimize, on).
cd(pykythe).
% [pykythe].
{pykythe}.
{os(loader), hook_flows(loader)}.
set_logtalk_flag(hook, hook_set([user,system])).
% set_logtalk_flag(duplicated_clauses,warning),
% set_logtalk_flag(unknown_predicates,silent),
% set_logtalk_flag(undefined_predicates,silent),
% set_logtalk_flag(unknown_entities,silent),
% set_logtalk_flag(missing_directives,silent).
os::directory_files('.',
                    Files,
                    [type(regular),
                     extensions(['.pl']),
                     paths(relative)]),
forall(member(File,Files), ignore(logtalk_load(File))).

{dead_code_scanner(loader)}.

writeln('***DEAD CODE***').

dead_code_scanner::entity(pykythe).
dead_code_scanner::file('pykythe.pl').
dead_code_scanner::directory('.').
