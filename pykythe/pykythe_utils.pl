% -*- mode: Prolog -*-

%% Utilities for pykythe.


:- module(pykythe_utils, [
                          absolute_dir/2,
                          absolute_file_name_rel/2,
                          absolute_file_name_rel/3,
                          base64_utf8/2,
                          convdict/3,
                          convdict_pairs/3,
                          dict_values/2,
                          do_if/2,
                          dump_term/2,
                          dump_term/3,
                          ensure_dict_fact/3,
                          ensure_dict_fact_base64_ascii/3,
                          ensure_dict_fact_base64_utf8/3,
                          get_dict_default/4,
                          hash_hex/2,
                          has_prefix/2,
                          has_suffix/2,
                          json_read_dict_validate/3,
                          json_write_dict_nl/2,
                          log_if/2,
                          log_if/3,
                          maybe_open_read/2,
                          my_json_read_dict/2,
                          opt/2,
                          opts/2,
                          print_term_cleaned/3,
                          remove_suffix_star/3,
                          remove_prefix/3,
                          remove_suffix/3,
                          remove_suffix_star/3,
                          safe_delete_file/1,
                          split_atom/4,
                          split_path_string_and_canonicalize/3,
                          term_to_canonical_atom/2,
                          %% update_dict/3,
                          update_new_dict/3,
                          write_atomic_file/2,
                          write_atomic_stream/2
                         ]).

:- meta_predicate
       convdict(2, +, -),
       convdict_pairs(2, +, -),
       do_if(0, 0),
       log_if(0, +),
       log_if(0, +, +),
       write_atomic_stream(1, +),
       write_atomic_file(1, +).

:- style_check(+singleton).
:- style_check(+var_branches).
:- style_check(+no_effect).
:- style_check(+discontiguous).

:- style_check(-var_branches).  % For library(http/json)
:- use_module(library(apply), [exclude/3, include/3, maplist/2, maplist/3, maplist/4, foldl/4, convlist/3]).
:- use_module(library(base64), [base64/2 as base64_ascii]).
:- use_module(library(utf8), [utf8_codes//1]).
:- use_module(library(filesex), [make_directory_path/1, directory_file_path/3]).
:- use_module(library(http/json), [json_read_dict/3, json_write_dict/3]).
:- use_module(library(lists), [append/2, append/3, list_to_set/2, member/2, reverse/2, select/3]).
:- use_module(library(pairs), [pairs_values/2]).
:- use_module(library(pcre), [re_replace/4]).
:- use_module(library(pprint), [print_term/2]).
:- use_module(library(rbtrees), [ord_list_to_rbtree/2, rb_insert/4, rb_visit/2] ).
:- use_module(library(rdet), [rdet/1]).
:- use_module(library(sha), [sha_hash/3, hash_atom/2]).
:- use_module(library(yall)).
:- use_module(must_once, [must_once/1, must_once_msg/2, must_once_msg/3, fail/1]).

:- style_check(+singleton).
:- style_check(+var_branches).
:- style_check(+no_effect).
:- style_check(+discontiguous).
%% :- set_prolog_flag(generate_debug_info, false).


:- if(true).  % Turning off rdet can sometimes make debugging easier.

:- maplist(rdet, [
                  %% base64_string/2, % handled by must_once
                  %% convdict/3, % rdet wrap interferes with meta_predicate declaration
                  %% do_if/2,    % rdet wrap interferes with meta_predicate declaration
                  %% log_if/2,   % rdet wrap interferes with meta_predicate declaration
                  %% log_if/3,   % rdet wrap interferes with meta_predicate declaration
                  hash_hex/2,
                  json_read_dict_validate/3,
                  opt/2,
                  opts/2,
                  print_term_cleaned/3,
                  remove_suffix_star/3,
                  split_atom/4,
                  split_path_string_and_canonicalize/3,
                  term_to_canonical_atom/2
                  %% write_atomic_stream/2, % rdet wrap interferes with meta_predicate declaration
                  %% write_atomic_file/2    % rdet wrap interferes with meta_predicate declaration
                 ]).

:- endif.

%! absolute_file_name_rel(+File, -Absolute) is det.
%% For now, this is the same as absolute_file_name/2.
%% However, it is intended that this should look up the corpus root
%% and remove the prefix (e.g., "/").
%% TODO: Kythe prefers to have paths without leading "/"; the code
%%       should use (corpus,root,path) for file names and create a
%%       filesystem name from a lookup of (corpus,root) to filesystem prefix.
%% TODO: https://github.com/kamahen/pykythe/issues/24
absolute_file_name_rel(File, Absolute) :-
    absolute_file_name(File, Absolute0),
    Absolute = Absolute0.  % atom_concat('/', Absolute, Absolute0).
absolute_file_name_rel(File, Absolute, Options) :-
    absolute_file_name(File, Absolute0, Options),
    Absolute = Absolute0.  % atom_concat('/', Absolute, Absolute0).

%! absolute_dir(+Path0:atom, -AbsPath:atom) is det.
%%  Apply absolute_file_name_rel to Path0, giving AbsPath, ensuring it's a
%%  directory and appending '/' to the name.
absolute_dir(/, /) :- !. % Special case for root dir, which otherwise would become '//'
absolute_dir(Path0, AbsPath) :-
    remove_suffix_star(Path0, '/', Path),
    absolute_file_name_rel(Path, AbsPath0, [access(read), file_type(directory), file_errors(fail)]),
    atomic_list_concat([AbsPath0, '/'], AbsPath).

convdict(Pred, Dict0, Dict) :-
    dict_pairs(Dict0, Tag, Pairs0),
    convlist(Pred, Pairs0, Pairs),
    dict_pairs(Dict, Tag, Pairs).

convdict_pairs(Pred, Dict0, Pairs) :-
    dict_pairs(Dict0, _Tag, Pairs0),
    convlist(Pred, Pairs0, Pairs).

%! dict_values(+Dict, -Values) is det.
%%    True when Values is an ordered set of the values appearing in Dict.
%% TODO: this should be in library(dicts).
%% TODO: this isn't used?
dict_values(Dict, Values) :-
    dict_pairs(Dict, _Tag, Pairs),
    pairs_values(Pairs, Values).

%! do_if(:Cond, :Pred) is det.
%% A handy meta-predicate for turning debug stuff on/off, according to Cond
do_if(Cond, Pred) :-
    (  call(Cond)
    -> call(Pred)
    ;  true
    ).

%! dump_term(+Msg:atom, +Term) is det.
%% TODO: delete this debugging code
dump_term(Msg, Term) :-
    dump_term(Msg, Term, [tab_width(0),
                          indent_arguments(2),
                          right_margin(120)]).

%! dump_term(+Msg:atom, +Term, +Options:list) is det.
%% TODO: use debug/3, etc. instead (also print_message/2).
%% TODO: Delete this debugging code
dump_term(Msg, Term, Options) :-
    must_once(dump_term_impl(Msg, Term, Options)).

%! dump_term_impl(+Msg:atom, +Term, +Options:list) is det.
dump_term_impl(Msg, Term, Options) :-
    (  Msg = ''
    -> true
    ;  log_if(true, '% === ~w ===~n', [Msg])
    ),
    print_term_cleaned(Term, Options, TermStr),
    (  Msg = ''
    -> log_if(true, '~s.', [TermStr])
    ;  log_if(true, '~s.~n', [TermStr]),
       log_if(true, '% === end ~w ===~n', [Msg])
    ).

%! ensure_dict_fact(+Dict, +Attr, ?Value) is semidet.
%% Die with an error message if Dict.Attr != Value
%% (Can also be used to get Dict.Attr into Value).
ensure_dict_fact(Dict, Attr, Value) :-
    must_once_msg(get_dict(Attr, Dict, Value),
                  'Invalid JSON, expecting ~q=~q in ~q',
                  [Attr, Value, Dict]).

%! ensure_dict_fact_base64_ascii(+Dict, +Attr, ?Value) is semidet.
%% Die with an error message if base64_ascii(Dict.Attr) != Value
%% (Can also be used to get Dict.Attr into Value).
ensure_dict_fact_base64_ascii(Dict, Attr, Value) :-
    must_once(get_dict(Attr, Dict, Value64)),
    must_once_msg(base64_ascii(Value, Value64),
                  'Invalid JSON, expecting base64 ~q=~q in ~q',
                  [Attr, Value, Dict]).

%! ensure_dict_fact_base64_utf8(+Dict, +Attr, ?Value) is semidet.
%% Die with an error message if base64_utf8(Dict.Attr) != Value
%% (Can also be used to get Dict.Attr into Value).
ensure_dict_fact_base64_utf8(Dict, Attr, Value) :-
    must_once(get_dict(Attr, Dict, Value64)),
    must_once_msg(base64_utf8(Value, Value64),
                  'Invalid JSON, expecting base64 ~q=~q in ~q',
                  [Attr, Value, Dict]).

get_dict_default(Key, Dict, Default, Value) :-
    (  get_dict(Key, Dict, Value)
    -> true
    ;  Value = Default
    ).

%! hash_hex(+Text, -Hex) is det.
%% ?- hash_hex('SWI-Prolog', Hex).
%% Hex = '3d80fc267945e555c730403bd0ab0716e2a68c68'.
%% Can take either a string or an atom for 1st arg.
%% Defaults to using SHA-1, which is what git uses.
%% However, SHA-224 or SHA-384 ought to be used:
%%          https://www.schneier.com/blog/archives/2018/12/md5_and_sha-1_s.html
%% (SHA-1 is 20 digits (40 chars),  SHA-224 is 28 digits, SHA-384 is 48 digits.
%% TODO: incorporate the encoding in this? (Note that
%%       Python's hashlib.sha1 requires bytes, not text.)
hash_hex(Text, Hex) :-
    sha_hash(Text, Hash, []),   % Default option is algorithm(sha1)
    hash_atom(Hash, Hex).

%! json_read_dict_validate(+KytheInputStream, +FactName, -Dict) is det.
%% Read a JSON "term" from KytheInputStream, verify that fact_name
%% is FactName and unify the entire term with Dict)
json_read_dict_validate(KytheInputStream, FactName, Dict) :-
    my_json_read_dict(KytheInputStream, Dict),
    ensure_dict_fact(Dict, fact_name, FactName).

%! json_write_dict_nl(+KytheStream:stream, +JsonAsDict:json_dict) is det.
%% Output a single Kythe fact.
json_write_dict_nl(KytheStream, JsonAsDict) :-
    %% The tags are ignored unless option tag(type) is specified
    %% (which it isn't). All dicts should have the tag 'json', for
    %% simplicity.
    json_write_dict(KytheStream, JsonAsDict, [width(0)]),
    nl(KytheStream).

log_if(Cond, Fmt) :- log_if(Cond, Fmt, []).

%! log_if(Cond:atom, Fmt:atom, Args:list) is det.
log_if(Cond, Fmt, Args) :-
    (  call(Cond)
    -> atomic_list_concat(['~` t~3f~8|: ', Fmt, '~n'], '', Fmt2),
       statistics(process_cputime, Time),
       format(user_error, Fmt2, [Time|Args])
    ;  true
    ).

%! maybe_open_read(+Path, -InputStream) is semidet.
%% Open Path for read or fail.
maybe_open_read(Path, InputStream) :-
    %% Wrapper for debugging
    (  maybe_open_read_impl(Path, InputStream)
    -> log_if(false, 'OPENed ~q', [Path])
    ;  log_if(false, 'OPEN-failed ~q', [Path]),
       fail
    ).

maybe_open_read_impl(Path, InputStream) :-
    catch(open(Path, read, InputStream),
          error(existence_error(source_sink, Path), _),
          fail).

%! my_json_read_dict(+Stream, -Dict) is det.
%% Wrapper on library(http/json, [json_read_dict/2]) that works
%% if autoload is turned off.
%% Also sets the dict tags to 'json' (json_read_dict/2 leaves the tag
%% as an uninstantiated variable).
%% And gets strings as atoms.
my_json_read_dict(Stream, Dict) :-
    %% TODO: fix library(http/json): it should have :- use_module(library(lists)).
    %%       and remove autoload below.
    current_prolog_flag(autoload, AutoloadFlag),
    set_prolog_flag(autoload, true), % TODO: Otherwise gets error: json:term_to_dict/3 - undefined select/3
    json_read_dict(Stream, Dict, [value_string_as(atom), end_of_file(@(end)), default_tag(json)]),
    set_prolog_flag(autoload, AutoloadFlag).

%! opts(Opts:list, Items:list) is det.
opts(Opts, Items) :- maplist(opt(Opts), Items).

%! opt(+Opts:list, Item) is det.
%% Allows maplist(opt(Opts), [option1(X), option2(Y)]) instead of
%%     maplist({Opts}/[X]>>memberchk(X, Opts), [option1(X), option2(Y)]).
opt(Opts, Item) :- memberchk(Item, Opts).

%! print_term_cleaned(+Term, +Options, -TermStr) is det.
%% print_term, cleaned up
print_term_cleaned(Term, Options, TermStr) :-
    %% print_term leaves trailing whitespace, so remove it
    with_output_to(
            string(TermStr0),
            (current_output(TermStream),
             print_term(Term, [output(TermStream)|Options]))),
    re_replace(" *\n"/g, "\n", TermStr0, TermStr).

%! remove_suffix_star(+Full:atom, +Suffix:atom, -NoSuffix:atom) is det.
%% Repeatedly removes suffix if present.
remove_suffix_star(Full, Suffix, NoSuffix) :-
    (  remove_suffix(Full, Suffix, NoSuffix0)
    -> remove_suffix_star(NoSuffix0, Suffix, NoSuffix)
    ;  NoSuffix = Full
    ).

%! safe_delete_file(?Path) is det.
%% delete file, catching any errors.
safe_delete_file(Path) :-
    %% The most common error is: error(existence_error(file, Path), _)
    %% but other errors are possible, such as
    %% error(instantiation_error, _).
    catch(delete_file(Path), _Error, true).

%! split_atom(+Atom:atom, +SepChars:atom, +PadChars:atom, -SubAtoms:list(atom)) is det.
%% Like split_string, but result is a list of atoms.
split_atom(Atom, SepChars, PadChars, SubAtoms) :-
    split_string(Atom, SepChars, PadChars, SubStrings),
    maplist([S,A]>>atom_string(A,S), SubStrings, SubAtoms).

%! split_path_string_and_canonicalize(+OptName:atom, +Opts0:list, -Opts:list) is det.
%%  Find the option given by OptName in Opts0, split the value into
%%  components in a list, add back into Opts (can be in a different
%%  position in the list).  The resulting list of dirs are all in
%%  canonical form, using absolute_dir/2.
split_path_string_and_canonicalize(OptName, Opts0, [NewOpt|Opts1]) :-
    Opt =.. [OptName, PathStr],
    select(Opt, Opts0, Opts1),
    split_atom(PathStr, ':', '', PathList0),
    convlist(maybe_absolute_dir, PathList0, PathList),
    NewOpt =.. [OptName, PathList].

maybe_absolute_dir(Path0, AbsPath) :-
    (  absolute_dir(Path0, AbsPath)
    -> true
    ;  log_if(true, 'WARNING: no such directory: ~q', [Path0])
    ).

%! term_canonical_atom(+Term, -Atom) is det.
%% Like term_to_atom/2 if Term is instantiated, but generates
%% an atom in canonical form (no operators).
term_to_canonical_atom(Term, Atom) :-
    format(atom(Atom), '~k', [Term]).

%% (NOT CURRENTLY USED)
%% %! update_dict(+KVs:list(pair), -Dict0:dict, +Dict:dict) is det.
%% %% Add/update all key-value pairs in KVs into the dict.
%% %% The keys are processed in order; the last one takes effect.
%% %% Note that dict_pairs(Dict, Tag, KVs) doesn't allow duplicate keys.
%% update_dict([], Dict, Dict).
%% update_dict([K-V|KVs], Dict0, Dict) :-
%%     (  get_dict(K, Dict0, V)  % prevent creating so much garbage
%%     -> Dict1 = Dict0
%%     ;  put_dict(K, Dict0, V, Dict1)
%%     ),
%%     update_dict(KVs, Dict1, Dict).

%! update_new_dict(+KVs:list(pair), -Dict0:dict, +Dict:dict) is det.
%% Add all key-value pairs in KVs into the dict, skipping any that
%% are already in the dict.
%% The keys are processed in order, so if a key is added, any subsequent
%% values of that key are ignored.
%% Note that dict_pairs(Dict, Tag, KVs) doesn't allow duplicate keys.

%% This is the straightforward implementation, but it creates
%% a lot of garbage and is slow.

%% update_new_dict([], Dict, Dict).
%% update_new_dict([K-V|KVs], Dict0, Dict) :-
%%     (  get_dict(K, Dict0, _)
%%     -> Dict1 = Dict0
%%     ;  put_dict(K, Dict0, V, Dict1)
%%     ),
%%     update_new_dict(KVs, Dict1, Dict).

%% The following is several times faster than the above.  (For 11,000
%% items, make_rb is about 20x faster than the equivalent loop for a
%% dict, but there's still significant cost converting to/from rbtree).
%% TODO: It would be better to just leave everything as a RB-tree.

update_new_dict(KVs, Dict0, Dict) :-
    dict_pairs(Dict0, Tag, KVs0),
    ord_list_to_rbtree(KVs0, Rb0),
    make_rb(KVs, Rb0, Rb1),
    rb_visit(Rb1, KVs1),
    dict_pairs(Dict, Tag, KVs1).

make_rb([], Rb, Rb).
make_rb([K-V|KVs], Rb0, Rb) :-
    rb_insert(Rb0, K, V, Rb1),
    make_rb(KVs, Rb1, Rb).

%! write_atomic_stream(:WritePred, +Path:atom) is semidet.
%% Write to a file "atomically" -- that is, if another process is
%% trying to write to the same file, there will be no collision (it is
%% undetermined which process will "win"; presumably they both are
%% trying to write the same content). If `WritePred` fails, the file
%% isn't created (even if `WritePred` fails and write_atomic/2 fails.
%% If needed, directories to Path are created.
%% WritePred must take the stream as its last argument.
write_atomic_stream(WritePred, Path) :-
    %% TODO: See '$stage_file' in /usr/lib/swi-prolog/boot/init.pl
    %%       and setup_call_catcher_cleanup
    %% TODO: the tmpfile/rename trick doesn't work if the tmp file is
    %% on a different file system. Is there a way of detecting this?
    %% Might need to pass in an opt ... but the ideal would be to
    %% create the tmp file with a "unique" suffix.  SWI-Prolog uses
    %% this to create a unique name:
    %%    Ssnprintf(temp, sizeof(temp), "%s/swipl_%s%s%d_%d%s%s",
    %%        tmpdir, id, sep, (int) getpid(),
    %%        MTOK_temp_counter++,
    %%        esep, ext)
    %% Note: current_prolog_flag(pid, Pid).
    %%       gensym(+Base, -Unique)
    %% so ... current_prolog_flag(pid, Pid),
    %%        format(atom(TmpFileName0), '/foo/bar-~d-', [Pid]),
    %%        gensym(TmpFileName0, TmpFileName).
    %% cf: Python's tempfile.mkstemp()
    %% Also: tmp_file and friends put entries into GD->os.tmp_files
    %% (see swipl-devel/src/os/pl-os.c), and that doesn't happen if
    %% we roll our own.
    directory_file_path(PathDir, _, Path),
    make_directory_path(PathDir),
    tmp_file_stream(TmpPath, Stream, [encoding(utf8)]),
    %% TODO: instead of at_halt/1, use setup_call_cleanup/3
    at_halt(pykythe_utils:safe_delete_file(TmpPath)), % in case WritePred crashes or fails
    (  call(WritePred, Stream)
    -> close(Stream),
       %% atomically rename file -- this prevents a race condition if
       %% two pykythe processes are processing the same file at the
       %% same time.
       rename_file(TmpPath, Path)
    ;  close(Stream),
       pykythe_utils:safe_delete_file(TmpPath),
       fail
    ).

%! write_atomic_file(+WritePred, +Path) is semidet.
%% Similar to write_atomic_stream, except it passes a path to Pred
%% instead of a stream.
%% WritePred must take the stream as its last argument.
write_atomic_file(WritePred, Path) :-
    directory_file_path(PathDir, _, Path),
    make_directory_path(PathDir),
    tmp_file_stream(TmpPath, Stream, [encoding(utf8)]),
    %% TODO: instead of setting up at_halt, use setup_call_cleanup/3
    at_halt(pykythe_utils:safe_delete_file(TmpPath)), % in case WritePred crashes or fails
    close(Stream),
    (  call(WritePred, TmpPath)
    -> rename_file(TmpPath, Path)
    ;  pykythe_utils:safe_delete_file(TmpPath),
       fail
    ).

%! remove_suffix(+Atom, +Suffix, -FirstPart) is semidet.
remove_suffix(Atom, Suffix, FirstPart) :-
    %% TODO: compare with:
    %%       sub_atom(Atom, Before, _, 0, Suffix),
    %%       Before0 is Before - 1,
    %%       sub_atom(Atom, 0, Before0, _, FirstPart)
    atom_concat(FirstPart, Suffix, Atom).

%! remove_prefix(+Atom, +Prefix, -SecondPart) is semidet.
remove_prefix(Atom, Prefix, SecondPart) :-
    sub_atom(Atom, 0, Len, After, Prefix),
    sub_atom(Atom, Len, After, 0, SecondPart).

has_suffix(Atom, Suffix) :-
    sub_atom(Atom, _, _, 0, Suffix).

has_prefix(Atom, Prefix) :-
    sub_atom(Atom, 0, _, _, Prefix).

%% Tests for atom_utf8_bytes are in pykythe.pl

%! base64_utf8(+Atom, -BytesBase64) is det.
%! base64_utf8(+Atom, -BytesBase64) is det.
%% in Python: BytesBase64 = base64.b64encode(Atom.encode('utf8'))
%%            Atom = base64.b64decode(BytesBase64).decode('utf8')
base64_utf8(Atom, BytesBase64) :-
    %% TODO: use the DCG forms of library(base64), to
    %%       avoid extra atom_codes/2 calls
    (  var(BytesBase64)
    -> atom_to_utf8_to_b64(Atom, BytesBase64)
    ;  b64_to_utf8_to_atom(BytesBase64, Atom)
    ).

%! atom_to_utf8_to_b64(+Atom, -BytesBase64) is det.
atom_to_utf8_to_b64(Atom, BytesBase64) :-
    atom_codes(Atom, Codes),
    phrase(utf8_codes(Codes), Utf8codes),
    atom_codes(Utf8, Utf8codes),
    base64_ascii(Utf8, BytesBase64).

%! b64_to_utf8_to_atom(+BytesBase64, -Atom) is det.
b64_to_utf8_to_atom(BytesBase64, Atom) :-
    base64_ascii(Utf8, BytesBase64),
    atom_codes(Utf8, Utf8codes),
    phrase(utf8_codes(Codes), Utf8codes),
    atom_codes(Atom, Codes).
