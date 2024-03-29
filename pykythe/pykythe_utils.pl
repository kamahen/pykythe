% -*- mode: Prolog -*-

%% Utilities for pykythe.


:- module(pykythe_utils, [
                          absolute_dir/2,
                          absolute_file_name_rel/2,
                          absolute_file_name_rel/3,
                          at_most/3,
                          base64_utf8/2,
                          do_if/2,
                          dump_term/2,
                          dump_term/3,
                          ensure_dict_fact/3,
                          ensure_dict_fact_base64_ascii/3,
                          ensure_dict_fact_base64_utf8/3,
                          get_dict_default/4,
                          has_prefix/2,
                          has_suffix/2,
                          hash_hex/2,
                          json_read_dict_validate/3,
                          log_if/2,
                          log_if/3,
                          maybe_absolute_dir/2,
                          maybe_file_sha1/2,
                          maybe_open_read/2,
                          print_term_cleaned/3,
                          pykythe_json_read_dict/2,
                          pykythe_json_write_dict_nl/2,
                          pykythe_tmp_file_stream/4,
                          remove_prefix/3,
                          remove_suffix/3,
                          remove_suffix_star/3,
                          safe_delete_file/1,
                          safe_hard_link_file/2,
                          safe_hard_link_file_dup_ok/2,
                          split_atom/4,
                          term_to_canonical_atom/2,
                          % update_dict/3,
                          validate_prolog_version/0,
                          write_atomic_file/3,
                          write_atomic_stream/3
                         ]).
:- encoding(utf8).
:- use_module(library(debug)). % explicit load to activate optimise_debug/0.
% :- set_prolog_flag(autoload, false).  % TODO: seems to break plunit, qsave

:- meta_predicate
       do_if(0, 0),
       log_if(0, +),
       log_if(0, +, +),
       write_atomic_stream(1, +, +),
       write_atomic_file(1, +, +).

:- style_check(+singleton).
:- style_check(+no_effect).
:- style_check(+discontiguous).
:- set_prolog_flag(warn_autoload, true).

:- use_module(library(apply), [maplist/2, maplist/3]).
:- use_module(library(base64), [base64/2 as base64_ascii]).
:- use_module(library(error)).
:- use_module(library(filesex), [make_directory_path/1, directory_file_path/3, link_file/3]).
:- use_module(library(http/json), [json_read_dict/3, json_write_dict/3]).
:- use_module(library(pcre), [re_replace/4]).
:- use_module(library(pprint), [print_term/2]).
:- use_module(library(prolog_pack), [pack_property/2]).
:- use_module(library(readutil), [read_file_to_string/3]).
:- use_module(library(sha), [sha_hash/3, hash_atom/2]).
:- use_module(library(utf8), [utf8_codes//1]).
:- use_module(library(yall)).   % For [S,A]>>atom_string(A,S) etc.

:- use_module(must_once, [must_once/1, must_once_msg/2, must_once_msg/3, fail/1]).

:- style_check(+var_branches).

% :- set_prolog_flag(generate_debug_info, false).


:- det(validate_prolog_version/0).
validate_prolog_version =>
    current_prolog_flag(version, PrologVersion),
    % Sync this with README.md and demo.sh:
    must_once_msg(PrologVersion >= 80324, 'SWI-Prolog version is too old'),
    pack_property(edcg, version(EdcgVersion)),
    must_once_msg(EdcgVersion @>= '0.9.1.7', 'pack(edcg) is missing or version is too old').

%! absolute_file_name_rel(+File, -Absolute) is semidet.
% For now, this is the same as absolute_file_name/2.  However, it is
% intended that this should look up the corpus root and remove the
% prefix (e.g., "/").
% This is semidet if [file_errors(fail), access(read)] are in the Options.
% It is typically det/error otherwise.
% TODO: Kythe prefers to have paths without leading "/"; the code
%       should use (corpus,root,path) for file names and create a
%       filesystem name from a lookup of (corpus,root) to filesystem prefix.
% TODO: https://github.com/kamahen/pykythe/issues/24
absolute_file_name_rel(File, Absolute) :-
    absolute_file_name(File, Absolute0),
    Absolute = Absolute0.  % atom_concat('/', Absolute, Absolute0).

absolute_file_name_rel(File, Absolute, Options) :-
    absolute_file_name(File, Absolute0, Options),
    Absolute = Absolute0.  % atom_concat('/', Absolute, Absolute0).

%! absolute_dir(+Path0:atom, -AbsPath:atom) is semidet.
%  Apply absolute_file_name_rel to Path0, giving AbsPath, ensuring it's a
%  directory and appending '/' to the name.
absolute_dir('/', AbsPath) => AbsPath = '/'. % Special case for root dir, which otherwise would become '//'
absolute_dir(Path0, AbsPath) =>
    remove_suffix_star(Path0, '/', Path),
    absolute_file_name_rel(Path, AbsPath0, [access(read), file_type(directory), file_errors(fail)]),
    atomic_list_concat([AbsPath0, '/'], AbsPath).

:- det(do_if/2).
%! do_if(:Cond, :Pred) is det.
% A handy meta-predicate for turning debug stuff on/off, according to Cond
do_if(Cond, Pred),
        call(Cond) =>
    call(Pred).
do_if(_Cond, _Pred) => true.

:- det(dump_term/2).
%! dump_term(+Msg:atom, +Term) is det.
% TODO: delete this debugging code
dump_term(Msg, Term) =>
    dump_term(Msg, Term, [indent_arguments(2),
                          right_margin(120)]).

:- det(dump_term/3).
%! dump_term(+Msg:atom, +Term, +Options:list) is det.
% TODO: use debug/3, etc. instead (also print_message/2).
% TODO: Delete this debugging code
dump_term(Msg, Term, Options) =>
    print_term_cleaned(Term, [indent_arguments(4)|Options], TermStr),
    dump_term2(Msg, TermStr).

:- det(dump_term2/2).
dump_term2('', TermStr) =>
    log_if(true, '~s.', [TermStr]).
dump_term2(Msg, TermStr) =>
    log_if(true, '% === ~w ===~n', [Msg]),
    log_if(true, '~s.~n', [TermStr]),
    log_if(true, '% === end ~w ===~n', [Msg]).

:- det(ensure_dict_fact/3).
%! ensure_dict_fact(+Dict, +Attr, ?Value) is det.
% Die with an error message if Dict.Attr != Value
% (Can also be used to get Dict.Attr into Value).
ensure_dict_fact(Dict, Attr, Value) =>
    must_once_msg(get_dict(Attr, Dict, Value),
                  'Invalid JSON, expecting ~q=~q in ~q',
                  [Attr, Value, Dict]).

:- det(ensure_dict_fact_base64_ascii/3).
%! ensure_dict_fact_base64_ascii(+Dict, +Attr, ?Value) is det.
% Die with an error message if base64_ascii(Dict.Attr) != Value
% (Can also be used to get Dict.Attr into Value).
ensure_dict_fact_base64_ascii(Dict, Attr, Value) =>
    get_dict(Attr, Dict, Value64),
    must_once_msg(base64_ascii(Value, Value64),
                  'Invalid JSON, expecting base64 ~q=~q in ~q',
                  [Attr, Value, Dict]).

:- det(ensure_dict_fact_base64_utf8/3).
%! ensure_dict_fact_base64_utf8(+Dict, +Attr, ?Value) is det
% Die with an error message if base64_utf8(Dict.Attr) != Value
% (Can also be used to get Dict.Attr into Value).
ensure_dict_fact_base64_utf8(Dict, Attr, Value) =>
    get_dict(Attr, Dict, Value64),
    must_once_msg(base64_utf8(Value, Value64),
                  'Invalid JSON, expecting base64 ~q=~q in ~q',
                  [Attr, Value, Dict]).

:- det(get_dict_default/4).
get_dict_default(Key, Dict, _Default, Value) :-
    get_dict(Key, Dict, Value),
    !.
get_dict_default(_Key, _Dict, Default, Value) :-
    Value = Default.

:- det(hash_hex/2).
%! hash_hex(+Text, -Hex) is det.
% ?- hash_hex('SWI-Prolog', Hex).
% Hex = '3d80fc267945e555c730403bd0ab0716e2a68c68'.
% Can take either a string or an atom for 1st arg.
% Defaults to using SHA-1, which is what git uses.
% However, SHA-224 or SHA-384 ought to be used:
%          https://www.schneier.com/blog/archives/2018/12/md5_and_sha-1_s.html
% (SHA-1 is 20 digits (40 chars),  SHA-224 is 28 digits, SHA-384 is 48 digits.
% TODO: incorporate the encoding in this? (Note that
%       Python's hashlib.sha1 requires bytes, not text.)
hash_hex(Text, Hex) =>
    sha_hash(Text, Hash, []),   % Default option is algorithm(sha1)
    hash_atom(Hash, Hex).

:- det(json_read_dict_validate/3).
%! json_read_dict_validate(+KytheInputStream, +FactName, -Dict) is det.
% Read a JSON "term" from KytheInputStream, verify that fact_name is
% FactName and unify the entire term with Dict) - throws an error if
% validation fails.
json_read_dict_validate(KytheInputStream, FactName, Dict) =>
    pykythe_json_read_dict(KytheInputStream, Dict),
    ensure_dict_fact(Dict, fact_name, FactName).

:- det(pykythe_json_write_dict_nl/2).
%! pykythe_json_write_dict_nl(+KytheStream:stream, +JsonAsDict:json_dict) is det.
% Output a single Kythe fact.
pykythe_json_write_dict_nl(KytheStream, JsonAsDict) =>
    % The tags are ignored unless option tag(type) is specified
    % (which it isn't). All dicts should have the tag 'json', for
    % simplicity.
    json_write_dict(KytheStream, JsonAsDict,
                    [width(0),true(#(true)),false(#(false)),null(#(null))]),
    nl(KytheStream).

:- det(log_if/2).
log_if(Cond, Fmt) => log_if(Cond, Fmt, []).

:- det(log_if/3).
%! log_if(Cond:atom, Fmt:atom, Args:list) is det.
log_if(Cond, Fmt, Args),
        call(Cond) =>
    atomic_list_concat(['~` t~3f~8|: ', Fmt, '~n'], '', Fmt2),
    statistics(process_cputime, Time),
    format(user_error, Fmt2, [Time|Args]).
log_if(_Cond, _Fmt, _Args) => true.

%! maybe_open_read(+Path, -InputStream) is semidet.
% Open Path for read or fail.
maybe_open_read(Path, InputStream) :-
    % Wrapper for debugging
    (   maybe_open_read_impl(Path, InputStream)
    ->  log_if(false, 'OPENed ~q', [Path])
    ;   log_if(false, 'OPEN-failed ~q', [Path]),
        fail
    ).

maybe_open_read_impl(Path, InputStream) :-
    catch(open(Path, read, InputStream, [encoding(octet),type(binary)]),
          error(existence_error(source_sink, Path), _),
          fail).

%! maybe_file_sha1(+SrcPath, -SrcSha1Hex) is semidet.
% Fails if file doesn't exist
maybe_file_sha1(SrcPath, SrcSha1Hex) :-
    read_file_to_string(SrcPath, SrcText, [file_errors(fail)]),
    hash_hex(SrcText, SrcSha1Hex).

:- det(pykythe_json_read_dict/2).
%! pykythe_json_read_dict(+Stream, -Dict) is det.
% Wrapper on library(http/json, [json_read_dict/2]) that sets the
% dict tags to 'json' (json_read_dict/2 leaves the tag as an
% uninstantiated variable).  And gets strings as atoms. And
% handles true/false.
pykythe_json_read_dict(Stream, Dict) =>
    json_read_dict(Stream, Dict,
                   [value_string_as(atom), end_of_file(@(end)), default_tag(json),
                    true(#(true)),false(#(false)),null(#(null))]).

:- det(print_term_cleaned/3).
%! print_term_cleaned(+Term, +Options, -TermStr) is det.
% print_term, cleaned up (tabs->blanks; remove trailing whitespace)
print_term_cleaned(Term, Options, TermStr) =>
    with_output_to(string(TermStr0),
                   print_term(Term, [tab_width(0),output(current_output)|Options])),
    re_replace(" +\n"/g, "\n", TermStr0, TermStr1),    % remove trailing whitespace
    % TODO: tab expansion should only be at beginning of line
    re_replace("\t"/g, "        ", TermStr1, TermStr). % tabs to blanks

:- det(remove_suffix_star/3).
%! remove_suffix_star(+Full:atom, +Suffix:atom, -NoSuffix:atom) is det.
% Repeatedly removes suffix if present.
remove_suffix_star(Full, Suffix, NoSuffix) :-
    remove_suffix(Full, Suffix, NoSuffix0),
    $,
    remove_suffix_star(NoSuffix0, Suffix, NoSuffix).
remove_suffix_star(Full, _Suffix, NoSuffix) :-
    NoSuffix = Full.

:- det(safe_delete_file/1).
%! safe_delete_file(?Path) is det.
% delete file, catching any errors.
safe_delete_file(Path) =>
    % The most common error is: error(existence_error(file, Path), _)
    % but other errors are possible, such as
    % error(instantiation_error, _).
    catch(delete_file(Path), _Error, true).

:-det(safe_hard_link_file/2).
safe_hard_link_file(OldPath, NewPath) =>
    % TODO: DO NOT SUBMIT - there's a race condition here; need
    %       to retry if error thrown from link_file/3
    safe_delete_file(NewPath),
    link_file(OldPath, NewPath, hard).

:- det(safe_hard_link_file_dup_ok/2).
% There is a possible race condition between deleting the NewPath and
% creating the hard link. This predicate assumes that whoever created
% the NewPath would have made it the same as the old one.
% TODO: compare OldPath, NewPath (which could also end up with
%       another race condition if a 3rd process was trying to
%       create the same output file).
safe_hard_link_file_dup_ok(OldPath, NewPath) =>
    safe_delete_file(NewPath),
    catch(link_file(OldPath, NewPath, hard),
          error(system_error, context(_, 'File exists')),
          log_if(true, 'Failed to hard-link (file exists) ~q to ~q', [OldPath, NewPath])).

:- det(split_atom/4).
%! split_atom(+Atom:atom, +SepChars:atom, +PadChars:atom, -SubAtoms:list(atom)) is det.
% Like split_string, but result is a list of atoms.
split_atom(Atom, SepChars, PadChars, SubAtoms) =>
    split_string(Atom, SepChars, PadChars, SubStrings),
    maplist([S,A]>>atom_string(A,S), SubStrings, SubAtoms).

maybe_absolute_dir(Path0, AbsPath) :-
    (   absolute_dir(Path0, AbsPath)
    ->  true
    ;   log_if(true, 'WARNING: no such directory: ~q', [Path0]),
        fail
    ).

:- det(term_to_canonical_atom/2).
%! term_canonical_atom(+Term, -Atom) is det.
% Like term_to_atom/2 if Term is instantiated, but generates
% an atom in canonical form (no operators).
term_to_canonical_atom(Term, Atom) =>
    format(atom(Atom), '~k', [Term]).

:- det(pykythe_tmp_file_stream/4).
%! pykythe_tmp_file_stream(+Dir, -FileName, -Stream, +Options) is det.
% Like tmp_file_stream/3, but allows specifying a directory rather
% than using TMPDIR or similar. Also creates Dir if needed.
pykythe_tmp_file_stream(Dir, FileName, Stream, Options) =>
    make_directory_path(Dir),
    % {set,current}_prolog_flag is copied to a thread, so
    % no need to use a mutex.
    current_prolog_flag(tmp_dir, SaveTmpDir),
    set_prolog_flag(tmp_dir, Dir),
    tmp_file_stream(FileName, Stream, Options),
    set_prolog_flag(tmp_dir, SaveTmpDir).

:- det(write_atomic_stream/3).
%! write_atomic_stream(:WritePred, +Path:atom, +OpenOptions:list) is semidet.
% Write to a file "atomically" -- that is, if another process is
% trying to write to the same file, there will be no collision (it is
% undetermined which process will "win"; presumably they both are
% trying to write the same content). If `WritePred` fails, the file
% isn't created (even if `WritePred` fails and write_atomic/2 fails.
% If needed, directories to Path are created.
% WritePred must take the stream as its last argument.
% TODO: contribute this as a SWI-Prolog package.
write_atomic_stream(WritePred, Path, OpenOptions) =>
    % TODO: See '$stage_file' in /usr/lib/swi-prolog/boot/init.pl
    %       and setup_call_catcher_cleanup
    % TODO: the tmpfile/rename trick doesn't work if the tmp file is
    % on a different file system. Is there a way of detecting this?
    % Might need to pass in an opt ... but the ideal would be to
    % create the tmp file with a "unique" suffix.  SWI-Prolog uses
    % this to create a unique name:
    %    Ssnprintf(temp, sizeof(temp), "%s/swipl_%s%s%d_%d%s%s",
    %        tmpdir, id, sep, (int) getpid(),
    %        MTOK_temp_counter++,
    %        esep, ext)
    % Note: current_prolog_flag(pid, Pid).
    %       gensym(+Base, -Unique)
    % so ... current_prolog_flag(pid, Pid),
    %        format(atom(TmpFileName0), '/foo/bar-~d-', [Pid]),
    %        gensym(TmpFileName0, TmpFileName).
    % cf: Python's tempfile.mkstemp()
    % Also: tmp_file and friends put entries into GD->os.tmp_files
    % (see swipl-devel/src/os/pl-os.c), and that doesn't happen if
    % we roll our own.
    directory_file_path(PathDir, _, Path),
    pykythe_tmp_file_stream(PathDir, TmpPath, Stream, OpenOptions),
    % TODO: instead of at_halt/1, use setup_call_cleanup/3
    at_halt(pykythe_utils:safe_delete_file(TmpPath)), % in case WritePred crashes or fails
    % TODO: the classic way to do this in Python is a bit different (better?)
    %         (see link_file/3 in library(filesex):
    %       with tempfile.NamedTemporaryFile(mode='wb') as f:
    %         write(f, ...)
    %         os.link(f.name, filename)
    %       # This depends on NamedTemporaryFile deleting on close
    (   call(WritePred, Stream)
    ->  % atomically rename file -- this prevents a race condition if
        % two pykythe processes are processing the same file at the
        % same time.
        rename_file(TmpPath, Path),
        close(Stream)
    ;   close(Stream),
        pykythe_utils:safe_delete_file(TmpPath),
        fail
    ).

:- det(write_atomic_file/3).
%! write_atomic_file(+WritePred, +Path, OpenOptions:list) is semidet.
% Similar to write_atomic_stream, except it passes a path to Pred
% instead of a stream.
% WritePred must take the path as its last argument.
write_atomic_file(WritePred, Path, OpenOptions) =>
    directory_file_path(PathDir, _, Path),
    pykythe_tmp_file_stream(PathDir, TmpPath, Stream, OpenOptions),
    % TODO: instead of setting up at_halt, use setup_call_cleanup/3
    at_halt(pykythe_utils:safe_delete_file(TmpPath)), % in case WritePred crashes or fails
    (   call(WritePred, TmpPath)
    ->  rename_file(TmpPath, Path),
        close(Stream)
    ;   close(Stream),
        pykythe_utils:safe_delete_file(TmpPath),
        fail
    ).

%! remove_suffix(+Atom, +Suffix, -FirstPart) is semidet.
remove_suffix(Atom, Suffix, FirstPart) :-
    % TODO: compare with:
    %       sub_atom(Atom, Before, _, 0, Suffix),
    %       Before0 is Before - 1,
    %       sub_atom(Atom, 0, Before0, _, FirstPart)
    atom_concat(FirstPart, Suffix, Atom).

%! remove_prefix(+Atom, +Prefix, -SecondPart) is semidet.
remove_prefix(Atom, Prefix, SecondPart) :-
    sub_atom(Atom, 0, Len, After, Prefix),
    sub_atom(Atom, Len, After, 0, SecondPart).

has_suffix(Atom, Suffix) :-
    sub_atom(Atom, _, _, 0, Suffix).

has_prefix(Atom, Prefix) :-
    sub_atom(Atom, 0, _, _, Prefix).

% Tests for atom_utf8_bytes are in pykythe.pl

%! base64_utf8(+Atom, -BytesBase64) is semidet.
% in Python: BytesBase64 = base64.b64encode(Atom.encode('utf8'))
%            Atom = base64.b64decode(BytesBase64).decode('utf8')
base64_utf8(Atom, BytesBase64) :-
    % TODO: use the DCG forms of library(base64), to
    %       avoid extra atom_codes/2 calls
    (   var(BytesBase64)
    ->  atom_to_utf8_to_b64(Atom, BytesBase64)
    ;   b64_to_utf8_to_atom(BytesBase64, Atom)
    ).

%! atom_to_utf8_to_b64(+Atom, -BytesBase64) is semidet.
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

:- det(at_most/3).
%! at_most(+List, +Length, -TruncatedList) is det.
% Takes the first Length items from List; if there are fewer, then
% TruncatedList = List
at_most([X|Xs], Length, TruncatedList),
        Length > 0 =>
    TruncatedList = [X|Ys],
    Length2 is Length - 1,
    at_most(Xs, Length2, Ys).
at_most(_Xs, _Length, TruncatedList) => TruncatedList = [].
