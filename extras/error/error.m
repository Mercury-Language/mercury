%------------------------------------------------------------------------------%
% Copyright (C) 2000,2003, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% Author: Tom Conway <conway@cs.mu.oz.au>
%
% Modifications by:	Tyson Dowd <trd@cs.mu.oz.au>
%
% error: a tool like the old unix tool of the same name.
%
% error will process compiler error message, and insert them into source
% files as comments.  This means you can fix a bunch of errors all in a
% single editing session.
%
% PLEASE NOTE: This is experimental software -- it will modify your
% source files.  Please be sure to back up your work before using this
% program.
%
% error takes a list of files on the command line, and looks for errors
% in the common format:
%
% filename:linenumber: error message
%
% for example:
%
% foo.m:041: In clause for `main(di, uo)':
% foo.m:041:   mode mismatch in disjunction.
% foo.m:041:   `Errors' :: free, ground.
%
% error will then insert the error message at the appropriate line in
% the file, with a ### preceding the error message, for example:
%
% /* ###  In clause for `main(di, uo)': */
% /* ###    mode mismatch in disjunction. */
% /* ###    `Errors' :: free, ground. */
% main -->
%
% Most compilers will output in this format (for example Mercury outputs
% error messages in this format, so does gcc).
%
% If the -v option is given, error will first insert the error messages,
% and then invoke your editor on the list of files which contained errors.  
% error looks in the environment variable EDITOR for your editor, and if
% that isn't found, it will attempt to use the editor "vi".
%
% possible improvements:
%	- better error handling
%	- look for variables other than EDITOR 
%	- usage message, help message
%	- take input from stdin if no filenames given or if - is a
%	  filename
%	- handle options using getopt
%	- handle other commenting styles than /* .... */

:- module error.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module bool, char, int, list, map, maybe, pair, string.

:- type errors	== map(file, map(line, list(message))).

:- type file	== string.

:- type line	== int.

:- type message	== string.

main -->
	io__command_line_arguments(Args0),
	{ Args0 = ["-v" | Args1] ->
		EditErrors = yes,
		Args = Args1
	;
		EditErrors = no,
		Args = Args0
	},
	{ map__init(Errors0) },
	read_error_files(Args, Errors0, MErrors),
	(
		{ MErrors = no }
	;
		{ MErrors = yes(Errors) },
		process_errors(Errors),
		( { EditErrors = yes } ->	
			edit_errors(Errors)
		;
			[]	
		)
	).

%------------------------------------------------------------------------------%

:- pred read_error_files(list(string), errors, maybe(errors),
		io__state, io__state).
:- mode read_error_files(in, in, out, di, uo) is det.

read_error_files([], Errors, yes(Errors)) --> [].
read_error_files([Name|Names], Errors0, MErrors) -->
	io__see(Name, Res),
	(
		{ Res = ok },
		read_errors(Errors0, MErrors0),
		io__seen,
		(
			{ MErrors0 = no },
			{ MErrors = no }
		;
			{ MErrors0 = yes(Errors1) },
			read_error_files(Names, Errors1, MErrors)
		)
	;
		{ Res = error(Err) },
		{ io__error_message(Err, Msg) },
		io__stderr_stream(StdErr),
		io__format(StdErr, "error: %s\n", [s(Msg)]),
		{ MErrors = no }
	).

:- pred read_errors(errors, maybe(errors), io__state, io__state).
:- mode read_errors(in, out, di, uo) is det.

read_errors(Errors0, MErrors) -->
	io__read_line(Res),
	(
		{ Res = eof },
		{ MErrors = yes(Errors0) }
	;
		{ Res = error(Err) },
		{ io__error_message(Err, Msg) },
		io__stderr_stream(StdErr),
		io__format(StdErr, "error: %s\n", [s(Msg)]),
		{ MErrors = no }
	;
		{ Res = ok(Chars) },
		( { parse_error(Chars, File, Line, Message) } ->
			{ insert_error(Errors0, File, Line, Message, Errors1) }
		;
			{ string__from_char_list(Chars, Str) },
			io__stderr_stream(StdErr),
			io__format(StdErr, "error: %s", [s(Str)]),
			{ Errors1 = Errors0 }
		),
		read_errors(Errors1, MErrors)
	).

:- pred insert_error(errors, file, line, message, errors).
:- mode insert_error(in, in, in, in, out) is det.

insert_error(Errs0, File, Line, Message, Errs) :-
	( search(Errs0, File, FileErrs0) ->
		( search(FileErrs0, Line, Messages0) ->
			append(Messages0, [Message], Messages)
		;
			Messages = [Message]
		),
		set(FileErrs0, Line, Messages, FileErrs)
	;
		map__from_assoc_list([Line - [Message]], FileErrs)
	),
	set(Errs0, File, FileErrs, Errs).

:- pred parse_error(list(char), file, line, message).
:- mode parse_error(in, out, out, out) is semidet.

parse_error(Chars, File, Line, Message) :-
	takewhile((pred(C0::in) is semidet :-
		C0 \= (':')
	), Chars, FileChars, [_|Rest0]),
	takewhile((pred(C1::in) is semidet :-
		C1 \= (':')
	), Rest0, LineChars, [_|Rest1]), % throw away the :
	takewhile((pred(C2::in) is semidet :-
		C2 \= ('\n')
	), Rest1, MsgChars, _),
	string__from_char_list(FileChars, File),
	string__from_char_list(LineChars, LineStr),
	string__to_int(LineStr, Line),
	string__from_char_list(MsgChars, Message).

%------------------------------------------------------------------------------%

:- pred process_errors(errors, io__state, io__state).
:- mode process_errors(in, di, uo) is det.

process_errors(Errors) -->
	{ map__to_assoc_list(Errors, FileErrorList) },
	process_2(FileErrorList).

:- pred process_2(list(pair(file, map(line, list(message)))),
		io__state, io__state).
:- mode process_2(in, di, uo) is det.

process_2([]) --> [].
process_2([File - FileErrors|Rest]) -->
	{ map__to_assoc_list(FileErrors, LineErrorList) },
	{ string__append(File, ".orig", OrigFile) },
	rename(File, OrigFile, Res0),
	(
		{ Res0 = yes },
		io__see(OrigFile, Res1),
		(
			{ Res1 = ok },
			io__tell(File, Res2),
			(
				{ Res2 = ok },
				merge_file(LineErrorList, 1),
				io__told,
				io__remove_file(OrigFile, _),
				% io__stderr_stream(StdErr),
				io__write_string(File),
				io__nl
			;
				{ Res2 = error(Err) },
				{ io__error_message(Err, Msg) },
				io__stderr_stream(StdErr),
				io__format(StdErr, "error: %s\n", [s(Msg)]),
				rename(OrigFile, File, _)
			),
			io__seen
		;
			{ Res1 = error(Err) },
			{ io__error_message(Err, Msg) },
			io__stderr_stream(StdErr),
			io__format(StdErr, "error: %s\n", [s(Msg)]),
			rename(OrigFile, File, _)
		)
	;
		{ Res0 = no },
		io__stderr_stream(StdErr),
		io__write_string(StdErr, "error: unable to rename file.\n")
	),
	process_2(Rest).

:- pred merge_file(list(pair(line, list(message))), int, io__state, io__state).
:- mode merge_file(in, in, di, uo) is det.

merge_file([], _) -->
	copy_rest.
merge_file([ELine - Errors|Rest], CurrentLine) -->
	( { ELine =< CurrentLine } ->
		foldl((pred(Error::in, di, uo) is det -->
			io__write_string("/* ###"),
			io__write_string(Error),
			io__write_string(" */\n")
		), Errors),
		merge_file(Rest, CurrentLine)
	;
		io__read_line(Res0),
		(
			{ Res0 = eof },
			error_rest([ELine - Errors|Rest])
		;
			{ Res0 = error(Err) },
			{ io__error_message(Err, Msg) },
			io__stderr_stream(StdErr),
			io__format(StdErr, "error: %s\n", [s(Msg)])
		;
			{ Res0 = ok(Chars) },
			{ string__from_char_list(Chars, Str) },
			io__write_string(Str),
			merge_file([ELine - Errors|Rest], CurrentLine + 1)
		)
	).

:- pred copy_rest(io__state, io__state).
:- mode copy_rest(di, uo) is det.

copy_rest -->
	io__read_line(Res0),
	(
		{ Res0 = eof }
	;
		{ Res0 = error(Err) },
		{ io__error_message(Err, Msg) },
		io__stderr_stream(StdErr),
		io__format(StdErr, "error: %s\n", [s(Msg)])
	;
		{ Res0 = ok(Chars) },
		{ string__from_char_list(Chars, Str) },
		io__write_string(Str),
		copy_rest
	).

:- pred error_rest(list(pair(line, list(message))), io__state, io__state).
:- mode error_rest(in, di, uo) is det.

error_rest([]) --> [].
error_rest([_Line - Messages|Rest]) -->
	foldl((pred(Error::in, di, uo) is det -->
		io__write_string("/* ### "),
		io__write_string(Error),
		io__write_string(" */\n")
	), Messages),
	error_rest(Rest).

%------------------------------------------------------------------------------%

:- pragma c_header_code("
	#include <unistd.h>
").

:- pred rename(string, string, bool, io__state, io__state).
:- mode rename(in, in, out, di, uo) is det.

:- pragma c_code(rename(Old::in, New::in, Res::out, IO0::di, IO::uo), "{
	int err;
	err = rename(Old, New);
	Res = (err == 0 ? 1 : 0);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

:- pred edit_errors(errors, io__state, io__state).
:- mode edit_errors(in, di, uo) is det.

edit_errors(Errors) -->
		% Get the editor
	io__get_environment_var("EDITOR", MaybeEditor),
	{ Editor = (if MaybeEditor = yes(Editor0) then Editor0 else "vi" ) },

		% Get all the files in reverse order
	{ map__sorted_keys(Errors, FileList0) },
	{ list__reverse(FileList0, FileList) },

		% Append all the filenames together (this will reverse
		% the order again.
	{ list__foldl((pred(X::in, Y::in, Z::out) is det :-
			string__append(" ", X, XSpace),
			string__append(XSpace, Y, Z)),
		FileList, "", FilesString) },
	{ string__format("%s +/### %s", [s(Editor), s(FilesString)], 
		CommandStr) },

		% XXX we ignore the error status, that isn't nice.
	io__call_system(CommandStr, _Res).

