%-----------------------------------------------------------------------------%
%
% A simple sorting program.
%
% It works on text files, considering each line to be a record.
% The entire line is considered to be the sort key.
%
% The algorithm used is simple insertion sort.
%
% This source file is hereby placed in the public domain.  -fjh (the author).
%
%-----------------------------------------------------------------------------%

:- module sort.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module string, list, char, require, std_util.

main -->
	io__command_line_arguments(Args),
	(
		{ Args = [] },
		handle_args(no, no),
		sort
	;
		{ Args = [Input] },
		handle_args(yes(Input), no),
		sort
	;
		{ Args = [Input, Output] },
		handle_args(yes(Input), yes(Output)),
		sort
	;
		{ Args = [_, _, _ | _] },
		io__write_string("Usage: sort [Input [Output]]\\n")
	).

:- pred handle_args(maybe(string), maybe(string), io__state, io__state).
:- mode handle_args(in, in, di, uo) is det.

handle_args(InArg, OutArg) -->
	(
		{ InArg = yes(InFilename) },
		io__see(InFilename, InResult),
		(
			{ InResult = ok }
		;
			{ InResult = error(InError) },
			{ io__error_message(InError, InMsg) },
			{ error(InMsg) }
		)
	;
		{ InArg = no }
	),
	(
		{ OutArg = yes(OutFilename) },
		io__tell(OutFilename, OutResult),
		(
			{ OutResult = ok }
		;
			{ OutResult = error(OutError) },
			{ io__error_message(OutError, OutMsg) },
			{ error(OutMsg) }
		)
	;
		{ OutArg = no }
	).

:- pred sort(io__state, io__state).
:- mode sort(di, uo) is det.

sort -->
	sort_2([]).

:- pred sort_2(list(list(char)), io__state, io__state).
:- mode sort_2(in, di, uo) is det.

sort_2(Lines0) -->
	io__read_line(Result),
	(
		{ Result = error(Error) },
		{ io__error_message(Error, Msg) },
		{ error(Msg) }
	;
		{ Result = eof },
		sort_output(Lines0)
	;
		{ Result = ok(Line) },
		{ insert(Lines0, Line, Lines1) },
		sort_2(Lines1)
	).

:- pred insert(list(T), T, list(T)).
:- mode insert(in, in, out) is det.

insert([], I, [I]).
insert([H | T], I, L) :-
	compare(R, I, H),
	( R = (<) ->
		L = [I, H | T]
	;
		insert(T, I, NT),
		L = [H | NT]
	).

:- pred sort_output(list(list(char)), io__state, io__state).
:- mode sort_output(in, di, uo) is det.

sort_output([]) --> [].
sort_output([Line | Lines]) -->
	{ string__from_char_list(Line, LineStr) },
	io__write_string(LineStr),
	sort_output(Lines).
