% Regression test.
% Tests predicates with multiple modes,
% reordering, and partially instantiated data structures.

:- module bidirectional.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module string, std_util, list, char.

main -->
	format_list_of_int(read, List),
	format_list_of_int(write, List).

:- type rw --->	read ; write.
:- mode read == in(bound(read)).
:- mode write == in(bound(write)).

:- pred format_list_of_int(rw, list(int), io__state, io__state).
:- mode format_list_of_int(write, in, di, uo) is det.
:- mode format_list_of_int(read, out, di, uo) is det.

format_list_of_int(RW, List) -->
	format_int(RW, Val),
	( { Val = no, List = [] }
	; { Val = yes(X), List = [X|_] }
	),
	( { List = [] }
	; { List = [_|_] },
	  format_list_of_int(RW, Xs),
	  { List = [_|Xs] }
	).

:- pred format_int(rw, maybe(int), io__state, io__state).
:- mode format_int(write, in, di, uo) is det.
:- mode format_int(read, out, di, uo) is det.

format_int(read, Val) -->
	my_read_line(MaybeLine),
	{ if	MaybeLine = ok(Chars),
		string__from_char_list(Chars, Line),
		string__to_int(Line, X)
	  then
		Val = yes(X)
	  else
		Val = no
	}.
format_int(write, yes(X)) -->
	io__write_int(X),
	io__write_string("\n").
format_int(write, no) -->
	io__write_string("\n").


:- pred my_read_line(io__result(list(char)), io__state, io__state).
:- mode my_read_line(out, di, uo) is det.

my_read_line(Result) -->
	io__read_line(Result0),
	{
		Result0 = ok(Line0),
		list__reverse(Line0, ['\n'|LineRev])
	->
		list__reverse(LineRev, Line),
		Result = ok(Line)
	;
		Result = Result0
	}.

