% An example program to illustrate the use of the `do_while'
% predicate in Mercury.  This program calls a nondeterministic
% predicate hello/1, prints the first solution it finds, and
% then asks the user if they want any more solutions;
% if they do, it finds another solution, prompts the user again,
% and so on.  It stops when there are no more solutions or
% when the user says no to the "More?" prompt.
%
% Note that in the standard "commutative" semantics, the order of
% solutions is unspecified.  If you want to force the order of
% evaluation, then you would need to use the "strict sequential semantics"
% (enabled by the `--strict-sequential' option to the Mercury compiler).

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module some_solutions.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module std_util, char, bool, list.

main -->
	do_while(hello, get_next),
	io__write_string("No (more) solutions\n").

:- pred hello(string::out) is multi.

hello("Hello, world\n").
hello("Good day, world\n").
hello("Greetings, world\n").

:- pred get_next(string::in, bool::out, io__state::di, io__state::uo) is det.

get_next(String, More) -->
	% print the first answer
	io__write_string(String),

	% see if the user wants more answers
	io__write_string("More? "),
	io__read_line(Line),
	{ if	Line = ok([FirstChar|_]),
		char__to_upper(FirstChar, 'Y')
	then	More = yes
	else	More = no
	}.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module committed_choice.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

main --> io__write_string("Hello, world\n").
main --> io__write_string("Goodbye, world\n").
