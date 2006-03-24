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

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module solutions.

main(!IO) :-
	do_while(hello, get_next, !IO),
	io.write_string("No (more) solutions\n", !IO).

:- pred hello(string::out) is multi.

hello("Hello, world\n").
hello("Good day, world\n").
hello("Greetings, world\n").

:- pred get_next(string::in, bool::out, io::di, io::uo) is det.

get_next(String, More, !IO) :-
	% print the first answer
	io.write_string(String, !IO),

	% see if the user wants more answers
	io.write_string("More? ", !IO),
	io.read_line(Line, !IO),
	( if	Line = ok([FirstChar|_]),
		char.to_upper(FirstChar, 'Y')
	then	More = yes
 	else	More = no
	).

