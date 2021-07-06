% An example program to illustrate the use of the `do_while'
% predicate in Mercury.  This program calls a nondeterministic
% predicate hello/1, and prints the first N solutions it finds
% (in this case for N = 2).
%
% Note that in the standard "commutative" semantics, the order of
% solutions is unspecified.  If you want to force the order of
% evaluation, then you would need to use the "strict sequential semantics"
% (enabled by the `--strict-sequential' option to the Mercury compiler).

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module n_solutions.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module bool.
:- import_module int.
:- import_module solutions.

main(!IO) :-
    N = 2,
    do_while(hello, get_next, {!.IO, N}, {!:IO, _}),
    print("Done\n", !IO).

:- pred hello(string::out) is multi.

hello("Hello, world\n").
hello("Good day, world\n").
hello("Greetings, world\n").

:- pred get_next(string::in, bool::out, {io, int}::di, {io, int}::uo)
    is det.

get_next(String, More, {IO0, Max0}, {IO, Max}) :-
    print(String, IO0, IO),
    More = (Max0 > 1 -> yes ; no),
    Max = Max0 - 1.
