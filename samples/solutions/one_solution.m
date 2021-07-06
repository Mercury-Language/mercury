% An example module to illustrate committed choice nondeterminism in Mercury.
% There is more than one answer which is logically correct,
% but the program will only compute one of them.
% In the standard "commutative" semantics, this program should print out
% _either_
%
%   Hello, World
% or
%   Goodbye, World
%
% Which one it prints out is unspecified: the implementation can pick either.
%
% In the "strict sequential semantics" (enabled by the `--strict-sequential'
% option to the Mercury compiler), it is guaranteed to print "Hello, World".

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module one_solution.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

main(!IO) :-
    io.write_string("Hello, world\n", !IO).
main(!IO) :-
    io.write_string("Goodbye, world\n", !IO).
