% An example module to illustrate committed choice nondeterminism in Mercury.
% In the standard "commutative" semantics, this program should print out
% _either_
%
%	Hello, World
% or
%	Goodbye, World
%
% Which one it prints out is unspecified: the implementation can pick either.
%
% In the "strict sequential semantics" (enabled by the `--strict-sequential'
% option to the Mercury compiler), it is guaranteed to print "Hello, World".

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module committed_choice.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

main --> io__write_string("Hello, world\n").
main --> io__write_string("Goodbye, world\n").
