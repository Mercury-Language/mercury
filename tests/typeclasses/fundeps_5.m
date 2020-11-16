%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% From the original bug report:
% 
%     Uncomment one line in do_something and this program compiles,
%     or remove the typevariable X from the typeclass definitions and
%     the program compiles. Note this comes from a test case where
%     there are methods which use X in typeclass definitions.
% 
% The problem was that the ancestors of assumed constraints (in this case,
% the constraint a(B, X) where B is the type of B1) were not being used
% when searching for opportunities for improvement. As a result, the unproven
% constraint a(B, Y) on the call to hello/1 could not be satisfied,
% since the improvement X = Y wasn't discovered.

:- module fundeps_5.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

:- typeclass a(A, X) <= ((A->X), (X->A)) where [
    func hello(A) = string
].

:- typeclass b(B, X) <= (a(B, X), (B->X), (X->B)) where [
    func goodbye(B) = string
].

:- type some_b
    --->    some [B, X] some_b(B) => b(B, X).

:- instance a(string, int) where [
    hello(S) = "hello " ++ S ++ "\n"
].

:- instance b(string, int) where [
    goodbye(S) = "goodbye " ++ S ++ "\n"
].

:- pred do_something(some_b::in, io::di, io::uo) is det.

do_something(SomeB1, !IO) :-
   SomeB1 = some_b(B1),
   io.write_string(hello(B1), !IO),
   % Uncomment the line below and this compiles.
   % io.write_string(goodbye(B1), !IO),
   true.

:- pred do_something_else(some_b::in, io::di, io::uo) is det.

do_something_else(SomeB1, !IO) :-
   SomeB1 = some_b(B1),
   io.write_string(hello(B1), !IO),
   % Comment the line below and this doesn't compile.
   io.write_string(goodbye(B1), !IO),
   true.

main(!IO)  :-
   SomeB1 = 'new some_b'("test"),
   do_something(SomeB1, !IO),
   do_something_else(SomeB1, !IO).
