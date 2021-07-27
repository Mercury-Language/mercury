%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Date: Wed, 1 Dec 1999 22:52:57 +1100
% Subject: compiler infinite loop for cyclic type classes
% 
% According to the language reference manual:
% 
%    Typeclass constraints on type class declarations gives rise to a
%    superclass relation. This relation must be acyclic. That is, it is an
%    error if a type class is its own (direct or indirect) superclass.
% 
% But if you try to compile modules containing cyclic typeclasses, the
% compiler goes into an infinite loop and eventually gets a stack overflow,
% rather than reporting a proper error message.

:- module cyclic_typeclass.

:- interface.

:- pred main(io::di, io::uo) is det.

:- import_module io.

:- implementation.

:- typeclass foo(A) <= bar(A) where [
    func foo(A) = int
].

:- typeclass bar(A) <= foo(A) where [
    func bar(A) = int
].

:- instance foo(int) where [
    foo(X) = X
].

:- instance bar(int) where [
    bar(X) = X
].

main(!IO) :-
    io.print_line(foo(42), !IO),
    io.print_line(bar(43), !IO).
