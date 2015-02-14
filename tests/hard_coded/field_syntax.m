%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% field_syntax.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Tue Dec 17 14:13:23 EST 2002
%---------------------------------------------------------------------------%

:- module field_syntax.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

:- func array(T) ^ elem_from_end(int) = T.
:- mode array_ui ^ elem_from_end(in)  = out is det.

A ^ elem_from_end(I) = A ^ elem(max(A) - I).

%---------------------------------------------------------------------------%

:- func (array(T) ^ elem_from_end(int) := T)  = array(T).
:- mode (array_di ^ elem_from_end(in)  := in) = array_uo is det.

% This formatting gives each part of the term a different context.
% This is a regression test for a bug in which the code that recognized
% field accesses insisted on the same context for parts of the term.
(A
    ^
        elem_from_end(I)
            :=
                X)
                    =
                        (A
                            ^
                                elem(max(A) - I)
                                    :=
                                        X).

%---------------------------------------------------------------------------%

main(!IO) :-
    A = array([1, 2, 3]),
    io.format("A                           = ", [], !IO),
    io.print(A, !IO),
    io.nl(!IO),
    io.format("A ^ elem(0)                 = %d\n",
        [i(A ^ elem(0))],          !IO),
    io.format("A ^ elem_from_end(0)        = %d\n",
        [i(A ^ elem_from_end(0))], !IO),
    io.format("(A ^ elem_from_end(2) := 4) = ", [], !IO),
    io.print(A ^ elem_from_end(2) := 4, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%
