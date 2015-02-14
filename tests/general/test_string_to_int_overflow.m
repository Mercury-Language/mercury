%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% test_string_to_int_overflow.m
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Mon Feb  2 13:29:05 EST 2009
%---------------------------------------------------------------------------%

:- module test_string_to_int_overflow.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    Xs = [
        ( if string.to_int("999", I0) then yes(I0) else no),
        ( if string.to_int("99999999999999999999", I1) then yes(I1) else no ),
        ( if base_string_to_int(16, "ffffffffff", I2) then yes(I2) else no ),
        ( if base_string_to_int(10, "999", I3) then yes(I3) else no )
    ],
    io.print(Xs, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
