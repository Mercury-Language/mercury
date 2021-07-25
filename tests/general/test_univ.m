%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Some very simple tests of type_to_univ and univ_to_type.

:- module test_univ.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module univ.

main(!IO) :-
    ( if test1 then
        io.write_string("test 1 ok\n", !IO)
    else
        io.write_string("test 1 failed\n", !IO)
    ),
    ( if test2 then
        io.write_string("test 2 failed\n", !IO)
    else
        io.write_string("test 2 ok\n", !IO)
    ),
    ( if test3 then
        io.write_string("test 3 failed\n", !IO)
    else
        io.write_string("test 3 ok\n", !IO)
    ),
    ( if test4(Compare) then
        ( if Compare = no then
            io.write_string("test 4 ok\n", !IO)
        else
            io.write_string("test 4 comparison failed\n", !IO)
        )
    else
        io.write_string("test 4 type_to_univ failed\n", !IO)
    ).

:- pred test1 is semidet.

test1 :-
    X = 1,
    type_to_univ(X, UnivX),
    Y = 1,
    type_to_univ(Y, UnivY),
    UnivX = UnivY.

:- pred test2 is semidet.

test2 :-
    X = 1,
    type_to_univ(X, UnivX),
    Y = 2,
    type_to_univ(Y, UnivY),
    UnivX = UnivY.

:- pred test3 is semidet.

test3 :-
    X = 1,
    type_to_univ(X, UnivX),
    type_to_univ(Y, UnivX),
    Y = "string".

:- pred test4(bool::out) is semidet.

test4(Compare) :-
    X = 1,
    type_to_univ(X, UnivX),
    type_to_univ(Y, UnivX),
    % The comparison should establish the type of Y as integer (same as X)
    ( if Y = 2 then
        Compare = yes
    else
        Compare = no
    ).
