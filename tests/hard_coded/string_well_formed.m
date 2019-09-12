%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_well_formed.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    Empty = "",
    Good = "\U0001F600", % 4 UTF-8 code units, 2 UTF-16 code units
    Bad1 = string.between(Good, 0, 1),
    Bad2 = string.between(Good, 0, length(Good) - 1),
    Bad3 = Bad1 ++ Good,
    Bad4 = Good ++ Bad2,
    test_well_formed("Empty", Empty, !IO),
    test_well_formed("Good", Good, !IO),
    test_well_formed("Bad1", Bad1, !IO),
    test_well_formed("Bad2", Bad2, !IO),
    test_well_formed("Bad3", Bad3, !IO),
    test_well_formed("Bad4", Bad4, !IO).

:- pred test_well_formed(string::in, string::in, io::di, io::uo) is det.

test_well_formed(Label, S, !IO) :-
    ( if string.is_well_formed(S) then
        io.write_string(Label ++ " is well-formed\n", !IO)
    else
        io.write_string(Label ++ " is not well-formed\n", !IO)
    ).
