%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test the operation of lookup switches on integer types other than
% int itself.
%
%---------------------------------------------------------------------------%

:- module bug582.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    Rows = [3u8, 5u8, 7u8],
    Cols = [0i16, 1i16, 2i16, 3i16, 4i16, 5i16, 6i16, 7i16, 8i16, 9i16, 10i16],
    list.foldl(test_action_table_row(Cols), Rows, !IO).

%---------------------------------------------------------------------------%

:- pred test_action_table_row(list(int16)::in, uint8::in,
    io::di, io::uo) is det.

test_action_table_row(Cols, Row, !IO) :-
    list.foldl(test_action_table_row_col(Row), Cols, !IO).

:- pred test_action_table_row_col(uint8::in, int16::in,
    io::di, io::uo) is det.

test_action_table_row_col(Row, Col, !IO) :-
    ( if action_table(Row, Col, Val) then
        io.format("action_table(%2u, %2i) = %u\n",
            [u8(Row), i16(Col), u16(Val)], !IO)
    else
        io.format("action_table(%2u, %2i) failed\n",
            [u8(Row), i16(Col)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred action_table(uint8::in, int16::in, uint16::out) is semidet.

action_table(5u8, 0i16, 1661u16).
action_table(5u8, 1i16, 1663u16).
action_table(5u8, 2i16, 1665u16).
action_table(5u8, 3i16, 1667u16).
action_table(5u8, 4i16, 1669u16).
action_table(7u8, 5i16, 1660u16).
action_table(7u8, 6i16, 1662u16).
action_table(7u8, 7i16, 1664u16).
action_table(7u8, 8i16, 1666u16).
action_table(7u8, 9i16, 1668u16).

%---------------------------------------------------------------------------%
