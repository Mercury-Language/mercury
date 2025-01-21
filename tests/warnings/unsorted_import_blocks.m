%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unsorted_import_blocks.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.
:- import_module list.
:- import_module int32.
:- use_module    int8.
:- import_module int16.

main(!IO) :-
    io.format("%d %d %d\n", [i8(1i8), i16(2i16), i32(3i32)], !IO).
