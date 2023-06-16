%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% A test of uint8 to decimal string conversion.
%
%---------------------------------------------------------------------------%

:- module uint8_to_string.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module uint8.

%---------------------------------------------------------------------------%

main(!IO) :-
    P =
        ( pred(U::in, !.IO::di, !:IO::uo) is det :-
            io.print_line(uint8_to_string(U), !IO)
        ),
    list.foldl(P, test_numbers, !IO).

:- func test_numbers = list(uint8).

test_numbers = [
    0u8,
    1u8,
    2u8,
    7u8,
    8u8,
    9u8,
    10u8,
    11u8,
    14u8,
    15u8,
    16u8,
    31u8,
    32u8,
    63u8,
    64u8,
    99u8,
    100u8,
    101u8,
    126u8,  % max_int8 - 1
    127u8,  % max_int8
    128u8,  % max_int8 + 1
    254u8,  % max_uint8 - 1
    255u8   % max_uint8
].

%---------------------------------------------------------------------------%
:- end_module uint8_to_string.
%---------------------------------------------------------------------------%
