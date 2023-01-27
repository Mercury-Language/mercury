%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the o specifier of string.format with ints.
%
% The .exp file is for when int is 32-bit.
% The .exp2 file is for when int is 64-bit.
%
%---------------------------------------------------------------------------%

:- module string_format_o.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int8.
:- import_module list.
:- import_module string.
:- import_module string_format_lib.

main(!IO) :-
    Ints = [
        i(min_int), i(-1), i(0), i(1), i(10), i(100), i(max_int),
        i8(min_int8), i8(-1i8), i8(0i8), i8(1i8), i8(max_int8),
        i16(min_int16), i16(-1i16), i16(0i16), i16(1i16), i16(max_int16),
        i32(min_int32), i32(-1i32), i32(0i32), i32(1i32), i32(max_int32)
    ],
    list.foldl(output_list(Ints), format_strings("o"), !IO).

%---------------------------------------------------------------------------%
