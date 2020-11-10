%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the d, i specifiers of string.format.
%
% The .exp file is for when int is 32-bit.
% The .exp2 file is for when int is 64-bit.
%
%---------------------------------------------------------------------------%

:- module string_format_d.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.
:- import_module string_format_lib.

main(!IO) :-
    Ints = [i(0), i(-1), i(1), i(10), i(-10),
        i(100), i(-100), i(min_int), i(max_int), i8(56i8), i32(-4200i32)],
    list.foldl(output_list(Ints), format_strings("d"), !IO),
    list.foldl(output_list(Ints), format_strings("i"), !IO).

%---------------------------------------------------------------------------%
