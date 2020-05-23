%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the u specifier of string.format with ints.
%
% The .exp file is for when int is 32-bit.
% The .exp2 file is for when int is 64-bit.
%
%---------------------------------------------------------------------------%

:- module string_format_u.
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
    Ints = [i(0), i(1), i(10), i(100), i(max_int)],
    list.foldl(output_list(Ints), format_strings("u"), !IO).

%---------------------------------------------------------------------------%
