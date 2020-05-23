%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the u specifier of string.format with uint values.
%
% The .exp file is for when uint is 32-bit.
% The .exp2 file is for when uint is 64-bit.
%
%---------------------------------------------------------------------------%

:- module string_format_uint_u.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.
:- import_module string_format_lib.
:- import_module uint.

main(!IO) :-
    UInts = [u(0u), u(1u), u(10u), u(100u), u(max_uint)],
    list.foldl(output_list(UInts), format_strings("u"), !IO).

%---------------------------------------------------------------------------%
