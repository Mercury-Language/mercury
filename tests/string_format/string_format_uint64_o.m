%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the o specifier of string.format with uint64 values.
%
%---------------------------------------------------------------------------%

:- module string_format_uint64_o.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.
:- import_module string_format_lib.
:- import_module uint64.

main(!IO) :-
    UInt64s = [u64(0u64), u64(1u64), u64(10u64), u64(100u64), u64(max_uint64)],
    list.foldl(output_list(UInt64s), format_strings("o"), !IO).

%---------------------------------------------------------------------------%
