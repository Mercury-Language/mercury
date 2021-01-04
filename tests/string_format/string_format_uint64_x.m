%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the x, X specifiers of string.format with uint64s.
%
%---------------------------------------------------------------------------%

:- module string_format_uint64_x.

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
    UInts = [u64(0u64), u64(1u64), u64(10u64), u64(100u64), u64(max_uint64)],
    list.foldl(output_list(UInts), format_strings("x"), !IO),
    list.foldl(output_list(UInts), format_strings("X"), !IO).

%---------------------------------------------------------------------------%
