%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the d specifier of string.format with int64 values.
%
%---------------------------------------------------------------------------%

:- module string_format_int64_d.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int64.
:- import_module list.
:- import_module string.
:- import_module string_format_lib.

main(!IO) :-
    Int64s = [i64(0i64), i64(1i64), i64(-1i64), i64(10i64), i64(-10i64),
        i64(100i64), i64(-100i64), i64(min_int64), i64(max_int64)],
    list.foldl(output_list(Int64s), format_strings("d"), !IO),
    list.foldl(output_list(Int64s), format_strings("i"), !IO).

%---------------------------------------------------------------------------%
