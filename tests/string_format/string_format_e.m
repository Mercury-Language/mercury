%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the e, E specifiers of string.format.
%
%---------------------------------------------------------------------------%

:- module string_format_e.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module list.
:- import_module string.
:- import_module string_format_lib.

main(!IO) :-
    FormatStrs_e = format_strings("e"),
    FormatStrs_E = format_strings("E"),

    list.foldl(output_list(standard_floats), FormatStrs_e, !IO),
    list.foldl(output_list(trailing_zero_floats), FormatStrs_e, !IO),
    list.foldl(output_list(rounding_floats), FormatStrs_e, !IO),
    list.foldl(output_list(extreme_floats), FormatStrs_e, !IO),
    list.foldl(output_list(denormal_floats), FormatStrs_e, !IO),

    list.foldl(output_list(standard_floats), FormatStrs_E, !IO),
    list.foldl(output_list(trailing_zero_floats), FormatStrs_E, !IO),
    list.foldl(output_list(rounding_floats), FormatStrs_E, !IO),
    list.foldl(output_list(extreme_floats), FormatStrs_E, !IO),
    list.foldl(output_list(denormal_floats), FormatStrs_E, !IO).

%---------------------------------------------------------------------------%
