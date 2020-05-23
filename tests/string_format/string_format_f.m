%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the f specifier of string.format.
%
%---------------------------------------------------------------------------%

:- module string_format_f.

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
    FormatStrs_f = format_strings("f"),

    % We don't test printing max because each different implementation
    % of printf prints a different number of significant digits,
    % which would require us to have lots of expected output files.
    ExtremeFloats = [f(min), f(-min)],

    list.foldl(output_list(standard_floats), FormatStrs_f, !IO),
    list.foldl(output_list(trailing_zero_floats), FormatStrs_f, !IO),
    list.foldl(output_list(rounding_floats), FormatStrs_f, !IO),
    list.foldl(output_list(ExtremeFloats), FormatStrs_f, !IO),
    list.foldl(output_list(denormal_floats), FormatStrs_f, !IO).

%---------------------------------------------------------------------------%
