%------------------------------------------------------------------------------%
% string_format_e.m
%
% Test the e,E specifiers of string__format.
%------------------------------------------------------------------------------%

:- module string_format_e.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module string_format_lib.
:- import_module float, list, string.

%------------------------------------------------------------------------------%

main -->
	{ FormatStrs_e = format_strings("e") },
	{ FormatStrs_E = format_strings("E") },

	list__foldl(output_list(standard_floats), FormatStrs_e),
	list__foldl(output_list(trailing_zero_floats), FormatStrs_e),
	list__foldl(output_list(rounding_floats), FormatStrs_e),
	list__foldl(output_list(extreme_floats), FormatStrs_e),
	list__foldl(output_list(denormal_floats), FormatStrs_e),
	list__foldl(output_list(infinite_floats), FormatStrs_e),

	list__foldl(output_list(standard_floats), FormatStrs_E),
	list__foldl(output_list(trailing_zero_floats), FormatStrs_E),
	list__foldl(output_list(rounding_floats), FormatStrs_E),
	list__foldl(output_list(extreme_floats), FormatStrs_E),
	list__foldl(output_list(denormal_floats), FormatStrs_E),
	list__foldl(output_list(infinite_floats), FormatStrs_E),
	[].

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
