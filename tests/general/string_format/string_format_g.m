%------------------------------------------------------------------------------%
% string_format_g.m
%
% Test the g,G specifiers of string__format.
%------------------------------------------------------------------------------%

:- module string_format_g.

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
	{ FormatStrs_g = format_strings("g") },
	{ FormatStrs_G = format_strings("G") },

	list__foldl(output_list(standard_floats), FormatStrs_g),
	list__foldl(output_list(trailing_zero_floats), FormatStrs_g),
	list__foldl(output_list(rounding_floats), FormatStrs_g),
	list__foldl(output_list(extreme_floats), FormatStrs_g),
	list__foldl(output_list(denormal_floats), FormatStrs_g),
	list__foldl(output_list(infinite_floats), FormatStrs_g),

	list__foldl(output_list(standard_floats), FormatStrs_G),
	list__foldl(output_list(trailing_zero_floats), FormatStrs_G),
	list__foldl(output_list(rounding_floats), FormatStrs_G),
	list__foldl(output_list(extreme_floats), FormatStrs_G),
	list__foldl(output_list(denormal_floats), FormatStrs_G),
	list__foldl(output_list(infinite_floats), FormatStrs_G),
	[].

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
