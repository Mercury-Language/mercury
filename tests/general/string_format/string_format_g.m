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
	list__foldl(output_list(standard_floats), format_strings("g")),
	list__foldl(output_list(trailing_zero_floats), format_strings("g")),
	list__foldl(output_list(rounding_floats), format_strings("g")),
	list__foldl(output_list(extreme_floats), format_strings("g")),
	list__foldl(output_list(denormal_floats), format_strings("g")),
	list__foldl(output_list(infinite_floats), format_strings("g")),

	list__foldl(output_list(standard_floats), format_strings("G")),
	list__foldl(output_list(trailing_zero_floats), format_strings("G")),
	list__foldl(output_list(rounding_floats), format_strings("G")),
	list__foldl(output_list(extreme_floats), format_strings("G")),
	list__foldl(output_list(denormal_floats), format_strings("G")),
	list__foldl(output_list(infinite_floats), format_strings("G")),
	[].

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
