:- module qual_adv_test.

% A test using module qualification to resolve ambiguous overloading.  The
% three versions of string__format should all produce different output, 
% despite having the same name, and strang and strung both importing string.m
%
% The implementation predicates are mostly have the same names too, and
% a couple have different clauses.
%

:- interface.
:- import_module io, list, string, qual_strang, qual_strung.

:- pred qual_adv_test:main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	{ string:string__format(FString, [s(String)], Out1) },
	io:io__write_string(Out1),
	{ qual_strang:string__format(FString, [s(String)], Out2) },
	io__write_string(Out2),
	{ qual_strung:string__format(FString, [s(String)], Out3) },
	io__write_string(Out3),
	{ String = "asdfjkfhaslks" },
	{ FString = "iii %s.\n"}.
