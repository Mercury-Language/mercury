:- module qual_adv_test.

% A test using module qualification to resolve ambiguous overloading.  The
% three versions of `format' should all produce different output, 
% despite having the same name, and strang and strung both importing string.m
%
% The implementation predicates are mostly have the same names too, and
% a couple have different clauses.
%

:- interface.
:- import_module io.

:- pred qual_adv_test:main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, string, qual_strang, qual_strung.

main -->
	{ String = "asdfjkfhaslks" },
	{ FString = "iii %s.\n"},
	{ string:format(FString, [s(String)], Out1) },
	io:write_string(Out1),
	{ qual_strang:format(FString, [s(String)], Out2) },
	io__write_string(Out2),
	{ qual_strung:format(FString, [s(String)], Out3) },
	io__write_string(Out3),
	{ Out4 = qual_strang:format_func(FString, [s(String)]) },
	{ Out5 = qual_strung:format_func(FString, [s(String)]) },
	(
		{ Out4 = Out2 },
		{ Out5 = Out3 }
	->
		io__write_string("ok\n")
	;
		io__write_string("failed\n")
	).
