%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module string_format_special_floats.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module float, list, string.

%------------------------------------------------------------------------------%

main -->
	{ Inf = (max+max) },
	io__write_string("Infinity:\n"),
	list__foldl(test_floats(is_infinity, [Inf, -Inf]), ["%e", "%f", "%g"]),
	io__write_string("Not a number:\n"),
	list__foldl(test_floats(is_nan, [0.0 * Inf]), ["%e", "%f", "%g"]).

:- pred test_floats(pred(string)::in(pred(in) is semidet), list(float)::in,
		string::in, io::di, io::uo) is det.

test_floats(IsValid, Floats, FormatString) -->
	list__foldl(test_float(FormatString, IsValid), Floats).


:- pred test_float(string::in, pred(string)::in(pred(in) is semidet),
		float::in, io::di, io::uo) is det.

test_float(FormatString, IsValid, Float) -->
	{ FloatString = string__format(FormatString, [f(Float)]) },
	io__format("%20s: ", [s(FormatString)]),
	( { IsValid(FloatString) }->
		io__write_string("success\n")
	;
		io__write_string("failure '" ++ FloatString ++ "'\n")
	).

:- pred is_infinity(string::in) is semidet.

is_infinity(String) :-
	LowerCaseString = string__to_lower(String),
	( LowerCaseString = "infinity"
	; LowerCaseString = "inf"
	; LowerCaseString = "-infinity"
	; LowerCaseString = "-inf"
	).

:- pred is_nan(string::in) is semidet.

is_nan(String) :-
	LowerCaseString = string__to_lower(String),
	LowerCaseString = "nan".

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
