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
	io__write_string("Plus Infinity:\n"),
	list__foldl(test_floats(is_plus_infinity, [Inf]), ["%e", "%f", "%g"]),
	io__write_string("Minus Infinity:\n"),
	list__foldl(test_floats(is_minus_infinity, [-Inf]),
		["%e", "%f", "%g"]),
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

:- pred is_plus_infinity(string::in) is semidet.

is_plus_infinity(String) :-
	LowerCaseString = string__to_lower(String),
	( LowerCaseString = "infinity"
	; LowerCaseString = "inf"
	).

:- pred is_minus_infinity(string::in) is semidet.

is_minus_infinity(String) :-
	LowerCaseString = string__to_lower(String),
	( LowerCaseString = "-infinity"
	; LowerCaseString = "-inf"
	).

:- pred is_nan(string::in) is semidet.

is_nan(String) :-
	LowerCaseString = string__to_lower(String),
	( LowerCaseString = "nan"
	% XXX Actually, it makes no sense to put a minus sign on a NaN,
	%     since NaNs aren't signed.  However, the printf() function in
	%     some C libraries (in particular, the one for Solaris 2.7)
	%     do that.  Arguably that's a bug, but we can't do much about
	%     bugs in the Solaris C library, so we don't want to report a
	%     test case failure for that.  Hence we allow -NaN here.
	; LowerCaseString = "-nan"
	).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
