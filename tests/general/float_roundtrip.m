% Test that we roundtrip floats.
:- module float_roundtrip.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list, string.

main -->
	test_float(7,  0.9092974),
	test_float(9,  0.123573124),
	test_float(15, 0.987654321012345),
	test_float(17, 0.12345678901234566).

:- pred test_float(int::in, float::in, io::di, io::uo) is det.

test_float(ReqPrecision, Float) -->
	{ FloatStr = string__format("%." ++ int_to_string(ReqPrecision) ++ "g",
			[f(Float)]) },
	{ Precision = string__length(FloatStr) - 2 },
	io__format("%-20s: ", [s(FloatStr)]), 
	( { Precision = ReqPrecision } ->
		( { roundtrip_float(Float) } ->
			io__write_string("success.\n")
		;
			io__write_string("failed.\n")
		)
	;
		io__write_string("failed as only "),
		io__write_int(Precision),
		io__write_string(" digits of precision.\n")
	).

	% Test that when we round-trip the float that we get the same float
	% back.
:- pred roundtrip_float(float::in) is semidet.

roundtrip_float(Float) :-
	float_to_string(Float, String),
	string__to_float(String, Float).
