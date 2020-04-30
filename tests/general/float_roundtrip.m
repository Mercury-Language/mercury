%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that we roundtrip floats.
%

:- module float_roundtrip.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    test_float(7,  0.9092974, !IO),
    test_float(9,  0.123573124, !IO),
    test_float(15, 0.987654321012345, !IO),
    test_float(17, 0.12345678901234566, !IO),
    test_float_roundtrippable(1.8e-10, !IO).

:- pred test_float(int::in, float::in, io::di, io::uo) is det.

test_float(ReqPrecision, Float, !IO) :-
    FloatStr =
        string.format("%." ++ int_to_string(ReqPrecision) ++ "g", [f(Float)]),
    Precision = string.length(FloatStr) - 2,
    io.format("%-20s: ", [s(FloatStr)], !IO),
    ( if Precision = ReqPrecision then
        ( if roundtrip_float(Float) then
            io.write_string("success.\n", !IO)
        else
            io.write_string("failed.\n", !IO)
        )
    else
        io.write_string("failed as only ", !IO),
        io.write_int(Precision, !IO),
        io.write_string(" digits of precision.\n", !IO)
    ).

:- pred test_float_roundtrippable(float::in, io::di, io::uo) is det.

test_float_roundtrippable(Flt, !IO) :-
    ( if roundtrip_float(Flt) then
        io.format("%-20s: ", [s(string.float_to_string(Flt))], !IO),
        io.write_string("success.\n", !IO)
    else
        io.format("failed to roundtrip %f\n", [f(Flt)], !IO)
    ).

    % Test that when we round-trip the float, we get the same float back.
    %
:- pred roundtrip_float(float::in) is semidet.

roundtrip_float(Float) :-
    float_to_string(Float, String),
    string.to_float(String, Float).
