%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A very basic check of floating point arithmetic and string.to_float.
% Now tests maths library stuff too.
%
%---------------------------------------------------------------------------%

:- module float_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module list.
:- import_module math.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if string.to_float("1234.5678", F) then
        write_message("1234.5678: ", F, !IO)
    else
        io.write_string("can't parse 1234.5678", !IO)
    ),
    test_arithmetic(3.0, 4.0, !IO),
    test_arithmetic(41.0, -3.0, !IO),
    test_constants(!IO),
    test_math_constants(!IO),
    test_rounding(2.7, !IO),
    test_rounding(-3.6, !IO),
    test_power(2.2, !IO),
    test_trig(0.5, !IO),
    test_inv_trig(0.6, !IO).

:- pred write_message(string::in, float::in, io::di, io::uo) is det.

write_message(String, Float, !IO) :-
    io.format("%s%6.3g\n", [s(String), f(Float)], !IO).

%---------------------------------------------------------------------------%

:- pred test_arithmetic(float::in, float::in, io::di, io::uo) is det.

test_arithmetic(X, Y, !IO) :-
    Plus = X + Y,
    Times = X * Y,
    Minus = X - Y,
    Divide = X / Y,
    Pow = math.pow(X, Y),
    write_message("X: ", X, !IO),
    write_message("Y: ", Y, !IO),
    write_message("X + Y: ", Plus, !IO),
    write_message("X * Y: ", Times, !IO),
    write_message("X - Y: ", Minus, !IO),
    write_message("X / Y: ", Divide, !IO),
    write_message("X ^ Y: ", Pow, !IO).

%---------------------------------------------------------------------------%

:- pred test_constants(io::di, io::uo) is det.

test_constants(!IO) :-
    write_message("Float max: ", float.max, !IO),
    write_message("Float min: ", float.min, !IO),
    write_message("Float epsilon: ", float.epsilon, !IO).

%---------------------------------------------------------------------------%

:- pred test_math_constants(io::di, io::uo) is det.

test_math_constants(!IO) :-
    write_message("Pi: ", math.pi, !IO),
    write_message("e: ", math.e, !IO).

%---------------------------------------------------------------------------%

:- pred test_rounding(float::in, io::di, io::uo) is det.

test_rounding(X, !IO) :-
    write_message("X: ", X, !IO),
    write_message("ceil(X): ", math.ceiling(X), !IO),
    write_message("floor(X): ", math.floor(X), !IO),
    write_message("round(X): ", math.round(X), !IO),
    write_message("truncate(X): ", math.truncate(X), !IO).

%---------------------------------------------------------------------------%

:- pred test_power(float::in, io::di, io::uo) is det.

test_power(X, !IO) :-
    write_message("X: ", X, !IO),
    write_message("sqrt(X): ", math.sqrt(X), !IO),
    write_message("ln(X): ", math.ln(X), !IO),
    write_message("log2(X): ", math.log2(X), !IO),
    write_message("log10(X): ", math.log10(X), !IO),
    write_message("log(2.1, X): ", math.log(2.1, X), !IO),
    write_message("exp(X): ", math.exp(X), !IO).

%---------------------------------------------------------------------------%

:- pred test_trig(float::in, io::di, io::uo) is det.

test_trig(X, !IO) :-
    write_message("X: ", X, !IO),
    write_message("sin(X): ", math.sin(X), !IO),
    write_message("cos(X): ", math.cos(X), !IO),
    write_message("tan(X): ", math.tan(X), !IO),
    write_message("sinh(X): ", math.sinh(X), !IO),
    write_message("cosh(X): ", math.cosh(X), !IO),
    write_message("tanh(X): ", math.tanh(X), !IO),
    write_message("atan2(sin(X), cos(X)): ",
        math.atan2(math.sin(X), math.cos(X)), !IO).

%---------------------------------------------------------------------------%

:- pred test_inv_trig(float::in, io::di, io::uo) is det.

test_inv_trig(X, !IO) :-
    write_message("X: ", X, !IO),
    write_message("asin(X): ", math.asin(X), !IO),
    write_message("acos(X): ", math.acos(X), !IO),
    write_message("atan(X): ", math.atan(X), !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
