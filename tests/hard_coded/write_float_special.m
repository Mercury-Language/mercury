%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Regression test for problems with io.write_float handling special float
% values. In Mercury 14.01 and before ".0" was appended to the string printed
% for special float values, for example:
%
% io.write_float(nan, !IO) ==> "nan.0"
%
%---------------------------------------------------------------------------%

:- module write_float_special.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.
:- import_module list.
:- import_module string.

main(!IO) :-
    Inf = float.max + float.max,
    NegInf = -Inf,
    % Avoid C compiler incorrectly optimising Inf * 0.0 to 0.0.
    Zero = float(length([] : list(float))),
    NaN = Inf * Zero,
    io.write_string("Inf: ", !IO),
    io.write_float(Inf, !IO),
    io.nl(!IO),
    io.write_string("-Inf: ", !IO),
    io.write_float(NegInf, !IO),
    io.nl(!IO),
    io.write_string("NaN: ", !IO),
    io.write_float(NaN, !IO),
    io.nl(!IO).
