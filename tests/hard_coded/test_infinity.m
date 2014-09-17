%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%------------------------------------------------------------------------------%
% Test the constant function float.infinity/0.
%------------------------------------------------------------------------------%

:- module test_infinity.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module list.
:- import_module math.
:- import_module string.

%------------------------------------------------------------------------------%

main(!IO) :-

   % Test io.write_float with infinity.
   %
   io.write_string("write_float(infinity) = ", !IO),
   io.write_float(infinity, !IO),
   io.nl(!IO),
   io.write_string("write_float(-infinity) = ", !IO),
   io.write_float(-infinity, !IO),
   io.nl(!IO),
  
   % Test format with infinity.
   %
   io.format("to %f and beyond!\n", [f(infinity)], !IO),
   io.format("format(-infinity) = %f\n", [f(-infinity)], !IO),

   % Test float classification predicates with infinity.
   %
   io.format("is_infinite(infinity) = %s\n",
       [s(string(pred_to_bool(is_infinite(infinity))))], !IO),
   io.format("is_infinite(-infinity) = %s\n",
       [s(string(pred_to_bool(is_infinite(-infinity))))], !IO),
   
   io.format("is_finite(infinity) = %s\n",
       [s(string(pred_to_bool(is_finite(infinity))))], !IO),
   io.format("is_finite(-infinity) = %s\n",
       [s(string(pred_to_bool(is_finite(-infinity))))], !IO),
   
   io.format("is_nan(infinity) = %s\n",
       [s(string(pred_to_bool(is_nan(infinity))))], !IO),
   io.format("is_nan(-infinity) = %s\n",
       [s(string(pred_to_bool(is_nan(-infinity))))], !IO),

   io.format("is_zero(infinity) = %s\n",
       [s(string(pred_to_bool(is_nan(infinity))))], !IO),
   io.format("is_zero(-infinity) = %s\n",
       [s(string(pred_to_bool(is_zero(-infinity))))], !IO),

    % Test abs with infinities.
    %
    io.format("abs(infinity) = %f\n", [f(abs(infinity))], !IO),
    io.format("abs(-infinity) = %f\n", [f(abs(-infinity))], !IO),

    % Test for pow.
    % XXX most other uses of infinity with pow currently cause a domain error
    % -- this is at variance with what IEEE 754-2008 says should happen in many
    % cases.
    io.format("pow(infinity, 0) = %f\n", [f(pow(infinity, 0))], !IO).

%------------------------------------------------------------------------------%
:- end_module test_infinity.
%------------------------------------------------------------------------------%
