%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests that the compiler's constant propagation for
% floating point division produce the same value as that produced
% by doing the division at runtime.
% This is a regression test; versions of the compiler prior to Nov 2002
% failed this test at -O3 and higher.
%

:- module float_consistency.
:- interface.
:- import_module io.
:- pred main(state::di, state::uo) is det.

:- implementation.

:- import_module float.
:- import_module string.

main(!IO) :-
    Lit_one = 1.0,
    Calc_one = same_as(Lit_one),
    print("Calc_one     = Lit_one:     " ++
        (if Calc_one = Lit_one then "true" else "false"), !IO),
    nl(!IO),
    print("Calc_one/9.0 = Lit_one/9.0: " ++
        ( if
            unchecked_quotient(Calc_one, 9.0)
                = unchecked_quotient(Lit_one, 9.0)
        then
            "true"
        else
            "false"), !IO),
    nl(!IO).

:- func same_as(float) = float.
:- pragma no_inline(same_as/1).

same_as(X) = X.
