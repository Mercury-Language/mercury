%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the error for ambiguous overloading.

:- module ambiguous_overloading_error.

:- interface.

:- import_module getopt.
:- import_module io.
:- import_module list.

:- type foo
    --->    f
    ;       g.
:- type bar
    --->    f
    ;       h.

:- pred ambig_overload1(list(foo)::out) is det.

:- type baz
    --->    a1
    ;       a2
    ;       a3.

:- type qux
    --->    a1
    ;       a2
    ;       a4.

:- pred ambig_overload2(list(baz)::out) is det.

:- pred test_lt(int::out) is det.

:- type set_param
    --->    set_print
    ;       set_browse
    ;       set_print_all
    ;       set_flat
    ;       set_raw_pretty
    ;       set_verbose
    ;       set_pretty.

:- pred set_browser_param_from_option_table(option_table(set_param)::in,
    io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module float.

ambig_overload1(L) :-
    A = f, B = f, C = f, D = f, E = f, F = f,
    G = f, H = f, I = f, J = f, K = f, L = f,
    L = [A, B, C, D, E, F, G, H, I, J, K, L].

ambig_overload2(L) :-
    A = a1, B = a1, C = a2, D = a2, E = a1, F = a1, G = a2,
    L = [A, B, C, D, E, F, G].

test_lt(X) :-
    ( if
        X1 < Y1,
        X2 < Y2,
        X3 < Y3,
        X4 < Y4,
        X5 < Y5,
        X6 < Y6,
        X7 < Y7,
        X8 < Y8,
        X9 < Y9,
        XA < YA,
        XB < YB,
        XC < YC,
        XD < YD,
        XE < YE,
        XF < YF,
        XG < YG,
        XH < YH,
        XI < YI,
        XJ < YJ,
        XK < YK,
        XL < YL,
        XM < YM,
        XN < YN,
        X1 = 1, Y1 = 11,
        X2 = 2, Y2 = 12,
        X3 = 3, Y3 = 13,
        X4 = 4.0, Y4 = 14.0,
        X5 = 5.0, Y5 = 15.0,
        X6 = 6.0, Y6 = 16.0,
        X7 = 7.0, Y7 = 17.0,
        X8 = 8.0, Y8 = 18.0,
        X9 = 9.0, Y9 = 19.0,
        XA = 10.0, YA = 20.0,
        XB = 11.0, YB = 21.0,
        XC = 12.0, YC = 22.0,
        XD = 13.0, YD = 23.0,
        XE = 14.0, YE = 24.0,
        XF = 15.0, YF = 25.0,
        XG = 16.0, YG = 26.0,
        XH = 17.0, YH = 27.0,
        XI = 18.0, YI = 28.0,
        XJ = 19.0, YJ = 29.0,
        XK = 20.0, YK = 30.0,
        XL = 21.0, YL = 31.0,
        XM = 22.0, YM = 32.0,
        XN = 23.0, YN = 33.0
    then
        X = 0
    else
        X = 1
    ).

set_browser_param_from_option_table(OptionTable, !IO) :-
    set_browser_param(
        lookup_bool_option(OptionTable, set_print),
        lookup_bool_option(OptionTable, set_browse),
        lookup_bool_option(OptionTable, set_print_all),
        lookup_bool_option(OptionTable, set_flat),
        lookup_bool_option(OptionTable, set_raw_pretty),
        lookup_bool_option(OptionTable, set_verbose),
        lookup_bool_option(OptionTable, set_pretty),
    !IO).

:- pred set_browser_param(bool::in, bool::in, bool::in, bool::in,
    bool::in, bool::in, bool::in, io::di, io::uo) is det.

set_browser_param(_, _, _, _, _, _, _, !IO).
