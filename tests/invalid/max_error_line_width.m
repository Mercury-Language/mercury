%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module max_error_line_width.  % was ambiguous_overloading_error.

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
:- import_module uint.

ambig_overload1(L) :-
    A = f, B = f, C = f, D = f, E = f, F = f,
    G = f, H = f, I = f, J = f, K = f, L = f,
    L = [A, B, C, D, E, F, G, H, I, J, K, L].

ambig_overload2(L) :-
    A = a1, B = a1, C = a2, D = a2, E = a1, F = a1, G = a2,
    L = [A, B, C, D, E, F, G].

test_lt(X) :-
    ( if
        _ = X1 `unchecked_left_shift` Y1,
        _ = X2 `unchecked_left_shift` Y2,
        _ = X3 `unchecked_left_shift` Y3,
        _ = X4 `unchecked_left_shift` Y4,
        _ = X5 `unchecked_left_shift` Y5,
        _ = X6 `unchecked_left_shift` Y6,
        _ = X7 `unchecked_left_shift` Y7,
        _ = X8 `unchecked_left_shift` Y8,
        _ = X9 `unchecked_left_shift` Y9,
        _ = XA `unchecked_left_shift` YA,
        _ = XB `unchecked_left_shift` YB,
        _ = XC `unchecked_left_shift` YC,
        _ = XD `unchecked_left_shift` YD,
        _ = XE `unchecked_left_shift` YE,
        _ = XF `unchecked_left_shift` YF,
        _ = XG `unchecked_left_shift` YG,
        _ = XH `unchecked_left_shift` YH,
        _ = XI `unchecked_left_shift` YI,
        _ = XJ `unchecked_left_shift` YJ,
        _ = XK `unchecked_left_shift` YK,
        _ = XL `unchecked_left_shift` YL,
        _ = XM `unchecked_left_shift` YM,
        _ = XN `unchecked_left_shift` YN,
        X1 = 1, Y1 = 11,
        X2 = 2, Y2 = 12,
        X3 = 3, Y3 = 13,
        X4 = 4u, Y4 = 14u,
        X5 = 5u, Y5 = 15u,
        X6 = 6u, Y6 = 16u,
        X7 = 7u, Y7 = 17u,
        X8 = 8u, Y8 = 18u,
        X9 = 9u, Y9 = 19u,
        XA = 10u, YA = 20u,
        XB = 11u, YB = 21u,
        XC = 12u, YC = 22u,
        XD = 13u, YD = 23u,
        XE = 14u, YE = 24u,
        XF = 15u, YF = 25u,
        XG = 16u, YG = 26u,
        XH = 17u, YH = 27u,
        XI = 18u, YI = 28u,
        XJ = 19u, YJ = 29u,
        XK = 20u, YK = 30u,
        XL = 21u, YL = 31u,
        XM = 22u, YM = 32u,
        XN = 23u, YN = 33u
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
