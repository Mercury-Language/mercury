%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for a special case of Mantis bug #480.
%
% Versions of the Mercury compiler between 2019 Jun 30 and 2019 Aug 7
% could not compile the correct code of get_g_arg below. The reason was that
% a fix for github issue 64 applied on Jun 30 prevented cse_detection.m
% from recognizing that the last two arms of the switch on FG are themselves
% a switch on the argument of g/1, because the inst of FG contains a unique
% component, namely the inst of the argument of f/1 (which is irrelevant
% to whether FG = g(...) can be pulled out of a disjunction).
%
%---------------------------------------------------------------------------%

:- module bug480a.
:- interface.

:- type sub_g
    --->    gs1(int)
    ;       gs2(int).

:- type fg
    --->    f(f1 :: int)
    ;       g(g1 :: sub_g).

:- inst u_fg for fg/0
    --->    f(unique)
    ;       g(ground).

:- mode u_g_fg == u_fg >> ground.

:- pred get_g_arg(fg::u_g_fg, int::out) is det.

:- implementation.

:- import_module int.

get_g_arg(FG, N) :-
    (
        FG = f(_),
        N = 0
    ;
        FG = g(gs1(N))
    ;
        FG = g(gs2(N))
    ).
