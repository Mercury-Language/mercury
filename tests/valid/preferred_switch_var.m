%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% The following program tests whether the presence of a require_switch_arms_X
% scope can get the compiler to recognize the switch inside the scope as 
% being an incomplete switch on the named variable, even though it could
% also recognize it as a *complete* switch on another variable.
%
% Normally, switch detection prefers complete switches to incomplete ones,
% but a scope that programmers use to specify the variable that they *expect*
% the switch to be switch on should trump that.
%
%---------------------------------------------------------------------------%

:- module preferred_switch_var.
:- interface.

:- type t
    --->    t1
    ;       t2
    ;       t3
    ;       t4
    ;       t5
    ;       t6
    ;       t7
    ;       t8.

:- type u
    --->    u1
    ;       u2.

:- pred p(t::in, u::in, int::out) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

p(T, U, N) :-
    require_switch_arms_semidet [T]
    (
        T = t1,
        U = u1,
        N = 11
    ;
        T = t2,
        U = u2,
        N = 22
    ;
        T = t3,
        U = u1,
        N = 31
    ;
        T = t4,
        U = u2,
        N = 42
    ).
