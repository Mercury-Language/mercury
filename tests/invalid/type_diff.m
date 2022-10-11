%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_diff.
:- interface.

:- type tx(T)
    --->    tx1(int, float, T).

:- type tx(T, U)
    --->    tx2(T, U).

:- pred p1(tx(string)::in) is semidet.

%---------------------%

:- type ho
    --->    ho_p((impure (pred(int::in, int::in, int::out) is det)))
    ;       ho_f((func(int::in, int::in) = (int::out)) is det).

:- inst ho
    --->    ho_p(pred(in, in, out) is det)
    ;       ho_f((func(in, in) = out) is det).

:- pred p2(ho::in(ho), int::in, int::in, int::out) is semidet.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module int.
:- import_module list.

%---------------------------------------------------------------------------%

p1(A) :-
    q1(A),
    q1_list([A]),
    q1_list_list([[A]]).

%---------------------%

:- pred q1(tx(T, U)::in) is semidet.

q1(tx2(_, _)).
% This would get a determinism warning *if* the compilation got that far.

%---------------------%

:- pred q1_list(list(tx(T, U))::in) is semidet.

q1_list([]).
q1_list([X | Xs]) :-
    q1(X),
    q1_list(Xs).

%---------------------%

:- pred q1_list_list(list(list(tx(T, U)))::in) is semidet.

q1_list_list([]).
q1_list_list([X | Xs]) :-
    q1_list(X),
    q1_list_list(Xs).

%---------------------------------------------------------------------------%

p2(HO, A, B, Z) :-
    (
        HO = ho_p(Pred),
        Pred(A, B, C),
        promise_pure (
            impure Pred(A, B),
            impure Pred(A, B, C, D),
            impure E : int = Pred(C, D)         % FUNC_CALL
        ),
        expect_pred_2_det(Pred, 1, Z1),
        expect_pred_3_semidet(Pred, 2, Z2),     % NO_HO_INST
        expect_func_2(Pred, 3, Z3),
        expect_func_3(Pred, 4, Z4),
        Z = E + Z1 + Z2 + Z3 + Z4
    ;
        HO = ho_f(Func),
        C = Func(A, B),
        promise_pure (
            impure B = Func(A),                 % FUNC_CALL
            impure D = Func(A, B, C),           % FUNC_CALL
            impure Func(C, D, E)
        ),
        expect_pred_2_det(Func, 1, Z1),
        expect_pred_3_semidet(Func, 2, Z2),
        expect_func_2(Func, 3, Z3),
        expect_func_3(Func, 4, Z4),
        Z = E + Z1 + Z2 + Z3 + Z4
    ).
    % Right now, we don't generate error messages that specify exact
    % differences for the lines marked FUNC_CALL. The reason is that
    % the code in typecheck_errors.m that deals with function calls
    % also has to deal with ordinary first-order unifications as well,
    % and this makes it harder to figure out when printing those differences
    % would be more likely to help than hinder.
    %
    % Right now, we also don't generate error messages that specify exact
    % differences for the lines marked NO_HO_INST. The reason is that
    % the types of the higher-order arguments in the expect_* predicates below 
    % don't contain higher-order inst information, which means that
    % typecheck_errors.m cannot diagnose errors in the higher-order inst
    % information of the actual argument in that position.

%---------------------------------------------------------------------------%

:- pred expect_pred_2_det(
    pred(int, int)::in(pred(in, out) is det),
    int::in, int::out) is det.

expect_pred_2_det(Pred, A, Z) :-
    Pred(A, Z).

:- pred expect_pred_3_semidet(
    pred(int, int, int)::in(pred(in, in, out) is semidet),
    int::in, int::out) is det.

expect_pred_3_semidet(Pred, A, Z) :-
    ( if Pred(A, A, ZPrime) then
        Z = ZPrime
    else
        Z = 42
    ).

%---------------------------------------------------------------------------%

:- pred expect_func_2((func(int, int) = int)::in,
    int::in, int::out) is det.

expect_func_2(Func, A, Z) :-
    Z = Func(A, A).

:- pred expect_func_3((func(int, int, int) = int)::in,
    int::in, int::out) is det.

expect_func_3(Func, A, Z) :-
    Z = Func(A, A, A).

%---------------------------------------------------------------------------%
