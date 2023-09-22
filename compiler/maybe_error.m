%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module parse_tree.maybe_error.
:- interface.

:- import_module parse_tree.error_spec.

:- import_module list.

%---------------------------------------------------------------------------%

:- type maybe_safe_to_continue
    --->    safe_to_continue
    ;       unsafe_to_continue.

%---------------------------------------------------------------------------%

:- type maybe1(T1, E)
    --->    error1(E)
    ;       ok1(T1).

:- type maybe2(T1, T2, E)
    --->    error2(E)
    ;       ok2(T1, T2).

:- type maybe3(T1, T2, T3, E)
    --->    error3(E)
    ;       ok3(T1, T2, T3).

:- type maybe4(T1, T2, T3, T4, E)
    --->    error4(E)
    ;       ok4(T1, T2, T3, T4).

:- type maybe5(T1, T2, T3, T4, T5, E)
    --->    error5(E)
    ;       ok5(T1, T2, T3, T4, T5).

:- type maybe1(T1) ==
    maybe1(T1, list(error_spec)).
:- type maybe2(T1, T2) ==
    maybe2(T1, T2, list(error_spec)).
:- type maybe3(T1, T2, T3) ==
    maybe3(T1, T2, T3, list(error_spec)).
:- type maybe4(T1, T2, T3, T4) ==
    maybe4(T1, T2, T3, T4, list(error_spec)).
:- type maybe5(T1, T2, T3, T4, T5) ==
    maybe5(T1, T2, T3, T4, T5, list(error_spec)).

%---------------------%

:- inst maybe1(I) for maybe1/2
    --->    error1(ground)
    ;       ok1(I).

:- inst maybe2(I1, I2) for maybe2/3
    --->    error2(ground)
    ;       ok2(I1, I2).

:- inst maybe3(I1, I2, I3) for maybe3/4
    --->    error3(ground)
    ;       ok3(I1, I2, I3).

:- inst maybe4(I1, I2, I3, I4) for maybe4/5
    --->    error4(ground)
    ;       ok4(I1, I2, I3, I4).

:- inst maybe5(I1, I2, I3, I4, I5) for maybe5/6
    --->    error5(ground)
    ;       ok5(I1, I2, I3, I4, I5).

%---------------------%

:- func get_any_errors1(maybe1(T1)) = list(error_spec).
:- func get_any_errors2(maybe2(T1, T2)) = list(error_spec).
:- func get_any_errors3(maybe3(T1, T2, T3)) = list(error_spec).
:- func get_any_errors4(maybe4(T1, T2, T3, T4)) = list(error_spec).
:- func get_any_errors5(maybe5(T1, T2, T3, T4, T5)) = list(error_spec).

:- func get_any_errors_warnings2(maybe2(T1, list(warning_spec))) =
    list(error_spec).
:- func get_any_errors_warnings3(maybe3(T1, T2, list(warning_spec))) =
    list(error_spec).
:- func get_any_errors_warnings4(maybe4(T1, T2, T3, list(warning_spec))) =
    list(error_spec).
:- func get_any_errors_warnings5(maybe5(T1, T2, T3, T4, list(warning_spec))) =
    list(error_spec).

:- pred project_ok1(maybe1(T1)::in, T1::out) is semidet.
:- pred det_project_ok1(maybe1(T1)::in, T1::out) is det.

:- pred separate_ok1_error1(list(maybe1(T1))::in,
    list(T1)::out, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

get_any_errors1(ok1(_)) = [].
get_any_errors1(error1(Specs)) = Specs.

get_any_errors2(ok2(_, _)) = [].
get_any_errors2(error2(Specs)) = Specs.

get_any_errors3(ok3(_, _, _)) = [].
get_any_errors3(error3(Specs)) = Specs.

get_any_errors4(ok4(_, _, _, _)) = [].
get_any_errors4(error4(Specs)) = Specs.

get_any_errors5(ok5(_, _, _, _, _)) = [].
get_any_errors5(error5(Specs)) = Specs.

%---------------------%

get_any_errors_warnings2(ok2(_, Specs)) = Specs.
get_any_errors_warnings2(error2(Specs)) = Specs.

get_any_errors_warnings3(ok3(_, _, Specs)) = Specs.
get_any_errors_warnings3(error3(Specs)) = Specs.

get_any_errors_warnings4(ok4(_, _, _, Specs)) = Specs.
get_any_errors_warnings4(error4(Specs)) = Specs.

get_any_errors_warnings5(ok5(_, _, _, _, Specs)) = Specs.
get_any_errors_warnings5(error5(Specs)) = Specs.

%---------------------%

project_ok1(Maybe1, Item) :-
    (
        Maybe1 = ok1(Item)
    ;
        Maybe1 = error1(_Specs),
        fail
    ).

det_project_ok1(Maybe1, Item) :-
    (
        Maybe1 = ok1(Item)
    ;
        Maybe1 = error1(_Specs),
        unexpected($pred, "error1")
    ).

%---------------------%

separate_ok1_error1(Maybes, OKs, Specs) :-
    separate_ok1_error1_loop(Maybes, [], RevOKs, [], Specs),
    list.reverse(RevOKs, OKs).

:- pred separate_ok1_error1_loop(list(maybe1(T1))::in,
    list(T1)::in, list(T1)::out, list(error_spec)::in, list(error_spec)::out)
    is det.

separate_ok1_error1_loop([], !RevOKs, !Specs).
separate_ok1_error1_loop([Maybe | Maybes], !RevOKs, !Specs) :-
    (
        Maybe = ok1(OK),
        !:RevOKs = [OK | !.RevOKs]
    ;
        Maybe = error1(CurSpecs),
        !:Specs = CurSpecs ++ !.Specs
    ),
    separate_ok1_error1_loop(Maybes, !RevOKs, !Specs).

%---------------------------------------------------------------------------%
:- end_module parse_tree.maybe_error.
%---------------------------------------------------------------------------%
