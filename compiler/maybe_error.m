%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015, 2017, 2021-2024, 2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module parse_tree.maybe_error.
:- interface.

:- import_module parse_tree.error_spec.

:- import_module list.
:- import_module one_or_more.

%---------------------------------------------------------------------------%

:- type maybe_safe_to_continue
    --->    safe_to_continue
    ;       unsafe_to_continue.

%---------------------------------------------------------------------------%

:- type maybe_found
    --->    not_found
    ;       found.

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

:- type maybe6(T1, T2, T3, T4, T5, T6, E)
    --->    error6(E)
    ;       ok6(T1, T2, T3, T4, T5, T6).

:- type maybe1(T1) ==
    maybe1(T1, one_or_more(err_spec)).
:- type maybe2(T1, T2) ==
    maybe2(T1, T2, one_or_more(err_spec)).
:- type maybe3(T1, T2, T3) ==
    maybe3(T1, T2, T3, one_or_more(err_spec)).
:- type maybe4(T1, T2, T3, T4) ==
    maybe4(T1, T2, T3, T4, one_or_more(err_spec)).
:- type maybe5(T1, T2, T3, T4, T5) ==
    maybe5(T1, T2, T3, T4, T5, one_or_more(err_spec)).
:- type maybe6(T1, T2, T3, T4, T5, T6) ==
    maybe6(T1, T2, T3, T4, T5, T6, one_or_more(err_spec)).

:- type maybe1el(T1) ==
    maybe1(T1, list(err_spec)).
:- type maybe2el(T1, T2) ==
    maybe2(T1, T2, list(err_spec)).
:- type maybe3el(T1, T2, T3) ==
    maybe3(T1, T2, T3, list(err_spec)).
:- type maybe4el(T1, T2, T3, T4) ==
    maybe4(T1, T2, T3, T4, list(err_spec)).

:- type maybe1eld(T1) ==
    maybe1(T1, list(diag_spec)).
:- type maybe2eld(T1, T2) ==
    maybe2(T1, T2, list(diag_spec)).
:- type maybe3eld(T1, T2, T3) ==
    maybe3(T1, T2, T3, list(diag_spec)).
:- type maybe4eld(T1, T2, T3, T4) ==
    maybe4(T1, T2, T3, T4, list(diag_spec)).

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

:- inst maybe6(I1, I2, I3, I4, I5, I6) for maybe6/7
    --->    error6(ground)
    ;       ok6(I1, I2, I3, I4, I5, I6).

%---------------------%

    % This type, which is used mostly by code that parses goals,
    % allows us to record both
    % - the presence of at least one error, and
    % - the presence of zero or more warnings.
    % The reason why we use tuples is that any readable function symbol
    % we could use would make the lines that construct errors too long.
:- type err_warn_error == {one_or_more(err_spec), list(warn_spec)}.

:- type parse_result1(T) ==
    maybe2(T, list(warn_spec), err_warn_error).
:- type parse_result2(T1, T2) ==
    maybe3(T1, T2, list(warn_spec), err_warn_error).
:- type parse_result3(T1, T2, T3) ==
    maybe4(T1, T2, T3, list(warn_spec), err_warn_error).

%---------------------%

:- func get_any_errors1(maybe1(T1)) = list(err_spec).
:- func get_any_errors2(maybe2(T1, T2)) = list(err_spec).
:- func get_any_errors3(maybe3(T1, T2, T3)) = list(err_spec).
:- func get_any_errors4(maybe4(T1, T2, T3, T4)) = list(err_spec).
:- func get_any_errors5(maybe5(T1, T2, T3, T4, T5)) = list(err_spec).
:- func get_any_errors6(maybe6(T1, T2, T3, T4, T5, T6)) = list(err_spec).

:- func get_any_errors1el(maybe1el(T1)) = list(err_spec).
:- func get_any_errors2el(maybe2el(T1, T2)) = list(err_spec).
:- func get_any_errors3el(maybe3el(T1, T2, T3)) = list(err_spec).
:- func get_any_errors4el(maybe4el(T1, T2, T3, T4)) = list(err_spec).

:- func get_any_errors1eld(maybe1eld(T1)) = list(diag_spec).
:- func get_any_errors2eld(maybe2eld(T1, T2)) = list(diag_spec).
:- func get_any_errors3eld(maybe3eld(T1, T2, T3)) = list(diag_spec).
:- func get_any_errors4eld(maybe4eld(T1, T2, T3, T4)) = list(diag_spec).

%---------------------------------------------------------------------------%

:- func get_any_errors_warnings2(maybe2(T1, list(warn_spec))) =
    list(diag_spec).
:- func get_any_errors_warnings3(maybe3(T1, T2, list(warn_spec))) =
    list(diag_spec).
:- func get_any_errors_warnings4(maybe4(T1, T2, T3, list(warn_spec))) =
    list(diag_spec).

:- pred get_all_errors_warnings1(
    maybe1(T1, err_warn_error)::in,
    list(err_spec)::out, list(warn_spec)::out) is det.

:- pred get_all_errors_warnings2(
    maybe2(T1, list(warn_spec), err_warn_error)::in,
    list(err_spec)::out, list(warn_spec)::out) is det.
:- pred get_all_errors_warnings3(
    maybe3(T1, T2, list(warn_spec), err_warn_error)::in,
    list(err_spec)::out, list(warn_spec)::out) is det.
:- pred get_all_errors_warnings4(
    maybe4(T1, T2, T3, list(warn_spec), err_warn_error)::in,
    list(err_spec)::out, list(warn_spec)::out) is det.

%---------------------------------------------------------------------------%

:- pred add_warns_to_err_warn_error(err_warn_error::in, list(warn_spec)::in,
    err_warn_error::out) is det.

%---------------------------------------------------------------------------%

:- pred project_ok1(maybe1(T1)::in, T1::out) is semidet.
:- pred det_project_ok1(maybe1(T1)::in, T1::out) is det.

:- pred separate_ok1_error1(list(maybe1(T1))::in,
    list(T1)::out, list(err_spec)::out) is det.

%---------------------------------------------------------------------------%

:- pred maybe1_to_maybe1el(maybe1(T)::in, maybe1el(T)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

get_any_errors1(ok1(_)) = [].
get_any_errors1(error1(OoMSpecs)) = one_or_more_to_list(OoMSpecs).

get_any_errors2(ok2(_, _)) = [].
get_any_errors2(error2(OoMSpecs)) = one_or_more_to_list(OoMSpecs).

get_any_errors3(ok3(_, _, _)) = [].
get_any_errors3(error3(OoMSpecs)) = one_or_more_to_list(OoMSpecs).

get_any_errors4(ok4(_, _, _, _)) = [].
get_any_errors4(error4(OoMSpecs)) = one_or_more_to_list(OoMSpecs).

get_any_errors5(ok5(_, _, _, _, _)) = [].
get_any_errors5(error5(OoMSpecs)) = one_or_more_to_list(OoMSpecs).

get_any_errors6(ok6(_, _, _, _, _, _)) = [].
get_any_errors6(error6(OoMSpecs)) = one_or_more_to_list(OoMSpecs).

%---------------------%

get_any_errors1el(ok1(_)) = [].
get_any_errors1el(error1(Specs)) = Specs.

get_any_errors2el(ok2(_, _)) = [].
get_any_errors2el(error2(Specs)) = Specs.

get_any_errors3el(ok3(_, _, _)) = [].
get_any_errors3el(error3(Specs)) = Specs.

get_any_errors4el(ok4(_, _, _, _)) = [].
get_any_errors4el(error4(Specs)) = Specs.

%---------------------%

get_any_errors1eld(ok1(_)) = [].
get_any_errors1eld(error1(Specs)) = Specs.

get_any_errors2eld(ok2(_, _)) = [].
get_any_errors2eld(error2(Specs)) = Specs.

get_any_errors3eld(ok3(_, _, _)) = [].
get_any_errors3eld(error3(Specs)) = Specs.

get_any_errors4eld(ok4(_, _, _, _)) = [].
get_any_errors4eld(error4(Specs)) = Specs.

%---------------------%

get_any_errors_warnings2(ok2(_, WarnSpecs)) = coerce(WarnSpecs).
get_any_errors_warnings2(error2(OoMErrSpecs)) =
    coerce(one_or_more_to_list(OoMErrSpecs)).

get_any_errors_warnings3(ok3(_, _, WarnSpecs)) = coerce(WarnSpecs).
get_any_errors_warnings3(error3(OoMErrSpecs)) =
    coerce(one_or_more_to_list(OoMErrSpecs)).

get_any_errors_warnings4(ok4(_, _, _, WarnSpecs)) = coerce(WarnSpecs).
get_any_errors_warnings4(error4(OoMErrSpecs)) =
    coerce(one_or_more_to_list(OoMErrSpecs)).

%---------------------%

get_all_errors_warnings1(ok1(_), [], []).
get_all_errors_warnings1(error1({OoMErrSpecs, WarnSpecs}),
        ErrSpecs, WarnSpecs) :-
    ErrSpecs = one_or_more_to_list(OoMErrSpecs).

%---------------------%

get_all_errors_warnings2(ok2(_, WarnSpecs), [], WarnSpecs).
get_all_errors_warnings2(error2({OoMErrSpecs, WarnSpecs}),
        ErrSpecs, WarnSpecs) :-
    ErrSpecs = one_or_more_to_list(OoMErrSpecs).

get_all_errors_warnings3(ok3(_, _, WarnSpecs), [], WarnSpecs).
get_all_errors_warnings3(error3({OoMErrSpecs, WarnSpecs}),
        ErrSpecs, WarnSpecs) :-
    ErrSpecs = one_or_more_to_list(OoMErrSpecs).

get_all_errors_warnings4(ok4(_, _, _, WarnSpecs), [], WarnSpecs).
get_all_errors_warnings4(error4({OoMErrSpecs, WarnSpecs}),
        ErrSpecs, WarnSpecs) :-
    ErrSpecs = one_or_more_to_list(OoMErrSpecs).

%---------------------%

add_warns_to_err_warn_error(ErrWarnError0, NewWarnSpecs, ErrWarnError) :-
    ErrWarnError0 = {OoMErrSpecs0, WarnSpecs0},
    ErrWarnError = {OoMErrSpecs0, NewWarnSpecs ++ WarnSpecs0}.

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
    list(T1)::in, list(T1)::out, list(err_spec)::in, list(err_spec)::out)
    is det.

separate_ok1_error1_loop([], !RevOKs, !Specs).
separate_ok1_error1_loop([Maybe | Maybes], !RevOKs, !Specs) :-
    (
        Maybe = ok1(OK),
        !:RevOKs = [OK | !.RevOKs]
    ;
        Maybe = error1(OoMCurSpecs),
        !:Specs = one_or_more_to_list(OoMCurSpecs) ++ !.Specs
    ),
    separate_ok1_error1_loop(Maybes, !RevOKs, !Specs).

%---------------------------------------------------------------------------%

maybe1_to_maybe1el(Maybe, MaybeEl) :-
    (
        Maybe = ok1(T),
        MaybeEl = ok1(T)
    ;
        Maybe= error1(OoM),
        MaybeEl = error1(one_or_more_to_list(OoM))
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.maybe_error.
%---------------------------------------------------------------------------%
