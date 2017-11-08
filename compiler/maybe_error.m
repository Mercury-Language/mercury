%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module parse_tree.maybe_error.
:- interface.

:- import_module parse_tree.error_util.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type maybe_safe_to_continue
    --->    safe_to_continue
    ;       unsafe_to_continue.

%-----------------------------------------------------------------------------%

:- type maybe1(T1)
    --->    error1(list(error_spec))
    ;       ok1(T1).

:- type maybe2(T1, T2)
    --->    error2(list(error_spec))
    ;       ok2(T1, T2).

:- type maybe3(T1, T2, T3)
    --->    error3(list(error_spec))
    ;       ok3(T1, T2, T3).

:- type maybe4(T1, T2, T3, T4)
    --->    error4(list(error_spec))
    ;       ok4(T1, T2, T3, T4).

:- inst maybe1(I) for maybe1/1
    --->    error1(ground)
    ;       ok1(I).

:- inst maybe2(I1, I2) for maybe2/2
    --->    error2(ground)
    ;       ok2(I1, I2).

:- inst maybe3(I1, I2, I3) for maybe3/3
    --->    error3(ground)
    ;       ok3(I1, I2, I3).

:- inst maybe4(I1, I2, I3, I4) for maybe4/4
    --->    error4(ground)
    ;       ok4(I1, I2, I3, I4).

:- func get_any_errors1(maybe1(T1)) = list(error_spec).
:- func get_any_errors2(maybe2(T1, T2)) = list(error_spec).
:- func get_any_errors3(maybe3(T1, T2, T3)) = list(error_spec).
:- func get_any_errors4(maybe4(T1, T2, T3, T4)) = list(error_spec).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

get_any_errors1(ok1(_)) = [].
get_any_errors1(error1(Specs)) = Specs.

get_any_errors2(ok2(_, _)) = [].
get_any_errors2(error2(Specs)) = Specs.

get_any_errors3(ok3(_, _, _)) = [].
get_any_errors3(error3(Specs)) = Specs.

get_any_errors4(ok4(_, _, _, _)) = [].
get_any_errors4(error4(Specs)) = Specs.

%-----------------------------------------------------------------------------%
:- end_module parse_tree.maybe_error.
%-----------------------------------------------------------------------------%
