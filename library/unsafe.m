%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1997, 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: unsafe.m
% Author: fjh
% Stability: low
%-----------------------------------------------------------------------------%
%
% WARNING: the procedures defined in this module are non-logical.
% They may have side effects, they may violate type safety,
% they may interfere with certain memory management strategies,
% and in general they may do lots of nasty things.
% They may not work with future release of the Mercury compiler,
% or with other Mercury implementations.
% Use only as a last resort, and only with great care!
%
% You have been warned.

%-----------------------------------------------------------------------------%

:- module unsafe.
:- interface.
:- import_module io.

    % unsafe_perform_io/1 performs I/O, in an unsafe manner.
    % It can be used to call a goal that does I/O or has
    % side effects from a context where you do not have an io__state.
    % It can be useful for printf-style debugging.
    % But backtracking over a call to `unsafe_perform_io'
    % can be very dangerous indeed, because with certain
    % memory allocation policies it can result in dangling pointers.
    %
:- impure pred unsafe_perform_io(pred(io__state, io__state)).
:- mode unsafe_perform_io(pred(di, uo) is det) is det.
:- mode unsafe_perform_io(pred(di, uo) is cc_multi) is det.

    % The function unsafe_promise_ground/1 can be used to assert to the
    % compiler that a particular value of inst `any' is in fact ground.
    % The assertion is *not* checked.  If it is false, all hell may break out.
    %
:- func unsafe_promise_ground(T::in(any)) = (T::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    unsafe_promise_ground(X::in(any)) = (Y::out),
    [will_not_call_mercury, promise_pure],
"
    Y = X;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    unsafe_perform_io(P::(pred(di, uo) is det)),
    [may_call_mercury],
"{
    call_io_pred_det(P);
}").
:- pragma foreign_proc("C",
    unsafe_perform_io(P::(pred(di, uo) is cc_multi)),
    [may_call_mercury],
"{
    call_io_pred_cc_multi(P);
}").

:- pred call_io_pred(pred(io__state, io__state), io__state, io__state).
:- mode call_io_pred(pred(di, uo) is det, di, uo) is det.
:- mode call_io_pred(pred(di, uo) is cc_multi, di, uo) is cc_multi.

:- pragma export(call_io_pred(pred(di, uo) is det, di, uo),
    "call_io_pred_det").
:- pragma export(call_io_pred(pred(di, uo) is cc_multi, di, uo),
    "call_io_pred_cc_multi").

call_io_pred(P, !IO) :-
    P(!IO).

%-----------------------------------------------------------------------------%
