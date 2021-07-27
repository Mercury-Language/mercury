%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
%
% Description of bug:
%   The liveness of polymorphic type variables for accurate gc
%   wasn't being computed correctly in the case of if-then-else.
%
% Symptom(s) of bug:
%   A sanity check in liveness.m is failed.
%
%   Software error: branches of if-then-else disagree on liveness
%   Then: HeadVar__3 TypeInfo_for_Y
%   Else: HeadVar__3 TypeInfo_for_X TypeInfo_for_Y
%
%
% Date bug existed: 19-June-1997
% Author: trd

:- module agc_ite.

:- interface.

:- import_module list.

:- pred filter_map(pred(X, Y)::in(pred(in, out) is semidet),
    list(X)::in, list(Y)::out) is det.

:- implementation.

filter_map(_, [],  []).
filter_map(Pred, [Head0 | Tail0], List) :-
    ( if call(Pred, Head0, Head) then
        List = [Head | List1]
    else
        List = List1
    ),
    filter_map(Pred, Tail0, List1).
