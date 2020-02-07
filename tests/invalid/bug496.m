%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% This module compiles successfully with Mercury 14.01.1 but using Mercury
% 20.01 results in a determinism error.
%---------------------------------------------------------------------------%

:- module bug496.
:- interface.

:- import_module bool.

:- type action
    --->    action_canonicalise
    ;       action_split.

:- type maybe_action
    --->    action_ok(action)
    ;       action_unspecified
    ;       action_conflict(string).

:- pred options_to_action(bool::in, bool::in, maybe_action::out) is det.

:- implementation.

:- import_module list.
:- import_module string.

options_to_action(Canonicalise, SplitSolns, MaybeAction) :-
    some [!Actions] (
        !:Actions = [],
        (
            Canonicalise = yes,
            !:Actions = [action_canonicalise | !.Actions]
        ;
            Canonicalise = no
        ),
        (
            SplitSolns = yes,
            !:Actions = [action_split | !.Actions]
        ;
            SplitSolns = no
        ),
        AllActions = !.Actions
    ),
    (
        AllActions = [],
        MaybeAction = action_unspecified
    ;
        AllActions = [Action],
        MaybeAction = action_ok(Action)
    ;
        AllActions = [_, _ | _],
        MaybeAction = action_conflict("")
    ).
    %%% This version works.
    /*
    (
        AllActions = [],
        MaybeAction = action_unspecified
    ;
        AllActions = [Action | OtherActions],
        (
            OtherActions = [],
            MaybeAction = action_ok(Action)
        ;
            OtherActions = [_ | _],
            MaybeAction = action_conflict("")
        )
    ).
    */
