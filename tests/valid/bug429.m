% vim: ft=mercury ts=4 sw=4 et

:- module bug429.
:- interface.

:- import_module bool.
:- import_module pair.

:- type index
    --->    first
    ;       second.

:- pred clear(index::in, pair(bool)::in, pair(bool)::out) is semidet.

:- implementation.

clear(I, P0, P) :-
    % The cse_detection pass pulls the unification P0 = V_14 - V_13
    % out of the switch on I before determinism analysis checks that
    % the switch on I is complete. The compiler should allow for the scope
    % to contain such pulled-out deconstructions before the switch itself;
    % this test checks whether it does.

    require_complete_switch [I]
    (
        I = first,
        P0 = yes - Y,
        P = no - Y
    ;
        I = second,
        P0 = X - yes,
        P = X - no
    ).
