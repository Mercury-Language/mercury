%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Since A occurs earlier in the body of oops than B, the old switch detection
% algorithm first tried to see whether the disjunction inside the
% require_complete_switch scope was a switch on A, before it tried B.
% Since the disjunction CAN be seen as a switch on A, it saw it that way,
% which (after the changes committed on 2016 april 19) generated an error.
%
% This is a test for the fix: when a disjunction is a subgoal of a scope
% that requires it to be a switch on a given variable, always start testing
% whether the disjunction is a switch on a variable with the variable named
% by the scope.

:- module required_var.
:- interface.

:- type xy
    --->    x
    ;       y(int).

:- pred oops(xy::in, xy::in, int::out) is det.

:- implementation.

:- import_module int.

oops(A, B, Result) :-
    % The compiler can see the disjunction either as a switch on A
    % with a switch on B in each arm, or as a switch on B with a switch on A
    % in each arm. Due to the require_complete_switch scope, it should prefer
    % the latter, even though A will have a lower variable number than B.
    require_complete_switch [B]
    (
        A = x,
        B = x,
        Result = 0
    ;
        A = x,
        B = y(BN),
        Result = BN
    ;
        A = y(AN),
        B = x,
        Result = AN
    ;
        A = y(AN),
        B = y(BN),
        Result = AN + BN
    ).
