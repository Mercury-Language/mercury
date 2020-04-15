%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%------------------------------------------------------------------------------%
%
% This test case tests both the occurs check warning and its suppression.
%

:- module occurs.

:- interface.
:- import_module list.

:- type t
    --->    f({int, t}).

:- pred test(list(int)::in, {int, t}::in,
    int::out, int::out, int::out, string::out) is det.

:- implementation.

:- import_module set.

test(A, B, X, Y, Z, MaybeFixpoint) :-
    ( if
        % We should get a warning for this.
        A = [_A0, _A1 | A]
    then
        X = 1
    else
        X = 2
    ),

    ( if
        % We should get a warning for this as well.
        % Unlike the list example above, this involves a cons_id
        % that does not have an entry in the cons_table.
        B = {_B0, f(B)}
    then
        Y = 1
    else
        Y = 2
    ),

    ( if
        disable_warning [suspected_occurs_check_failure] (
            % We should not get a warning for this occurs check
            % violation because the warning has been disabled.
            A = [_A2, _A3, _A4 | A]
        )
    then
        Z = 1
    else
        Z = 2
    ),

    Program = program(0),
    Interpretation = set.make_singleton_set("xyzzy"),
    % We should not get a warning for this occurs check violation
    % because the function symbol involved is not a *data* constructor.
    ( if Interpretation = tp(Program, Interpretation) then
        MaybeFixpoint = "have reached fixpoint"
    else
        MaybeFixpoint = "have not reached fixpoint"
    ).

:- type program
    --->    program(int).

:- type interpretation == set(string).

:- func tp(program, interpretation) = interpretation.

tp(_Program, Interpretation) = Interpretation.
