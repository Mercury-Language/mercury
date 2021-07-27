%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module negation_in_dupl_for_switch.
:- interface.

:- type f
    --->    f1
    ;       f2
    ;       f3
    ;       f4.

:- pred test(f::in, int::in, int::in, int::out) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module int.

:- type g
    --->    g1
    ;       g2
    ;       g3
    ;       g4.

test(F, A, B, Result) :-
    (
        (
            F = f1,
            G = g1,
            C = A + B
        ;
            F = f2,
            G = g2,
            C = A - B
        ;
            F = f3,
            G = g3,
            C = A << B
        ),
        ( if
            C > 10,
            % Clearly, this *should* be a semidet test, but the compiler
            % will create a separate copy of this if-then-else for each
            % of the three switch arms F=f1, F=f2, and F=f3, and in each
            % of those copies, the negated goal's outcome *will* be known.
            % However, since the known outcome will be different in different
            % copies, the compiler should *not* generate a warning for this
            % goal.
            not G = g3
        then
            Result = C - B
        else
            Result = B - C
        )
    ;
        F = f4,
        Result = A + B
    ).
