%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Author: Ralph Becket <rafe@cs.mu.oz.au>
%
% This is a regression test for a bug involving the interaction of the mode
% system and quantification, triggered by the state variable transformation.
%
%---------------------------------------------------------------------------%

:- module state_var_mode_bug.

:- interface.

:- import_module bool.
:- import_module int.

:- pred p(bool::in, bool::in, int::in, int::out) is semidet.

:- implementation.

p(A, B, !Y) :-
    some [!X] (
        p0(!:X),
        p2(!X),
        p2(!X),
        ( if
            A = yes
        then
            (
                B = yes,
                p1(!.X),
                p2(!X),
                p1(!.X)
            ;
                B = no,
                ( if
                    p1(!.X),
                    p2(!X)
                then
                    p1(!.X)
                else
                    true
                )
            )
        else
            ( if
                p1(!.X),
                p2(!X)
            then
                (
                    A = yes,
                    p1(!.X),
                    p2(!X),
                    % The bug occurs here. The final value of !X is defined
                    % in both branches of this switch, but this final value
                    % is not used. The unique modes can therefore optimize away
                    % the copy unification in the "no" branch, but cannot
                    % optimize away the binding of the variable representing
                    % the value in the call to p2 in the "yes" branch. Later
                    % compiler passes that check whether the two branches bind
                    % the same set of variables in the process of recomputing
                    % instmap delta then find that they don't, leading to a
                    % compiler abort.
                    (
                        B = yes,
                        p2(!X)
                    ;
                        B = no
                    )
                ;
                    A = no
                )
            else if
                p1(!.X)
            then
                p1(!.X),
                p2(!X),
                p1(!.X)
            else
                true
            )
        )
    ).

:- pred p0(int::out) is det.

p0(0).

:- pred p1(int::in) is semidet.

p1(1).

:- pred p2(int::in, int::out) is semidet.

p2(!X) :-
    p1(!.X).
