%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Any copyright is dedicated to the Public Domain.
% http://creativecommons.org/publicdomain/zero/1.0/
%
% Released by Transnat Games for testing purposes.
%
% This is a regression test for github issue #85.
%
% The bug was that the presence of the second mode on test1 used to silence
% the "disjunct has no solutions" warning for the second disjunct.

:- module gh85.

:- interface.

:- type x
    --->    a
    ;       b
    ;       c.

:- type y
    --->    ok
    ;       error(string).

:- pred test1(x, y).
:- mode test1(out, in) is det.
:- mode test1(in, out) is det.

:- pred test2(x, y).
:- mode test2(out, in) is det.
:- mode test2(in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- use_module exception.

% In the first mode, which is the <out,in> mode, the second disjunct here
% is not part of the switch on the second argument; it is just a disjunct
% that can have no solution. Since --warn-simple-code is on by default,
% the compiler should generate a warning about this fact.
%
% The bug was that although the compiler *did* generate this warning,
% the warning was marked report_only_if_in_all_modes. In the second mode,
% which is the <in,out> mode, the second clause turns into a switch arm
% in the switch on the first argument. In this mode, we don't generate
% the same message about a disjunct having no solutions, for two separate
% and distinct reasons: first, it is ok (and common) for a switch arm
% to have no solutions, and second, there is no disjunct left to complain
% about anyway (switch arms are *made* from disjuncts, but *are not*
% disjuncts anymore).

test1(X, Y) :-
    (
        X = a,
        Y = ok
    ;
        X = b,
        exception.throw(exception.software_error("ERROR"))
    ;
        X = c,
        Y = error("c")
    ).

test2(X, Y) :-
    disable_warning [no_solution_disjunct]
    (
        X = a,
        Y = ok
    ;
        X = b,
        exception.throw(exception.software_error("ERROR"))
    ;
        X = c,
        Y = error("c")
    ).
