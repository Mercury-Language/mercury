%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests that the termination analyser gives a reason for reporting
% non-termination when `--check-termination' is enabled rather than
% just reporting that it couldn't prove termination for unknown reasons.
%
%---------------------------------------------------------------------------%

:- module term_indirect_warning.

:- interface.

:- type nat
    --->    zero
    ;       s(nat).

:- pred foo(pred(nat), nat, nat).
:- mode foo(pred(in) is det, in, out) is det.

:- implementation.

foo(P, X, s(s(X))) :-
    P(X).

:- end_module term_indirect_warning.
