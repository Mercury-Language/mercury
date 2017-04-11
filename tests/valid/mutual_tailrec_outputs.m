%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case triggered a problem in the code in mark_tail_calls.m
% that checks for mutual recursion (part of the implementation of
% --inline-linear-tail-rec-sccs): the compiler crashed, because
% the callee and the caller had different number of parameters.

:- module mutual_tailrec_outputs.

:- interface.

:- import_module list.

:- type a
    --->    a.

:- pred my_condense(list(list(list(a)))::in, list(a)::out) is det.

:- implementation.

my_condense([], []).
my_condense([GList | Gs0], Gs) :-
    ( if GList = [GList0], Gs0 = [] then
        % Fast path, avoid rebuilding list.
        Gs = GList0
    else
        my_condense0(GList, Gs0, Gs)
    ).

:- pred my_condense0(list(list(a))::in, list(list(list(a)))::in, list(a)::out)
    is det.

my_condense0([], Gs1, Gs) :-
    my_condense(Gs1, Gs).
my_condense0([G0 | Gs0], Gs1, Gs) :-
    my_condense1(G0, Gs0, Gs1, Gs).

:- pred my_condense1(list(a)::in, list(list(a))::in, list(list(list(a)))::in,
    list(a)::out) is det.

my_condense1([], Gs1, Gs2, Gs) :-
    my_condense0(Gs1, Gs2, Gs).
my_condense1([G | Gs0], Gs1, Gs2, Gs) :-
    my_condense1(Gs0, Gs1, Gs2, Gs3),
    Gs = [G | Gs3].
