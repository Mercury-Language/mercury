
:- module mutual_tailrec_outputs.

:- interface.

:- import_module list.

:- type a ---> a.

% This case triggered a problem with mark_tail_calls when it checked for
% mutual recursion, which was introduced with --inline-linear-tail-rec-sccs.
% The compiler crashed when the number of parameters in the callee and
% caller didn't match.

:- pred my_condense(list(list(list(a))), list(a)).
:- mode my_condense(in, out) is det.

:- implementation.

my_condense([], []).
my_condense([GList|Gs0], Gs) :-
    ( if GList = [GList0], Gs0 = [] then
        % fast path, avoid rebuilding list
        Gs = GList0
    else
        my_condense0(GList, Gs0, Gs)
    ).

:- pred my_condense0(list(list(a)), list(list(list(a))), list(a)).
:- mode my_condense0(in, in, out) is det.

my_condense0([], Gs1, Gs) :-
    my_condense(Gs1, Gs).

my_condense0([G0|Gs0], Gs1, Gs) :-
    my_condense1(G0, Gs0, Gs1, Gs).

:- pred my_condense1(list(a), list(list(a)), list(list(list(a))), list(a)).
:- mode my_condense1(in, in, in, out) is det.

my_condense1([], Gs1, Gs2, Gs) :-
    my_condense0(Gs1, Gs2, Gs).

my_condense1([G|Gs0], Gs1, Gs2, Gs) :-
    my_condense1(Gs0, Gs1, Gs2, Gs3),
    Gs = [G|Gs3].


