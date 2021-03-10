%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_unify_tvars.
:- interface.

:- import_module list.

:- type one_or_more(T) =< list(T)
    --->    [T | list(T)].

%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- pred append(one_or_more(T), one_or_more(T), one_or_more(T)).
:- mode append(di, di, uo) is det.
:- mode append(in, in, out) is det.
:- mode append(in, in, in) is semidet.    % implied

append(OoMA, OoMB, OoMC) :-
    LA = coerce(OoMA),
    LB = coerce(OoMB),
    list.append(LA, LB, LC),
    (
        LC = [],
        unexpected($pred, "empty list")
    ;
        LC = [_ | _],
        OoMC = coerce(LC)
    ).

:- pred length(one_or_more(T)::in, int::out) is det.

length(OoM, Length)  :-
    L = coerce(OoM),
    list.length(L, Length).

:- pred same_length(one_or_more(T1)::in, one_or_more(T2)::in) is semidet.

same_length(A, B) :-
    LA = coerce(A),
    LB = coerce(B),
    list.same_length(LA, LB).

:- pred same_length2(one_or_more(T1)::in, one_or_more({T2})::in) is semidet.

same_length2(A, B) :-
    LA = coerce(A),
    LB = coerce(B),
    list.same_length(LA, LB).

%---------------------------------------------------------------------------%

    % Obviously the two head type parameters cannot be unified.
    % This test is mainly just to force an error in this module.
    %
:- pred head_type_params(list(T1)::in, list(T2)::in) is semidet.

head_type_params(X, coerce(X)).
