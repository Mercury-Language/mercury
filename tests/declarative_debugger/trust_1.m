%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module trust_1.

:- interface.

:- import_module string.

:- type w
    --->    w(string)
    where comparison is w_cmp.

:- pred w_cmp(builtin.comparison_result::uo, w::in, w::in) is det.

:- implementation.

w_cmp(R, W1, W2) :-
    R = unsafe_promise_unique(
        promise_only_solution(
            ( pred(R1::out) is cc_multi :-
                W1 = w(S1),
                W2 = w(S2),
                compare(R1, to_upper(S1)`with_type`string,
                    to_upper(S2))
            )
        )
    ).

