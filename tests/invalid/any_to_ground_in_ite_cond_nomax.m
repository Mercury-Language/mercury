%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% We shouldn't be able to ground a solver variable in the condition
% of an if-then-else.
%
% This version of the any_to_ground_in_ite_cond.m test case contains identical
% code, but is compiled with --no-max-error-line-width to test that option.
%
%---------------------------------------------------------------------------%

:- module any_to_ground_in_ite_cond_nomax.

:- interface.

:- import_module io.

:- pred main(io :: di, io :: uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- solver type st where representation is int.

%---------------------------------------------------------------------------%

main(!IO) :-
    i(X),
    promise_pure (
        ( if p(X) then
            io.write_string("aye\n", !IO)
        else
            io.write_string("nay\n", !IO)
        )
    ).

:- pred i(st::oa) is det.

i(X) :-
    promise_pure (
        impure X = 'representation to any st/0'(42)
    ).

:- pred p(st::(any >> ground)) is semidet.

:- pragma foreign_proc("C",
    p(_X::(any >> ground)),
    [promise_pure],
"
    SUCCESS_INDICATOR = MR_TRUE;
").

%---------------------------------------------------------------------------%
