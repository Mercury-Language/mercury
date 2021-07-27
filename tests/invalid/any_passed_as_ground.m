%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Main shouldn't be able to pass Xs to member/2 because member/2 expects Xs
% to be ground. The compiler should report an error.
%
%---------------------------------------------------------------------------%

:- module any_passed_as_ground.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pair.

:- solver type st where representation is int.

main(!IO) :-
    p(Xs),
    promise_pure ( if member((X - _), Xs) then Y = X else Y = 0 ),
    io.write_int(Y, !IO).

:- pred i(st::oa) is det.

i(X) :-
    promise_pure(impure X = 'representation to any st/0'(42)).

:- pred p(list(pair(int, st))::oa) is det.

p([1 - A, 2 - B, 3 - C]) :-
    i(A), i(B), i(C).

%---------------------------------------------------------------------------%
