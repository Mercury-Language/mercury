%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module semi_tail_call_in_nonlast_disjunct.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
    ( if test([1, 2, 3, 4, 5, 6]) then
        io.write_string("success.\n", !IO)
    else
        io.write_string("failure.\n", !IO)
    ).

:- pred test(list(int)::in) is semidet.

test([X1 | X2s]) :-
    (
        X2s = [_X2 | X3s],
        % If the compiler believes this to be a tail call,
        % then it will miss the success due the second and
        % third disjuncts.
        test(X3s)
    ;
        test(X2s)
    ;
        X1 = 2
    ).
