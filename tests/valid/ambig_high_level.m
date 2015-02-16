%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ambig_high_level.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

% :- import_module ambig_confuse1.
% :- import_module ambig_confuse2.
:- import_module ambig_types.

main(!IO) :-
    X = foo,
    ambig(confuse(X), X, X).

:- pred ambig(pred(A, B)::in(pred(in, in) is det),
    A::in, B::in) is det.

ambig(Pred, X, Y) :-
    Pred(X, Y).

:- pred confuse(a::in, a::in, a::in) is det.
confuse(_, _, _).

:- end_module ambig_high_level.
