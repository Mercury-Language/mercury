%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bind_var_errors.
:- interface.

:- pred bind_var_in_negation is semidet.

:- pred bind_var_in_ite_cond(int :: in) is semidet.

:- pred bind_var_in_lambda is semidet.

:- pred share_var_in_lambda(T :: di) is det.

:- pred share_dead_var_in_lambda(T :: di) is det.

:- pred clobber_var_in_lambda(T :: di) is det.

:- implementation.

:- pragma no_inline(consume/1).
:- pred consume(T :: in) is det.
consume(_).

:- pragma no_inline(destroy/1).
:- pred destroy(T :: di) is det.
destroy(_).

:- pragma no_inline(share/1).
:- pred share(T :: in) is det.
share(_).

bind_var_in_negation :-
    \+ (X = 42),
    consume(X).

bind_var_in_ite_cond(X) :-
    ( if
        X = 42,
        Y = 42
    then
        true
    else
        true
    ),
    consume(Y).

bind_var_in_lambda :-
    call((pred) is det :- Y = 42),
    consume(Y).

share_var_in_lambda(X) :-
    call((pred) is det :- share(X)),
    destroy(X).

    % This one is OK since X is dead after the lambda.
share_dead_var_in_lambda(X) :-
    call((pred) is det :- share(X)).

clobber_var_in_lambda(X) :-
    call((pred) is det :- destroy(X)),
    destroy(X).
