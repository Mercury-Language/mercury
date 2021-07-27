%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests the case where a higher-order specialization needs type specialization.

:- module higher_order3.
:- interface.

:- pred bar is semidet.

:- implementation.
:- import_module list.

bar :-
    foo((pred(X::in) is semidet :- X = [_ | _]), []).

:- pred foo(pred(T)::in(pred(in) is semidet), T::in) is semidet.

foo(P, T) :-
    call(P, T).
