% Tests the case where a higher-order specialization needs type specialization.

:- module higher_order3.
:- interface.
:- pred bar is semidet.
:- implementation.
:- import_module list.

bar :- foo((pred(X::in) is semidet :- X = [_|_]), []).

:- pred foo(pred(T), T).
:- mode foo(pred(in) is semidet, in) is semidet.
foo(P, T) :-
        call(P, T).


