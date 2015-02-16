%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module my_list.

:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- pred list(list(T)::in) is semidet.

:- implementation.

list([_H | Ts]) :-
    list(Ts).
list([]).
