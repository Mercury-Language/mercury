%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_opt_2.
:- interface.

:- import_module list.

:- type non_empty_list(T) =< list(T)
    --->    [T | list(T)].

:- func to_list(non_empty_list(T)) = list(T).

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma inline(func(to_list/1)).

to_list(Xs) = coerce(Xs).
