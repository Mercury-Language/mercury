%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ho_default_func_2.id.
:- interface.

:- type id(T).

:- func mkid(T) = id(T).
:- func getval(id(T)) = T.

:- implementation.

:- type id(T)
    --->    id(T).

mkid(X) = id(X).

getval(id(X)) = X.
