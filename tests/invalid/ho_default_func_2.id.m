:- module ho_default_func_2__id.
:- interface.

:- type id(T).

:- func mkid(T) = id(T).
:- func getval(id(T)) = T.

:- implementation.

:- type id(T) ---> id(T).

mkid(X) = id(X).

getval(id(X)) = X.
