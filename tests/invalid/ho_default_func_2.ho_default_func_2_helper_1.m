%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ho_default_func_2.ho_default_func_2_helper_1.
:- interface.

:- type id(T).

:- func mkid(T) = id(T).
:- func getval(id(T)) = T.

:- func bar(int) = int.
:- mode bar(out) = in is det.

:- implementation.

:- import_module int.

:- type id(T)
    --->    id(T).

mkid(X) = id(X).

getval(id(X)) = X.

bar(X) = X + 1.
