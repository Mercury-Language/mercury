%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_prop_into_inst.
:- interface.

:- import_module list.

:- type mypred == (pred(int, int, int)).
:- inst mypred == (pred(in, in, out) is det).

% This test came from tests/hard_coded/ho_solns.m.
:- pred convert_list(list(T), list(T)).
:- mode convert_list(in, out(list_skel(mypred))) is det.

% This test came from tests/hard_coded/curry.m.
:- func my_map(func(T1) = T2, list(T1)) = list(T2).
:- mode my_map(func(in) = out(func(in) = out is det) is det, in) = out is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_proc("C",
    convert_list(L0 :: in, L :: out(list_skel(mypred))),
    [will_not_call_mercury, promise_pure],
"
    L = L0;
").
:- pragma foreign_proc("C#",
    convert_list(L0 :: in, L :: out(list_skel(mypred))),
    [promise_pure],
"
    L = L0;
").
:- pragma foreign_proc("Java",
    convert_list(L0 :: in, L :: out(list_skel(mypred))),
    [promise_pure],
"
    L = L0;
").

my_map(_F, []) = [].
my_map(F, [X | Xs]) = [apply(F, X) | my_map(F, Xs)].
