%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Test the error messages generated  if a `with_inst` annotation is invalid.
%---------------------------------------------------------------------------%

:- module bad_with_inst.
:- interface.

:- type t ---> t.

:- pred foo_pred(T, T) `with_inst` 1234.

:- func foo_func(T::in) : int `with_inst` 5678.

:- typeclass bar(T) where [
    pred method1(T, T) `with_inst` "Hello",

    func method2(T) : int `with_inst` "World"
].
