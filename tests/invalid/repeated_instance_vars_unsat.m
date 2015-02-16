%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that we get an error if a repeated type variable in an instance head
% is bound to different types.

:- module repeated_instance_vars_unsat.
:- interface.

:- import_module list.

:- typeclass foo(A, B) where [
    pred method1(A::in, B::in) is semidet
].

:- instance foo(list(T), list(T)).

:- pred test(list(int)::in, list(float)::in) is semidet.

:- implementation.

:- instance foo(list(T), list(T)) where [
    ( method1([_ | _], [_ | _] ) )
].

test(Ints, Floats) :-
    method1(Ints, Floats).
