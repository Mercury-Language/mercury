% vim: ts=4 sw=4 ft=mercury

:- module user_field_access_decl_override2.

:- interface.

:- type dummy ---> dummy.

:- implementation.

:- type foo(T)
    --->    foo(
                f1 :: int,
                f2 :: int
            ).

    % These user-supplied declarations restrict the
    % field access functions to foo(int) instead of foo(T).
    %
:- func foo(int) ^ f1 = int is semidet.
:- func (foo(int) ^ f1 := int) = foo(int) is semidet.

:- func get_foo_f1(foo(T)) = int is det.

get_foo_f1(Foo) = X :-
    % The user-declared function declaration should be used instead of
    % an automatically generated declaration, so this will be a type error.
    X = Foo ^ f1.

:- pred set_foo_f1(T::in, foo(T)::in, foo(T)::out) is det.

set_foo_f1(X, !Foo) :-
    % The user-declared function declaration should be used instead of
    % an automatically generated declaration, so this will be a type error.
    !Foo ^ f1 := X.
