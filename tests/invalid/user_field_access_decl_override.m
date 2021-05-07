% vim: ts=4 sw=4 ft=mercury

:- module user_field_access_decl_override.

:- interface.

:- type dummy ---> dummy.

:- implementation.

:- type foo(T)
    --->    foo(
                f1 :: int,
                f2 :: int
            ).

    % These user-supplied declarations deliberately have looser determinisms
    % than automatically generated declarations.
    %
:- func foo(T) ^ f1 = int is semidet.
:- func (foo(T) ^ f1 := int) = foo(T) is semidet.

:- func get_foo_f1(foo(T)) = int is det.

get_foo_f1(Foo) = X :-
    % The user-declared function declaration should be used instead of
    % an automatically generated declaration, so this will be semidet.
    X = Foo ^ f1.

:- pred set_foo_f1(int::in, foo(T)::in, foo(T)::out) is det.

set_foo_f1(X, !Foo) :-
    % The user-declared function declaration should be used instead of
    % an automatically generated declaration, so this will be semidet.
    !Foo ^ f1 := X.

%---------------------------------------------------------------------------%

:- type bar
    --->    bar(
                f1 :: int,
                f2 :: uint
            ).

:- func get_bar_f1(bar) = int.

get_bar_f1(Bar) = X :-
    % The user-supplied declaration of foo(T) ^ f1
    % should not shadow any automatically generated declaration
    % of the field access function for the f1 field of bar.
    X = Bar ^ f1.

:- pred set_bar_f1(int, bar, bar).
:- mode set_bar_f1(in, in, out) is det.

set_bar_f1(X, !Bar) :-
    % Similarly for the field update function.
    !Bar ^ f1 := X.
