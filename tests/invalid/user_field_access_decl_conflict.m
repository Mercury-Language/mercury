% vim: ts=4 sw=4 ft=mercury

:- module user_field_access_decl_conflict.

:- interface.

:- type foo
    --->    foo(
                f0 :: int,
                f1 :: int
            ).

:- type bar
    --->    bar(
                f1 :: uint,
                f2 :: int
            ).

:- func foo ^ f0 = int.
:- func (foo ^ f0 := int) = foo.

:- func foo ^ f1 = int.
:- func bar ^ f1 = uint.            % conflict

:- func (foo ^ f1 := int) = foo.
:- func (bar ^ f1 := uint) = bar.   % conflict
