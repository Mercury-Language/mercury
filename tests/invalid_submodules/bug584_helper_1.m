%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug584_helper_1.
:- interface.

    :- module sub1.
    :- interface.

    % By the old rules, foo would be visible in this interface section due to
    % the `import_module' declaration in the implementation section
    % of the parent module.

    :- type struct
        --->    struct(
                    f1 :: int,
                    f2 :: foo,
                    f3 :: bug584_helper_1.sub2.foo
                ).

    :- end_module sub1.

:- implementation.

    :- import_module bug584_helper_1.sub2.

    :- module sub2.
    :- interface.

    :- type foo
        --->    foo.

    :- end_module sub2.

:- end_module bug584_helper_1.
