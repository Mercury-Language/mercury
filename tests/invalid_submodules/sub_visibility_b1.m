%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module sub_visibility_b1.
:- interface.

:- use_module sub_visibility_b1.sub2.

%---------------------------------------------------------------------------%

    :- module sub1.
    :- interface.

    :- import_module sub_visibility_b1.sub2. % overrides decl in parent

    :- type int_uq ---> int_uq(foo). % ok
    :- type int_fq ---> int_fq(sub_visibility_b1.sub2.foo). % ok

    :- end_module sub1.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

    :- module sub1.
    :- implementation.

    :- type imp_uq ---> imp_uq(foo). % ok
    :- type imp_fq ---> imp_fq(sub_visibility_b1.sub2.foo). % ok

    :- end_module sub1.

%---------------------------------------------------------------------------%

    :- module sub2.
    :- interface.

    :- type foo ---> foo.

    :- end_module sub2.

:- end_module sub_visibility_b1.
