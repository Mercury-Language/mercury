%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .err_exp file is for --no-use-subdirs.
% The .err_exp2 file is for --use-subdirs.
%
%---------------------------------------------------------------------------%

:- module sub_visibility_b4.
:- interface.

:- use_module sub_visibility_b4.sub2.

%---------------------------------------------------------------------------%

    :- module sub1.
    :- interface.

    :- type int_uq ---> int_uq(foo). % error
    :- type int_fq ---> int_fq(sub_visibility_b4.sub2.foo). % error

    :- end_module sub1.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

    :- module sub1.
    :- implementation.

    :- use_module sub_visibility_b4.sub2. % overrides decl in parent

    :- type imp_uq ---> imp_uq(foo). % error
    :- type imp_fq ---> imp_fq(sub_visibility_b4.sub2.foo). % ok

    :- end_module sub1.

%---------------------------------------------------------------------------%

    :- module sub2.
    :- interface.

    :- type foo ---> foo.

    :- end_module sub2.

:- end_module sub_visibility_b4.
