%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_type_visibility.

:- implementation.

:- type foreign.

:- interface.

:- pragma foreign_type(java, foreign, "java.lang.Object").

:- type foreign2.

:- pragma foreign_type(java, foreign2, "java.lang.Object").

:- type foreign3
    --->    foreign3(c_pointer).

:- implementation.

:- pragma foreign_type(c, foreign2, "void *").

:- pragma foreign_type(c, foreign3, "void *").
