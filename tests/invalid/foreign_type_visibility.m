%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .err_exp file is for when we are targeting C.
% The .err_exp2 file is for when we are targeting Java.
% The .err_exp3 file is for when we are targeting C#.
%
% The contexts of some error messages are wrong in all of them,
% with the cause being that adding a foreign_type decl for a type
% causes the compiler to update that type's context, even when
% the foreign_type declaration is not relevant for the grade.
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
