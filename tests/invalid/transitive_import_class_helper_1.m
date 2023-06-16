%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. Up until May 2001, types, insts, modes and
% typeclasses from transitively imported modules (for which the `.int2' file
% is read) could be referred to if the reference was fully module qualified.

:- module transitive_import_class_helper_1.

:- interface.

:- import_module transitive_import_class_helper_2.

:- instance my_enum(int).

:- implementation.

:- instance my_enum(int) where [
    to_int(X) = X,
    from_int(X) = X
].
