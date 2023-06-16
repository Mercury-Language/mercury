%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. Up until May 2001, typeclass methods
% from transitively imported modules (for which the `.int2' file is read)
% could be used if each reference was fully module qualified.

:- module transitive_import_class.

:- interface.

:- import_module int.

:- func semidet_id(int) = int is semidet.

:- implementation.

:- import_module transitive_import_class_helper_1.

semidet_id(X) =
    transitive_import_class_helper_2.to_int(
        transitive_import_class_helper_2.from_int(X) `with_type` int).
