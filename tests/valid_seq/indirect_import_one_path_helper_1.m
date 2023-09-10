%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module indirect_import_one_path_helper_1.
:- interface.

:- use_module indirect_import_one_path_helper_2.
:- type indirect_import_one_path_helper_1.foo
    == indirect_import_one_path_helper_2.foo.

