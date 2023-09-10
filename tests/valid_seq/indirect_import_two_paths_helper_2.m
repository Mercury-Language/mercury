%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module indirect_import_two_paths_helper_2.
:- interface.

:- use_module indirect_import_two_paths_helper_3.
:- type indirect_import_two_paths_helper_2.foo
    == indirect_import_two_paths_helper_3.foo.

