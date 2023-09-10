%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test was originally called module_b.
%

:- module indirect_import_one_path.
:- interface.

:- use_module indirect_import_one_path_helper_1.
:- type indirect_import_one_path.foo == indirect_import_one_path_helper_1.foo.
