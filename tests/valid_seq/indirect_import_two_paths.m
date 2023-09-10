%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test was originally called module_a.
%

:- module indirect_import_two_paths.
:- interface.

:- use_module io.
% XXX
% The commit message for commit 9d07a2ccdbc91192a7fb012096c489b0c2051e7c,
% which introduced the import_module declaration just below
% (when this test case was in the valid directory, and the imported
% module was named module_e.m) lead me (zs) to believe that the situation
% that this import was meant to test was that it was a direct import
% of a module that we also indirectly import through another path
% (module_b then, indirect_import_two_paths_helper_1 now).
% However, indirect_import_two_paths_helper_3 does not import or use
% indirect_import_two_paths_helper_4, so the second path to ..._helper_4
% does not exist.
:- import_module indirect_import_two_paths_helper_4.
:- use_module indirect_import_two_paths_helper_1.

:- type indirect_import_two_paths.foo
    == indirect_import_two_paths_helper_1.foo.
