%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test case to test the error messages that occur if you omit an
% `import_module' declaration for a parent module, in particular in the case
% where the parent module still gets imported indirectly via another module.
% This is a regression test -- we used to issue a quite misleading
% error message for this test case.
%

:- module missing_parent_import.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- import_module missing_parent_import_helper_2.
% The ommission of the following import is the bug that this
% test case is testing.
% :- import_module missing_parent_import_helper_1.

:- import_module missing_parent_import_helper_1.child.
:- use_module missing_parent_import_helper_1.child2.
:- import_module std_util.
:- import_module require.

main(!IO) :-
    missing_parent_import_helper_1.child.hello(!IO),
    child.hello(!IO),
    hello(!IO),
    missing_parent_import_helper_1.child2.hello(!IO),
    missing_parent_import_helper_2.hello(!IO).

%---------------------------------------------------------------------------%
:- end_module missing_parent_import.
%---------------------------------------------------------------------------%
