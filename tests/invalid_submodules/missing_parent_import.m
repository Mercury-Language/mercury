%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test case to test the error messages that occur if you
% omit an `import_module' declaration for a parent module,
% in particular in the case where the parent module still
% gets imported indirectly via another module.
% This is a regression test -- we used to issue a quite misleading
% error message for this test case.

:- module missing_parent_import.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- import_module children2.
% The ommission of the following import is the bug that this
% test case is testing.
% :- import_module children.

:- import_module children.child.
:- use_module children.child2.
:- import_module std_util.
:- import_module require.

main -->
    children.child.hello,
    child.hello,
    hello,
    children.child2.hello,
    child2.hello,

    { true }.

%---------------------------------------------------------------------------%

:- end_module missing_parent_import.
