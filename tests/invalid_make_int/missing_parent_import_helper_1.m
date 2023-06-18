%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is part of the missing_parent_import test case.

:- module missing_parent_import_helper_1.
:- interface.

:- module missing_parent_import_helper_1.child.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- end_module missing_parent_import_helper_1.child.

:- module missing_parent_import_helper_1.child2.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- end_module missing_parent_import_helper_1.child2.

%---------------------------------------------------------------------------%

:- implementation.

:- module missing_parent_import_helper_1.child.
:- implementation.

hello(!IO) :-
    io.write_string("missing_parent_import_helper_1.child.hello\n", !IO).

:- end_module missing_parent_import_helper_1.child.

:- module missing_parent_import_helper_1.child2.
:- implementation.

hello(!IO) :-
    io.write_string("missing_parent_import_helper_1.child2.hello\n", !IO).

:- end_module missing_parent_import_helper_1.child2.

:- end_module missing_parent_import_helper_1.
