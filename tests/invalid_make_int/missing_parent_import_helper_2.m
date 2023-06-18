%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is part of the missing_parent_import test case.

:- module missing_parent_import_helper_2.
:- interface.

:- module missing_parent_import_helper_2.sub1.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- end_module missing_parent_import_helper_2.sub1.

:- module missing_parent_import_helper_2.sub2.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- end_module missing_parent_import_helper_2.sub2.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module missing_parent_import_helper_1.
:- module missing_parent_import_helper_2.sub1.
:- implementation.

hello(!IO) :-
    io.write_string("missing_parent_import_helper_2.sub1.hello\n", !IO).

:- end_module missing_parent_import_helper_2.sub1.

:- module missing_parent_import_helper_2.sub2.
:- implementation.

hello(!IO) :-
    io.write_string("missing_parent_import_helper_2.sub2.hello\n", !IO).

:- end_module missing_parent_import_helper_2.sub2.

:- end_module missing_parent_import_helper_2.

%---------------------------------------------------------------------------%
