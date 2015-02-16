%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is part of the missing_parent_import test case.

:- module children.
:- interface.

:- module children.child.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- end_module children.child.

:- module children.child2.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- end_module children.child2.

%---------------------------------------------------------------------------%

:- implementation.

:- module children.child.
:- implementation.

hello -->
    io.write_string("children.child.hello\n").

:- end_module children.child.

:- module children.child2.
:- implementation.

hello -->
    io.write_string("children.child2.hello\n").

:- end_module children.child2.

:- end_module children.
