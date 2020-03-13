%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is part of the missing_parent_import test case.

:- module children2.
:- interface.

:- module children2.sub1.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- end_module children2.sub1.

:- module children2.sub2.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- end_module children2.sub2.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module children.
:- module children2.sub1.
:- implementation.

hello -->
    io.write_string("children2.sub1.hello\n").

:- end_module children2.sub1.

:- module children2.sub2.
:- implementation.

hello -->
    io.write_string("children2.sub2.hello\n").

:- end_module children2.sub2.

:- end_module children2.

%---------------------------------------------------------------------------%
