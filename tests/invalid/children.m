% This is part of the missing_parent_import test case.
:- module children.
:- interface.

:- module children:child.
:- interface.
:- import_module io.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- end_module children:child.

:- module children:child2.
:- interface.
:- import_module io.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- end_module children:child2.

%-----------------------------------------------------------------------------%

:- implementation.

:- module children:child.
:- implementation.

hello --> io__write_string("children:child:hello\n").

:- end_module children:child.

:- module children:child2.
:- implementation.

hello --> io__write_string("children:child2:hello\n").

:- end_module children:child2.

:- end_module children.

