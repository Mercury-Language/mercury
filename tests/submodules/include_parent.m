% Used by use_submodule.m

:- module include_parent.

:- interface.

:- import_module io.
:- include_module separate, separate2.

:- pred hello(io__state::di, io__state::uo) is det.

  :- module include_parent__nested.
  :- interface.
  :- pred hello(io__state::di, io__state::uo) is det.
  :- end_module include_parent__nested.

:- implementation.

hello -->
	io__write_string("include_parent: hello\n").

%-----------------------------------------------------------------------------%

:- module include_parent__nested.
:- implementation.

hello -->
	io__write_string("include_parent__nested: hello\n").

:- end_module include_parent__nested.

%-----------------------------------------------------------------------------%
