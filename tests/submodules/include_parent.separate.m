% Used by use_submodule.m

:- module include_parent__separate.

:- interface.

% The parent module includes io.

:- pred hello(io__state::di, io__state::uo) is det.

:- pred hello2(io__state::di, io__state::uo) is det.

  :- module include_parent__separate__nested.
  :- interface.
  :- pred hello3(io__state::di, io__state::uo) is det.
  :- end_module include_parent__separate__nested.

:- implementation.

hello -->
	io__write_string("include_parent__separate: hello\n").

hello2 -->
	io__write_string("include_parent__separate: hello2\n").

  :- module include_parent__separate__nested.
  :- implementation.
  hello3 -->
	io__write_string("include_parent__separate__nested: hello\n").
  :- end_module include_parent__separate__nested.
