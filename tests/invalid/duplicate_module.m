% Test of modules declared as both nested and separate modules.
% This is referenced from duplicate_module_test.m.

:- module duplicate_module.
:- interface.
:- import_module io.

:- pred do_main(io__state::di, io__state::uo) is det.

:- implementation.

%-----------------------------------------------------------------------------%

:- include_module duplicate_module:child.

:- module duplicate_module:child.
:- interface.
:- import_module io.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- implementation.

hello --> io__write_string("duplicate_module:child:hello\n").

:- end_module duplicate_module:child.

%-----------------------------------------------------------------------------%

:- include_module child2.

:- module duplicate_module:child2.
:- interface.
:- import_module io.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- implementation.

hello --> io__write_string("duplicate_module:child2:hello\n").

:- end_module duplicate_module:child2.

%-----------------------------------------------------------------------------%

:- include_module duplicate_module:child3.

:- module child3.
:- interface.
:- import_module io.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- implementation.

hello --> io__write_string("duplicate_module:child2:hello\n").

:- end_module child3.

%-----------------------------------------------------------------------------%

% now we're back in the parent module.

:- use_module duplicate_module:child.
:- use_module duplicate_module:child2.
:- use_module duplicate_module:child3.
:- import_module std_util, require.

do_main -->
	duplicate_module:child:hello,
	duplicate_module:child2:hello,
	duplicate_module:child3:hello.

:- end_module duplicate_module.
