% Test that we can use both nested and separate sub-modules.

:- module use_submodule.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module include_parent.
:- import_module include_parent__nested.
:- import_module include_parent__separate.
:- import_module include_parent__separate__nested.
:- use_module include_parent__separate2.
:- use_module include_parent__separate2__nested.

main -->
	include_parent__hello,
	include_parent__nested__hello,
	nested__hello,
	include_parent__separate__hello,
	separate__hello,
	include_parent__separate__hello2,
	separate__hello2,
	hello2,
	hello3,
	include_parent__separate2__hello,
	include_parent__separate2__nested__hello.
