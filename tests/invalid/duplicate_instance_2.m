:- module duplicate_instance_2.
:- interface.
:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module duplicate_instance_1.

:- instance foo(int) where [].

main --> [].
