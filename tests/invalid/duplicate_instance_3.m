:- module duplicate_instance_3.
:- interface.
:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module enum.

% This instance definition conflicts with the one in library/int.m.
:- instance enum(int) where [
        to_int(_) = 0,
	from_int(X) = X
].

main --> [].
