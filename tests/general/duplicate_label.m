:- module duplicate_label.
:- interface.
:- import_module io.

% An old version of the Mercury compiler
% generated incorrect C code for this module
% (there was a duplicate definition for a static constant).

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list.

main -->
	{ X = "foo", Y = [X, X], Z = [Y, Y],
	list__condense(Z, L) },
	io__write_strings(L).
