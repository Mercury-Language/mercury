%
% Ideally we ought to issue a warning for this module,
% e.g. `nothing exported' or `main/2 unused'.
%
:- module no_exports.

% :- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	io__write_string("hello\n").
