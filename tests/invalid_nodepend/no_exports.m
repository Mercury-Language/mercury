%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Ideally we ought to issue a warning for this module,
% e.g. `nothing exported' or `main/2 unused'.

:- module no_exports.

% :- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_string("hello\n", !IO).
