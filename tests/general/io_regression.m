%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module io_regression.

% io.read_file_as_string stopped working one day, and it wasn't noticed
% because it wasn't in any of the test cases. So now it is.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.read_file_as_string(Res, !IO),
    ( if Res = ok(Str) then
        io.write_string(Str, !IO)
    else
        io.write_string("Error reading file.\n", !IO)
    ).
