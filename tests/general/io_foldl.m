%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module io_foldl.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module require.

main(!IO) :-
    io.input_stream_foldl_io(io__write_char, Res, !IO),
    require(unify(Res, ok), "Error reading file.").
