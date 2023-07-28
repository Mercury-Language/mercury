%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The different exepcted outputs correspond to different interleavings of
% standard output and error.
%---------------------------------------------------------------------------%

:- module runtime_opt.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_string("Hello world (with non-standard options).\n", !IO).
