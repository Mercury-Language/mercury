%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .err_exp file is for systems where the directory separator is "/".
% The .err_exp2 file is for systems where the directory separator is "\".

:- module infinite_include_direct.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_string("Hello, world.\n", !IO).
