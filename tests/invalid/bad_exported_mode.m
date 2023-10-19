%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .err_exp file is for --no-use-subdirs.
% The .err_exp2 file is for --use-subdirs.
%

:- module bad_exported_mode.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bad_exported_mode_helper_1.

main(!IO) :-
    ( if p(41, 42) then
        io.print_line("yes", !IO)
    else
        io.print_line("no", !IO)
    ).
