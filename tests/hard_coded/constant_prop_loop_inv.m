%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for Mantis bug 532.
%

:- module constant_prop_loop_inv.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.

main(!IO) :-
    ( if "abc" ++ "xyz" = "abcxyz" then
        io.write_string("yes\n", !IO)
    else
        link_error(!IO)
    ).

    % We should be able to optimize away the call to this procedure
    % at compile time, so we should not even emit a reference to it.
:- pred link_error(io::di, io::uo) is det.
:- pragma external_pred(link_error/2).
