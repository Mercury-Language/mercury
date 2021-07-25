%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module double_error2.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module require.
:- import_module std_util.

main(!IO) :-
    ( if semidet_succeed then
        io.write_string("yes\n", !IO)
    else
        io.progname("foo", Name, !IO),
        error(Name)
    ),
    ( if semidet_succeed then
        io.write_string("yes\n", !IO)
    else
        io.progname("bar", Name, !IO),
        error(Name)
    ).
