%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module trigraphs.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_string("??(  ??)  ??<  ??>  ??=  ??/n  ??'  ??!  ??-", !IO),
    io.nl(!IO).
