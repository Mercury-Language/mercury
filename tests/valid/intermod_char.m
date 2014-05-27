% Character constants in clause heads were doubly-escaped in .opt files.

:- module intermod_char.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module intermod_char2.

main(!IO) :-
    ( p('\r') ->
        true
    ;
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
