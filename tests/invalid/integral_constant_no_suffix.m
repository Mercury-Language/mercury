%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module integral_constant_no_suffix.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    N = 6,
    ( if life_universe_everything(N) then
        io.write_line("yes", !IO)
    else
        io.write_line("no", !IO)
    ).

:- pred life_universe_everything(uint8::in) is semidet.

life_universe_everything(42).
