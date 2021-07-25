%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module double_error.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module require.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    list.length(Args, Num),
    ( if p(Num) then
        io.write_string("1\n", !IO)
    else
        Arg = "not 1\n",
        error(Arg)
    ),
    ( if q(Num) then
        io.write_string("2\n", !IO)
    else
        Arg = "not 2\n",
        error(Arg)
    ).

:- pred p(int::in) is semidet.

p(0).
p(1).
p(2).
p(3).

:- pred q(int::in) is semidet.

q(0).
q(4).
q(5).
q(6).
