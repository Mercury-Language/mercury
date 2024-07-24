%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module higher_order_mutable.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

:- mutable(numerals, (pred(int::in, string::out) is semidet), english, ground,
    [untrailed, attach_to_io_state]).

%---------------------------------------------------------------------------%


main(!IO) :-
    test(1, !IO),
    test(2, !IO),
    test(3, !IO),
    test(4, !IO),

    io.nl(!IO),
    set_numerals(spanish, !IO),

    test(1, !IO),
    test(2, !IO),
    test(3, !IO),
    test(4, !IO).

:- pred test(int::in, io::di, io::uo) is det.

test(N, !IO) :-
    get_numerals(Numerals, !IO),
    ( if Numerals(N, NStr) then
        io.format("numeral %d -> %s\n", [i(N), s(NStr)], !IO)
    else
        io.format("numeral %d is unknown\n", [i(N)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred english(int::in, string::out) is semidet.

english(1, "one").
english(2, "two").
english(3, "three").

:- pred spanish(int::in, string::out) is semidet.

spanish(1, "una").
spanish(2, "dos").
spanish(3, "tres").
