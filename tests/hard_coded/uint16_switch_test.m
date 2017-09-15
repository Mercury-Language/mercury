%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module uint16_switch_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    test_foo_mode_0([0u16, 1u16, 2u16, 3u16, 4u16, 5u16, 6u16, 7u16, 8u16], !IO).

:- pred foo(uint16, string).
:- mode foo(in, out) is semidet.

foo(1u16, "one").
foo(3u16, "three").
foo(5u16, "five").
foo(7u16, "seven").

:- pred test_foo_mode_0(list(uint16)::in, io::di, io::uo) is det.

test_foo_mode_0(Values, !IO) :-
    (
        Values = []
    ;
        Values = [Value | ValuesPrime],
        ( if foo(Value, Result) then
            io.format("foo(%s, %s)\n", [s(string(Value)), s(string(Result))], !IO)
        else
            io.format("foo(%s, _) ==> <<FALSE>>\n", [s(string(Value))], !IO)
        ),
        test_foo_mode_0(ValuesPrime, !IO)
    ).
