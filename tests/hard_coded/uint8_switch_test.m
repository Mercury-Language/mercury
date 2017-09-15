%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module uint8_switch_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    test_foo_mode_0([0u8, 1u8, 2u8, 3u8, 4u8, 5u8, 6u8, 7u8, 8u8], !IO).

:- pred foo(uint8, string).
:- mode foo(in, out) is semidet.

foo(1u8, "one").
foo(3u8, "three").
foo(5u8, "five").
foo(7u8, "seven").

:- pred test_foo_mode_0(list(uint8)::in, io::di, io::uo) is det.

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
