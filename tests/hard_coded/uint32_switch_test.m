%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module uint32_switch_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    test_foo_mode_0([0u32, 1u32, 2u32, 3u32, 4u32, 5u32, 6u32, 7u32, 8u32], !IO).

:- pred foo(uint32, string).
:- mode foo(in, out) is semidet.

foo(1u32, "one").
foo(3u32, "three").
foo(5u32, "five").
foo(7u32, "seven").

:- pred test_foo_mode_0(list(uint32)::in, io::di, io::uo) is det.

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
