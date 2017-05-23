%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module uint_switch_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    test_foo_mode_0([0u, 1u, 2u, 3u, 4u, 5u, 6u, 7u, 8u], !IO).

:- pred foo(uint, string).
:- mode foo(in, out) is semidet.

foo(1u, "one").
foo(3u, "three").
foo(5u, "five").
foo(7u, "seven").

:- pred test_foo_mode_0(list(uint)::in, io::di, io::uo) is det.

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
