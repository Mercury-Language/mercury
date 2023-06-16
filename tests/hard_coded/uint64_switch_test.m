%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module uint64_switch_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    test_foo_mode_0([0u64, 1u64, 2u64, 3u64, 4u64, 5u64, 6u64, 7u64, 8u64],
        !IO).

:- pred foo(uint64, string).
:- mode foo(in, out) is semidet.

foo(1u64, "one").
foo(3u64, "three").
foo(5u64, "five").
foo(7u64, "seven").

:- pred test_foo_mode_0(list(uint64)::in, io::di, io::uo) is det.

test_foo_mode_0(Values, !IO) :-
    (
        Values = []
    ;
        Values = [Value | ValuesPrime],
        ( if foo(Value, Result) then
            io.format("foo(%s, %s)\n",
                [s(string(Value)), s(string(Result))], !IO)
        else
            io.format("foo(%s, _) ==> <<FALSE>>\n", [s(string(Value))], !IO)
        ),
        test_foo_mode_0(ValuesPrime, !IO)
    ).
