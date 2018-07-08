%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.

% Versions rotd-2000-04-03 and earlier
% got a software error when compiling this test.

:- module unused_args.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module std_util.

:- pragma require_feature_set([memo]).

main(!IO) :-
    test(!IO),
    test(!IO),
    test(!IO),
    test_no_input(!IO),
    test_no_input(!IO).

:- pred test(io::di, io::uo) is det.
:- pragma no_inline(test/2).

test(!IO) :-
    foo_unused_args(42, Z),
    print(Z, !IO),
    nl(!IO),
    ( if foo_unused_args_semi(42, Y) then
        print(Y, !IO),
        nl(!IO)
    else
        io.write_string("foo_unused_args failed\n", !IO)
    ),
    ( if foo_fail(X) then
        print(X, !IO),
        nl(!IO)
    else
        io.write_string("foo_fail failed, as expected\n", !IO)
    ).

:- pred foo_unused_args(int::in, string::out) is det.
:- pragma memo(foo_unused_args/2).

foo_unused_args(_, "foo").

:- pred foo_unused_args_semi(int::in, string::out) is semidet.
:- pragma memo(foo_unused_args_semi/2).

foo_unused_args_semi(_, "bar") :- semidet_succeed.

:- pred foo_fail(string::out) is semidet.
:- pragma memo(foo_fail/1).

foo_fail("FOO_FAIL SUCCEEDED (this is an error)") :-
    semidet_fail.

:- pred test_no_input(io::di, io::uo) is det.
:- pragma no_inline(test_no_input/2).

test_no_input(!IO) :-
    no_input(X1),
    io.write_string(X1, !IO),
    io.nl(!IO).

:- pred no_input(string::out) is det.
:- pragma memo(no_input/1).

:- pragma foreign_proc("C",
    no_input(X::out),
    [will_not_call_mercury, promise_pure],
"
    printf(""no_input executed\\n"");
    X = (MR_String) (MR_Integer) ""no_input_output"";
").
