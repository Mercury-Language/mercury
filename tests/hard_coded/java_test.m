%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test of the Java interface.

:- module java_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

:- func foo(int) = int.

:- pragma foreign_export("Java", foo(in) = out, "foo").
foo(X) = X + 1.

main(!IO) :-
    java_write_string("Hello, world\n", !IO),
    ( if java_semidet_succeed then
        true
    else
        java_write_string("java_semidet_succeed failed\n", !IO)
    ),
    ( if java_semidet_fail then
        java_write_string("java_semidet_fail succeeded\n", !IO)
    else
        true
    ).

:- pragma foreign_decl("Java", "
    // some Java top-level declarations
    class Foo {}
").

:- pragma foreign_code("Java", "
    // some Java in-class declarations
    static void bar() {
        // test `:- pragma foreign_export' functions.
        if (foo(42) != 43) {
            throw new java.lang.Error(""bar: foo failed"");
        }
    }
").

:- pred java_write_string(string::in, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    java_write_string(Message::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    // a Java procedure
    System.out.print(Message);
    // test that foreign_decl declarations are visible
    Foo f;
    // test that foreign_code declarations are visible
    bar();
").

:- pred java_semidet_succeed is semidet.
:- pred java_semidet_fail is semidet.
:- pragma foreign_proc("Java", java_semidet_succeed,
    [will_not_call_mercury, promise_pure], "SUCCESS_INDICATOR = true;").
:- pragma foreign_proc("Java", java_semidet_fail,
    [will_not_call_mercury, promise_pure], "SUCCESS_INDICATOR = false;").
