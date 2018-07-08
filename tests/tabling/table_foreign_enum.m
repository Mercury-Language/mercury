%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module table_foreign_enum.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.

:- pragma require_feature_set([memo]).

main(!IO) :-
    test_foreign_enum(bar, baz, Result),
    io.write_string("First result: " ++ Result ++ "\n", !IO),
    print_second_result(!IO).

:- pragma no_inline(print_second_result/2).
:- pred print_second_result(io::di, io::uo) is det.

print_second_result(!IO) :-
    test_foreign_enum(bar, baz, Result),
    io.write_string("Second result: " ++ Result ++ "\n", !IO).

:- pragma memo(test_foreign_enum/3).
:- pred test_foreign_enum(foo::in, foo::in, string::out) is det.

test_foreign_enum(A, B, Out) :-
    StrA = string.string(A),
    Number = mystery(B),
    Out = StrA ++ int_to_string(Number).

:- pragma no_inline(mystery/1).
:- func mystery(foo) = int.
:- pragma foreign_proc("C",
    mystery(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure],
"
    static int called = 0;

    if (called) {
        fprintf(stdout, ""mystery has been called again\\n"");
        fflush(stdout);
    }

    Y = X + 1001;
    called = 1;
").

:- type foo
    --->    foo
    ;       bar
    ;       baz.

:- pragma foreign_decl("C", "

    #define CONSTANT1 300
    #define CONSTANT2 400
    #define CONSTANT3 500
").

:- pragma foreign_enum("C", foo/0,
    [
        foo - "CONSTANT1",
        bar - "CONSTANT2",
        baz - "CONSTANT3"
    ]).
