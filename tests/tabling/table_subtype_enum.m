%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module table_subtype_enum.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.

:- pragma require_feature_set([memo]).

main(!IO) :-
    test_subtype_enum(bar, baz, Result),
    io.write_string("First result: " ++ Result ++ "\n", !IO),
    print_second_result(!IO).

:- pragma no_inline(print_second_result/2).
:- pred print_second_result(io::di, io::uo) is det.

print_second_result(!IO) :-
    test_subtype_enum(bar, baz, Result),
    io.write_string("Second result: " ++ Result ++ "\n", !IO).

:- pragma memo(test_subtype_enum/3).
:- pred test_subtype_enum(foo::in, foo::in, string::out) is det.

test_subtype_enum(A, B, Out) :-
    StrA = string.string(A),
    Number = mystery(B),
    Out = StrA ++ ", " ++ int_to_string(Number).

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

:- type base_foo
    --->    foo_0
    ;       foo_1
    ;       foo_2
    ;       foo_3
    ;       bar     % 4
    ;       foo_5
    ;       foo_6
    ;       foo_7
    ;       foo_8
    ;       foo_9
    ;       baz     % 10
    ;       foo_11
    ;       foo_12
    ;       foo_13
    ;       foo_14
    ;       foo_15.

:- type foo =< base_foo
    --->    baz     % 10
    ;       bar.    % 4 (deliberately reordered)
