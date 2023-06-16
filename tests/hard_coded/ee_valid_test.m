%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ee_valid_test.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_string("default mapping: apple = ", !IO),
    io.write_line(get_default_apple, !IO),
    io.write_string("default mapping: orange = ", !IO),
    io.write_line(get_default_orange, !IO),
    io.write_string("with prefix: pear = ", !IO),
    io.write_line(get_prefix_pear, !IO),
    io.write_string("user mapping: lemon = ", !IO),
    io.write_line(get_user_lemon, !IO),
    io.write_string("default mapping with quoted names: bar = ", !IO),
    io.write_line(get_bar, !IO).

:- type fruit
    --->    apple
    ;       orange
    ;       pear
    ;       lemon.

:- type foo
    --->    'FOO'
    ;       'BAR'
    ;       'BAZ'.

    % Default mapping.
    %
:- pragma foreign_export_enum("C", fruit/0).
:- pragma foreign_export_enum("C#", fruit/0).
:- pragma foreign_export_enum("Java", fruit/0).

    % Default mapping with prefix.
    %
:- pragma foreign_export_enum("C", fruit/0, [prefix("PREFIX_")]).
:- pragma foreign_export_enum("C#", fruit/0, [prefix("PREFIX_")]).
:- pragma foreign_export_enum("Java", fruit/0, [prefix("PREFIX_")]).

    % User-specified mapping.
    % Also checks that module qualifiers on constructor names are handled.
    %
:- pragma foreign_export_enum("C", fruit/0, [prefix("USER_")],
    [
        ee_valid_test.apple - "APPLE",
        orange - "ORANGE",
        ee_valid_test.pear - "PEAR",
        ee_valid_test.lemon - "LEMON"
    ]).

:- pragma foreign_export_enum("C#", fruit/0, [prefix("USER_")],
    [
        ee_valid_test.apple - "APPLE",
        orange - "ORANGE",
        ee_valid_test.pear - "PEAR",
        ee_valid_test.lemon - "LEMON"
    ]).

:- pragma foreign_export_enum("Java", fruit/0, [prefix("USER_")],
    [
        ee_valid_test.apple - "APPLE",
        orange - "ORANGE",
        ee_valid_test.pear - "PEAR",
        ee_valid_test.lemon - "LEMON"
    ]).

    % Default mapping for quoted Mercury names.
    %
:- pragma foreign_export_enum("C", foo/0).
:- pragma foreign_export_enum("C#", foo/0).
:- pragma foreign_export_enum("Java", foo/0).

:- func get_default_apple = fruit.
:- pragma foreign_proc("C",
    get_default_apple = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = apple;
").
:- pragma foreign_proc("C#",
    get_default_apple = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = apple;
").
:- pragma foreign_proc("Java",
    get_default_apple = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = apple;
").

:- func get_default_orange = fruit.
:- pragma foreign_proc("C",
    get_default_orange = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = orange;
").
:- pragma foreign_proc("C#",
    get_default_orange = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = orange;
").
:- pragma foreign_proc("Java",
    get_default_orange = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = orange;
").

:- func get_prefix_pear = fruit.
:- pragma foreign_proc("C",
    get_prefix_pear = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = PREFIX_pear;
").
:- pragma foreign_proc("C#",
    get_prefix_pear = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = PREFIX_pear;
").
:- pragma foreign_proc("Java",
    get_prefix_pear = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = PREFIX_pear;
").

:- func get_user_lemon = fruit.
:- pragma foreign_proc("C",
    get_user_lemon = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = USER_LEMON;
").
:- pragma foreign_proc("C#",
    get_user_lemon = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = USER_LEMON;
").
:- pragma foreign_proc("Java",
    get_user_lemon = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = USER_LEMON;
").

:- func get_bar = foo.
:- pragma foreign_proc("C",
    get_bar = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = BAR;
").
:- pragma foreign_proc("C#",
    get_bar = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = BAR;
").
:- pragma foreign_proc("Java",
    get_bar = (X::out),
    [will_not_call_mercury, promise_pure],
"
    X = BAR;
").
