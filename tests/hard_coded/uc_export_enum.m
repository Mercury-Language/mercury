% vim: ft=mercury ts=4 sw=4 et
% Check the `uppercase' attribute with foreign_export_enum pragmas.

:- module uc_export_enum.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

main(!IO) :-
    test_uc(UC),
    ( UC = foo ->
        io.write_string("test_uc - succeeded.\n", !IO)
    ;
        io.write_string("test_uc - failed.\n", !IO)
    ),
    test_lc(LC),
    ( LC = foo ->
        io.write_string("test_lc - succeeded.\n", !IO)
    ;
        io.write_string("test_lc - failed.\n", !IO)
    ),
    test_or(X, Y, Z),
    ( X = foo, Y = bar, Z = baz ->
        io.write_string("test_or - succeeded.\n", !IO)
    ;
        io.write_string("test_or - failed.\n", !IO)
    ).

:- pred test_uc(foo::out) is det.
:- pragma foreign_proc("C",
    test_uc(X::out),
    [will_not_call_mercury, promise_pure],
"
    X = UC_foo_FOO;
").

:- pred test_lc(foo::out) is det.
:- pragma foreign_proc("C",
    test_lc(X::out),
    [will_not_call_mercury, promise_pure],
"
    X = LC_foo_foo;
").

:- pred test_or(foo::out, foo::out, foo::out) is det.
:- pragma foreign_proc("C",
    test_or(X::out, Y::out, Z::out),
    [will_not_call_mercury, promise_pure],
"
    X = OR_foo_lowercase_foo;
    Y = OR_foo_mixed1234_bAr;
    Z = OR_foo_BAZ;
").

%----------------------------------------------------------------------------%

:- type foo
    --->    foo
    ;       bar
    ;       baz.

    % Check that uppercase applies only the constructors and not to the prefix.
    %
:- pragma foreign_export_enum("C", foo/0, [prefix("UC_foo_"), uppercase]).

    % Check that uppercase applies only when the uppercase attribute is specified.
    %
:- pragma foreign_export_enum("C", foo/0, [prefix("LC_foo_")]).

    % Check that the uppercase attribute does not apply to user supplied foreign
    % names.
    %
:- pragma foreign_export_enum("C", foo/0, [prefix("OR_foo_"), uppercase], [
    foo  - "lowercase_foo",
    bar  - "mixed1234_bAr"
]).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
