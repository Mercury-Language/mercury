%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that foreign_enums can be re-exported using pragma foreign_export_enum.

:- module exported_foreign_enum.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ( if test(bar) then
        io.write_string("Success.\n", !IO)
    else
        io.write_string("ERROR.\n", !IO)
    ).

:- pred test(foo::in) is semidet.

:- pragma foreign_proc("C",
    test(X::in),
    [will_not_call_mercury, promise_pure],
"
    if (X == FOO_bar) {
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

:- pragma foreign_proc("C#",
    test(X::in),
    [will_not_call_mercury, promise_pure],
"
    if (X == FOO_bar) {
        SUCCESS_INDICATOR = true;
    } else {
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_export_enum("C", foo/0, [prefix("FOO_")]).
:- pragma foreign_export_enum("C#", foo/0, [prefix("FOO_")]).

:- type foo
    --->    foo
    ;       bar
    ;       baz.

:- pragma foreign_enum("C", foo/0, [
    foo - "400",
    bar - "500",
    baz - "600"
]).

:- pragma foreign_enum("C#", foo/0, [
    foo - "400",
    bar - "500",
    baz - "600"
]).
