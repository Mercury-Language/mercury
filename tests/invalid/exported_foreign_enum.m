%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module exported_foreign_enum.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ( test(bar) ->
        io.write_string("Success.\n", !IO)
    ;
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

%---------------------------------------------------------------------------%

:- type foo
    --->    foo(int)
    ;       bar
    ;       baz.

:- pragma foreign_export_enum("C", foo/0, [prefix("FOO_")]).

:- pragma foreign_enum("C", foo/0, [
    foo - "400",
    bar - "500",
    baz - "600"
]).

% :- pragma reserve_tag(foo/0). This pragma is not supported anymore.

%---------------------------------------------------------------------------%

:- interface.

:- type t1
    --->    f11
    ;       f12
    ;       f13(int).

:- implementation.

:- pragma foreign_enum("C", t1/0, [
    f11 - "14",
    f12 - "15",
    f13 - "16"
]).

%---------------------------------------------------------------------------%

:- interface.

:- type t2
    --->    f21
    ;       f22
    ;       f23.

:- implementation.

:- pragma foreign_enum("C", t2/0, [
    f21 - "24",
    f22 - "25",
    f23 - "26"
]).

%---------------------------------------------------------------------------%

:- type t3 == t1.

:- pragma foreign_enum("C", t3/0, [
    f31 - "34",
    f32 - "35",
    f33 - "36"
]).

%---------------------------------------------------------------------------%
