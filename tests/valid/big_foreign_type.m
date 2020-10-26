%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the use of foreign types,
% e.g. those that don't fit exactly into a single word.

:- module big_foreign_type.
:- interface.

:- type foo.
:- pragma foreign_type(c, foo, "struct Foo").
:- pragma foreign_type(java, foo, "Foo").

:- type foo2.
:- pragma foreign_type(c, foo2, "char").
:- pragma foreign_type(java, foo2, "Character").

:- type foo3.
:- pragma foreign_type(c, foo3, "double").
:- pragma foreign_type(java, foo3, "Double").

:- type foo4.
:- pragma foreign_type(c, foo4, "enum e").
:- pragma foreign_type(java, foo4, "e").

:- func bar(foo) = foo.
:- func bar2(foo2) = foo2.
:- func bar3(foo3) = foo3.
:- func bar4(foo4) = foo4.

:- func baz(foo) = foo.
:- func baz2(foo2) = foo2.
:- func baz3(foo3) = foo3.
:- func baz4(foo4) = foo4.

:- implementation.

:- pragma foreign_decl("C", "
    struct Foo {
        int x, y, z;
    };

    enum e { e0, e1, e2, e42 = 42 };
").

:- pragma foreign_decl("C#", "
public class Foo {
    int x, y, z;
}

public enum e { e0, e1, e2, e42=42 };
").

:- pragma foreign_decl("Java", "
class Foo {
    int x, y, z;
}

enum e { e0, e1, e2 };
").

:- pragma foreign_proc(c, bar(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = X;").
:- pragma foreign_proc(c, bar2(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = X;").
:- pragma foreign_proc(c, bar3(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = 2.0 * X;").
:- pragma foreign_proc(c, bar4(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = X;").

:- pragma foreign_proc("C#", bar(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = X;").
:- pragma foreign_proc("C#", bar2(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = X;").
:- pragma foreign_proc("C#", bar3(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = 2.0 * X;").
:- pragma foreign_proc("C#", bar4(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = X;").

:- pragma foreign_proc("Java", bar(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = X;").
:- pragma foreign_proc("Java", bar2(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = X;").
:- pragma foreign_proc("Java", bar3(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = 2.0 * X;").
:- pragma foreign_proc("Java", bar4(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure], "Y = X;").

baz(X) = X.
baz2(X) = X.
baz3(X) = X.
baz4(X) = X.

:- pragma foreign_export("C", baz(in) = out, "baz").
:- pragma foreign_export("C", baz2(in) = out, "baz2").
:- pragma foreign_export("C", baz3(in) = out, "baz3").
:- pragma foreign_export("C", baz4(in) = out, "baz4").
