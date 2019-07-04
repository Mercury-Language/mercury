%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module export_test2.

:- interface.

:- import_module export_test2.sub.
:- import_module int.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- pred foo(io.output_stream::in, io.output_stream::out,
    foo::in, foo::out) is det.

:- pred bar(io.output_stream::in, io.output_stream::out,
    foo::in, foo::out) is det.

    :- module export_test2.sub.
    :- interface.
    :- import_module enum.
    :- type foo.
    :- instance enum(foo).
    :- end_module export_test2.sub.

:- implementation.

:- import_module enum.
:- import_module require.

main(!IO) :-
    io.stdout_stream(Stream0, !IO),
    ( if Foo = from_int(41) then
        bar(Stream0, Stream, Foo, X),
        io.write(Stream, to_int(X), !IO),
        io.write_char(Stream, '\n', !IO)
    else
        error("from_int failed")
    ).

foo(S, S, X0, X) :-
    ( if X1 = from_int(to_int(X0) + 1) then
        X = X1
    else
        error("from_int failed")
    ).

:- pragma foreign_decl("C", "
#include ""mercury_library_types.h""

/*
** Make sure the foreign type definitions of io.input_stream
** and export_test2.sub.foo are available here.
** If not, the automatically generated definition of foo() will be
**
**      void foo(MR_Word, MR_Word *, MR_Word, MR_Word *);
*/

void foo(MercuryFilePtr, MercuryFilePtr *, int, int *);

").

:- pragma foreign_export("C", foo(in, out, in, out), "foo").

:- pragma foreign_proc("C",
    bar(S::in, T::out, X::in, Y::out),
    [may_call_mercury, promise_pure],
"
    foo(S, &T, X, &Y);
").
:- pragma foreign_proc("C#",
    bar(S::in, T::out, X::in, Y::out),
    [may_call_mercury, promise_pure],
"
    export_test2.mercury_code.foo(S, ref T, X, ref Y);
").

    :- module export_test2.sub.
    :- implementation.

    :- type foo
        --->    foo(int).
    :- pragma foreign_type("C", foo, "MT_Foo").

    % This needs to be visible in export_test2 for any use
    % of the foreign type to work.
    :- pragma foreign_decl("C", "typedef int MT_Foo;").

    :- instance enum(foo) where [
        to_int(X) = foo_to_int(X),
        from_int(X) = foo_from_int(X)
    ].

    :- func foo_to_int(foo) = int.
    foo_to_int(foo(Int)) = Int.
    :- pragma foreign_proc("C",
        foo_to_int(Foo::in) = (Int::out),
        [will_not_call_mercury, promise_pure, thread_safe],
    "
        Int = Foo;
    ").

    :- func foo_from_int(int) = foo.
    foo_from_int(Int) = foo(Int).
    :- pragma foreign_proc("C",
        foo_from_int(Int::in) = (Foo::out),
        [will_not_call_mercury, promise_pure, thread_safe],
    "
        Foo = Int;
    ").
    :- end_module export_test2.sub.
