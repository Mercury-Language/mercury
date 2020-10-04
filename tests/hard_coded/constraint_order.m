%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that constraint propagation maintains unique mode correctness.
% The calls to q/2 and test/1 in p/2 must not be reordered.

:- module constraint_order.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ( if p(3, Y) then
        io.write_string("succeeded: ", !IO),
        io.write_int(Y, !IO),
        io.nl(!IO)
    else
        io.write_string("failed\n", !IO)
    ).

:- pred p(int::di, int::out) is semidet.

p(X, Y) :-
    q(X, Y),
    test(X).

:- pred q(int::ui, int::out) is det.
:- pragma promise_pure(q/2).
:- pragma no_inline(q/2).
:- pragma terminates(q/2).

q(_, 1) :-
    impure puts("call to q").

:- pred test(int::di) is semidet.
:- pragma promise_pure(test/1).
:- pragma no_inline(test/1).
:- pragma terminates(test/1).

test(3) :-
    impure puts("call to test").

:- impure pred puts(string::in) is det.

:- pragma foreign_proc("C", puts(Str::in), [], "puts(Str);").
:- pragma foreign_proc("C#", puts(Str::in), [],
        "System.Console.WriteLine(Str);").
:- pragma foreign_proc("Java", puts(Str::in), [],
"
    System.out.println(Str);
").
:- pragma foreign_proc("Erlang", puts(Str::in), [],
"
    io:put_chars(Str),
    io:nl()
").
