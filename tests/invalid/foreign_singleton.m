:- module foreign_singleton.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main -->
       f(X),
       io__write_int(X),
       io__nl,
       g(Y),
       io__write_int(Y),
       io__nl.

:- pred f(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", f(X::out, IO0::di, _IO::uo),
        [will_not_call_mercury, promise_pure], "
    X = 5;
").

f(X) --> [].

:- pred g(int::out, io::di, io::uo) is det.

g(X) --> [].

:- pragma foreign_proc("C", g(X::out, IO0::di, _IO::uo),
        [will_not_call_mercury, promise_pure], "
    X = 5;
").

