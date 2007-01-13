:- module dg_fib.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module maybe.
:- import_module require.

main(!IO) :-
    fibonacci(30, Result),
    io.write_int(Result, !IO),
    io.nl(!IO).

:- pred fibonacci(int::in, int::out) is det.

fibonacci(X, Y) :-
    ( X = 0 ->
        Y = 0
    ;
        ( X = 1 ->
            Y = 1
        ;
            ( X > 1 ->
                J = X - 1,
                K = X - 2,  
                ( 
                    fibonacci(J, Jout) 
                &
                    fibonacci(K, Kout)
                ),
                Y = Jout + Kout
            ;
                error("fibonacci: wrong value")
            )
        )
    ).
