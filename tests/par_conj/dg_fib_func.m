:- module dg_fib_func.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module maybe.
:- import_module require.

main(!IO) :-
    Result = fibonacci(30),
    io.write_int(Result, !IO),
    io.nl(!IO).

:- func fibonacci(int) = int.

fibonacci(X) = Y :-
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
                    Jout = fibonacci(J) 
                &
                    Kout = fibonacci(K)
                ),
                Y = Jout + Kout
            ;
                error("fibonacci: wrong value")
            )
        )
    ).
