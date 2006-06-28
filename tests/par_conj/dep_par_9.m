% higher-order

:- module dep_par_9.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    HO = get_ho,
    (
	X = HO(1)
    &
	Y = HO(X)
    ),
    io.print(X*Y, !IO),
    io.nl(!IO).

:- func get_ho = (func(int) = int).
:- pragma no_inline(get_ho/0).
get_ho = my_ho.

:- func my_ho(int) = int.
:- pragma no_inline(my_ho/1).
my_ho(X) = X+1.
