:- module lambda_output.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
        fork(a(_), b).

:- pred fork(pred(io__state, io__state), pred(io__state, io__state),
                io__state, io__state).
:- mode fork(pred(di, uo) is det, pred(di, uo) is det,
                di, uo) is det.

fork(A, B) -->
        call(A), call(B).

:- pred a(int, io__state, io__state).
:- mode a(out, di, uo) is det.

a(42) --> [].

:- pred b(io__state, io__state).
:- mode b(di, uo) is det.

b --> [].

