%-----------------------------------------------------------------------------%
% Regression test.  The variable holding the higher order term being called was
% not considered a goal input.  Hence the goal may be incorrectly considered
% loop-invariant.

:- module loop_inv_test4.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module int.

%-----------------------------------------------------------------------------%

main(!IO) :-
    List = [int.plus, int.times, int.minus],
    use_list(List, !IO).

:- type myfunc == (func(int, int) = int).

:- pred use_list(list(myfunc)::in, io::di, io::uo) is det.

use_list([], !IO).
use_list([P | Ps], !IO) :-
    X = P(5, 1),
    io.write_int(X, !IO),
    io.write_string("\n", !IO),
    use_list(Ps, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
