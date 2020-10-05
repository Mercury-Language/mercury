%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module func_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module std_util.

main(!IO) :-
    io.write_int(f(7 * 8) + g(9 * 10), !IO),
    io.write_string("\n", !IO),
    io.write_int(test, !IO),
    io.write_string("\n", !IO),
    io.write_int(g(f(test)), !IO),
    io.write_string("\n", !IO),
    io.write_int(test2, !IO),
    io.write_string("\n", !IO),
    io.write_int(g(f(test2)), !IO),
    io.write_string("\n", !IO).

:- mode test2 == out.

:- func f(int) = int.
:- mode f(in) = test2 is det.

f(X) = Y :- Y = X + 1.

:- func g(int) = int.

g(X) = X + 2.

:- func test = int.
:- mode test = out is det.
:- mode test = in is semidet.

test = 123.

% test type inference

test2 = 456.
