%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

% Test predicates from the int module that operate over int ranges.
% The fold ops are tested separately in int_fold_up_down.m.

:- module int_range_ops.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

%---------------------------------------------------------------------------%

main(!IO) :-
    % Test nondet_int_in_range/3.
    solutions(nondet_int_in_range(10, 0), EmptyRangeResult),
    io.write_string("solutions(nondet_int_in_range(1, 0)) = ", !IO),
    io.print_line(EmptyRangeResult, !IO),
    solutions(nondet_int_in_range(0, 1), SingletonRangeResult),
    io.write_string("solutions(nondet_int_in_range(1, 1)) = ", !IO),
    io.print_line(SingletonRangeResult, !IO),
    solutions(nondet_int_in_range(1, 10), ManyRangeResult),
    io.write_string("solutions(nondet_int_in_range(1, 10)) = ", !IO),
    io.print_line(ManyRangeResult, !IO),

    % Test all_true_in_range/3.
    io.write_string("all_true_in_range(int.even, 1, 0): ", !IO),
    ( if all_true_in_range(int.even, 1, 0) then
        io.print_line("true", !IO)
    else
        io.print_line("false", !IO)
    ),
    io.write_string("all_true_in_range(int.even, 1, 1): ", !IO),
    ( if all_true_in_range(int.even, 1, 1) then
        io.print_line("true", !IO)
    else
        io.print_line("false", !IO)
    ),
    io.write_string("all_true_in_range(int.odd, 1, 1): ", !IO),
    ( if all_true_in_range(int.odd, 1, 1) then
        io.print_line("true", !IO)
    else
        io.print_line("false", !IO)
    ),
    LessThan10 = (pred(I::in) is semidet :- I < 10),
    io.write_string("all_true_in_range(LessThan10, 1, 5): ", !IO),
    ( if all_true_in_range(LessThan10, 1, 5) then
        io.print_line("true", !IO)
    else
        io.print_line("false", !IO)
    ),
    io.write_string("all_true_in_range(LessThan10, 8, 11): ", !IO),
    ( if all_true_in_range(LessThan10, 8, 11) then
        io.print_line("true", !IO)
    else
        io.print_line("false", !IO)
    ).

%---------------------------------------------------------------------------%
