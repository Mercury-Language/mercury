%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The code caused the compiler the abort, rather than report an error
% for the unsatisfiable constraints.
%
% Software Error: map.lookup: key not found
% Key Type: term.var(parse_tree.prog_data.prog_var_type)
% Key Value: var(6)
% Value Type: parse_tree.prog_data.mer_type

:- module unsatisfiable_constraint_bug.
:- interface.
:- import_module io.
:- import_module stream.

:- pred test_stream(S::in, B::di, B::uo, io::di, io::uo) is det
    <= (reader(S, int, B, E), writer(S, int, B)).
    % XXX add these two lines and the program compiles
    % reader(S, float, B, E), writer(S, float, B)).

:- implementation.

test_stream(S, !Buffer, !IO) :-
    stream.put(S, 10, !Buffer),
    stream.put(S, 3.14, !Buffer),
    stream.get(S, ResultA : stream.result(int, E), !Buffer),
    stream.get(S, ResultB : stream.result(float, E), !Buffer),
    stream.get(S, ResultC : stream.result(float, E), !Buffer),
    io.write_line(ResultA, !IO),
    io.write_line(ResultB, !IO),
    io.write_line(ResultC, !IO).
