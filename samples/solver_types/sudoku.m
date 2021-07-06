%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% Author: Ralph Becket
%
% Sudoku test case for the eqneq type.
%
%-----------------------------------------------------------------------------%

:- module sudoku.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module eqneq.
:- import_module int.
:- import_module list.
:- import_module string.

:- pragma require_feature_set([trailing]).

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if Args = [InputFile] then
        io.see(InputFile, SeeResult, !IO),
        (
            SeeResult = ok,
            io.read_file_as_string(ReadResult, !IO),
            (
                ReadResult = ok(StartString),
                Start = string.words(StartString),
                ( if solve_sudoku(Start, Solution) then
                    write_solution(9, 1, Solution, !IO)
                else
                    io.write_string("No solution.\n", !IO)
                )
            ;
                ReadResult = error(_, Error),
                io.write_string(io.error_message(Error), !IO),
                io.nl(!IO),
                io.set_exit_status(1, !IO)
            )
        ;
            SeeResult = error(Error),
            io.write_string(io.error_message(Error), !IO),
            io.nl(!IO),
            io.set_exit_status(1, !IO)
        )
    else
        io.write_string("usage: sudoku <filename>\n", !IO),
        io.set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------------%

    % Solve a sudoku problem.
    %
:- pred solve_sudoku(list(string)::in, list(int)::out) is nondet.

solve_sudoku(Start, Solution) :-
    % The following if-then-else is used to ensure that the call to
    % label_board/2 occurs after the constraints have been posted.
    % (In principle the compiler could reorder the code so that the call
    % to label_board/2 occurs before the constraints have been posted.)
    ( if
        % Set up the board.
        init_board(Start, Board),

        Board = [X11, X12, X13,  X14, X15, X16,  X17, X18, X19,
                 X21, X22, X23,  X24, X25, X26,  X27, X28, X29,
                 X31, X32, X33,  X34, X35, X36,  X37, X38, X39,

                 X41, X42, X43,  X44, X45, X46,  X47, X48, X49,
                 X51, X52, X53,  X54, X55, X56,  X57, X58, X59,
                 X61, X62, X63,  X64, X65, X66,  X67, X68, X69,

                 X71, X72, X73,  X74, X75, X76,  X77, X78, X79,
                 X81, X82, X83,  X84, X85, X86,  X87, X88, X89,
                 X91, X92, X93,  X94, X95, X96,  X97, X98, X99],

        % The digits in each row must be different.
        eqneq.all_different([X11, X12, X13, X14, X15, X16, X17, X18, X19]),
        eqneq.all_different([X21, X22, X23, X24, X25, X26, X27, X28, X29]),
        eqneq.all_different([X31, X32, X33, X34, X35, X36, X37, X38, X39]),
        eqneq.all_different([X41, X42, X43, X44, X45, X46, X47, X48, X49]),
        eqneq.all_different([X51, X52, X53, X54, X55, X56, X57, X58, X59]),
        eqneq.all_different([X61, X62, X63, X64, X65, X66, X67, X68, X69]),
        eqneq.all_different([X71, X72, X73, X74, X75, X76, X77, X78, X79]),
        eqneq.all_different([X81, X82, X83, X84, X85, X86, X87, X88, X89]),
        eqneq.all_different([X91, X92, X93, X94, X95, X96, X97, X98, X99]),

        % The digits in each column must be different.
        eqneq.all_different([X11, X21, X31, X41, X51, X61, X71, X81, X91]),
        eqneq.all_different([X12, X22, X32, X42, X52, X62, X72, X82, X92]),
        eqneq.all_different([X13, X23, X33, X43, X53, X63, X73, X83, X93]),
        eqneq.all_different([X14, X24, X34, X44, X54, X64, X74, X84, X94]),
        eqneq.all_different([X15, X25, X35, X45, X55, X65, X75, X85, X95]),
        eqneq.all_different([X16, X26, X36, X46, X56, X66, X76, X86, X96]),
        eqneq.all_different([X17, X27, X37, X47, X57, X67, X77, X87, X97]),
        eqneq.all_different([X18, X28, X38, X48, X58, X68, X78, X88, X98]),
        eqneq.all_different([X19, X29, X39, X49, X59, X69, X79, X89, X99]),

        % The digits in each subsquare must be different.
        eqneq.all_different([X11, X12, X13, X21, X22, X23, X31, X32, X33]),
        eqneq.all_different([X14, X15, X16, X24, X25, X26, X34, X35, X36]),
        eqneq.all_different([X17, X18, X19, X27, X28, X29, X37, X38, X39]),
        eqneq.all_different([X41, X42, X43, X51, X52, X53, X61, X62, X63]),
        eqneq.all_different([X44, X45, X46, X54, X55, X56, X64, X65, X66]),
        eqneq.all_different([X47, X48, X49, X57, X58, X59, X67, X68, X69]),
        eqneq.all_different([X71, X72, X73, X81, X82, X83, X91, X92, X93]),
        eqneq.all_different([X74, X75, X76, X84, X85, X86, X94, X95, X96]),
        eqneq.all_different([X77, X78, X79, X87, X88, X89, X97, X98, X99])
    then
        % Assign a digit to each square on the board.
        label_board(Board, Solution)
    else
        false
    ).

%-----------------------------------------------------------------------------%

    % Convert a board description into a board representation. Each "word" in
    % the board description is either a digit, in which case we fix that board
    % entry in the representation, or a non-digit, in which case we leave that
    % board entry unconstrained.
    %
:- pred init_board(list(string)::in, list(eqneq(int))::oa) is semidet.

init_board([], []).
init_board([Start | Starts], [EqNeq | EqNeqs]) :-
    eqneq.new(EqNeq),
    ( if string.to_int(Start, X) then
        eqneq.bind(EqNeq, X)
    else
        true
    ),
    init_board(Starts, EqNeqs).

%-----------------------------------------------------------------------------%

    % Assign a digit to each square on the board.
    %
:- pred label_board(list(eqneq(int))::ia, list(int)::out) is nondet.

label_board([], []).
label_board([EqNeq | EqNeqs], [X | Xs]) :-
    ( X = 1 ; X = 2 ; X = 3
    ; X = 4 ; X = 5 ; X = 6
    ; X = 7 ; X = 8 ; X = 9
    ),
    eqneq.bind(EqNeq, X),
    label_board(EqNeqs, Xs).

%-----------------------------------------------------------------------------%

    % Pretty-print a solution.
    %
:- pred write_solution(int::in, int::in, list(int)::in, io::di, io::uo) is det.

write_solution(_, _, [], !IO) :-
    io.nl(!IO).
write_solution(N, R, [X | Xs], !IO) :-
    ( if N = 0 then
        io.nl(!IO),
        ( if (R mod 3) = 0 then
            io.nl(!IO)
        else
            true
        ),
        write_solution(9, R + 1, [X | Xs], !IO)
    else
        io.write_int(X, !IO),
        io.write_char(' ', !IO),
        ( if (N mod 3) = 1 then
            io.write_char(' ', !IO)
        else
            true
        ),
        write_solution(N - 1, R + 1, Xs, !IO)
    ).

%-----------------------------------------------------------------------------%
:- end_module sudoku.
%-----------------------------------------------------------------------------%
