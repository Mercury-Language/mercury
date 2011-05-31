% vim: ts=4 sw=4 et ft=mercury

% Authors : schachte (Peter Schachte)
% Purpose : Matrix multiply using a list encoding
%
% Transliterated from Ciao Prolog
% Origin:  http://www.ciaohome.org/ciao_html/ciao_111.html#SEC458
%
% Adopted into a test case by Paul Bone.

:- module impure_wait.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int.
:- import_module require.
:- import_module string.

main(!IO) :-
    Size = 100,
    matrix(1, Size, M1),
    matrix(2, Size, M2),
    mmultiply(M1, M2, M),
    print_matrix(M, !IO).

:- type matrix == list(list(int)).

:- pred matrix(int::in, int::in, matrix::out) is det.

matrix(M, N, Matrix) :-
    matrix_rows(M, N, N, Matrix).

:- pred matrix_rows(int::in, int::in, int::in, matrix::out) is det.

matrix_rows(M, N, R, Matrix) :-
    ( R =< 0 ->
        Matrix = []
    ;
        matrix_row(M, N, Row),
        matrix_rows(M+1, N, R-1, Rows),
        Matrix = [Row | Rows]
    ).

:- pred matrix_row(int::in, int::in, list(int)::out) is det.

matrix_row(M, N, Row) :-
    ( N = 0 ->
        Row = []
    ;
        matrix_row(M+1, N-1, Ms),
        Row = [M | Ms]
    ).

:- pred print_matrix(matrix::in, io::di, io::uo) is det.

print_matrix([], !IO) :-
    nl(!IO).
print_matrix([Row | Rows], !IO) :-
    print_row(Row, !IO),
    print_matrix(Rows, !IO).

:- pred print_row(list(int)::in, io::di, io::uo) is det.

print_row([], !IO) :-
    nl(!IO).
print_row([N | Ns], !IO) :-
    format("  %8d", [i(N)], !IO),
    print_row(Ns, !IO).

:- pred mmultiply(matrix::in, matrix::in, matrix::out) is det.

    % One of the auto-parallelisations expands mmultiply in this way.  V0
    % becomes a shared variable and it's wait is pushed into the call to
    % mutiply, where it is not used by the base case and used by the recursive
    % case only after the recursive call.  Therefore a wait is inserted at the
    % end of the procedure but optimised out later on.
mmultiply(L0, V1, L) :-
    (
        L0 = [],
        L = []
    ; 
        L0 = [_ | Rest],
        (
            (
                mmultiply(Rest, V1, Others),
                V0 = det_head(L0)
            )
        &
            multiply(V1, V0, Result)
        ),
        L = [Result | Others]
    ).
            
:- pred multiply(matrix::in, list(int)::in, list(int)::out) is det.

multiply([], _, []).
multiply([V0 | Rest], V1, [Result | Others]):-
    multiply(Rest, V1, Others),
    vmul(V0, V1, Result).

:- pred vmul(list(int)::in, list(int)::in, int::out) is det.

vmul([], [], 0).
vmul([], [_ | _], _) :-
    error("mmultiply: incompatible matrix sizes").
vmul([H1 | T1], [H2 | T2], Result):-
    vmul(T1, T2, Newresult),
    Result = H1 * H2 + Newresult.
vmul([_ | _], [], _) :-
    error("mmultiply: incompatible matrix sizes").
