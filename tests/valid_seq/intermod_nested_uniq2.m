%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_nested_uniq2.

:- interface.

:- import_module array.

:- inst uniq_f_matrix == unique(f_matrix(ground, ground, uniq_array)).

:- mode f_matrix_di == di(uniq_f_matrix).
:- mode f_matrix_uo == out(uniq_f_matrix).
:- mode f_matrix_ui == in(uniq_f_matrix).

:- type f_matrix.

:- pred init(int, int, f_matrix).
:- mode init(in, in, f_matrix_uo) is det.

    % Lookup element (I, J) in the f_matrix.
    % If (I, J) is not a valid index to the matrix, the behaviour is undefined.
    %
:- pred lookup(int::in, int::in, f_matrix::f_matrix_ui, float::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module float.
:- import_module int.

:- type f_matrix
    --->    f_matrix(
                int,            % M
                int,            % N
                array(float)    % the elements of the matrix
            ).

%---------------------------------------------------------------------------%

init(M, N, Matrix) :-
    array.init(M * N, 0.0, Array),
    Matrix = f_matrix(M, N, Array).

lookup(I, J, f_matrix(_M, N, Array), Elem) :-
    array.lookup(Array, (I * N) + J, Elem).
