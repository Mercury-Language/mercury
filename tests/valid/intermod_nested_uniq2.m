%-----------------------------------------------------------------------------%
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

:- pred lookup(int, int, f_matrix, float).
:- mode lookup(in, in, f_matrix_ui, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array, float, int.

:- type f_matrix --->
	f_matrix(
		 int,		% M
		 int,		% N
		 array(float)	% the elements of the matrix
		).

%-----------------------------------------------------------------------------%

init(M, N, Matrix) :-
	array__init(M * N, 0.0, Array),
	Matrix = f_matrix(M, N, Array).

% Lookup element (I, J) in the f_matrix.
% If (I, J) is not a valid index to the matrix, the behaviour is undefined
lookup(I, J, f_matrix(_M, N, Array), Elem) :-
	array__lookup(Array, (I * N) + J, Elem).

