% 
%      Patricia Fasel
%      Los Alamos National Laboratory
%      1990 August
%      Translated from Haskell to Mercury by Peter Schachte, 2010-12-23
%
:- module utils.
:- interface.
:- import_module pic_type.
:- import_module list.

% coarsemesh, finemesh
:- func build_mesh(int, int, value, (func(int,int)=value)) = mesh.
:- func accum_mesh((func(value,value)=value), value, int, int,
		      list(mesh_assoc)) = mesh.
:- func applyOpTomesh((func(mesh,range)=value), mesh, indx) = mesh.
:- func coarsemesh(mesh, indx) = mesh.
:- func finemesh(mesh, indx) = mesh.
:- func genRand(value) = value.
:- func int `posmod` int = int.
:- func elem(indx, indx, mesh) = value.
:- func 'elem :='(indx, indx, mesh, value) = mesh.

:- implementation.
:- import_module array2d.
:- import_module int, float.


% apply the given operator to a mesh of given size
% operator is applied to the position and to the 8 surrounding positions
% so value(i,j) is decided by (i-1,j-1), (i,j-1), (i+1,j-1), etc.
% corners and edges are handled as if the mesh was a torus

applyOpTomesh(Operator, Mesh, N1) =
	build_mesh(N1, N1, 0.0,
		      (func(I,J) = Operator(Mesh,range(I,J)))).


build_mesh(X, Y, Dummy, F) = mesh(X, Y, A) :-
	A0 = array2d.init(X, Y, Dummy),
	map_array2d_with_indices(X, Y, func(I,J,_)=F(I,J), A0, A).


:- pred map_array2d_with_indices(int::in, int::in, (func(int,int,T)=T)::in,
				 array2d(T)::array2d_di,
				 array2d(T)::array2d_uo).

map_array2d_with_indices(X, Y, F, !Array) :-
	(   Y = 0
	->  true
	;   Y1 = Y - 1,
	    map_array2d_with_indices_row(X, Y1, F, !Array),
	    map_array2d_with_indices(X, Y1, F, !Array)
	).


:- pred map_array2d_with_indices_row(int::in, int::in, (func(int,int,T)=T)::in,
				     array2d(T)::array2d_di,
				     array2d(T)::array2d_uo).

map_array2d_with_indices_row(X, Y, F, Array0, Array) :-
	(   X = 0
	->  Array = Array0
	;   X1 = X - 1,
	    V = Array0 ^ elem(X1, Y),
	    W = F(X1,Y,V),
	    Array1 = Array0 ^ elem(X1, Y) := W,
	    map_array2d_with_indices_row(X1, Y, F, Array1, Array)
	).


accum_mesh(F, Init, Rows, Columns, Updates) = mesh(Rows, Columns, A) :-
	A0 = array2d.init(Rows, Columns, Init),
	map_mesh_array_updates(Updates, F, Rows, Columns, A0, A).
	

:- pred map_mesh_array_updates(
		list(mesh_assoc)::in, (func(value,value)=value)::in,
		int::in, int::in,
		array2d(value)::array2d_di, array2d(value)::array2d_uo).

map_mesh_array_updates([], _, _, _, !_).
map_mesh_array_updates([mesh_assoc(X,Y,Delta)|Updates], F, R, C, !Array) :-
	X1 = X `posmod` R,
	Y1 = Y `posmod` C,
	!Array ^ elem(X1,Y1) := F(!.Array ^ elem(X1,Y1), Delta),
	map_mesh_array_updates(Updates, F, R, C, !Array).
	


% project a mesh onto a mesh of half the rank
% a  b  c  d  e  f		a  c  e
% g  h  i  j  k  l	=>	m  o  q
% m  n  o  p  q  r		y  0  2
% s  t  u  v  w  x
% y  z  0  1  2  3
% 4  5  6  7  8  9


coarsemesh(Mesh, N) =
		build_mesh(Nhalf, Nhalf, 0.0,
			      func(I,J) = Mesh ^ elem(I*2,J*2)) :-
	Nhalf = N `div` 2.
	    


%================================================================
% interpolate a mesh of half rank onto a full mesh
% values aren't just copied but are a function of the letter shown
% a  b  c  d  e  f		A  B  C
% g  h  i  j  k  l	<=	D  E  F
% m  n  o  p  q  r		G  H  I
% s  t  u  v  w  x
% y  z  0  1  2  3
% 4  5  6  7  8  9
%
% a = A, c = B, e = C, m = D, o = E, q = F, y = G, 0 = H, 2 = I
% g = .5(A+D), i = .5(B+E), k = .5(C+F), b = .5(A+B), d = .5(B+C), f = .5(C+A)
% s = .5(D+G), u = .5(E+H), w = .5(F+I), n = .5(D+E), p = .5(E+F), r = .5(F+D)
% 4 = .5(G+A), 6 = .5(H+B), 8 = .5(I+C), z = .5(G+H), 1 = .5(H+I), 3 = .5(I+G)
% h = .25(A+B+D+E), j = .25(B+C+E+F), l = .25(C+A+F+D) ...
%================================================================
% But actually, it doesn't do that at all.  Instead it puts 6.0 in interior
% cells, 5 where the y index is 0 or n, 4 where x is 0 or n, and 3 where both
% x and y are 0 or n.

finemesh(_Mesh, Nhalf1) = 
		build_mesh(N, N, 0.0,
			      (func(I,J) = 6.0 - X - Y :-
			          ( ( I = 0 ; I = N ) -> X = 2.0 ; X = 0.0 ),
			          ( ( J = 0 ; J = N ) -> Y = 1.0 ; Y = 0.0
				  ))) :-
	N = 2 * Nhalf1.


% random number generator
% really it's more like a floating point hash function

genRand(Seed) =	R1 / 655357.0 :-
	R1 = (31453257.0*Seed + 271829.0) `fiRem` 655357.

:- func value `fiRem` int = value.

X `fiRem` M = X - float((truncate_to_int(X) `div` M) * M).

X `posmod` Y = ((X `mod` Y) + Y) `mod` Y.


% elem for a mesh gives torroidal wrap-around.

mesh(Rows,Cols,Array) ^ elem(I,J) =
	Array ^ elem(I `posmod` Rows, J `posmod` Cols).
     
(mesh(Rows,Cols,Array) ^ elem(I,J) := Value) =
	mesh(Rows,Cols,
	     (	 Array ^ elem(I `posmod` Rows, J `posmod` Cols) := Value)).
