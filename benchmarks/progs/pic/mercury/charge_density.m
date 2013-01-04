%      Patricia Fasel
%      Los Alamos National Laboratory
%      1990 August

:- module charge_density.
:- interface.

:- import_module pic_type.

:- func chargeDensity(particle_heap) = rho.


% Phase I: calculate the charge density rho
% Each particle represents an amount of charge distributed over an entire cell.
% In discretizing the charge density at the grid points we split the cell into
%    four rectangles and assign to each corner an amount of charge proportional
%    to the area of the opposite diagonal sub rectangle
% So each particle contributes a portion of charge to four quadrants
% particle position is (i+dx, j+dy)
% nw quadrant (1-dx)(1-dy)(charge) is added to rho(i,j)
% ne quadrant (dx)(1-dy)(charge) is added to rho(i,j+1)
% sw quadrant (1-dx)(dy)(charge) is added to rho(i+1,j)
% se quadrant (dx)(dy)(charge) is added to rho(i+1,j+1)
% wrap around used for edges and corners

:- implementation.

:- import_module consts.
:- import_module utils.
:- import_module list.
:- import_module float.
:- import_module int.

chargeDensity(particle_heap(Xypos,_)) =
	accum_mesh((+), 0.0, N, N, accumCharge(Xypos)) :-
		  N = nCell - 1.


% for each particle, calculate the distribution of density
% based on (x,y), a proportion of the charge goes to each rho

:- func accumCharge(list(position)) = list(mesh_assoc).

accumCharge([]) = [].
accumCharge([position(X,Y)|Xys]) =
	[mesh_assoc(I ,J , charge * (1.0-Dx) * (1.0-Dy)),
	 mesh_assoc(I1,J , charge * Dx * (1.0-Dy)),
	 mesh_assoc(I ,J1, charge * (1.0-Dx) * Dy),
	 mesh_assoc(I1,J1, charge * Dx * Dy) | accumCharge(Xys)] :-
		I  = truncate_to_int(X),
		I1 = (I+1) rem nCell,
		J  = truncate_to_int(Y),
		J1 = (J+1) rem nCell,
		Dx = X - float(I),
		Dy = Y - float(J).
