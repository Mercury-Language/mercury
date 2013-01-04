% 
%      Patricia Fasel
%      Los Alamos National Laboratory
%      1990 August
%      Translated from Haskell to Mercury by Peter Schachte, 2010-12-23
%
:- module potential.

:- interface.

:- import_module pic_type.

% Given charge density matrix, rho
% Compute new electrostatic potential phi' where del2(phi') = rho
% phi from the previous timestep is used as the initial value
% assume:	phi = phi' + error
% then:	d_phi = Laplacian(phi) = Laplacian(phi') + Laplacian(error)
%	 	d_error = d_phi - rho = Laplacian(error)
% 		error' = InvLaplacian(d_error) = InvLaplacian(Laplacian(error))
% 		phi' = phi - error'

:- func potential(phi, rho, indx, indx) = phi.

:- implementation.

:- import_module consts.
:- import_module utils.
:- import_module int, float.


potential(Phi, Rho, Depth, Niter) =
	(   Niter =< 0 -> Phi
	;   potential(vCycle(Rho, Phi, nCell, Depth),
		      Rho, Depth, Niter-1)
	).


% vCycle is a multigrid laplacian inverse
% Given d_phi, find phi where Laplacian(phi) = d_phi
% Algorithm is to invert d_phi on a course mesh and interpolate to get phi

:- func vCycle(phi, rho, indx, indx) = phi.

vCycle(Phi, Rho, N, Depth) = Result :-
	Phi1 = relax(Phi, Rho, N),
	(   Depth = 0
	->  Result = relax(Phi1, Rho, N)
	;   Nhalf = N `div` 2,
	    Rho1 = residual(Phi1, Rho, N),
	    Rcoarse = coarsemesh(Rho1, N),
	    Ezero = build_mesh(Nhalf, Nhalf, 0.0, (func(_,_)=0.0)),
	    Ecoarse = vCycle(Ezero, Rcoarse, Nhalf, Depth-1),
	    Result = correct(Phi1, Ecoarse, N, Nhalf)
	).


% laplacian operator
% mesh configuration where e=(i,j) position, b + d + f + h - 4e
% a   b   c
% d   e   f
% g   h   i
 
:- func laplacianOp(mesh, range) = value.

laplacianOp(Mesh, range(I,J)) =
        -(Mesh^elem(I-1,J) + Mesh^elem(I,J+1) + Mesh^elem(I,J-1) +
	  Mesh^elem(I+1,J) - 4.0*Mesh^elem(I,J)).


% subtract laplacian of mesh from mesh'
% residual = mesh' - Laplacian(mesh)

:- func residual(phi, rho, indx) = rho.

residual(Mesh, Mesh1, N) = applyOpTomesh(residualOp(Mesh1), Mesh, N).


:- func residualOp(rho, phi, range) = value.

residualOp(Mesh1, Mesh, range(I,J)) =
	Mesh1^elem(I,J) - laplacianOp(Mesh, range(I,J)).


:- func relax(phi, rho, indx) = phi.

relax(Mesh, Mesh1, N) =	applyOpTomesh(relaxOp(Mesh1), Mesh, N).


:- func relaxOp(rho, phi, range) = value.

relaxOp(Mesh1, Mesh, range(I,J)) =	
	0.25 * Mesh1^elem(I,J) + 
	0.25 * (Mesh^elem(I-1,J)+Mesh^elem(I,J-1)+
	        Mesh^elem(I,J+1)+Mesh^elem(I+1,J)).

:- func correct(phi, mesh, indx, indx) = phi.

correct(Phi, Ecoarse, N, Nhalf) =
	build_mesh(N, N, 0.0,
		      (func(I,J) = Phi^elem(I,J) + Efine^elem(I,J))) :-
	    Efine = finemesh(Ecoarse, Nhalf).
