%      Patricia Fasel
%      Los Alamos National Laboratory
%      1990 August
%      Translated from Haskell to Mercury by Peter Schachte, 2010-12-23
%
% Copyright, 1990, The Regents of the University of California.
% This software was produced under a U.S. Government contract (W-7405-ENG-36)
% by the Los Alamos National Laboratory, which is operated by the University
% of California for the U.S. Department of Energy.  The U.S. Government is
% licensed to use, reproduce, and distribute this software.  Permission is
% granted to the public to copy and use this software without charge, provided
% that this notice and any statement of authorship are reproduced on all
% copies.  Neither the Government nor the University makes any warranty,
% express or implied, or assumes any liability for the use of this software.

:- module pic_calc.
:- interface.
:- import_module pic_type.

:- pred pic(indx::in, value::out) is det.

:- implementation.
:- import_module consts.
:- import_module utils.
:- import_module charge_density.
:- import_module potential.
:- import_module elec_field.
:- import_module push_particle.
:- import_module list.
:- import_module float.
:- import_module math.
:- import_module int.

% PIC, particle in cell, a basic electrodynamics application
% Given an initial configuration of particles, follow how they move under the
%     electric field they induce
% Torroidal boundary conditions are assumed, so wrap in both dimensions
% given nPart the number of particles considered
% given nStep the number of time steps to put the particles through
% given nCell the dimension of the matrix of cells

pic(Npart, Result) :-
	Partheap0 = initParticles(Npart),
	timeStep(Partheap0, _, initphi(Partheap0), _, 0.001, Result, 0, nStep).


% during each time step perform the following calculations
% calculate the charge density (rho), using position of particles
% calculate the new potential (phi), by solving Laplace's equation
% 	del2(phi) = rho , using rho and phi of last timestep
% calculate the electric field, E = del(phi), using phi of this time step
% push each particle some distance and velocity using electric field, for a
% 	timestep deltaTime, small enough that no particle moves more than the
% 	width of a cell
% an NxN mesh is used to represent value of x and y in the interval [0,1]
% so delta_x = delta_y = 1/n
%
% phi ((0,0), (n,n)) = electrostatic potential at grid point (i,j)
% rho ((0,0), (n,n)) = charge density at grid point (i,j)
% xElec ((0,0), (n,n)) = x component of electric field between (i,j) (i,j+1)
% yElec ((0,0), (n,n)) = y component of electric field between (i,j) (i+1,j)
% [xyPos] = (x,y) coordinate of particle displacement in units of delta_x
% [xyVel] = (x,y) coordinate of particle velocity in units of delta_x/sec

:- pred timeStep(particle_heap::in, particle_heap::out, phi::in, phi::out,
		 value::in, value::out, indx::in, indx::in) is det.

timeStep(!Partheap, !Phi, !Dt, Depth, Step) :-
	(   Step = 0
	->  true
	;   Rho = chargeDensity(!.Partheap),
	    !:Phi = potential(!.Phi, Rho, Depth, 1),
	    Xyelec = elecField(!.Phi),
	    some [!Maxvel,!Maxacc]
	    (   !:Maxvel = 0.0,
		!:Maxacc = 0.0,
		push_particle(!Partheap, Xyelec, !.Dt, !Maxvel, !Maxacc),
		!:Dt = (sqrt(!.Maxvel * !.Maxvel + 2.0 * !.Maxacc) - !.Maxvel)
			/ !.Maxacc
	    ),
	    Depth1 = (Depth+1) `mod` maxDepth,
	    timeStep(!Partheap, !Phi, !Dt, Depth1, Step-1)
	).


:- func initParticles(indx) = particle_heap.

initParticles(Npart) = particle_heap(Xypos, Xyvel) :-
	Ncelld = float(nCell),
	Npartd = float(Npart+1),
	Xypos = map(randpos(Ncelld,Npartd), 1 `..` Npart),
	Xyvel = duplicate(Npart, velocity(0.0,0.0)).


:- func randpos(value,value,indx) = position.

randpos(Ncelld,Npartd,I) = position(Xpos,Ypos) :-
	Xpos = Ncelld * genRand(float(I) / Npartd),
	Ypos = Ncelld * genRand(Xpos).


:- func initphi(particle_heap) = phi.

initphi(Partheap) = potential(Phi0, Rho, maxDepth, 1) :-
	Rho = chargeDensity(Partheap),
	Phi0 = build_mesh(nCell, nCell, 0.0, (func(_,_)=0.0)).
