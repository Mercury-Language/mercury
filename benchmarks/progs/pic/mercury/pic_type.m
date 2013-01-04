% 
%      Patricia Fasel
%      Los Alamos National Laboratory
%	PIC: Particle in Cell
%      1990 August
%      Translated from Haskell to Mercury by Peter Schachte, 2010-12-23
%
:- module pic_type.
:- interface.
:- import_module list.
:- import_module array2d.

:- type	count		  == int.
:- type	indx		  == int.
:- type	value		  == float.
:- type	mesh		---> mesh(int, int, array2d(value)).
:- type	mesh_assoc	---> mesh_assoc(int, int, value).
:- type	electric	---> electric(mesh, mesh).
:- type	phi		  == mesh.
:- type	rho		  == mesh.
:- type	position	---> position(value, value).
			     % range refers to the specified cell and the 8
			     % immediately surrounding cells, with torroidal
			     % wrap-around.
:- type	range	        ---> range(indx,indx).
:- type	velocity	---> velocity(value, value).
:- type	particle_heap	---> particle_heap(list(position), list(velocity)).
