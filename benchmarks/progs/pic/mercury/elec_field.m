%      Patricia Fasel
%      Los Alamos National Laboratory
%      1990 August
%      Translated from Haskell to Mercury by Peter Schachte, 2010-12-23

:- module elec_field.
:- interface.

:- import_module pic_type.

% Phase III: Calculate electric fields
% the x and y components of the electric field are approximated
% by the first partial difference in each dimension

:- func elecField(phi) = electric.

:- implementation.

:- import_module consts.
:- import_module utils.
:- import_module int.
:- import_module float.

elecField(Phi) =
	electric(build_mesh(nCell, nCell, 0.0,
			       (func(I,J)=
			        Phi^elem(I-1, J) - Phi^elem(I,J))),
		 build_mesh(nCell, nCell, 0.0,
			       (func(I,J)=
			        Phi^elem(I, J+1) - Phi^elem(I,J)))).
