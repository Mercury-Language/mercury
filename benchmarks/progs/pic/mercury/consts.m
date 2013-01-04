%	Patricia Fasel
%	Los Alamos National Laboratory
%	PIC: Particle in Cell
%	1990 August
%	Translated from Haskell to Mercury by Peter Schachte, 2010-12-23

:- module consts.
:- interface.
:- import_module pic_type.

:- func nCell = indx.
:- func nStep = indx.
:- func maxDepth = indx.
:- func charge = value.
:- func mass = value.

:- implementation.
:- import_module int.

nCell		= 16.			% number of Cells
nStep		= 10.			% number of time steps
maxDepth	= log2(nCell) - 1.	%
charge		= 1.0.
mass		= 1.0.
