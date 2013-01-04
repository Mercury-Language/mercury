%
%	Patricia Fasel
%	Los Alamos National Laboratory
%	1990 August
%      Translated from Haskell to Mercury by Peter Schachte, 2010-12-23
%
:- module push_particle.
:- interface.

:- import_module pic_type.

% Phase IV: Particle push
% Each particle has an initial velocity determined by its motion during the
%     previous timestep and is accelerated by the field induced by the 
%     collection of particles
%
% Compute the acceleration of each particle
% Find the maximum acceleration in x and y directions
% Determine a safe delta t, such that no particle will move a
%     distance greater than the cell
% Compute new position and velocity of each particle

:- pred push_particle(particle_heap::in, particle_heap::out,
		      electric::in, value::in,
		      value::in, value::out, value::in, value::out) is det.

:- implementation.

:- import_module consts.
:- import_module utils.
:- import_module require.
:- import_module list.
:- import_module int, float.


push_particle(!Pheap, Electric, Dt, !Maxacc, !Maxvel) :-
	(   !.Pheap = particle_heap([],[])
	->  true
	;   !.Pheap = particle_heap([position(Xpos,Ypos)|Xypos],
				    [velocity(Xvel,Yvel)|Xyvel])
	->  !:Pheap = particle_heap(Xypos,Xyvel),
	    Electric = electric(Xelec, Yelec),
	% (maxAcc2, maxVel2, 
	%     (((xPos1,yPos1):xyPos1), ((xVel1,yVel1):xyVel1)))
	    I = truncate_to_int(Xpos),
	    J = truncate_to_int(Ypos),
	    I1 = (I+1) `rem` nCell,
	    J1 = (J+1) `rem` nCell,
	    Dx = Xpos - float(I),
	    Dy = Ypos - float(J),
	    Xacc = (charge/mass) *
			(Xelec^elem(I,J)*(1.0-Dy) + Xelec^elem(I,J1)*Dy),
	    Yacc = (charge/mass) *
			(Yelec^elem(I,J)*(1.0-Dx) + Yelec^elem(I1,J)*Dx),
	    Xtv = Xacc*Dt + Xvel,
	    Ytv = Yacc*Dt + Yvel,
	    Xt = Xtv*Dt + Xpos,
	    Yt = Ytv*Dt + Ypos,
	    !:Maxacc = max(!.Maxacc, max(abs(Xacc), abs(Yacc))),
	    !:Maxvel = max(!.Maxvel, max(abs(Xtv), abs(Ytv))),
	    Xpos1 = (   Xt >= float(nCell)
		    ->  Xt - float(nCell)
		    ;   Xt < 0.0
		    ->  Xt + float(nCell)
		    ;	Xt
		    ),
	    Ypos1 = (   Yt >= float(nCell)
		    ->  Yt - float(nCell)
		    ;   Yt < 0.0
		    ->  Yt + float(nCell)
		    ;	Yt
		    ),
	    push_particle(!Pheap, Electric, Dt, !Maxacc, !Maxvel),
	    !.Pheap = particle_heap(Xypos1, Xyvel1),
	    !:Pheap = particle_heap([position(Xpos1,Ypos1)|Xypos1],
				    [velocity(Xtv,Ytv)|Xyvel1])
	;   error("coordinated lists not the same length")
	).
