%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use62.

:- interface.

:- use_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, string.
:- import_module qcheck, rnd.

%---------------------------------------------------------------------------%
%	arbitrary user-defined types for testing purposes
%---------------------------------------------------------------------------%

:- type bullet 
        --->    good(color) 
        ;       inaccurate(color) 
        ;      	defective(color).

:- type color
	--->	black
	;	white.	

%---------------------------------------------------------------------------%

main -->
	{ freq_B(B) },
	{ freq_W(W) },
        qcheck(qcheck__f(prop2), "bullet fight", 10000, [[],B,W], []).

:- pred freq_B(list(frequency)).
:- mode freq_B(out) is det.
freq_B(Out) :-
	Out = [ {50, [ [ {100, []}, {0, []} ] ] },
                {10, [ [ {100, []}, {0, []} ] ] },
                {40, [ [ {100, []}, {0, []} ] ] }
              ].

:- pred freq_W(list(frequency)).
:- mode freq_W(out) is det.
freq_W(Out) :-
	Out = [ {40, [ [ {0, []}, {100, []} ] ] },
                {30, [ [ {0, []}, {100, []} ] ] },
                {30, [ [ {0, []}, {100, []} ] ] }
              ].

:- func prop2(int, bullet, bullet) = property.
prop2(Seed, B, W) = fight(Seed, B, W) `>>>` 
			({"ComB",B} `>>>` 
				({"ComW", W} `>>>` [yes])
			).

:- func fight(int, bullet, bullet) = string.
:- mode fight(in, in, in) = out is det.
fight(Seed, B, W) = String :-
	rnd__init(Seed, RS0),
	B_hit = is_hit(B, RS0, RS1),
	W_hit = is_hit(W, RS1, _),
	(if		B_hit = W_hit
	 then
			String = "draw"
	 else if	B_hit > W_hit
	      then
			String = "B win"
	 else
			String = "W win"
	).

:- func is_hit(bullet, rnd, rnd) = int.
:- mode is_hit(in, in, out) = out is det.
is_hit(Bullet, RS0, RS) = Int :-
	Temp = rand_allint(RS0, RS) rem 2,
	(
		Bullet = good(_),
		Int = 1
	;
		Bullet = inaccurate(_),
		(if	Temp = 0	
		 then		
			Int = 1
		 else
			Int = 0
		)
	;
		Bullet = defective(_),
		Int = 0
	).
