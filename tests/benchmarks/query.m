%   query
%
%   David H. D. Warren
%
%   query population and area database to find coun-
%   tries of approximately equal population density

:- module query.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is cc_multi.

:- implementation.

:- import_module int, prolog.

:- type quad ---> quad(string, int, string, int).

main -->
	( { main1(Out), Out = quad(C1, D1, C2, D2) } ->
		io__write_string(C1),
		io__write_string(" has density "),
		io__write_int(D1),
		io__write_string(" while "),
		io__write_string(C2),
		io__write_string(" has density "),
		io__write_int(D2),
		io__write_string("\n")
	;
		io__write_string("No solutions\n")
	).

:- pred main1(quad).
:- mode main1(out) is nondet.

main1(Out) :-	
	query(Out).

:- pred query(quad).
:- mode query(out) is nondet.

query(quad(C1, D1, C2, D2)) :- 
	density(C1, D1), 
	density(C2, D2),
	D1 > D2,
	T1 is 20 * D1,
	T2 is 21 * D2,
	T1 < T2.

:- pred density(string, int).
:- mode density(out, out) is nondet.

density(C, D) :- 
	pop(C, P),
	area(C, A),
	P100 is P * 100,
	D is P100 // A.

:- pred pop(string, int).
:- mode pop(out, out) is multidet.

% populations in 100000s
pop("china",		8250).
pop("india",		5863).
pop("ussr",		2521).
pop("usa",		2119).
pop("indonesia",	1276).
pop("japan",		1097).
pop("brazil",		1042).
pop("bangladesh",	 750).
pop("pakistan", 	 682).
pop("w_germany",	 620).
pop("nigeria",		 613).
pop("mexico",		 581).
pop("uk",		 559).
pop("italy",		 554).
pop("france",		 525).
pop("philippines",	 415).
pop("thailand",		 410).
pop("turkey",		 383).
pop("egypt",		 364).
pop("spain",		 352).
pop("poland",		 337).
pop("s_korea",		 335).
pop("iran",		 320).
pop("ethiopia",		 272).
pop("argentina",	 251).

:- pred area(string, int).
:- mode area(in, out) is semidet.

% areas in 1000s of square miles
area("china",		3380).
area("india",		1139).
area("ussr",		8708).
area("usa",		3609).
area("indonesia",	 570).
area("japan",		 148).
area("brazil",		3288).
area("bangladesh",	  55).
area("pakistan",	 311).
area("w_germany",	  96).
area("nigeria",		 373).
area("mexico",		 764).
area("uk",		  86).
area("italy",		 116).
area("france",		 213).
area("philippines",	  90).
area("thailand",	 200).
area("turkey",		 296).
area("egypt",		 386).
area("spain",		 190).
area("poland",		 121).
area("s_korea",		  37).
area("iran",		 628).
area("ethiopia",	 350).
area("argentina",	1080).
