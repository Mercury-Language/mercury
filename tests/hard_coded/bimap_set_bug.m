:- module bimap_set_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bimap, pprint.

:- type element
	---> 	hydrogen
	;	helium
	;	lithium
	;	beryllium
	;	boron
	;	carbon
	;	nitrogen
	;	oxygen
	;	fluorine
	;	neon	
	;	sodium.

:- type symbol
	--->	h
	;	he
	;	li
	;	be
	;	b
	;	c
	;	n
	;	o
	;	f
	;	ne
	;	na.	

main(!IO) :-
	some [!Bimap] (
		bimap.init(!:Bimap),
		bimap.set(!.Bimap, hydrogen,   na,  !:Bimap),	
		bimap.set(!.Bimap, helium,     he,  !:Bimap),
		bimap.set(!.Bimap, lithium,    li,  !:Bimap),
		bimap.set(!.Bimap, beryllium,  be,  !:Bimap),
		bimap.set(!.Bimap, hydrogen,   na,  !:Bimap),	
		bimap.set(!.Bimap, hydrogen,   h,   !:Bimap),	
		bimap.set(!.Bimap, sodium,     h,   !:Bimap),    
		bimap.set(!.Bimap, sodium,     na,  !:Bimap),
		bimap.set(!.Bimap, hydrogen,   h,   !:Bimap),
		Forward = to_doc(bimap.forward_map(!.Bimap)),
		Reverse = to_doc(bimap.reverse_map(!.Bimap)),
		io.write_string("Forward map is:\n", !IO),
		pprint.write(4, Forward, !IO),
		io.nl(!IO),
		io.write_string("Reverse map is:\n", !IO),
		pprint.write(4, Reverse, !IO),
		io.nl(!IO)
	).
