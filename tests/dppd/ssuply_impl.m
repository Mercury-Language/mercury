
:- module ssuply_impl.

:- interface.

:- type supplier
	--->	s1
	;	s2
	;	s3
	;	s4
	;	s5.

:- type part 
	--->	p1
	;	p2
	;	p3.

:- type supplier_name
	--->	smith
	;	jones
	;	blake
	;	clark
	;	adams.

:- type city
	--->	london
	;	paris
	;	athens
	;	hongkong
	;	regina
	;	saskatoon
	;	rome.

:- pred ssuply(supplier::in, part::in, int::out) is semidet.

:- implementation.

:- import_module int.

ssuply( Snum, Pnum, Qty ) :-
    london_red_heavy_parts( Pnum, _Pname, _Weight ),
    best_suppliers( Snum, _Sname, _Status, _City ),
    supply( Snum, Pnum, Qty ).

:- pred best_suppliers(supplier::in, supplier_name::out, 
		int::out, city::out) is nondet.

best_suppliers( Snum, Sname, Status, Scity ) :-
    good_suppliers( Snum, Sname, Status, Scity ),
    good_cities( City ),
    City = Scity.

:- pred good_suppliers(supplier::in, supplier_name::out,
		int::out, city::out) is semidet.

good_suppliers( Snum, Sname, Status, City ) :-
    suppliers( Snum, Sname, Status, City ),
    Status > 10 .

:- pred london_red_heavy_parts(part::in, part_name::out, int::out) is semidet.

london_red_heavy_parts( Pnum, Pname, Weight ) :-
    london_red_parts( Pnum, Pname, Weight ),
    london_heavy_weights( Pno, Pname, _Colour, Weight ),
    Pnum = Pno .

:- pred london_red_parts(part::in, part_name::out, int::out) is semidet.

london_red_parts( Pnum, Pname, Weight ) :-
    red_parts( Pnum, Pname, Weight, City ),
    City = london .

:- pred red_parts(part::in, part_name::out, int::out, city::out) is semidet.

red_parts( Pnum, Pname, Weight, City ) :-
    parts( Pnum, Pname, Colour, Weight, City ),
    Colour = red .

:- pred london_heavy_weights(part::in, part_name::in, 
		colour::out, int::out) is semidet.

london_heavy_weights( Pnum, Pname, Colour, Weight ) :-
    heavy_weights( Pnum, Pname, Colour, Weight, City ),
    City = london .

:- pred heavy_weights(part::in, part_name::in, colour::out, 
		int::out, city::out) is semidet.

heavy_weights( Pnum, Pname, Colour, Weight, City ) :-
    parts( Pnum, Pname, Colour, Weight, City ),
    Weight > 10 .

:- pred good_cities(city).
:- mode good_cities(in) is semidet.
:- mode good_cities(out) is multi.

good_cities( paris ).
good_cities( london ).
good_cities( hongkong ).
good_cities( regina ).
good_cities( saskatoon ).

:- pred supply(supplier::in, part::in, int::out) is semidet.

supply( s1, p1, 300 ).
supply( s1, p2, 200 ).
supply( s1, p3, 400 ).
supply( s2, p1, 300 ).
supply( s2, p2, 400 ).
supply( s3, p1, 400 ).
supply( s4, p1, 200 ).
supply( s5, p1, 500 ).
supply( s5, p2, 400 ).


:- type part_name
	--->	nut
	;	bolt
	;	screw.

:- type colour
	--->	red
	;	green
	;	blue.

:- pred parts(part::in, part_name::out, colour::out, 
		int::out, city::out) is det.

parts( p1, nut, red, 12, london ).
parts( p2, bolt, green, 17, paris ).
parts( p3, screw, blue, 17, rome ).

:- pred suppliers(supplier::in, supplier_name::out, 
		int::out, city::out) is det.

suppliers( s1, smith, 20, london ).
suppliers( s2, jones, 10, paris ).
suppliers( s3, blake, 30, paris ).
suppliers( s4, clark, 20, london ).
suppliers( s5, adams, 30, athens ).

