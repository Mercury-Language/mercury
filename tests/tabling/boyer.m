% generated: 20 November 1989
% option(s): 
%
%   boyer
%
%   Evan Tick (from Lisp version by R. P. Gabriel)
%
%   November 1985
%
%   prove arithmetic theorem

%	in Mercury by Bart Demoen - started 17 Jan 1997

:- module boyer .

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.

main --> boyer.

:- pred boyer(io__state::di, io__state::uo) is det .

boyer -->
	{wff(Wff)} ,
        io__write_string("rewriting...") ,
	{ rewrite(Wff,NewWff) } ,
        io__write_string("proving...") ,
	({tautology(NewWff,[],[])} ->  io__write_string("done...\n") 
	; io__write_string("not done ...")
	) .

:- type type_wff --->
		(implies(type_wff,type_wff) ;
		 and(type_wff,type_wff) ;
		 f(type_wff) ;
		 plus(type_wff,type_wff) ;
		 equal(type_wff,type_wff) ;
		 append(type_wff,type_wff) ;
		 lessp(type_wff,type_wff) ;
		 times(type_wff,type_wff) ;
		 reverse(type_wff) ;
		 difference(type_wff,type_wff) ;
		 remainder(type_wff,type_wff) ;
		 member(type_wff,type_wff) ;
		 length(type_wff) ;
		 if(type_wff,type_wff,type_wff) ;
		 a1 ; b1 ; c1 ; d1 ; t1 ; f1 ; x1 ; y1 ; zero ; []
		) .

:- pred wff(type_wff) .
:- mode wff(out) is det .

wff(implies(and(implies(X,Y),
                and(implies(Y,Z),
                    and(implies(Z,U),
                        implies(U,W)))),
            implies(X,W))) :-
        X = f(plus(plus(a1,b1),plus(c1,zero))),
        Y = f(times(times(a1,b1),plus(c1,d1))),
        Z = f(reverse(append(append(a1,b1),[]))),
        U = equal(plus(a1,b1),difference(x1,y1)),
        W = lessp(remainder(a1,b1),member(a1,length(b1))).

:- pred tautology(type_wff,list(type_wff),list(type_wff)) .
:- mode tautology(in,in,in) is semidet .

tautology(Wff,Tlist,Flist) :-
        (truep(Wff,Tlist) -> true
        ;falsep(Wff,Flist) -> fail
        ;Wff = if(If,Then,Else) ->
		(truep(If,Tlist) -> tautology(Then,Tlist,Flist)
		;falsep(If,Flist) -> tautology(Else,Tlist,Flist)
		;tautology(Then,[If|Tlist],Flist), tautology(Else,Tlist,[If|Flist])
                )
	; fail
        ).

:- pragma memo(rewrite/2).

:- pred rewrite(type_wff,type_wff) .
:- mode rewrite(in,out) is det .

rewrite(a1,a1) .
rewrite(b1,b1) .
rewrite(c1,c1) .
rewrite(d1,d1) .
rewrite(f1,f1) .
rewrite(t1,t1) .
rewrite(x1,x1) .
rewrite(y1,y1) .
rewrite(zero,zero) .
rewrite([],[]) .

rewrite(lessp(X1,X2),New) :-
	rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = lessp(Y1,Y2) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(member(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = member(Y1,Y2) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(remainder(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = remainder(Y1,Y2) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(plus(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = plus(Y1,Y2) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(and(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = and(Y1,Y2) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(equal(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = equal(Y1,Y2) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(difference(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = difference(Y1,Y2) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(append(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = append(Y1,Y2) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(times(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = times(Y1,Y2) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(implies(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = implies(Y1,Y2) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(length(X1),New) :-
        rewrite(X1,Y1) ,
	Mid = length(Y1) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(f(X1),New) :-
        rewrite(X1,Y1) ,
	Mid = f(Y1) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(reverse(X1),New) :-
        rewrite(X1,Y1) ,
	Mid = reverse(Y1) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(if(X1,X2,X3),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) , rewrite(X3,Y3) ,
	Mid = if(Y1,Y2,Y3) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

:- pred rewrite_args(list(type_wff),list(type_wff)) .
:- mode rewrite_args(in,out) is det .

rewrite_args([],[]) .
rewrite_args([A|RA],[B|RB]) :- 
        rewrite(A,B),
        rewrite_args(RA,RB).

:- pred truep(type_wff,list(type_wff)) .
:- mode truep(in,in) is semidet .

truep(Wff,List) :- Wff = t1 -> true ; member_chk(Wff,List) .

:- pred falsep(type_wff,list(type_wff)) .
:- mode falsep(in,in) is semidet .

falsep(Wff,List) :- Wff = f1 -> true ; member_chk(Wff,List) .

:- pred member_chk(type_wff,list(type_wff)) .
:- mode member_chk(in,in) is semidet .

member_chk(X,[Y|T]) :- X = Y -> true ; member_chk(X,T).

:- pred equal(type_wff,type_wff) .
:- mode equal(in,out) is semidet .

equal(  and(P,Q),
        if(P,if(Q,t1,f1),f1)
        ).
equal(  append(append(X,Y),Z),
        append(X,append(Y,Z))
        ).
equal(  difference(A,B),
        C
        ) :- difference(A,B,C).
equal(  equal(A,B),
        C
        ) :- eq(A,B,C).
equal(  if(if(A,B,C),D,E),
        if(A,if(B,D,E),if(C,D,E))
        ).
equal(  implies(P,Q),
        if(P,if(Q,t1,f1),t1)
        ).
equal(  length(A),
        B
        ) :- mylength(A,B).
equal(  lessp(A,B),
        C
        ) :- lessp(A,B,C).
equal(  plus(A,B),
        C
        ) :- plus(A,B,C).
equal(  remainder(A,B),
        C
        ) :- remainder(A,B,C).
equal(  reverse(append(A,B)),
        append(reverse(B),reverse(A))
        ).

:- pred difference(type_wff,type_wff,type_wff) .
:- mode difference(in,in,out) is semidet .

difference(X,Y,Z) :-
		(X = Y -> Z = zero
		;
		 (X = plus(A,B), Y = plus(A,C) -> Z = difference(B,C)
		 ;
		  X = plus(B,plus(Y,C)) -> Z = plus(B,C) ; fail
		 )
		) .

:- pred eq(type_wff,type_wff,type_wff) .
:- mode eq(in,in,out) is semidet .

eq(append(A,B), append(A,C), equal(B,C)) .
eq(lessp(X,Y), Z, if(lessp(X,Y),
                     equal(t1,Z),
                     equal(f1,Z))).

:- pred mylength(type_wff,type_wff) .
:- mode mylength(in,out) is semidet .

mylength(reverse(X),length(X)).

:- pred lessp(type_wff,type_wff,type_wff) .
:- mode lessp(in,in,out) is semidet .

lessp(plus(X,Y), plus(X,Z), lessp(Y,Z)) :- true.

:- pred plus(type_wff,type_wff,type_wff) .
:- mode plus(in,in,out) is semidet .

plus(plus(X,Y),Z,
     plus(X,plus(Y,Z))).

:- pred remainder(type_wff,type_wff,type_wff) .
:- mode remainder(in,in,out) is semidet .

remainder(U,V,zero) :-
		(U = V -> true ;
		 U = times(A,B) , (B = V -> true ; A = V)
		) .

:- pred times(type_wff,type_wff,type_wff) .
:- mode times(in,in,out) is semidet .

times(A,B,C) :-
		(B = plus(Y,Z) -> C = plus(times(A,Y),times(A,Z))
		;
		 A = times(X,Y) -> C = times(X,times(Y,B))
		;
		 B = difference(CC,W) , C = difference(times(CC,A),times(W,A))
		) .
