% This is a regression test: it triggered a performance bug in type inference.

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

:- module boyer.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.

main --> b(3) .

% :- pred b(int,io__state,io__state) .
:- mode b(in,di,uo) is det .

b(X) --> {X > 0} -> boyer , {Y = X - 1} , b(Y) ; {true} .

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
		 my_plus(type_wff,type_wff) ;
		 equal1(type_wff,type_wff) ;
		 append(type_wff,type_wff) ;
		 lessp(type_wff,type_wff) ;
		 my_times(type_wff,type_wff) ;
		 reverse(type_wff) ;
		 difference(type_wff,type_wff) ;
		 remainder(type_wff,type_wff) ;
		 b_member(type_wff,type_wff) ;
		 b_length(type_wff) ;
		 if(type_wff,type_wff,type_wff) ;
		 a1 ; b1 ; c1 ; d1 ; t1 ; f1 ; x1 ; y1 ; zero ; nil
		) .

:- pred wff(type_wff) .
:- mode wff(out) is det .

wff(implies(and(implies(X,Y),
                and(implies(Y,Z),
                    and(implies(Z,U),
                        implies(U,W)))),
            implies(X,W))) :-
        X = f(my_plus(my_plus(a1,b1),my_plus(c1,zero))),
        Y = f(my_times(my_times(a1,b1),my_plus(c1,d1))),
        Z = f(reverse(append(append(a1,b1),nil))),
        U = equal1(my_plus(a1,b1),difference(x1,y1)),
        W = lessp(remainder(a1,b1),b_member(a1,b_length(b1))).

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

% :- pred rewrite(type_wff,type_wff) .
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
rewrite(nil,nil) .

rewrite(lessp(X1,X2),New) :-
	rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = lessp(Y1,Y2) ,
	(equal(Mid,Next) -> rewrite(Next,New)
	;
	 New = Mid
	) .

rewrite(b_member(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = b_member(Y1,Y2) ,
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

rewrite(my_plus(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = my_plus(Y1,Y2) ,
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

rewrite(equal1(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = equal1(Y1,Y2) ,
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

rewrite(my_times(X1,X2),New) :-
        rewrite(X1,Y1) , rewrite(X2,Y2) ,
	Mid = my_times(Y1,Y2) ,
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

rewrite(b_length(X1),New) :-
        rewrite(X1,Y1) ,
	Mid = b_length(Y1) ,
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

% :- pred rewrite_args(list(type_wff),list(type_wff)) .
:- mode rewrite_args(in,out) is det .

rewrite_args([],[]) .
rewrite_args([A|RA],[B|RB]) :- 
        rewrite(A,B),
        rewrite_args(RA,RB).

:- pred truep(type_wff,list(type_wff)) .
:- mode truep(in,in) is semidet .

truep(Wff,List) :- Wff = t1 -> true ; member_chk(Wff,List) .

% :- pred falsep(type_wff,list(type_wff)) .
:- mode falsep(in,in) is semidet .

falsep(Wff,List) :- Wff = f1 -> true ; member_chk(Wff,List) .

% :- pred member_chk(type_wff,list(type_wff)) .
:- mode member_chk(in,in) is semidet .

member_chk(X,[Y|T]) :- X = Y -> true ; member_chk(X,T).

% :- pred equal(type_wff,type_wff) .
:- mode equal(in,out) is semidet .

equal(  and(P,Q),
        if(P,if(Q,t1,f1),f1)
        ).
equal(  append(append(X,Y),Z),
        append(X,append(Y,Z))
        ).
equal(  difference(A,B),
        C
        ) :- difference1(A,B,C).
equal(  equal1(A,B),
        C
        ) :- eq(A,B,C).
equal(  if(if(A,B,C),D,E),
        if(A,if(B,D,E),if(C,D,E))
        ).
equal(  implies(P,Q),
        if(P,if(Q,t1,f1),t1)
        ).
equal(  b_length(A),
        B
        ) :- my_length(A,B).
equal(  lessp(A,B),
        C
        ) :- lessp1(A,B,C).
equal(  my_plus(A,B),
        C
        ) :- plus1(A,B,C).
equal(  remainder(A,B),
        C
        ) :- remainder1(A,B,C).
equal(  reverse(append(A,B)),
        append(reverse(B),reverse(A))
        ).

% :- pred difference1(type_wff,type_wff,type_wff) .
:- mode difference1(in,in,out) is semidet .

difference1(X,Y,Z) :-
		(X = Y -> Z = zero
		;
		 (X = my_plus(A,B), Y = my_plus(A,C) -> Z = difference(B,C)
		 ;
		  X = my_plus(B,my_plus(Y,C)) -> Z = my_plus(B,C) ; fail
		 )
		) .

% :- pred eq(type_wff,type_wff,type_wff) .
:- mode eq(in,in,out) is semidet .

eq(append(A,B), append(A,C), equal1(B,C)) .
eq(lessp(X,Y), Z, if(lessp(X,Y),
                     equal1(t1,Z),
                     equal1(f1,Z))).

% :- pred my_length(type_wff,type_wff) .
:- mode my_length(in,out) is semidet .

my_length(reverse(X),b_length(X)).

% :- pred lessp1(type_wff,type_wff,type_wff) .
:- mode lessp1(in,in,out) is semidet .

lessp1(my_plus(X,Y), my_plus(X,Z), lessp(Y,Z)) .

% :- pred plus1(type_wff,type_wff,type_wff) .
:- mode plus1(in,in,out) is semidet .

plus1(my_plus(X,Y),Z,
     my_plus(X,my_plus(Y,Z))).

% :- pred remainder1(type_wff,type_wff,type_wff) .
:- mode remainder1(in,in,out) is semidet .

remainder1(U,V,zero) :-
		(U = V -> true ;
		 U = my_times(A,B) , (B = V -> true ; A = V)
		) .

