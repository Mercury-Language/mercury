:- module pl3_1_1.

:- interface.

:- pred a is semidet.	% DIAGNOSED BY COMPILER
:- pred b is semidet.	% DIAGNOSED BY COMPILER
:- pred c is semidet.	% DIAGNOSED BY COMPILER
:- pred d is semidet.	% DIAGNOSED BY COMPILER
:- pred e is semidet.	% DIAGNOSED BY COMPILER
:- pred f is semidet.	% DIAGNOSED BY COMPILER
:- pred g is semidet.	% DIAGNOSED BY COMPILER

:- implementation.

a :- b.
a :- e.
b :- c.
c :- d.
d :- b.
e :- f.
f :- g.
g :- e.
