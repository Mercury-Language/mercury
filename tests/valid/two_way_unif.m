% The unification of the argument with vertex(I, V)
% contains two subunifications, one of which instantiates the argument.

% This is a regression test. Earlier versions of the code generator used to
% generate LLDS code that referred to vars. Such LLDS code cannot even be
% converted into C source.

:- module two_way_unif.

:- interface.

:- type vec	--->		vec(float, float).

:- type face_vertex --->	vertex(int, vec).

:- implementation.

:- pred p(face_vertex).
:- mode p(mostly_unique(vertex(ground, free)) >> bound(vertex(ground, ground))) is semidet.

p(vertex(I, V)) :-
	I = 0,
	V = vec(1.0, 2.0).
