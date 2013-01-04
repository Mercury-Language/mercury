:- module mat3.
% 3x3 matrix module.  Makes use of the the vec3 module.

:- interface.

:- import_module float, vec3.

% define a 3x3 matrix as 3 column vectors
:- type mat3    --->    mat(vec3, vec3, vec3).

:- func '+'(mat3, mat3) = mat3.

:- func '-'(mat3, mat3) = mat3.

:- func '*'(mat3, mat3) = mat3.

:- func scale(float, mat3) = mat3.

:- func mat_transpose(mat3) = mat3.

:- func mat_identity = mat3.

:- implementation.

:- import_module math.

mat(V1, V2, V3) + mat(U1, U2, U3) = mat(V1+U1, V2+U2, V3+U3).

mat(V1, V2, V3) - mat(U1, U2, U3) = mat(V1-U1, V2-U2, V3-U3).

V * U = mat(W1, W2, W3) :-
    mat(V1, V2, V3) = mat_transpose(V),
    mat(U1, U2, U3) = U,
    W1 = vec(dot(V1,U1), dot(V2,U1), dot(V3,U1)),
    W2 = vec(dot(V1,U2), dot(V2,U2), dot(V3,U2)),
    W3 = vec(dot(V1,U3), dot(V2,U3), dot(V3,U3)).

scale(F, mat(U, V, W)) = mat(scale(F,U), scale(F, V), scale(F, W)).

mat_transpose(mat(vec(A,B,C), vec(D,E,F), vec(G,H,I))) = 
    mat(vec(A,D,G), vec(B,E,H), vec(C,F,I)).

mat_identity = mat(vec(1.0,0.0,0.0), vec(0.0,1.0,0.0), vec(0.0,0.0,1.0)).
