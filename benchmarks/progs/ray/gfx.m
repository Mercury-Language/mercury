:- module gfx.

% define some graphics-related types to be used throughout the program

:- interface.

:- import_module vec3, string, float, list, int.

% light source
:- type light --->
        light(
                float,      % power
                vec3        % location
             ).

% background of image
:- type background --->
            filename(string)
        ;
            solid(
                    int,    % image width
                    int,    % image height
                    colour  % colour of background
                ).

% colour
:- type colour --->
            colour(
                    float,      % red
                    float,      % green
                    float       % blue
                  ).

% define a couple of useful things we can do with colour

% add 2 colours together
:- func '+'(colour, colour) = colour.

% scale the intensity of a colour
:- func '*'(float, colour) = colour.

% default colour for polygon faces
:- func default_colour = colour.

% type of shading to use
:- type shade_type ---> full ; phong.

% calculated shade of object
:- type shade --->
        shade(colour) ; unknown.

% classification of vertices
:- type vert_class --->
        convex ; concave.

% the scene returned to the rest of the program by get_scene is a list(polytri).
% Each polytri gives all the triangles from a particular face of the polyhedron.
% They are grouped like this to take advantage of the fact that they are all on
% the same plane to improve efficiency later on.

:- type scene == list(polytri).

:- type triangle --->
        tri(        % vertices go anticlockwise when viewed from outside the
                    % polyhedron.
            vec3,   % vertex 1
            vec3,   % vertex 2
            vec3,   % vertex 3
            vec3,   % normal at vertex 1
            vec3,   % normal at vertex 2
            vec3    % normal at vertex 3
            ).  

:- type polytri --->
        polytri(
                    list(triangle), % list of triangles in this polygon
                    vec3,           % normal to plane of polygon
                    colour          % intrinsic colour of polygon
               ).
% Check to see if a point lies within a triangle on the xy plane.
% The first 3 parameters are the vertices of the triangle, the last parameter is
% the point to test.

:- pred point_in_triangle(vec3, vec3, vec3, vec3).
:- mode point_in_triangle(in, in, in, in) is semidet.

:- implementation.

:- import_module require.

colour(R1, G1, B1) + colour(R2, G2, B2) = colour(R1 + R2, G1 + G2, B1 + B2).

F * colour(R, G, B) = colour(F * R, F * G, F * B).

% default colour of a polygon
default_colour = colour(0.666, 0.666, 0.666).
                        
        
% Check to see if a point lies within a triangle on the xy plane.
% The algorithm is from the Geometry Centre's geom_utils package of C routines:
%
%   Draw a line from the vertex to the first point in question and find out
%   where that intersects the opposite side.  If the point of intersection lies
%   outside the segment from the first point to the test point and inside the
%   other segment, then the point is inside the triangle.
%
% The first 3 parameters are the vertices of the triangle, the last parameter is
% the point to test.

point_in_triangle(vec(X1, Y1, _), vec(X2, Y2, _), vec(X3, Y3, _), vec(Xp, Yp, _)) :-
    (
        % common denominator for parametric equations
        Denom = (-(Yp*X2) + Yp*X3 + X2*Y1 - X3*Y1 + Xp*Y2 - X1*Y2 - Xp*Y3 + X1*Y3),
        Denom \= 0.0,
        % parameter for segment from point 1 to test point
        T = (X2*Y1 - X3*Y1 - X1*Y2 + X3*Y2 + X1*Y3 - X2*Y3) / Denom,

        T >= 1.0,

        % parameter for segment from point 2 to point 3
        R = (Yp*X1 - Yp*X2 - Xp*Y1 + X2*Y1 + Xp*Y2 - X1*Y2) / Denom,

        R >= 0.0,
        R =< 1.0
    ).
