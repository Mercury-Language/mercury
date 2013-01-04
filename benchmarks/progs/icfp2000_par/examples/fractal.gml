% fractal.gml
%
% OUTPUTS fractal.ppm
%
% Fractal thingie with spheres.
%

#include "surface.ins"


0.4 0.5 0.6 point 1.0 0.1 1.0 const-surface apply plane /p

{ /col
  { /v /u /face
    col
    0.1 0.99 6.0
  } sphere 0.9 uscale
} /mksphere

[
  0.5 0.7 0.9 point
  0.5 0.9 0.5 point
  0.6 0.6 0.7 point
  1.0 0.7 0.5 point
  0.9 1.0 0.6 point
  1.0 0.5 0.3 point
  1.0 0.8 0.9 point
  1.0 1.0 0.6 point
  1.0 1.0 1.0 point
] /colors

{ 1 addi colors length modi } /incrmod

{
  /self /col /depth /base
  depth 0 eqi
  { colors col get base apply 0.9 uscale }
  { 
    col incrmod apply /col
    colors col get base apply
    col incrmod apply /col
    base depth 1 subi col self self apply 2.5 0.0 0.0 translate union
    col incrmod apply /col
    base depth 1 subi col self self apply
    2.5 0.0 0.0 translate 60.0 rotatez union
    col incrmod apply /col
    base depth 1 subi col self self apply
    2.5 0.0 0.0 translate 120.0 rotatez union
    col incrmod apply /col
    base depth 1 subi col self self apply
    2.5 0.0 0.0 translate 180.0 rotatez union
    col incrmod apply /col
    base depth 1 subi col self self apply
    2.5 0.0 0.0 translate 240.0 rotatez union
    col incrmod apply /col
    base depth 1 subi col self self apply
    2.5 0.0 0.0 translate 300.0 rotatez union

    col incrmod apply /col
    base depth 1 subi col self self apply
    3.0 0.0 0.0 translate 90.0 rotatey union
    col incrmod apply /col
    base depth 1 subi col self self apply
    3.0 0.0 0.0 translate -90.0 rotatey union

    1.0 3.0 divf uscale
  }
  if
} /rec

mksphere 3 0 rec rec apply 30.0 rotatex 40.0 rotatey
0.8 uscale 0.0 0.3 0.5 translate

p 0.0 -1.0 0.0 translate union
0.0 -0.2 0.3 translate
 /scene

				% directional light
0.8 -1.0 0.4 point		  % direction
1.0  1.0 1.0 point light /l1	  % directional light

0.4 0.4 0.4 point		  % ambient light
[ l1 ]				  % lights
scene				  % scene to render
3				  % tracing depth
100.0				  % field of view
300 200				  % image wid and height
"fractal.ppm"			  % output file
render

