% cone-fractal.gml
%
% OUTPUTS: cone-fractal.ppm
%

#include "surface.ins"
#include "colors.ins"

% ground plane
0.4 0.5 0.6 point 1.0 0.1 1.0 const-surface apply plane /p

% background plane
0.6 0.5 0.5 point matte apply plane
  -90.0 rotatex 0.0 0.0 500.0 translate /background

3.0 sqrt /sqrt3

% compute the height of a triangle/cone from the length of its side.
% ht = sz * sqrt(3)
{ sqrt3 mulf } /triHt

[
  red green blue yellow cyan magenta
] /colors

{ 1 addi colors length modi } /incrmod

{ /sz /color
  color 0.2 0.9 10.0 const-surface apply
  cone
  1.0 -1.0 1.0 scale 0.0 1.0 0.0 translate	% flip so base is at y=0.
  sz sz triHt apply sz scale
} /mkCone

{ /self /depth /sz /col
  depth 0 eqi
  { colors col get sz mkCone apply col incrmod apply }
  { sz triHt apply 0.5 mulf /halfHt
    sz 0.5 mulf /halfSz
  % top triangle
    col halfSz depth 1 subi self self apply /col
    0.0 halfHt 0.0 translate /tri1
  % bottom left
    col halfSz depth 1 subi self self apply /col
    0.0 0.0 halfHt translate
    120.0 rotatey /tri2
  % bottom right
    col halfSz depth 1 subi self self apply /col
    0.0 0.0 halfHt translate
    -120.0 rotatey /tri3
  % bottom back
    col halfSz depth 1 subi self self apply /col
    0.0 0.0 halfHt translate /tri4
  % form the composite image
    tri1 tri2 union tri3 tri4 union union col
  } if
} /genFractal

0 2.0 3 genFractal genFractal apply /col
10.0 rotatey
0.0 -2.5 5.0 translate
p 0.0 -5.0 0.0 translate union
-15.0 rotatex
background union
 /scene

				% directional light
0.8 -1.0 0.4 point		  % direction
0.8  0.8 0.8 point light /l1	  % directional light

0.0 2.0 6.0 point
0.9 0.9 0.9 point pointlight /l2

0.4 0.4 0.4 point		  % ambient light
[ l1 l2 ]			  % lights
scene				  % scene to render
3				  % tracing depth
90.0				  % field of view
300 200				  % image wid and height
"cone-fractal.ppm"		  % output file
render

