% pipe.gml
%
% OUTPUTS: pipe.ppm
%

#include "surface.ins"
#include "colors.ins"

% ground plane
0.8 0.8 0.8 point 0.8 0.0 1.0 const-surface apply plane /p

% background plane
0.4 0.5 0.6 point matte apply plane
  -90.0 rotatex 0.0 0.0 500.0 translate /background

blue matte apply cylinder
  2.0 20.0 2.0 scale

white 0.0 1.0 10.0 const-surface apply cylinder
  1.8 24.0 1.8 scale
  0.0 -2.0 0.0 translate
  difference
  90.0 rotatex /pipe

red 0.9 0.2 2.0 const-surface apply sphere 0.2 uscale /ball1
yellow 0.9 0.2 2.0 const-surface  apply cube 0.5 uscale /box
green 0.9 0.2 2.0 const-surface  apply cone 0.4 uscale /Cone
white 0.4 0.8 2.0 const-surface apply sphere 0.2 uscale /ball2

background
pipe union
ball1 0.3 -0.05 6.0 translate union
ball2 0.5 -0.25 1.0 translate union
box -0.5 0.2 13.0 translate union
Cone -15.0 rotatey -1.0 -0.3 11.0 translate union
0.0 0.0 4.0 translate
p 0.0 -5.0 0.0 translate union
/scene

				% directional light
0.8 -1.0 0.4 point		  % direction
0.8  0.8 0.8 point light /l1	  % directional light

0.0 0.0 6.0 point
0.9 0.9 0.9 point pointlight /l2

0.8 0.8 0.8 point		  % ambient light
[ l1 l2 ]				  % lights
scene				  % scene to render
3				  % tracing depth
60.0				  % field of view
400 300				  % image wid and height
"pipe.ppm"		  % output file
render
