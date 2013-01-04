% intercyl.gml
%
% OUTPUTS: intercyl.ppm
%
% intersecting orthogonal disks
%

#include "surface.ins"

0.7 0.3 0.3 point 1.0 0.9 1.0 const-surface apply
cylinder
-0.0 -0.5 -0.0 translate 1.0 0.1 1.0 scale 90.0 rotatex

0.3 0.7 0.3 point 1.0 0.9 1.0 const-surface apply
cylinder
-0.0 -0.5 -0.0 translate 1.0 0.1 1.0 scale 90.0 rotatez

union

0.3 0.3 0.7 point 1.0 0.9 1.0 const-surface apply
cylinder
-0.0 -0.5 -0.0 translate 1.0 0.1 1.0 scale

union

45.0 rotatey

45.0 rotatex

0.25 uscale

 /scene

                                % directional light
0.8 -1.0 0.4 point                % direction
0.6  0.6 0.5 point light /l1      % directional light

0.0 1.5 -0.4 point  % origin
0.4 0.5 0.6 point pointlight /l2

0.5 0.5 0.5 point                 % ambient light
[ l1 l2 ]                         % lights
scene                             % scene to render
5                                 % tracing depth
90.0                              % field of view
640 480                           % image wid and height
"intercyl.ppm"                    % output file
render

