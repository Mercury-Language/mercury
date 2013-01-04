% large.gml
%
% OUTPUTS: large.ppm
%
% This program is designed to stress test the memory
% system.  It builds some huge scenes that then do not get
% rendered (or only partially rendered).
%

#include "surface.ins"
#include "loops.ins"
#include "rgb.ins"

% get array[i % length array]
%
{ /i /array
  array i array length modi get
} /get-mod

[ PeachPuff AliceBlue MintCream DarkTurquoise CadetBlue
  SteelBlue1 dodger-blue RoyalBlue3 DarkOliveGreen3 DarkGoldenrod4
  RosyBrown3 chocolate1 firebrick1 MediumPurple2 dark-magenta
] /colors

{ /i
  colors i colors length modi get	% get colors[i % length(colors)]
  matte apply sphere
  0.75 uscale
} /mkSphere

% create an array of 100000 spheres
[ mkSphere 25000 foreach apply ] /spheres

% compute a random translation
{ /seed
  seed randomf apply /seed 10.0 mulf 5.0 subf   % x coord in [-5..5]
  seed randomf apply /seed 10.0 mulf 5.0 subf   % y coord in [-5..5]
  seed randomf apply /seed 15.0 mulf            % z coord in [0..15]
  seed
} /randomp

% pick a few spheres to actually render
35 /seed
seed random apply /seed spheres seed get-mod apply
  seed randomp apply /seed translate
seed random apply /seed spheres seed get-mod apply
  seed randomp apply /seed translate union
seed random apply /seed spheres seed get-mod apply
  seed randomp apply /seed translate union
seed random apply /seed spheres seed get-mod apply
  seed randomp apply /seed translate union
seed random apply /seed spheres seed get-mod apply
  seed randomp apply /seed translate union
seed random apply /seed spheres seed get-mod apply
  seed randomp apply /seed translate union
seed random apply /seed spheres seed get-mod apply
  seed randomp apply /seed translate union
seed random apply /seed spheres seed get-mod apply
  seed randomp apply /seed translate union
0.0 0.0 3.0 translate
/scene

% directional light
1.0 -1.0 1.0 point
1.0 1.0 1.0 point light /l

% then we render with directional lighting
0.3 0.3 0.3 point       % ambient
[l]                     % lights
scene
1
100.0
320 200
"large.ppm"
render

