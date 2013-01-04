% rotate.gml
%
% OUTPUTS: rotate.ppm
%
% Test rotation of planes
%

#include "surface.ins"

% left side
red matte apply plane -15.0 rotatez -1.0 -1.0 0.0 translate

% right side
blue matte apply plane 15.0 rotatez 1.0 -1.0 0.0 translate
union

% back
green matte apply plane -45.0 rotatex 0.0 0.0 6.0 translate
union /scene

1.0 1.0 1.0 point
[]
scene
1
90.0
320 200
"rotate.ppm"
render

