% fov.gml
%
% OUTPUTS: fov-30.ppm fov-60.ppm fov-90.ppm fov-120.ppm
%
% test rendering with different field of views
%

#include "surface.ins"

red matte apply sphere
0.0 0.0 3.0 translate

blue matte apply plane
-90.0 rotatex 0.0 0.0 5.0 translate

green matte apply plane
0.0 -2.0 0.0 translate

union union /scene

{ /file /fov
  1.0 1.0 1.0 point	% ambient
  []			% lights
  scene
  1
  fov
  320 200
  file
  render
} /Render

30.0 "fov-30.ppm" Render apply
60.0 "fov-60.ppm" Render apply
90.0 "fov-90.ppm" Render apply
120.0 "fov-120.ppm" Render apply

