% spheres2.gml
%
% A pair of spheres over a white plane
%

				% a sphere
{ /v /u /face			  % bind arguments
  0.8 0.2 v point		  % surface color
  1.0 0.2 1.0			  % kd ks n
} sphere /s

				% a matte white plane
{ /v /u /face
  1.0 1.0 1.0 point
  1.0 0.0 1.0
} plane /p

				% scene consisting of two spheres
s -1.2 0.0 3.0 translate 	  % sphere at (-1, 0, 3)
s  1.2 1.0 3.0 translate	  % sphere at (1, 1, 3)
p  0.0 -3.0 0.0 translate	  % plane at Y = -3
union union /scene		  % compose

				% directional light
1.0 -1.0 0.0 point		  % direction
1.0  1.0 1.0 point light /l	  % directional light

				% render
0.4 0.4 0.4 point		  % ambient light
[ l ]				  % lights
scene				  % scene to render
3				  % tracing depth
90.0				  % field of view
320 240				  % image wid and height
"spheres2.ppm"			  % output file
render

