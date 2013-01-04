% reflect.gml
%
% This scene consists of a sphere in front of the viewer and another
% behind the viewer.  Also, there is a mirror that reflects both spheres.
%

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
0.0  1.0  0.0  point /green
0.0  0.75 0.0  point /darkgreen
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan

% ... <level>  grey  ==>  <color>
{ clampf /level level level level point } /grey

% ... <color> matte  ==>  ... <surface>
{ /color
  { /v /u /face
    color 1.0 0.0 1.0
  }
} /matte

% ... <color> <kd> <ks> <n>  ==>  ... <surface>
{ /n /ks /kd /color
  { /v /u /face
    color kd ks n
  }
} /const-surface

% a directional light
1.0 -1.0 -1.0 point
1.0 1.0 1.0 point light /light1

% a mirror plane
white 0.1 1.0 1.0 const-surface apply plane /mirror

				% a sphere
{ /v /u /face			  % bind arguments
  cyan				  % surface color
  0.4 0.7 5.0			  % kd ks n
} sphere 1.5 uscale /s1

{ /v /u /face			  % bind arguments
  0.8 v 0.2 point	  	  % surface color
  1.0 0.0 1.0			  % kd ks n
} sphere /s2

s1 0.0 1.5 -2.0 translate	  % behind and above viewer
s2 1.4 1.3 2.0 translate	  % in front of viewer
union
mirror -90.0 rotatex 0.0 0.0 6.0 translate
darkgreen matte apply plane 0.0 -2.0 0.0 translate union
union /scene

0.5 0.5	0.5 point	% ambient
[light1] 		% lights
scene			% object
4			% depth
90.0			% fov
320 240			% wid ht
"reflect.ppm"		% output file
render

