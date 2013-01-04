% A simple matte plane with a spotlight
%
{ /x } /pop		% pop a stack item
{ /x x x } /dup		% duplicate a stack item


% point addition
% ... p1 p2  addp  ==> ... p3
{ /p1 /p2
  p1 getx p2 getx addf
  p1 gety p2 gety addf
  p1 getz p2 getz addf
  point
} /addPt

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
0.0  1.0  0.0  point /green
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan

% ... <level>  grey  ==>  <color>
{ clampf /level level level level point } /grey


% ... <color> matte  ==>  ... <surface>
{ /color
  { /v /u /face		% discard face, u, v
    color 1.0 0.0 1.0
  }
} /matte

% ... <color> <kd> <ks> <n>  ==>  ... <surface>
{ /n /ks /kd /color
  { /v /u /face		% discard face, u, v arguments
    color kd ks n
  }
} /const-surface

white matte apply plane
0.0 -1.0 0.0 translate
/scene

{ /pos /color
  pos					% position
  pos 0.0 -1.0 0.0 point addPt apply	% at; (points down)
  color					% color
  20.0					% cutoff
  10.0					% exp
  spotlight
} /spot

red -1.0 3.0 4.0 point spot apply /redSpot
blue 1.0 3.0 4.0 point spot apply /greenSpot
green 0.0 3.0 4.0 3.0 sqrt subf point spot apply /blueSpot

0.2 0.2	0.2 point		% ambient
[redSpot greenSpot blueSpot]	% lights
scene				% object
1				% depth
90.0				% fov
320 240				% wid ht
"spotlight.ppm"			% output file
render

