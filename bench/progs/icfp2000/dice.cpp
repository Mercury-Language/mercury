% dice.gml
%

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

{ /x } /pop		% pop a stack item
{ /x x x } /dup		% duplicate a stack item

% dot product
% ... v2 v1  dot  ==> ... r
{ /v1 /v2
  v1 getx v2 getx mulf
  v1 gety v2 gety mulf addf
  v1 getz v2 getz mulf addf
} /dot

% normalize
% ... v1  normalize  ==> ... v2
{ /v
  1.0 v v dot apply sqrt divf /s	% s = sqrt(1.0/v dot v)
  s v getx mulf				% push s*x
  s v gety mulf				% push s*y
  s v getz mulf				% push s*z
  point					% make normalized vector
}

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

0.0 4.0 0.0 point 0.9 0.9 0.9 point pointlight /light1

1.0 12.0 divf /one12
1.0 6.0 divf /one6

white 0.1 1.0 1.0 const-surface apply plane /mirror

magenta 0.5 0.5 2.0 const-surface apply sphere one12 uscale /pip

blue 0.6 0.5 3.0 const-surface apply cube
-0.5 -0.5 -0.5 translate			% center cube
% front is five pips
  pip 0.0 0.0 -0.5 translate difference
  pip one6 -2.0 mulf one6 -2.0 mulf -0.5 translate difference
  pip one6 -2.0 mulf one6  2.0 mulf -0.5 translate difference
  pip one6  2.0 mulf one6 -2.0 mulf -0.5 translate difference
  pip one6  2.0 mulf one6  2.0 mulf -0.5 translate difference
% back is two pips
  pip one6  2.0 mulf one6  2.0 mulf 0.5 translate difference
  pip one6 -2.0 mulf one6 -2.0 mulf 0.5 translate difference
% top is six pips
  pip one6 -2.0 mulf 0.5 one6  2.0 mulf translate difference
  pip            0.0 0.5 one6  2.0 mulf translate difference
  pip one6  2.0 mulf 0.5 one6  2.0 mulf translate difference
  pip one6 -2.0 mulf 0.5 one6 -2.0 mulf translate difference
  pip            0.0 0.5 one6 -2.0 mulf translate difference
  pip one6  2.0 mulf 0.5 one6 -2.0 mulf translate difference
% bottom is one pip
  pip 0.0 -0.5 0.0 translate difference
% left is four pips
  pip -0.5 one6 -2.0 mulf one6 -2.0 mulf translate difference
  pip -0.5 one6 -2.0 mulf one6  2.0 mulf translate difference
  pip -0.5 one6  2.0 mulf one6 -2.0 mulf translate difference
  pip -0.5 one6  2.0 mulf one6  2.0 mulf translate difference
% right is three pips
  pip 0.5 one6  2.0 mulf one6  2.0 mulf translate difference
  pip 0.5 0.0 0.0 translate difference
  pip 0.5 one6 -2.0 mulf one6 -2.0 mulf translate difference
/die

die 15.0 rotatey 15.0 rotatex
  -1.5 -1.0 4.0 translate
die -25.0 rotatez -20.0 rotatex
   2.0 0.5 3.5 translate
union
mirror -90.0 rotatex 0.0 0.0 10.0 translate
0.5 grey apply matte apply plane 0.0 -2.0 0.0 translate union

union /scene

0.4 0.4	0.4 point	% ambient
[light1] 		% lights
scene			% object
3			% depth
90.0			% fov
640 400			% wid ht
"dice.ppm"		% output file
render

