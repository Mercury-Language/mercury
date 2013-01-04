% fib.gml
%

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
0.0  1.0  0.0  point /green
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan

[ red green blue magenta yellow cyan ] /colors

% ... <color> matte  ==>  ... <surface>
{ /color
  { /v /u /face		% discard face, u, v
    color 1.0 0.0 1.0
  }
} /matte

{ /self /i
  i 3 lessi
  { 1 }
  { i 1 subi self self apply
    i 2 subi self self apply
    addi
  } if
} /fib

{ /z /x				% maps (x, z) to (2.5*x, fib((x+z) div 2), 2.5*z)
  x z addi 2 divi 1 addi fib fib apply /y	% y = fib(1 + (x+z) div 2)
  x real 2.5 mulf y real z real 2.5 mulf
} /coord

{ /z /x
  colors x z addi colors length modi get matte apply
  sphere
  x z coord apply translate
} /mkSphere
  
{ /n /f					% apply a closure to the values 0..(n-1)
  { /self /i
    n i eqi
    { }
    { i f apply i 1 addi self self apply
    } if
  } /loop
  0 loop loop apply
} /repeat

[
  { 1 addi /x
    { 1 addi /z x z mkSphere apply } 5 repeat apply
  } 5 repeat apply
] /spheres

% union the array of spheres together to get a complete
% scene
{ /self 1 addi /i
  spheres length i eqi
  { }
  { spheres i get union
    i self self apply
  } if
} /union-loop

spheres 0 get 0 union-loop union-loop apply
-7.0 -3.0 3.0 translate /scene


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
"fib.ppm"			  % output file
render
