% features.gml
%
% This program tests various features of the GML language.
% if a feature fails, we execute an illegal operation that
% should halt the machine
%

% this function should force an exit with non-zero exit status
{
  [] -1 get
  render
} /error

{ /x } /okay

% we push a few values on the stack and make sure that they are still
% there when were done.
257 -99 106

% test booleans
"b1" true okay error if
"b2" false error okay if

% test simple variable binding
true /t
false /f
"vb1" t okay error if
"vb2" f error okay if
1 /do-we-allow-very-long-and-silly_File-Names-with-numbers-123456-in-them

% test ints
"i1" 17 -42 addi
     17 42 negi subi muli
     -1475 eqi
     okay error if
"i2" 35 35 1 addi lessi okay error if
"i3" -8388608 8388607 addi 2 divi 0 eqi okay error if
"i4" 5 3 modi 2 eqi okay error if

% test reals; note that addition, subtraction, and multiplication with
% whole numbers should be exact.
"r01" 1.0 2.0 addf 3.0 eqf okay error if
"r02" 1.0 2.0 subf -1.0 eqf okay error if
"r03" 3.0 -2.0 mulf -6.0 eqf okay error if

% a function to test if y is within epsilon of x
1.0e-7 /epsilon
{ /x /y x epsilon subf y lessf
  { y x epsilon addf lessf}
  { false }
  if
} /almost-eq

% test trig functions
"t01" 0.0 sin 0.0 eqf okay error if
"t02" 0.0 cos 1.0 eqf okay error if
"t03" 45.0 sin 45.0 cos almost-eq apply okay error if
"t04" 45.0 sin 2.0 sqrt 0.5 mulf almost-eq apply okay error if
"t05" 90.0 sin 1.0 eqf okay error if
"t06" 90.0 cos 0.0 eqf okay error if
"t07" 180.0 sin 0.0 eqf okay error if
"t08" 180.0 cos -1.0 eqf okay error if
"t09" 270.0 sin -1.0 eqf okay error if
"t10" 270.0 cos 0.0 eqf okay error if
"t11" -90.0 sin -1.0 eqf okay error if
"t12" -90.0 cos 0.0 eqf okay error if

% test points
1.0 2.0 3.0 point /p
"p1" p getx 1.0 eqf okay error if
"p2" p gety 2.0 eqf okay error if
"p3" p getz 3.0 eqf okay error if

% test arrays
[0 1 2 3 4 5 6 7 8 9] /a
"a01" a length 10 eqi okay error if
"a02" a 0 get 0 eqi okay error if
"a03" a 1 get 1 eqi okay error if
"a04" a 2 get 2 eqi okay error if
"a05" a 3 get 3 eqi okay error if
"a06" a 4 get 4 eqi okay error if
"a07" a 5 get 5 eqi okay error if
"a08" a 6 get 6 eqi okay error if
"a09" a 7 get 7 eqi okay error if
"a10" a 8 get 8 eqi okay error if
"a11" a 9 get 9 eqi okay error if
[{ 1 } { 2 } false [2 3] { [4] }] /b
"a12" b length 5 eqi okay error if
"a13" b 0 get apply 1 eqi okay error if
"a14" b 1 get apply 2 eqi okay error if
"a15" b 2 get error okay if
"a16" b 3 get 0 get 2 eqi okay error if
"a17" b 3 get 1 get 3 eqi okay error if
"a18" b 4 get apply 0 get 4 eqi okay error if

% test lexical scoping
23 /x
{ x addi /z z z muli } /f
0 /x
1 /z
"scope" z f apply z addi 577 eqi okay error if

% test recursion
{ /self /n
  n 1 lessi
  { 1 }
  { n 1 subi self self apply n muli }
  if
} /fact
"rec" 10 fact fact apply
  3628800 eqi okay error if

% check that are values are still on the stack
106 eqi {} error if
-99 eqi {} error if
257 eqi {} error if

% if we get here, the interpreter is probably working

