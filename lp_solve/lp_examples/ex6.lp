/*

This example is from "Integer and Combinatorial Optimization" by George L.
Nemhauser and Laurence A. Wolsey page 443.

They claim that:

"It is not hard to show that an optimal solution is

x1 = 33
x2 = 1
x3 = 0
x4 = 1
x5 = 0
x6 = 0
x7 = 0

and that the optimal OF value is 19972"

This is not true. lp_solve will find a better solution. Just try.

*/

max: 592x1 + 381x2 + 273x3 + 55x4 + 48x5 + 37x6 + 23x7;
3534x1 + 2356x2 + 1767x3 + 589x4 + 528x5 + 451x6 + 304x7 < 119567;
int x1, x2, x3, x4, x5, x6, x7;
