
The "depth" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A Lam & Kusalik benchmark. It consists of a simple non-ground
   meta-interpreter which has to be specialised for a fully-unfoldable
   object program. It uses neither negations nor built-ins. For a
   slightly different and more intricate example see the ex_depth
   benchmark (which is not fully unfoldable).
   
  The benchmark program
  
depth( true, 0 ).
depth( (_g1,_gs), _depth ) :-
    depth( _g1, _depth_g1 ),
    depth( _gs, _depth_gs ),
    max( _depth_g1, _depth_gs, _depth ).
depth( _goal, s(_depth) ) :-
    prog_clause( _goal, _body ),
    depth( _body, _depth ).

max( _num, 0, _num ).
max( 0, s(_num), s(_num) ).
max( s(_x), s(_y), s(_max) ) :-
    max( _x, _y, _max ).

prog_clause( member( _X, _Xs ), append( _, [_X|_], _Xs ) ).
prog_clause( append( [], _L, _L ), true ).
prog_clause( append( [_X|_L1], _L2, [_X|_L3] ), append( _L1, _L2, _L3 ) ).

  The partial deduction query
  
 :- depth(member(X,[a,b,c,m,d,e,m,f,g,m,i,j]),Depth).

  The run-time queries
  
 :- depth(member(i,[a,b,c,m,d,e,m,f,g,m,i,j]),Depth).

  Example solution
  
   This benchmark can be fully unfolded. With the ECCE partial deduction
   system one can obtain the following:

depth__1(a,s(s(0))).
depth__1(b,s(s(s(0)))).
depth__1(c,s(s(s(s(0))))).
depth__1(m,s(s(s(s(s(0)))))).
depth__1(d,s(s(s(s(s(s(0))))))).
depth__1(e,s(s(s(s(s(s(s(0)))))))).
depth__1(m,s(s(s(s(s(s(s(s(0))))))))).
depth__1(f,s(s(s(s(s(s(s(s(s(0)))))))))).
depth__1(g,s(s(s(s(s(s(s(s(s(s(0))))))))))).
depth__1(m,s(s(s(s(s(s(s(s(s(s(s(0)))))))))))).
depth__1(i,s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))).
depth__1(j,s(s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))))).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
