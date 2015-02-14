%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The "match-app" Benchmark
% Part of the DPPD Library.
% 
% A very naive string matcher, written with 2 appends. Same runtime
% queries as match.kmp. This benchmark contains no builtins or negations.

:- module match_append.

:- interface.

:- pred match_append is semidet.

:- implementation.

:- import_module list.
:- import_module match_append_impl.

match_append :-
    match_aab([a, a, a, a, c, d, a, a, a, e, f, g, h, a, a, b, d, f]),
    match_aab([a, b, a, b, a, a, a, a, c, a, a, a, a, a, a, a, a, b]),
    match_aab([a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
        a, a, a, a, a, b]),
    match_aab([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s,
        t, u, v, a, a, b, w, x, y, z]).

% The partial deduction query
% 
% :- match([a, a, b], String).
% 
% The run-time queries
% 
% :- match([a, a, b], [a, a, a, a, c, d, a, a, a, e, f, g, h, a, a, b, d, f]).
% :- match([a, a, b], [a, b, a, b, a, a, a, a, c, a, a, a, a, a, a, a, a, b]).
% :- match([a, a, b], [a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
%     a, a, a, a, a, a, b]).
% :- match([a, a, b], [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r,
%     s, t, u, v, a, a, b, w, x, y, z]).
% 
% Example solution
% 
% The following can be obtained by the ECCE partial deduction system .
% Although it runs considerably faster than the original program (33
% times actually) it is still not a KMP style matcher.
% 
% match__1([X1, X2, X3 | X4]) :- app__app(X4, X1, X2, X3).
% 
% app__app(X1, a, a, b).
% app__app([X1 | X2], X3, X4, X5) :- app__app(X2, X4, X5, X1).
% 
% Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
