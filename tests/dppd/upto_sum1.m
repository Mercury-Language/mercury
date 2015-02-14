%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The "upto.sum1" Benchmark.
% Part of the DPPD Library.
% 
% This is a sophisticated deforestation example coming from the
% functional programming community. It was handed over to me by Jesper
% Jorgensen, who adapted it from Wadler's paper Deforestation:
% Transforming programs to eliminate intermediate trees in TCS,
% 73:231-248, 1990. The program calculates the sum of squares for 1 up to n.

:- module upto_sum1.

:- interface.

:- pred upto_sum1 is semidet.

:- implementation.

:- import_module run.
:- import_module upto_sum_impl.

upto_sum1 :-
    sumsquaresupto(5, X1),
    use(X1),
    sumsquaresupto(15, X2),
    use(X2),
    sumsquaresupto(25, X3),
    use(X3),
    sumsquaresupto(30, X4),
    use(X4).

% The partial deduction query
% 
% :- sumsquaresupto(N, S).
% 
% The run-time queries
% 
% :- sumsquaresupto(5, X).
% :- sumsquaresupto(15, X).
% :- sumsquaresupto(25, X).
% :- sumsquaresupto(30, X).
% 
% Example solution
% 
% to be inserted
% 
% Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
