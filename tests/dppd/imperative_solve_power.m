%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The "imperative-solve.power" Benchmark
% Part of the DPPD Library.
% 
% The program to be specialised is a solver for a small imperative
% language which uses environments to store values for variables. The
% program contains built-ins and negations. The task is to specialise a
% sub-program calculating the power (X^Y) for a known power and base but
% an unknown environment.

:- module imperative_solve_power.

:- interface.

:- pred imperative_solve_power is semidet.

:- implementation.

:- import_module assoc_list.
:- import_module imperative_solve_impl.
:- import_module list.
:- import_module pair.
:- import_module run.

imperative_solve_power :-
    power_2_5([], Eout1),
    use(Eout1),
    power_2_5(["z" - 1, "y" - 3], Eout2),
    use(Eout2).

% The partial deduction query
% 
% :- power(2, 5, Ein, Eout).
% 
% The run-time queries
% 
% :- power(2, 5, [], Eout).
% :- power(2, 5, [z/1, y/3], Eout).
% 
% Example solution
% 
% to be inserted
% 
% Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
