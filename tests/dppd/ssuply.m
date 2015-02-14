%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The "ssuply" Benchmark.
% Part of the DPPD Library.
% 
% A Lam & Kusalik benchmark which can be fully unfolded. It uses some
% simple built-ins.

:- module ssuply.

:- interface.

:- pred ssuply is semidet.

:- implementation.

:- import_module run.
:- import_module ssuply_impl.

ssuply :-
    ssuply(s3, p1, R), use(R).

% The partial deduction query
% 
% :- ssupply(_supplier, _part, _quantity).
% 
% The run-time queries
% 
% :- ssupply(s3, p1, _quantity).
% 
% Example solution
% 
% This benchmark can be fully unfolded. With the ECCE partial deduction system
% one can obtain the following program, which runs more than 16 times faster
% than the original:
% 
% ssupply__1(s1, p1, 300).
% ssupply__1(s3, p1, 400).
% ssupply__1(s4, p1, 200).
% 
% Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
