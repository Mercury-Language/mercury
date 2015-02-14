%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% 
% The "contains.kmp" Benchmark.
% Part of the DPPD Library.
% 
% This is exactly the same benchmark as the Lam & Kusalik contains benchmark,
% except that the run-time query is more challenging (in the sense that
% a Knuth-Morris-Pratt like optimisation will pay off).

:- module contains_kmp.

:- interface.

:- pred contains_kmp is semidet.

:- implementation.

:- import_module contains.
:- import_module list.

contains_kmp :-
    contains_aab([a, a, a, a, c, a, a, c, a, b, a, a, a, a, a, b]).

% Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
