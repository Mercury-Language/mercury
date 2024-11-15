%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for term_norm.m
% Symptom: "Software Error: Unmatched lists in functor_norm_filter_args."
% This was caused by the list of counted arguments in the weight table
% differing from the list of arguments the termination analyser provided
% when it called functor norm. The code that constructed the weight table
% was ignoring type_infos when constructing the list of counted arguments.
%
%---------------------------------------------------------------------------%
%
% This test case happens to also expose another issue about intermodule
% optimization.
%
% Whether the deconstruct_univ predicate below terminates or not is determined
% entirely by whether private_builtin.typed_unify terminates or not. However,
% private_builtin.opt and private_builtin.trans_opt in the library directory
% disagree about this: the .opt file gives typed_unify's termination status
% as can_loop, while .trans_opt gives it as cannot_loop. (The difference
% is that the compiler knows that a predicate that typed_unify calls,
% type_desc.type_of/1, terminates only when it has access to type_desc.opt.)
%
% Until 15 Nov 2024, the code of the get_plain_trans_opt_deps predicate in
% write_deps_file.m had code to ignore a module's .opt file and pay attention
% only to its .trans_opt file *if* the module's source file exists.
% (This behavior was present when this predicate was originally added
% to the compiler on 6 Jan 1998, though the predicate was then named
% get_both_opt_deps, and it was in modules.m.) The .trans_opt_exp file
% therefore had deconstruct_univ's termination status as cannot_loop.
% Since we now pay attention to modules' .opt files whether or not their
% source files are reachable, its expected status is now can_loop.
%
% Ideally, if we have access to both the .opt and the .trans_opt file
% of a module, we should pay attention to termination information
% in only the .trans_opt file, since it incorporates information
% not just from that module's .opt file, but other modules' .opt files
% as well. However, we do not (yet) do that.
%
%---------------------------------------------------------------------------%

:- module existential_error1.

:- interface.

:- type univ
    --->    some [T] univ_cons(T).

:- pred deconstruct_univ(univ::in, T::out) is semidet.

:- implementation.

deconstruct_univ(Univ, T) :-
    Univ = univ_cons(T0),
    private_builtin.typed_unify(T0, T).

:- end_module existential_error1.
