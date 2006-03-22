%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: std_util.m.
% Main author: fjh.
% Stability: medium.

% This file is intended for all the useful standard utilities
% that don't belong elsewhere, like <stdlib.h> in C.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module std_util.

:- interface.

:- import_module bool.
:- import_module list.
:- import_module set.
:- import_module type_desc.

%-----------------------------------------------------------------------------%
%
% The universal type `univ'.

    % An object of type `univ' can hold the type and value of an object of any
    % other type.
:- type univ.

    % type_to_univ(Object, Univ):
    %
    % True iff the type stored in `Univ' is the same as the type of `Object',
    % and the value stored in `Univ' is equal to the value of `Object'.
    %
    % Operational, the forwards mode converts an object to type `univ',
    % while the reverse mode converts the value stored in `Univ'
    % to the type of `Object', but fails if the type stored in `Univ'
    % does not match the type of `Object'.
    %
:- pred type_to_univ(T, univ).
:- mode type_to_univ(di, uo) is det.
:- mode type_to_univ(in, out) is det.
:- mode type_to_univ(out, in) is semidet.

    % univ_to_type(Univ, Object) :- type_to_univ(Object, Univ).
    %
:- pred univ_to_type(univ, T).
:- mode univ_to_type(in, out) is semidet.
:- mode univ_to_type(out, in) is det.
:- mode univ_to_type(uo, di) is det.

    % The function univ/1 provides the same functionality as type_to_univ/2.
    % univ(Object) = Univ :- type_to_univ(Object, Univ).
    %
:- func univ(T) = univ.
:- mode univ(in) = out is det.
:- mode univ(di) = uo is det.
:- mode univ(out) = in is semidet.

    % det_univ_to_type(Univ, Object):
    %
    % The same as the forwards mode of univ_to_type, but aborts
    % if univ_to_type fails.
    %
:- pred det_univ_to_type(univ::in, T::out) is det.

    % univ_type(Univ):
    %
    % Returns the type_desc for the type stored in `Univ'.
    %
:- func univ_type(univ) = type_desc.type_desc.

    % univ_value(Univ):
    %
    % Returns the value of the object stored in Univ.
    %
:- some [T] func univ_value(univ) = T.

%-----------------------------------------------------------------------------%
%
% The "maybe" type.
%

:- type maybe(T) ---> no ; yes(T).
:- inst maybe(I) ---> no ; yes(I).

:- type maybe_error ---> ok ; error(string).
:- type maybe_error(T) ---> ok(T) ; error(string).
:- inst maybe_error(I) ---> ok(I) ; error(ground).

    % map_maybe(P, yes(Value0), yes(Value)) :- P(Value, Value).
    % map_maybe(_, no, no).
    %
:- pred map_maybe(pred(T, U), maybe(T), maybe(U)).
:- mode map_maybe(pred(in, out) is det, in, out) is det.
:- mode map_maybe(pred(in, out) is semidet, in, out) is semidet.
:- mode map_maybe(pred(in, out) is multi, in, out) is multi.
:- mode map_maybe(pred(in, out) is nondet, in, out) is nondet.

    % map_maybe(F, yes(Value)) = yes(F(Value)).
    % map_maybe(_, no) = no.
    %
:- func map_maybe(func(T) = U, maybe(T)) = maybe(U).

    % fold_maybe(P, yes(Value), Acc0, Acc) :- P(Value, Acc0, Acc).
    % fold_maybe(_, no, Acc, Acc).
    %
:- pred fold_maybe(pred(T, U, U), maybe(T), U, U).
:- mode fold_maybe(pred(in, in, out) is det, in, in, out) is det.
:- mode fold_maybe(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode fold_maybe(pred(in, di, uo) is det, in, di, uo) is det.

    % fold_maybe(F, yes(Value), Acc0) = F(Acc0).
    % fold_maybe(_, no, Acc) = Acc.
    %
:- func fold_maybe(func(T, U) = U, maybe(T), U) = U.

    % map_fold_maybe(P, yes(Value0), yes(Value), Acc0, Acc) :-
    %       P(Value, Value, Acc0, Acc).
    % map_fold_maybe(_, no, no, Acc, Acc).
    %
:- pred map_fold_maybe(pred(T, U, Acc, Acc), maybe(T), maybe(U), Acc, Acc).
:- mode map_fold_maybe(pred(in, out, in, out) is det, in, out, in, out) is det.
:- mode map_fold_maybe(pred(in, out, di, uo) is det, in, out, di, uo) is det.

    % As above, but with two accumulators.
    %
:- pred map_fold2_maybe(pred(T, U, Acc1, Acc1, Acc2, Acc2),
    maybe(T), maybe(U), Acc1, Acc1, Acc2, Acc2).
:- mode map_fold2_maybe(pred(in, out, in, out, in, out) is det, in, out,
    in, out, in, out) is det.
:- mode map_fold2_maybe(pred(in, out, in, out, di, uo) is det,
    in, out, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%
% The "unit" type - stores no information at all.
%

:- type unit        --->    unit.

:- type unit(T)     --->    unit1.

%-----------------------------------------------------------------------------%
%
% The "pair" type.  Useful for many purposes.
%

:- type pair(T1, T2)
    --->    (T1 - T2).
:- type pair(T) ==  pair(T, T).

:- inst pair(I1, I2)
    --->    (I1 - I2).
:- inst pair(I) ==  pair(I, I).

    % Return the first element of the pair.
    %
:- pred fst(pair(X, Y)::in, X::out) is det.
:- func fst(pair(X, Y)) = X.

    % Return the second element of the pair.
    %
:- pred snd(pair(X, Y)::in, Y::out) is det.
:- func snd(pair(X, Y)) = Y.

:- func pair(T1, T2) = pair(T1, T2).

%-----------------------------------------------------------------------------%

    % solutions/2 collects all the solutions to a predicate and returns
    % them as a list in sorted order, with duplicates removed.
    % solutions_set/2 returns them as a set.  unsorted_solutions/2 returns
    % them as an unsorted list with possible duplicates; since there are an
    % infinite number of such lists, this must be called from a context in
    % which only a single solution is required.
    %
:- pred solutions(pred(T), list(T)).
:- mode solutions(pred(out) is multi, out(non_empty_list)) is det.
:- mode solutions(pred(out) is nondet, out) is det.

:- func solutions(pred(T)) = list(T).
:- mode solutions(pred(out) is multi) = out(non_empty_list) is det.
:- mode solutions(pred(out) is nondet) = out is det.

:- pred solutions_set(pred(T), set(T)).
:- mode solutions_set(pred(out) is multi, out) is det.
:- mode solutions_set(pred(out) is nondet, out) is det.

:- func solutions_set(pred(T)) = set(T).
:- mode solutions_set(pred(out) is multi) = out is det.
:- mode solutions_set(pred(out) is nondet) = out is det.

:- pred unsorted_solutions(pred(T), list(T)).
:- mode unsorted_solutions(pred(out) is multi, out(non_empty_list))
    is cc_multi.
:- mode unsorted_solutions(pred(out) is nondet, out) is cc_multi.

:- func aggregate(pred(T), func(T, U) = U, U) = U.
:- mode aggregate(pred(out) is multi, func(in, in) = out is det, in)
    = out is det.
:- mode aggregate(pred(out) is nondet, func(in, in) = out is det, in)
    = out is det.

%-----------------------------------------------------------------------------%

    % aggregate/4 generates all the solutions to a predicate,
    % sorts them and removes duplicates, then applies an accumulator
    % predicate to each solution in turn:
    %
    % aggregate(Generator, Accumulator, Acc0, Acc) <=>
    %   solutions(Generator, Solutions),
    %   list.foldl(Accumulator, Solutions, Acc0, Acc).
    %
:- pred aggregate(pred(T), pred(T, U, U), U, U).
:- mode aggregate(pred(out) is multi, pred(in, in, out) is det,
    in, out) is det.
:- mode aggregate(pred(out) is multi, pred(in, di, uo) is det,
    di, uo) is det.
:- mode aggregate(pred(out) is nondet, pred(in, di, uo) is det,
    di, uo) is det.
:- mode aggregate(pred(out) is nondet, pred(in, in, out) is det,
    in, out) is det.

    % aggregate2/6 generates all the solutions to a predicate,
    % sorts them and removes duplicates, then applies an accumulator
    % predicate to each solution in turn:
    %
    % aggregate2(Generator, Accumulator, AccA0, AccA, AccB0, AccB) <=>
    %   solutions(Generator, Solutions),
    %   list.foldl2(Accumulator, Solutions, AccA0, AccA, AccB0, AccB).
    %
:- pred aggregate2(pred(T), pred(T, U, U, V, V), U, U, V, V).
:- mode aggregate2(pred(out) is multi, pred(in, in, out, in, out) is det,
    in, out, in, out) is det.
:- mode aggregate2(pred(out) is multi, pred(in, in, out, di, uo) is det,
    in, out, di, uo) is det.
:- mode aggregate2(pred(out) is nondet, pred(in, in, out, di, uo) is det,
    in, out, di, uo) is det.
:- mode aggregate2(pred(out) is nondet, pred(in, in, out, in, out) is det,
    in, out, in, out) is det.

    % unsorted_aggregate/4 generates all the solutions to a predicate
    % and applies an accumulator predicate to each solution in turn.
    % Declaratively, the specification is as follows:
    %
    % unsorted_aggregate(Generator, Accumulator, Acc0, Acc) <=>
    %   unsorted_solutions(Generator, Solutions),
    %   list.foldl(Accumulator, Solutions, Acc0, Acc).
    %
    % Operationally, however, unsorted_aggregate/4 will call the
    % Accumulator for each solution as it is obtained, rather than
    % first building a list of all the solutions.
    %
:- pred unsorted_aggregate(pred(T), pred(T, U, U), U, U).
:- mode unsorted_aggregate(pred(out) is multi, pred(in, in, out) is det,
    in, out) is cc_multi.
:- mode unsorted_aggregate(pred(out) is multi, pred(in, in, out) is cc_multi,
    in, out) is cc_multi.
:- mode unsorted_aggregate(pred(out) is multi, pred(in, di, uo) is det,
    di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is multi, pred(in, di, uo) is cc_multi,
    di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(muo) is multi, pred(mdi, di, uo) is det,
    di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, di, uo) is det,
    di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, di, uo) is cc_multi,
    di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, in, out) is det,
    in, out) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, in, out) is cc_multi,
    in, out) is cc_multi.
:- mode unsorted_aggregate(pred(muo) is nondet, pred(mdi, di, uo) is det,
    di, uo) is cc_multi.

    % This is a generalization of unsorted_aggregate which allows the
    % iteration to stop before all solutions have been found.
    % Declaratively, the specification is as follows:
    %
    %   do_while(Generator, Filter, !Acc) :-
    %       unsorted_solutions(Generator, Solutions),
    %       do_while_2(Solutions, Filter, !Acc).
    %
    %   do_while_2([], _, !Acc).
    %   do_while_2([X | Xs], Filter, !Acc) :-
    %       Filter(X, More, !Acc),
    %       ( More = yes ->
    %           do_while_2(Xs, Filter, !Acc)
    %       ;
    %           true
    %       ).
    %
    % Operationally, however, do_while/4 will call the Filter
    % predicate for each solution as it is obtained, rather than
    % first building a list of all the solutions.
    %
:- pred do_while(pred(T), pred(T, bool, T2, T2), T2, T2).
:- mode do_while(pred(out) is multi, pred(in, out, in, out) is det, in, out)
    is cc_multi.
:- mode do_while(pred(out) is multi, pred(in, out, di, uo) is det, di, uo)
    is cc_multi.
:- mode do_while(pred(out) is multi, pred(in, out, di, uo) is cc_multi, di, uo)
    is cc_multi.
:- mode do_while(pred(out) is nondet, pred(in, out, in, out) is det, in, out)
    is cc_multi.
:- mode do_while(pred(out) is nondet, pred(in, out, di, uo) is det, di, uo)
    is cc_multi.
:- mode do_while(pred(out) is nondet, pred(in, out, di, uo) is cc_multi, di, uo)
    is cc_multi.

%-----------------------------------------------------------------------------%
%
% General purpose higher-order programming constructs.
%

    % compose(F, G, X) = F(G(X))
    %
    % Function composition.
    % XXX It would be nice to have infix `o' or somesuch for this.
    %
:- func compose(func(T2) = T3, func(T1) = T2, T1) = T3.

    % converse(F, X, Y) = F(Y, X).
    %
:- func converse(func(T1, T2) = T3, T2, T1) = T3.

    % pow(F, N, X) = F^N(X)
    %
    % Function exponentiation.
    %
:- func pow(func(T) = T, int, T) = T.

    % The identity function.
    %
:- func id(T) = T.

%-----------------------------------------------------------------------------%

    % maybe_pred(Pred, X, Y) takes a closure Pred which transforms an
    % input semideterministically. If calling the closure with the input
    % X succeeds, Y is bound to `yes(Z)' where Z is the output of the
    % call, or to `no' if the call fails.
    %
:- pred maybe_pred(pred(T1, T2), T1, maybe(T2)).
:- mode maybe_pred(pred(in, out) is semidet, in, out) is det.

:- func maybe_func(func(T1) = T2, T1) = maybe(T2).
:- mode maybe_func(func(in) = out is semidet, in) = out is det.

%-----------------------------------------------------------------------------%

    % isnt(Pred, X) <=> not Pred(X)
    %
    % This is useful in higher order programming, e.g.
    %   Odds  = list.filter(odd, Xs)
    %   Evens = list.filter(isnt(odd), Xs)
    %
:- pred isnt(pred(T)::(pred(in) is semidet), T::in) is semidet.

%-----------------------------------------------------------------------------%

    % `semidet_succeed' is exactly the same as `true', except that
    % the compiler thinks that it is semi-deterministic.  You can
    % use calls to `semidet_succeed' to suppress warnings about
    % determinism declarations which could be stricter.
    % Similarly, `semidet_fail' is like `fail' except that its
    % determinism is semidet rather than failure, and
    % `cc_multi_equal(X, Y)' is the same as `X=Y' except that it
    % is cc_multi rather than det.

:- pred semidet_succeed is semidet.

:- pred semidet_fail is semidet.

:- pred cc_multi_equal(T, T).
:- mode cc_multi_equal(di, uo) is cc_multi.
:- mode cc_multi_equal(in, out) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- interface.

% The rest of the interface is for use by implementors only.

    % dynamic_cast(X, Y) succeeds with Y = X iff X has the same
    % ground type as Y (so this may succeed if Y is of type
    % list(int), say, but not if Y is of type list(T)).
    %
:- pred dynamic_cast(T1::in, T2::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module construct.
:- import_module deconstruct.
:- import_module int.
:- import_module require.
:- import_module set.
:- import_module string.

:- use_module private_builtin. % for the `heap_pointer' type.

% XXX This should not be necessary, but the current compiler is broken in that
% it puts foreign_proc clauses into deconstruct.opt without also putting the
% foreign_decl they require into deconstruct.opt as well.

:- pragma foreign_decl("C", "

#include ""mercury_deconstruct.h""
#include ""mercury_deconstruct_macros.h""

").

%-----------------------------------------------------------------------------%

map_maybe(_, no, no).
map_maybe(P, yes(T0), yes(T)) :- P(T0, T).

map_maybe(_, no) = no.
map_maybe(F, yes(T)) = yes(F(T)).

fold_maybe(P, yes(Value), Acc0, Acc) :- P(Value, Acc0, Acc).
fold_maybe(_, no, Acc, Acc).

fold_maybe(F, yes(Value), Acc0) = F(Value, Acc0).
fold_maybe(_, no, Acc) = Acc.

map_fold_maybe(_, no, no, Acc, Acc).
map_fold_maybe(P, yes(T0), yes(T), Acc0, Acc) :-
    P(T0, T, Acc0, Acc).

map_fold2_maybe(_, no, no, A, A, B, B).
map_fold2_maybe(P, yes(T0), yes(T), A0, A, B0, B) :-
    P(T0, T, A0, A, B0, B).

%   Is this really useful?
% % for use in lambda expressions where the type of functor '-' is ambiguous
% :- pred pair(X, Y, pair(X, Y)).
% :- mode pair(in, in, out) is det.
% :- mode pair(out, out, in) is det.
%
% pair(X, Y, X-Y).

fst(X-_Y) = X.
fst(P, X) :-
    X = fst(P).

snd(_X-Y) = Y.
snd(P, X) :-
    X = snd(P).

maybe_pred(Pred, X, Y) :-
    ( call(Pred, X, Z) ->
        Y = yes(Z)
    ;
        Y = no
    ).

%-----------------------------------------------------------------------------%
%
% This section defines builtin_aggregate/4 which takes a closure of type
% pred(T) in which the remaining argument is output, and backtracks over
% solutions for this, using the second argument to aggregate them however the
% user wishes.  This is basically a generalization of solutions/2.

:- pred builtin_aggregate(pred(T), pred(T, U, U), U, U).
:- mode builtin_aggregate(pred(out) is multi, pred(in, in, out) is det,
    in, out) is det. % really cc_multi
:- mode builtin_aggregate(pred(out) is multi, pred(in, di, uo) is det,
    di, uo) is det.  % really cc_multi
:- mode builtin_aggregate(pred(out) is multi, pred(in, in, out) is cc_multi,
    in, out) is det. % really cc_multi
:- mode builtin_aggregate(pred(out) is multi, pred(in, di, uo) is cc_multi,
    di, uo) is det.  % really cc_multi
:- mode builtin_aggregate(pred(muo) is multi, pred(mdi, di, uo) is det,
    di, uo) is det.  % really cc_multi
:- mode builtin_aggregate(pred(out) is nondet, pred(in, di, uo) is det,
    di, uo) is det.  % really cc_multi
:- mode builtin_aggregate(pred(out) is nondet, pred(in, di, uo) is cc_multi,
    di, uo) is det.  % really cc_multi
:- mode builtin_aggregate(pred(out) is nondet, pred(in, in, out) is det,
    in, out) is det. % really cc_multi
:- mode builtin_aggregate(pred(out) is nondet, pred(in, in, out) is cc_multi,
    in, out) is det. % really cc_multi
:- mode builtin_aggregate(pred(muo) is nondet, pred(mdi, di, uo) is det,
    di, uo) is det.  % really cc_multi

% If we're doing heap reclamation on failure, then
% in order to implement any sort of code that requires terms to survive
% backtracking, we need to (deeply) copy them out of the heap and into some
% other area before backtracking.  The obvious thing to do then is just call
% the generator predicate, let it run to completion, and copy its result into
% another memory area (call it the solutions heap) before forcing
% backtracking.  When we get the next solution, we do the same, this time
% passing the previous collection (which is still on the solutions heap) to
% the collector predicate.  If the result of this operation contains the old
% collection as a part, then the deep copy operation is smart enough
% not to copy again.  So this could be pretty efficient.
%
% But what if the collector predicate does something that copies the previous
% collection?  Then on each solution, we'll copy the previous collection to
% the heap, and then deep copy it back to the solution heap.  This means
% copying solutions order N**2 times, where N is the number of solutions.  So
% this isn't as efficient as we hoped.
%
% So we use a slightly different approach.  When we find a solution, we deep
% copy it to the solution heap.  Then, before calling the collector code, we
% sneakily swap the runtime system's notion of which is the heap and which is
% the solutions heap.  This ensures that any terms are constructed on the
% solutions heap.  When this is complete, we swap them back, and force the
% engine to backtrack to get the next solution.  And so on.  After we've
% gotten the last solution, we do another deep copy to move the solution back
% to the 'real' heap, and reset the solutions heap pointer (which of course
% reclaims all the garbage of the collection process).
%
% Note that this will work with recursive calls to builtin_aggregate as
% well.  If the recursive invocation occurs in the generator pred, there can
% be no problem because by the time the generator succeeds, the inner
% do_ call will have completed, copied its result from the solutions heap,
% and reset the solutions heap pointer.  If the recursive invocation happens
% in the collector pred, then it will happen when the heap and solutions heap
% are 'swapped.'  This will work out fine, because the real heap isn't needed
% while the collector pred is executing, and by the time the nested do_ is
% completed, the 'real' heap pointer will have been reset.
%
% If the collector predicate throws an exception while they are swapped,
% then the code for builtin_throw/1 will unswap the heaps.
% So we don't need to create our own exception handlers to here to
% cover that case.
%
% If we're not doing heap reclamation on failure (as is currently the
% case when using conservative GC), then all of the heap-swapping
% and copying operations are no-ops, so we get a "zero-copy" solution.

% Note that the code for builtin_aggregate is very similar to the code
% for do_while (below).

:- pragma promise_pure(builtin_aggregate/4).

builtin_aggregate(GeneratorPred, CollectorPred, Accumulator0, Accumulator) :-
    % Save some of the Mercury virtual machine registers
    impure get_registers(HeapPtr, SolutionsHeapPtr, TrailPtr),
    impure start_all_soln_neg_context,

    % Initialize the accumulator
    % /* Mutvar := Accumulator0 */
    impure new_mutvar(Accumulator0, Mutvar),

    (
        % Get a solution.
        GeneratorPred(Answer0),

        % Check that the generator didn't leave any delayed goals outstanding.
        impure check_for_floundering(TrailPtr),

        % Update the accumulator.
        % /* MutVar := CollectorPred(MutVar) */
        impure swap_heap_and_solutions_heap,
        impure partial_deep_copy(HeapPtr, Answer0, Answer),
        impure get_mutvar(Mutvar, Acc0),
        impure non_cc_call(CollectorPred, Answer, Acc0, Acc1),
        impure set_mutvar(Mutvar, Acc1),
        impure swap_heap_and_solutions_heap,

        % Force backtracking, so that we get the next solution.
        % This will automatically reset the heap and trail.
        fail
    ;
        % There are no more solutions.
        impure end_all_soln_neg_context_no_more,

        % So now we just need to copy the final value of the accumulator
        % from the solutions heap back onto the ordinary heap, and then we can
        % reset the solutions heap pointer. We also need to discard the trail
        % ticket created by get_registers/3.
        % /* Accumulator := MutVar */
        impure get_mutvar(Mutvar, Accumulator1),
        impure partial_deep_copy(SolutionsHeapPtr, Accumulator1, Accumulator),
        impure reset_solutions_heap(SolutionsHeapPtr),
        impure discard_trail_ticket
    ).

% The code for do_while/4 is essentially the same as the code for
% builtin_aggregate (above).  See the detailed comments above.
%
% XXX It would be nice to avoid the code duplication here,
% but it is a bit tricky -- we can't just use a lambda expression,
% because we'd need to specify the mode, but we want it to work
% for multiple modes.  An alternative would be to use a typeclass,
% but typeclasses still don't work in `jump' or `fast' grades.

:- pragma promise_pure(do_while/4).

do_while(GeneratorPred, CollectorPred, Accumulator0, Accumulator) :-
    impure get_registers(HeapPtr, SolutionsHeapPtr, TrailPtr),
    impure new_mutvar(Accumulator0, Mutvar),
    impure start_all_soln_neg_context,
    (
        GeneratorPred(Answer0),

        impure check_for_floundering(TrailPtr),

        impure swap_heap_and_solutions_heap,
        impure partial_deep_copy(HeapPtr, Answer0, Answer),
        impure get_mutvar(Mutvar, Acc0),
        impure non_cc_call(CollectorPred, Answer, More, Acc0, Acc1),
        impure set_mutvar(Mutvar, Acc1),
        impure swap_heap_and_solutions_heap,

        % If More = yes, then backtrack for the next solution.
        % If More = no, then we're done.
        More = no,
        impure end_all_soln_neg_context_more
    ;
        impure end_all_soln_neg_context_no_more
    ),
    impure get_mutvar(Mutvar, Accumulator1),
    impure partial_deep_copy(SolutionsHeapPtr, Accumulator1, Accumulator),
    impure reset_solutions_heap(SolutionsHeapPtr),
    impure discard_trail_ticket.

    % This is the same as call/4, except that it is not cc_multi
    % even when the called predicate is cc_multi.
:- impure pred non_cc_call(pred(T, Acc, Acc), T, Acc, Acc).
:- mode non_cc_call(pred(in, in, out) is det, in, in, out) is det.
:- mode non_cc_call(pred(in, in, out) is cc_multi, in, in, out) is det.
:- mode non_cc_call(pred(in, di, uo) is det, in, di, uo) is det.
:- mode non_cc_call(pred(in, di, uo) is cc_multi, in, di, uo) is det.
:- mode non_cc_call(pred(mdi, di, uo) is det, mdi, di, uo) is det.

non_cc_call(P::pred(in, in, out) is det, X::in, Acc0::in, Acc::out) :-
    P(X, Acc0, Acc).
non_cc_call(P::pred(in, in, out) is cc_multi, X::in, Acc0::in, Acc::out) :-
    Pred = (pred(Soln::out) is cc_multi :- P(X, Acc0, Soln)),
    impure Acc = builtin.get_one_solution(Pred).
non_cc_call(P::pred(in, di, uo) is cc_multi, X::in, Acc0::di, Acc::uo) :-
    impure builtin.get_one_solution_io(
        (pred({}::out, di, uo) is cc_multi --> P(X)),
        _, Acc0, Acc).
non_cc_call(P::pred(in, di, uo) is det, X::in, Acc0::di, Acc::uo) :-
    P(X, Acc0, Acc).
non_cc_call(P::pred(mdi, di, uo) is det, X::mdi, Acc0::di, Acc::uo) :-
    P(X, Acc0, Acc).

    % This is the same as call/5, except that it is not cc_multi
    % even when the called predicate is cc_multi.
:- impure pred non_cc_call(pred(T1, T2, Acc, Acc), T1, T2, Acc, Acc).
:- mode non_cc_call(pred(in, out, in, out) is det, in, out, in, out) is det.
:- mode non_cc_call(pred(in, out, di, uo) is det, in, out, di, uo) is det.
:- mode non_cc_call(pred(in, out, di, uo) is cc_multi, in, out, di, uo) is det.

non_cc_call(P::pred(in, out, di, uo) is det, X::in, More::out,
        Acc0::di, Acc::uo) :-
    P(X, More, Acc0, Acc).
non_cc_call(P::pred(in, out, in, out) is det, X::in, More::out,
        Acc0::in, Acc::out) :-
    P(X, More, Acc0, Acc).
non_cc_call(P::pred(in, out, di, uo) is cc_multi, X::in, More::out,
        Acc0::di, Acc::uo) :-
    impure builtin.get_one_solution_io(
        (pred(M::out, di, uo) is cc_multi --> P(X, M)),
        More, Acc0, Acc).

:- type heap_ptr == private_builtin.heap_pointer.
:- type trail_ptr ---> trail_ptr(c_pointer).

% Save the state of the Mercury heap and trail registers,
% for later use in partial_deep_copy/3 and reset_solutions_heap/1.
% Note that this allocates a trail ticket;
% you need to dispose of it properly when you're finished with it,
% e.g. by calling discard_trail_ticket/0.
:- impure pred get_registers(heap_ptr::out, heap_ptr::out, trail_ptr::out)
    is det.

:- pragma foreign_proc("C",
    get_registers(HeapPtr::out, SolutionsHeapPtr::out, TrailPtr::out),
    [will_not_call_mercury, thread_safe],
"
    /* save heap states */
#ifdef MR_RECLAIM_HP_ON_FAILURE
    HeapPtr = (MR_Word) MR_hp;
    SolutionsHeapPtr = (MR_Word) MR_sol_hp;
#else
    HeapPtr = SolutionsHeapPtr = 0;
#endif

    /* save trail state */
#ifdef MR_USE_TRAIL
    MR_store_ticket(TrailPtr);
#else
    TrailPtr = 0;
#endif
").

:- pragma foreign_proc("C#",
    get_registers(HeapPtr::out, SolutionsHeapPtr::out, TrailPtr::out),
    [will_not_call_mercury, thread_safe],
"
    /*
    ** For .NET we always use the MS garbage collector,
    ** so we don't have to worry here about heap reclamation on failure.
    */
    HeapPtr = null;
    SolutionsHeapPtr = null;

#if MR_USE_TRAIL
    /* XXX trailing not yet implemented for the MLDS back-end */
    mercury.runtime.Errors.SORRY(""foreign code for get_registers"");
#else
    TrailPtr = null;
#endif
").

:- pragma foreign_proc("Java",
    get_registers(HeapPtr::out, SolutionsHeapPtr::out, TrailPtr::out),
    [will_not_call_mercury, thread_safe],
"
    /*
    ** Java has a builtin garbage collector,
    ** so we don't have to worry here about heap reclamation on failure.
    */
    HeapPtr = null;
    SolutionsHeapPtr = null;

    /* XXX No trailing for the Java back-end. */
    TrailPtr = null;
").

:- impure pred check_for_floundering(trail_ptr::in) is det.

:- pragma foreign_proc("C",
    check_for_floundering(TrailPtr::in),
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
    /* check for outstanding delayed goals (``floundering'') */
    MR_reset_ticket(TrailPtr, MR_solve);
#endif
").

:- pragma foreign_proc("C#",
    check_for_floundering(_TrailPtr::in),
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    mercury.runtime.Errors.SORRY(""foreign code for check_for_floundering"");
#endif
").

:- pragma foreign_proc("Java",
    check_for_floundering(_TrailPtr::in),
    [will_not_call_mercury, thread_safe],
"
    /* XXX No trailing for the Java back-end, so take no action. */
").

%
% Discard the topmost trail ticket.
%
:- impure pred discard_trail_ticket is det.

:- pragma foreign_proc("C",
    discard_trail_ticket,
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
    MR_discard_ticket();
#endif
").

:- pragma foreign_proc("C#",
    discard_trail_ticket,
    [will_not_call_mercury, thread_safe],
"
#if MR_USE_TRAIL
    mercury.runtime.Errors.SORRY(""foreign code for discard_trail_ticket"");
#endif
").

:- pragma foreign_proc("Java",
    discard_trail_ticket,
    [will_not_call_mercury, thread_safe],
"
    /* XXX No trailing for the Java back-end, so take no action. */
").

%
% Swap the heap with the solutions heap
%
:- impure pred swap_heap_and_solutions_heap is det.

:- pragma foreign_proc("C",
    swap_heap_and_solutions_heap,
    [will_not_call_mercury, thread_safe],
"{
#ifdef MR_RECLAIM_HP_ON_FAILURE
    MR_MemoryZone   *temp_zone;
    MR_Word         *temp_hp;

    temp_zone = MR_ENGINE(MR_eng_heap_zone);
    MR_ENGINE(MR_eng_heap_zone) = MR_ENGINE(MR_eng_solutions_heap_zone);
    MR_ENGINE(MR_eng_solutions_heap_zone) = temp_zone;
    temp_hp = MR_hp;
    MR_hp_word = (MR_Word) MR_sol_hp;
    MR_sol_hp = temp_hp;
#endif
}").

:- pragma foreign_proc("C#",
    swap_heap_and_solutions_heap,
    [will_not_call_mercury, thread_safe],
"
    //
    // For the .NET back-end, we use the system heap, rather
    // than defining our own heaps.  So we don't need to
    // worry about swapping them.  Hence do nothing here.
    //
").

:- pragma foreign_proc("Java",
    swap_heap_and_solutions_heap,
    [will_not_call_mercury, thread_safe],
"
    /*
    ** For the Java back-end, as for the .NET back-end, we don't define
    ** our own heaps.  So take no action here.
    */
").

%
% partial_deep_copy(SolutionsHeapPtr, OldVal, NewVal):
%   Make a copy of all of the parts of OldVar that occur between
%   SolutionsHeapPtr and the top of the current solutions heap.
%
:- impure pred partial_deep_copy(heap_ptr, T, T) is det.
:-        mode partial_deep_copy(in, di, uo) is det.
:-        mode partial_deep_copy(in, mdi, muo) is det.
:-        mode partial_deep_copy(in, in, out) is det.

:- pragma foreign_decl("C", "

#include ""mercury_deep_copy.h""

#ifndef MR_RECLAIM_HP_ON_FAILURE
  /* for conservative GC, shallow copies suffice */
  #define MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr,                        \\
        OldVar, NewVal, TypeInfo_for_T)                                 \\
    do {                                                                \\
        NewVal = OldVal;                                                \\
    } while (0)
#else
  /*
  ** Note that we need to save/restore the MR_hp register, if it
  ** is transient, before/after calling MR_deep_copy().
  */
  #define MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr,                        \\
        OldVar, NewVal, TypeInfo_for_T)                                 \\
    do {                                                                \\
        MR_save_transient_hp();                                         \\
        NewVal = MR_deep_copy(OldVal, (MR_TypeInfo) TypeInfo_for_T,     \\
            (const MR_Word *) SolutionsHeapPtr,                         \\
            MR_ENGINE(MR_eng_solutions_heap_zone)->MR_zone_top);        \\
        MR_restore_transient_hp();                                      \\
    } while (0)
#endif

").

:- pragma foreign_proc("C",
    partial_deep_copy(SolutionsHeapPtr::in, OldVal::in, NewVal::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").
:- pragma foreign_proc("C",
    partial_deep_copy(SolutionsHeapPtr::in, OldVal::mdi, NewVal::muo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").
:- pragma foreign_proc("C",
    partial_deep_copy(SolutionsHeapPtr::in, OldVal::di, NewVal::uo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").

:- pragma foreign_proc("C#",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::in, NewVal::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    //
    // For the IL back-end, we don't do heap reclamation on failure,
    // so we don't need to worry about making deep copies here.
    // Shallow copies will suffice.
    //
    NewVal = OldVal;
").
:- pragma foreign_proc("C#",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::mdi, NewVal::muo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    NewVal = OldVal;
").
:- pragma foreign_proc("C#",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::di, NewVal::uo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    NewVal = OldVal;
").

:- pragma foreign_proc("Java",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::in, NewVal::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    /*
    ** For the Java back-end, as for the .NET implementation,
    ** we don't do heap reclamation on failure,
    ** so we don't need to worry about making deep copies here.
    ** Shallow copies will suffice.
    */
    NewVal = OldVal;
").
:- pragma foreign_proc("Java",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::mdi, NewVal::muo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    NewVal = OldVal;
").
:- pragma foreign_proc("Java",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::di, NewVal::uo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    NewVal = OldVal;
").

    % reset_solutions_heap(SolutionsHeapPtr):
    %
    % Reset the solutions heap pointer to the specified value,
    % thus deallocating everything allocated on the solutions
    % heap since that value was obtained via get_registers/3.
    %
:- impure pred reset_solutions_heap(heap_ptr::in) is det.

:- pragma foreign_proc("C",
    reset_solutions_heap(SolutionsHeapPtr::in),
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_RECLAIM_HP_ON_FAILURE
    MR_sol_hp = (MR_Word *) SolutionsHeapPtr;
#endif
").

:- pragma foreign_proc("C#",
    reset_solutions_heap(_SolutionsHeapPtr::in),
    [will_not_call_mercury, thread_safe],
"
    // For the IL back-end, we don't have a separate `solutions heap'.
    // Hence this operation is a NOP.
").

:- pragma foreign_proc("Java",
    reset_solutions_heap(_SolutionsHeapPtr::in),
    [will_not_call_mercury, thread_safe],
"
    /* As above, we take no action. */
").

:- impure pred start_all_soln_neg_context is det.
:- impure pred end_all_soln_neg_context_more is det.
:- impure pred end_all_soln_neg_context_no_more is det.

:- pragma foreign_proc("C",
    start_all_soln_neg_context,
    % In minimal model tabling grades, there are no threads.
    % In all other grades, this predicate is a noop.
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_MINIMAL_MODEL_STACK_COPY
    MR_pneg_enter_cond();
#endif
").

:- pragma foreign_proc("C",
    end_all_soln_neg_context_more,
    % In minimal model tabling grades, there are no threads.
    % In all other grades, this predicate is a noop.
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_MINIMAL_MODEL_STACK_COPY
    MR_pneg_enter_then();
#endif
").

:- pragma foreign_proc("C",
    end_all_soln_neg_context_no_more,
    % In minimal model tabling grades, there are no threads.
    % In all other grades, this predicate is a noop.
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_MINIMAL_MODEL_STACK_COPY
    MR_pneg_enter_else(""end_all_soln_neg_context"");
#endif
").

start_all_soln_neg_context.
end_all_soln_neg_context_more.
end_all_soln_neg_context_no_more.

%-----------------------------------------------------------------------------%

%%% :- module mutvar.
%%% :- interface.

%  A non-backtrackably destructively modifiable reference type
:- type mutvar(T).

%  Create a new mutvar given a term for it to reference.
:- impure pred new_mutvar(T, mutvar(T)).
:-        mode new_mutvar(in, out) is det.
:-        mode new_mutvar(di, uo) is det.

%  Get the value currently referred to by a reference.
:- impure pred get_mutvar(mutvar(T), T) is det.
:-        mode get_mutvar(in, uo) is det.   % XXX this is a work-around
/*
XXX `ui' modes don't work yet
:-        mode get_mutvar(in, uo) is det.
:-        mode get_mutvar(ui, uo) is det.   % unsafe, but we use it safely
*/

%  destructively modify a reference to refer to a new object.
:- impure pred set_mutvar(mutvar(T), T) is det.
:-        mode set_mutvar(in, in) is det.
/*
XXX `ui' modes don't work yet
:-        pred set_mutvar(ui, di) is det.
*/

%%% :- implementation.

%  This type is a builtin-in type whose operations are implemented in C.
:- type mutvar(T)
    --->    mutvar(private_builtin.ref(T)).

:- pragma inline(new_mutvar/2).
:- pragma inline(get_mutvar/2).
:- pragma inline(set_mutvar/2).

:- pragma foreign_proc("C",
    new_mutvar(X::in, Ref::out),
    [will_not_call_mercury, thread_safe],
"
    MR_offset_incr_hp_msg(Ref, MR_SIZE_SLOT_SIZE, MR_SIZE_SLOT_SIZE + 1,
        MR_PROC_LABEL, ""std_util.mutvar/1"");
    MR_define_size_slot(0, Ref, 1);
    * (MR_Word *) Ref = X;
").
:- pragma foreign_proc("C",
    new_mutvar(X::di, Ref::uo),
    [will_not_call_mercury, thread_safe],
"
    MR_offset_incr_hp_msg(Ref, MR_SIZE_SLOT_SIZE, MR_SIZE_SLOT_SIZE + 1,
        MR_PROC_LABEL, ""std_util.mutvar/1"");
    MR_define_size_slot(0, Ref, 1);
    * (MR_Word *) Ref = X;
").

:- pragma foreign_proc("C",
    get_mutvar(Ref::in, X::uo),
    [will_not_call_mercury, thread_safe],
"
    X = * (MR_Word *) Ref;
").

:- pragma foreign_proc("C",
    set_mutvar(Ref::in, X::in),
    [will_not_call_mercury, thread_safe],
"
    *(MR_Word *) Ref = X;
").

:- pragma foreign_proc("C#",
    new_mutvar(X::in, Ref::out),
    [will_not_call_mercury, thread_safe],
"
    Ref = new object[1];
    Ref[0] = X;
").
:- pragma foreign_proc("C#",
    new_mutvar(X::di, Ref::uo),
    [will_not_call_mercury, thread_safe],
"
    Ref = new object[1];
    Ref[0] = X;
").

:- pragma foreign_proc("C#",
    get_mutvar(Ref::in, X::uo),
    [will_not_call_mercury, thread_safe],
"
    X = Ref[0];
").

:- pragma foreign_proc("C#",
    set_mutvar(Ref::in, X::in),
    [will_not_call_mercury, thread_safe],
"
    Ref[0] = X;
").

:- pragma foreign_code("Java",
"
    public static class Mutvar {
        public Object object;

        public Mutvar(Object init) {
            object = init;
        }
    }
").
:- pragma foreign_type(java, mutvar(T), "mercury.std_util.Mutvar").

:- pragma foreign_proc("Java",
    new_mutvar(X::in, Ref::out),
    [will_not_call_mercury, thread_safe],
"
    Ref = new mercury.std_util.Mutvar(X);
").
:- pragma foreign_proc("Java",
    new_mutvar(X::di, Ref::uo),
    [will_not_call_mercury, thread_safe],
"
    Ref = new mercury.std_util.Mutvar(X);
").

:- pragma foreign_proc("Java",
    get_mutvar(Ref::in, X::uo),
    [will_not_call_mercury, thread_safe],
"
    X = Ref.object;
").

:- pragma foreign_proc("Java",
    set_mutvar(Ref::in, X::in),
    [will_not_call_mercury, thread_safe],
"
    Ref.object = X;
").

%%% end_module mutvar.

%-----------------------------------------------------------------------------%

solutions(Pred, List) :-
    builtin_solutions(Pred, UnsortedList),
    list.sort_and_remove_dups(UnsortedList, List).

solutions_set(Pred, Set) :-
    builtin_solutions(Pred, List),
    set.list_to_set(List, Set).

unsorted_solutions(Pred, List) :-
    builtin_solutions(Pred, UnsortedList),
    cc_multi_equal(UnsortedList, List).

:- pred builtin_solutions(pred(T), list(T)).
:- mode builtin_solutions(pred(out) is multi, out)
    is det. /* really cc_multi */
:- mode builtin_solutions(pred(out) is nondet, out)
    is det. /* really cc_multi */

builtin_solutions(Generator, UnsortedList) :-
    builtin_aggregate(Generator, list.cons, [], UnsortedList).

%-----------------------------------------------------------------------------%

aggregate(Generator, Accumulator, !Acc) :-
    solutions(Generator, Solutions),
    list.foldl(Accumulator, Solutions, !Acc).

aggregate2(Generator, Accumulator, !Acc1, !Acc2) :-
    solutions(Generator, Solutions),
    list.foldl2(Accumulator, Solutions, !Acc1, !Acc2).

unsorted_aggregate(Generator, Accumulator, !Acc) :-
    builtin_aggregate(Generator, Accumulator, !Acc),
    cc_multi_equal(!Acc).

%-----------------------------------------------------------------------------%

% semidet_succeed and semidet_fail, implemented using the C interface
% to make sure that the compiler doesn't issue any determinism warnings
% for them.

:- pragma foreign_proc("C",
    semidet_succeed,
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = MR_TRUE;
").
:- pragma foreign_proc("C",
    semidet_fail,
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = MR_FALSE;
").
:- pragma foreign_proc("C",
    cc_multi_equal(X::in, Y::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Y = X;
").
:- pragma foreign_proc("C",
    cc_multi_equal(X::di, Y::uo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Y = X;
").

:- pragma foreign_proc("C#",
    semidet_succeed,
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = true;
").
:- pragma foreign_proc("C#",
    semidet_fail,
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("C#",
    cc_multi_equal(X::in, Y::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Y = X;
").
:- pragma foreign_proc("C#",
    cc_multi_equal(X::di, Y::uo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Y = X;
").

:- pragma foreign_proc("Java",
    cc_multi_equal(X::in, Y::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Y = X;
").
:- pragma foreign_proc("Java",
    cc_multi_equal(X::di, Y::uo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Y = X;
").

% We can't just use "true" and "fail" here, because that provokes warnings
% from determinism analysis, and the library is compiled with --halt-at-warn.
% So instead we use 0+0 = (or \=) 0.
% This is guaranteed to succeed or fail (respectively),
% and with a bit of luck will even get optimized by constant propagation.
% But this optimization won't happen until after determinism analysis,
% which doesn't know anything about integer arithmetic,
% so this code won't provide a warning from determinism analysis.

semidet_succeed :-
    0 + 0 = 0.
semidet_fail :-
    0 + 0 \= 0.

:- pragma promise_pure(cc_multi_equal/2).

cc_multi_equal(X, X).

%-----------------------------------------------------------------------------%

    % We call the constructor for univs `univ_cons' to avoid ambiguity
    % with the univ/1 function which returns a univ.
:- type univ
    --->    some [T] univ_cons(T).

univ_to_type(Univ, X) :- type_to_univ(X, Univ).

univ(X) = Univ :- type_to_univ(X, Univ).

det_univ_to_type(Univ, X) :-
    ( type_to_univ(X0, Univ) ->
        X = X0
    ;
        UnivTypeName = type_desc.type_name(univ_type(Univ)),
        ObjectTypeName = type_desc.type_name(type_desc.type_of(X)),
        string.append_list(["det_univ_to_type: conversion failed\\n",
            "\tUniv Type: ", UnivTypeName,
            "\\n\tObject Type: ", ObjectTypeName], ErrorString),
        error(ErrorString)
    ).

univ_value(univ_cons(X)) = X.

:- pragma promise_pure(type_to_univ/2).

type_to_univ(T::di, Univ::uo) :-
    Univ0 = 'new univ_cons'(T),
    unsafe_promise_unique(Univ0, Univ).
type_to_univ(T::in, Univ::out) :-
    Univ0 = 'new univ_cons'(T),
    unsafe_promise_unique(Univ0, Univ).
type_to_univ(T::out, Univ::in) :-
    Univ = univ_cons(T0),
    private_builtin.typed_unify(T0, T).

univ_type(Univ) = type_desc.type_of(univ_value(Univ)).

:- pred construct_univ(T, univ).
:- mode construct_univ(in, out) is det.
:- pragma export(construct_univ(in, out), "ML_construct_univ").

construct_univ(X, Univ) :-
    Univ = univ(X).

:- some [T] pred unravel_univ(univ, T).
:- mode unravel_univ(in, out) is det.
:- pragma export(unravel_univ(in, out), "ML_unravel_univ").

unravel_univ(Univ, X) :-
    univ_value(Univ) = X.

dynamic_cast(X, Y) :-
    private_builtin.typed_unify(X, Y).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%   Function forms added.

pair(X, Y) =
    X-Y.

maybe_func(PF, X) =
    ( if Y = PF(X) then yes(Y) else no ).

compose(F, G, X) =
    F(G(X)).

converse(F, X, Y) =
    F(Y, X).

pow(F, N, X) =
    ( if N = 0 then X else pow(F, N - 1, F(X)) ).

isnt(P, X) :-
    not P(X).

id(X) = X.

solutions(P) = S :- solutions(P, S).

solutions_set(P) = S :- solutions_set(P, S).

aggregate(P, F, Acc0) = Acc :-
    aggregate(P, (pred(X::in, A0::in, A::out) is det :- A = F(X, A0)),
        Acc0, Acc).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
