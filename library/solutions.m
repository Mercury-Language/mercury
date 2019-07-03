%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2007 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: solutions.m.
% Main author: fjh.
% Stability: medium.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module solutions.
:- interface.

:- import_module bool.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

    % solutions/2 collects all the solutions to a predicate and returns
    % them as a list in sorted order, with duplicates removed.
    % solutions_set/2 returns them as a set. unsorted_solutions/2 returns
    % them as an unsorted list with possible duplicates; since there are
    % an infinite number of such lists, this must be called from a context
    % in which only a single solution is required.
    %
:- pred solutions(pred(T), list(T)).
:- mode solutions(pred(out) is multi, out(non_empty_list)) is det.
:- mode solutions(pred(out) is nondet, out) is det.

:- func solutions(pred(T)) = list(T).
:- mode solutions(pred(out) is multi) = out(non_empty_list) is det.
:- mode solutions(pred(out) is nondet) = out is det.

:- func solutions_set(pred(T)) = set(T).
:- mode solutions_set(pred(out) is multi) = out is det.
:- mode solutions_set(pred(out) is nondet) = out is det.

:- pred solutions_set(pred(T), set(T)).
:- mode solutions_set(pred(out) is multi, out) is det.
:- mode solutions_set(pred(out) is nondet, out) is det.

:- pred unsorted_solutions(pred(T), list(T)).
:- mode unsorted_solutions(pred(out) is multi, out(non_empty_list))
    is cc_multi.
:- mode unsorted_solutions(pred(out) is nondet, out)
    is cc_multi.

:- func aggregate(pred(T), func(T, U) = U, U) = U.
:- mode aggregate(pred(out) is multi, func(in, in) = out is det, in)
    = out is det.
:- mode aggregate(pred(out) is nondet, func(in, in) = out is det, in)
    = out is det.

    % aggregate/4 generates all the solutions to a predicate,
    % sorts them and removes duplicates, then applies an accumulator
    % predicate to each solution in turn:
    %
    % aggregate(Generator, AccumulatorPred, Acc0, Acc) <=>
    %   solutions(Generator, Solutions),
    %   list.foldl(AccumulatorPred, Solutions, Acc0, Acc).
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
    % aggregate2(Generator, AccumulatorPred, AccA0, AccA, AccB0, AccB) <=>
    %   solutions(Generator, Solutions),
    %   list.foldl2(AccumulatorPred, Solutions, AccA0, AccA, AccB0, AccB).
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
    % unsorted_aggregate(Generator, AccumulatorPred, Acc0, Acc) <=>
    %   unsorted_solutions(Generator, Solutions),
    %   list.foldl(AccumulatorPred, Solutions, Acc0, Acc).
    %
    % Operationally, however, unsorted_aggregate/4 will call the
    % AccumulatorPred for each solution as it is obtained, rather than
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

    % unsorted_aggregate2/6 generates all the solutions to a predicate
    % and applies an accumulator predicate to each solution in turn.
    % Declaratively, the specification is as follows:
    %
    % unsorted_aggregate2(Generator, AccumulatorPred, !Acc1, !Acc2) <=>
    %   unsorted_solutions(Generator, Solutions),
    %   list.foldl2(AccumulatorPred, Solutions, !Acc1, !Acc2).
    %
    % Operationally, however, unsorted_aggregate2/6 will call the
    % AccumulatorPred for each solution as it is obtained, rather than
    % first building a list of all the solutions.
    %
:- pred unsorted_aggregate2(pred(T), pred(T, U, U, V, V), U, U, V, V).
:- mode unsorted_aggregate2(pred(out) is multi,
    pred(in, in, out, in, out) is det, in, out, in, out) is cc_multi.
:- mode unsorted_aggregate2(pred(out) is multi,
    pred(in, in, out, in, out) is cc_multi, in, out, in, out) is cc_multi.
:- mode unsorted_aggregate2(pred(out) is multi,
    pred(in, in, out, di, uo) is det, in, out, di, uo) is cc_multi.
:- mode unsorted_aggregate2(pred(out) is multi,
    pred(in, in, out, di, uo) is cc_multi, in, out, di, uo) is cc_multi.
:- mode unsorted_aggregate2(pred(out) is nondet,
    pred(in, in, out, in, out) is det, in, out, in, out) is cc_multi.
:- mode unsorted_aggregate2(pred(out) is nondet,
    pred(in, in, out, in, out) is cc_multi, in, out, in, out) is cc_multi.
:- mode unsorted_aggregate2(pred(out) is nondet,
    pred(in, in, out, di, uo) is det, in, out, di, uo) is cc_multi.
:- mode unsorted_aggregate2(pred(out) is nondet,
    pred(in, in, out, di, uo) is cc_multi, in, out, di, uo) is cc_multi.

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
    %       (
    %           More = yes,
    %           do_while_2(Xs, Filter, !Acc)
    %       ;
    %           More = no
    %       ).
    %
    % Operationally, however, do_while/4 will call the Filter
    % predicate for each solution as it is obtained, rather than
    % first building a list of all the solutions.
    %
:- pred do_while(pred(T), pred(T, bool, T2, T2), T2, T2).
:- mode do_while(pred(out) is multi, pred(in, out, in, out) is det,
    in, out) is cc_multi.
:- mode do_while(pred(out) is multi, pred(in, out, di, uo) is det,
    di, uo) is cc_multi.
:- mode do_while(pred(out) is multi, pred(in, out, di, uo) is cc_multi,
    di, uo) is cc_multi.
:- mode do_while(pred(out) is nondet, pred(in, out, in, out) is det,
    in, out) is cc_multi.
:- mode do_while(pred(out) is nondet, pred(in, out, di, uo) is det,
    di, uo) is cc_multi.
:- mode do_while(pred(out) is nondet, pred(in, out, di, uo) is cc_multi,
    di, uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mutvar.
:- import_module require.

%---------------------------------------------------------------------------%

% NOTE: the all-solutions predicates are handled specially in
%       browser/declarative_tree.m. Any changes here may need to be reflected
%       there.

%---------------------------------------------------------------------------%

solutions(Pred, List) :-
    promise_equivalent_solutions [List0] (
        builtin_solutions(Pred, UnsortedList),
        list.sort_and_remove_dups(UnsortedList, List0)
    ),
    assert_num_solutions(Pred, List0, List).

solutions(P) = S :-
    solutions(P, S).

solutions_set(P) = S :-
    solutions_set(P, S).

solutions_set(Pred, Set) :-
    promise_equivalent_solutions [Set] (
        builtin_solutions(Pred, List),
        set.list_to_set(List, Set)
    ).

unsorted_solutions(Pred, List) :-
    builtin_solutions(Pred, List).

aggregate(P, F, Acc0) = Acc :-
    aggregate(P, (pred(X::in, A0::in, A::out) is det :- A = F(X, A0)),
        Acc0, Acc).

aggregate(Generator, AccumulatorPred, !Acc) :-
    solutions(Generator, Solutions),
    list.foldl(AccumulatorPred, Solutions, !Acc).

aggregate2(Generator, AccumulatorPred, !Acc1, !Acc2) :-
    solutions(Generator, Solutions),
    list.foldl2(AccumulatorPred, Solutions, !Acc1, !Acc2).

unsorted_aggregate(Generator, AccumulatorPred, !Acc) :-
    builtin_aggregate(Generator, AccumulatorPred, !Acc).

unsorted_aggregate2(Generator, AccumulatorPred, !Acc1, !Acc2) :-
    builtin_aggregate2(Generator, AccumulatorPred, !Acc1, !Acc2).

%---------------------------------------------------------------------------%

:- pred builtin_solutions(pred(T), list(T)).
:- mode builtin_solutions(pred(out) is multi, out(non_empty_list))
     is cc_multi.
:- mode builtin_solutions(pred(out) is nondet, out)
     is cc_multi.

builtin_solutions(Generator, UnsortedList) :-
    builtin_aggregate(Generator, list.cons, [], UnsortedList0),
    assert_num_solutions(Generator, UnsortedList0, UnsortedList).

:- pred assert_num_solutions(pred(T), list(T), list(T)).
:- mode assert_num_solutions(pred(out) is multi,
     in, out(non_empty_list)) is det.
:- mode assert_num_solutions(pred(out) is nondet,
     in, out) is det.

:- pragma promise_equivalent_clauses(assert_num_solutions/3).

assert_num_solutions(_Pred::pred(out) is multi,
        List0::in, List::out(non_empty_list)) :-
    (
        List0 = [],
        unexpected($pred, "no solutions")
    ;
        List0 = [_ | _],
        List = List0
    ).
assert_num_solutions(_Pred::pred(out) is nondet, List::in, List::out).

%---------------------------------------------------------------------------%
%
% This section defines builtin_aggregate/4 which takes a closure of type
% pred(T) in which the remaining argument is output, and backtracks over
% solutions for this, using the second argument to aggregate them however the
% user wishes. This is basically a generalization of solutions/2.

:- pred builtin_aggregate(pred(T), pred(T, U, U), U, U).
:- mode builtin_aggregate(pred(out) is multi, pred(in, in, out) is det,
    in, out) is cc_multi.
:- mode builtin_aggregate(pred(out) is multi, pred(in, di, uo) is det,
    di, uo) is cc_multi.
:- mode builtin_aggregate(pred(out) is multi, pred(in, in, out) is cc_multi,
    in, out) is cc_multi.
:- mode builtin_aggregate(pred(out) is multi, pred(in, di, uo) is cc_multi,
    di, uo) is cc_multi.
:- mode builtin_aggregate(pred(muo) is multi, pred(mdi, di, uo) is det,
    di, uo) is cc_multi.
:- mode builtin_aggregate(pred(out) is nondet, pred(in, di, uo) is det,
    di, uo) is cc_multi.
:- mode builtin_aggregate(pred(out) is nondet, pred(in, di, uo) is cc_multi,
    di, uo) is cc_multi.
:- mode builtin_aggregate(pred(out) is nondet, pred(in, in, out) is det,
    in, out) is cc_multi.
:- mode builtin_aggregate(pred(out) is nondet, pred(in, in, out) is cc_multi,
    in, out) is cc_multi.
:- mode builtin_aggregate(pred(muo) is nondet, pred(mdi, di, uo) is det,
    di, uo) is cc_multi.

% If we're doing heap reclamation on failure, then in order to implement any
% sort of code that requires terms to survive backtracking, we need to
% (deeply) copy them out of the heap and into some other area before
% backtracking. The obvious thing to do then is just call the generator
% predicate, let it run to completion, and copy its result into another memory
% area (call it the solutions heap) before forcing backtracking. When we get
% the next solution, we do the same, this time passing the previous collection
% (which is still on the solutions heap) to the collector predicate. If the
% result of this operation contains the old collection as a part, then the
% deep copy operation is smart enough not to copy again. So this could be
% pretty efficient.
%
% But what if the collector predicate does something that copies the previous
% collection?  Then on each solution, we'll copy the previous collection to
% the heap, and then deep copy it back to the solution heap. This means
% copying solutions order N**2 times, where N is the number of solutions.
% So this isn't as efficient as we hoped.
%
% So we use a slightly different approach. When we find a solution, we deep
% copy it to the solution heap. Then, before calling the collector code, we
% sneakily swap the runtime system's notion of which is the heap and which is
% the solutions heap. This ensures that any terms are constructed on the
% solutions heap. When this is complete, we swap them back, and force the
% engine to backtrack to get the next solution. And so on. After we've
% gotten the last solution, we do another deep copy to move the solution back
% to the 'real' heap, and reset the solutions heap pointer (which of course
% reclaims all the garbage of the collection process).
%
% Note that this will work with recursive calls to builtin_aggregate as
% well. If the recursive invocation occurs in the generator pred, there can
% be no problem because by the time the generator succeeds, the inner
% do_ call will have completed, copied its result from the solutions heap,
% and reset the solutions heap pointer. If the recursive invocation happens
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

builtin_aggregate(GeneratorPred, CollectorPred, !Acc) :-
    % Save some of the Mercury virtual machine registers
    impure get_registers(HeapPtr, SolutionsHeapPtr, TrailPtr),
    impure start_all_soln_neg_context,

    % Initialize the accumulator
    % /* Mutvar := !.Acc */
    impure new_mutvar(!.Acc, Mutvar),

    (
        % Get a solution.
        GeneratorPred(Answer0),

        % Check that the generator didn't leave any delayed goals outstanding.
        impure check_for_floundering(TrailPtr),

        % Update the accumulator.
        % /* Mutvar := CollectorPred(MutVar) */
        impure swap_heap_and_solutions_heap,
        impure partial_deep_copy(HeapPtr, Answer0, Answer),
        impure get_mutvar(Mutvar, Acc0),
        % This is a lie: CollectorPred may choose from multiple possible
        % solutions which are not all equivalent.
        promise_equivalent_solutions [Acc1] (
            call(CollectorPred, Answer, Acc0, Acc1)
        ),
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
        % /* !:Acc := Mutvar */
        impure get_mutvar(Mutvar, !:Acc),
        impure partial_deep_copy(SolutionsHeapPtr, !Acc),
        impure reset_solutions_heap(SolutionsHeapPtr),
        impure discard_trail_ticket
    ),

    % The solution is not unique because it may depend on the order that
    % GeneratorPred returned its solutions and the choice made by
    % CollectorPred.
    cc_multi_equal(!Acc).

%---------------------------------------------------------------------------%

:- pragma promise_pure(builtin_aggregate2/6).

:- pred builtin_aggregate2(pred(T), pred(T, U, U, V, V), U, U, V, V).
:- mode builtin_aggregate2(pred(out) is multi,
    pred(in, in, out, in, out) is det,
    in, out, in, out) is cc_multi.
:- mode builtin_aggregate2(pred(out) is multi,
    pred(in, in, out, in, out) is cc_multi,
    in, out, in, out) is cc_multi.
:- mode builtin_aggregate2(pred(out) is multi,
    pred(in, in, out, di, uo) is det,
    in, out, di, uo) is cc_multi.
:- mode builtin_aggregate2(pred(out) is multi,
    pred(in, in, out, di, uo) is cc_multi,
    in, out, di, uo) is cc_multi.
:- mode builtin_aggregate2(pred(out) is nondet,
    pred(in, in, out, in, out) is det,
    in, out, in, out) is cc_multi.
:- mode builtin_aggregate2(pred(out) is nondet,
    pred(in, in, out, in, out) is cc_multi,
    in, out, in, out) is cc_multi.
:- mode builtin_aggregate2(pred(out) is nondet,
    pred(in, in, out, di, uo) is det,
    in, out, di, uo) is cc_multi.
:- mode builtin_aggregate2(pred(out) is nondet,
    pred(in, in, out, di, uo) is cc_multi,
    in, out, di, uo) is cc_multi.

builtin_aggregate2(GeneratorPred, CollectorPred, !Acc1, !Acc2) :-
    % Save some of the Mercury virtual machine registers
    impure get_registers(HeapPtr, SolutionsHeapPtr, TrailPtr),
    impure start_all_soln_neg_context,

    % Initialize the accumulator
    impure new_mutvar(!.Acc1, Mutvar1),
    impure new_mutvar(!.Acc2, Mutvar2),

    (
        % Get a solution.
        GeneratorPred(Answer0),

        % Check that the generator didn't leave any delayed goals outstanding.
        impure check_for_floundering(TrailPtr),

        % Update the accumulators.
        impure swap_heap_and_solutions_heap,
        impure partial_deep_copy(HeapPtr, Answer0, Answer),
        impure get_mutvar(Mutvar1, SubAccA0),
        impure get_mutvar(Mutvar2, SubAccB0),
        % This is a lie: CollectorPred may choose from multiple possible
        % solutions which are not all equivalent.
        promise_equivalent_solutions [SubAccA, SubAccB] (
            call(CollectorPred, Answer, SubAccA0, SubAccA, SubAccB0, SubAccB)
        ),
        impure set_mutvar(Mutvar1, SubAccA),
        impure set_mutvar(Mutvar2, SubAccB),
        impure swap_heap_and_solutions_heap,

        % Force backtracking, so that we get the next solution.
        % This will automatically reset the heap and trail.
        fail
    ;
        % There are no more solutions.
        impure end_all_soln_neg_context_no_more,

        % So now we just need to copy the final value of the accumulators
        % from the solutions heap back onto the ordinary heap, and then we can
        % reset the solutions heap pointer. We also need to discard the trail
        % ticket created by get_registers/3.
        impure get_mutvar(Mutvar1, !:Acc1),
        impure get_mutvar(Mutvar2, !:Acc2),
        impure partial_deep_copy(SolutionsHeapPtr, !Acc1),
        impure partial_deep_copy(SolutionsHeapPtr, !Acc2),
        impure reset_solutions_heap(SolutionsHeapPtr),
        impure discard_trail_ticket
    ),

    % The solution is not unique because it may depend on the order that
    % GeneratorPred returned its solutions and the choice made by
    % CollectorPred.
    cc_multi_equal(!Acc1),
    cc_multi_equal(!Acc2).

%---------------------------------------------------------------------------%

% The code for do_while/4 is essentially the same as the code for
% builtin_aggregate (above). See the detailed comments above.
%
% XXX It would be nice to avoid the code duplication here,
% but it is a bit tricky -- we can't just use a lambda expression,
% because we'd need to specify the mode, but we want it to work
% for multiple modes. An alternative would be to use a typeclass,
% but typeclasses still don't work in `jump' or `fast' grades.

:- pragma promise_pure(do_while/4).

do_while(GeneratorPred, CollectorPred, !Acc) :-
    impure get_registers(HeapPtr, SolutionsHeapPtr, TrailPtr),
    impure new_mutvar(!.Acc, Mutvar),
    impure start_all_soln_neg_context,
    (
        GeneratorPred(Answer0),

        impure check_for_floundering(TrailPtr),

        impure swap_heap_and_solutions_heap,
        impure partial_deep_copy(HeapPtr, Answer0, Answer),
        impure get_mutvar(Mutvar, Acc0),
        promise_equivalent_solutions [More, Acc1] (
            call(CollectorPred, Answer, More, Acc0, Acc1)
        ),
        impure set_mutvar(Mutvar, Acc1),
        impure swap_heap_and_solutions_heap,

        % If More = yes, then backtrack for the next solution.
        % If More = no, then we're done.
        More = no,
        impure end_all_soln_neg_context_more
    ;
        impure end_all_soln_neg_context_no_more
    ),
    impure get_mutvar(Mutvar, !:Acc),
    impure partial_deep_copy(SolutionsHeapPtr, !Acc),
    impure reset_solutions_heap(SolutionsHeapPtr),
    impure discard_trail_ticket,
    cc_multi_equal(!Acc).

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
    // Save heap states.
#ifdef MR_RECLAIM_HP_ON_FAILURE
    HeapPtr = (MR_Word) MR_hp;
    SolutionsHeapPtr = (MR_Word) MR_sol_hp;
#else
    HeapPtr = SolutionsHeapPtr = 0;
#endif

    // Save trail state.
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
    // For .NET we always use the MS garbage collector,
    // so we don't have to worry here about heap reclamation on failure.
    HeapPtr = null;
    SolutionsHeapPtr = null;

#if MR_USE_TRAIL
    // XXX Trailing not yet implemented for the MLDS back-end.
    mercury.runtime.Errors.SORRY(""foreign code for get_registers"");
#else
    TrailPtr = null;
#endif
").

:- pragma foreign_proc("Java",
    get_registers(HeapPtr::out, SolutionsHeapPtr::out, TrailPtr::out),
    [will_not_call_mercury, thread_safe],
"
    // Java has a builtin garbage collector,
    // so we don't have to worry here about heap reclamation on failure.
    HeapPtr = null;
    SolutionsHeapPtr = null;

    // XXX No trailing for the Java back-end.
    TrailPtr = null;
").

:- pragma foreign_proc("Erlang",
    get_registers(HeapPtr::out, SolutionsHeapPtr::out, TrailPtr::out),
    [will_not_call_mercury, thread_safe],
"
    % Erlang has a builtin garbage collector,
    % so we don't have to worry here about heap reclamation on failure.
    HeapPtr = null,
    SolutionsHeapPtr = null,
    TrailPtr = null
").

:- impure pred check_for_floundering(trail_ptr::in) is det.

:- pragma foreign_proc("C",
    check_for_floundering(TrailPtr::in),
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
    // Check for outstanding delayed goals (``floundering'').
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
    // XXX No trailing for the Java back-end, so take no action.
").

:- pragma foreign_proc("Erlang",
    check_for_floundering(_TrailPtr::in),
    [will_not_call_mercury, thread_safe],
"
    % XXX No trailing for the Erlang back-end, so take no action.
    void
").

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
    // XXX No trailing for the Java back-end, so take no action.
").

:- pragma foreign_proc("Erlang",
    discard_trail_ticket,
    [will_not_call_mercury, thread_safe],
"
    % XXX No trailing for the Erlang back-end, so take no action.
    void
").

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
    // For the .NET back-end, we use the system heap, rather than
    // defining our own heaps. So we don't need to worry about swapping them.
    // Hence do nothing here.
").

:- pragma foreign_proc("Java",
    swap_heap_and_solutions_heap,
    [will_not_call_mercury, thread_safe],
"
    // For the Java back-end we don't define our own heaps.
    // So take no action here.
").

:- pragma foreign_proc("Erlang",
    swap_heap_and_solutions_heap,
    [will_not_call_mercury, thread_safe],
"
    % For the Erlang back-end, as for the .NET back-end, we don't define
    % our own heaps. So take no action here.
    void
").

    % partial_deep_copy(SolutionsHeapPtr, OldVal, NewVal):
    %
    % Make a copy of all of the parts of OldVar that occur between
    % SolutionsHeapPtr and the top of the current solutions heap.
    %
:- impure pred partial_deep_copy(heap_ptr, T, T).
:-        mode partial_deep_copy(in, di, uo) is det.
:-        mode partial_deep_copy(in, mdi, muo) is det.
:-        mode partial_deep_copy(in, in, out) is det.

:- pragma foreign_decl("C", "

#include ""mercury_deep_copy.h""

#ifndef MR_RECLAIM_HP_ON_FAILURE
  // For conservative GC, shallow copies suffice.
  #define MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr,                        \\
        OldVar, NewVal, TypeInfo_for_T)                                 \\
    do {                                                                \\
        NewVal = OldVal;                                                \\
    } while (0)
#else
  // Note that we need to save/restore the MR_hp register,
  // if it is transient, before/after calling MR_deep_copy().
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
    [will_not_call_mercury, thread_safe],
"
    MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").
:- pragma foreign_proc("C",
    partial_deep_copy(SolutionsHeapPtr::in, OldVal::mdi, NewVal::muo),
    [will_not_call_mercury, thread_safe],
"
    MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").
:- pragma foreign_proc("C",
    partial_deep_copy(SolutionsHeapPtr::in, OldVal::di, NewVal::uo),
    [will_not_call_mercury, thread_safe],
"
    MR_PARTIAL_DEEP_COPY(SolutionsHeapPtr, OldVal, NewVal, TypeInfo_for_T);
").

:- pragma foreign_proc("C#",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::in, NewVal::out),
    [will_not_call_mercury, thread_safe],
"
    //
    // For the C# back-end, we don't do heap reclamation on failure,
    // so we don't need to worry about making deep copies here.
    // Shallow copies will suffice.
    //
    NewVal = OldVal;
").
:- pragma foreign_proc("C#",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::mdi, NewVal::muo),
    [will_not_call_mercury, thread_safe],
"
    NewVal = OldVal;
").
:- pragma foreign_proc("C#",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::di, NewVal::uo),
    [will_not_call_mercury, thread_safe],
"
    NewVal = OldVal;
").

:- pragma foreign_proc("Java",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::in, NewVal::out),
    [will_not_call_mercury, thread_safe],
"
    // For the Java back-end, we don't do heap reclamation on failure,
    // so we don't need to worry about making deep copies here.
    // Shallow copies will suffice.
    NewVal = OldVal;
").
:- pragma foreign_proc("Java",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::mdi, NewVal::muo),
    [will_not_call_mercury, thread_safe],
"
    NewVal = OldVal;
").
:- pragma foreign_proc("Java",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::di, NewVal::uo),
    [will_not_call_mercury, thread_safe],
"
    NewVal = OldVal;
").

:- pragma foreign_proc("Erlang",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::in, NewVal::out),
    [will_not_call_mercury, thread_safe],
"
    % For the Erlang back-end, as for the .NET implementation,
    % we don't do heap reclamation on failure,
    % so we don't need to worry about making deep copies here.
    % Shallow copies will suffice.
    NewVal = OldVal
").
:- pragma foreign_proc("Erlang",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::mdi, NewVal::muo),
    [will_not_call_mercury, thread_safe],
"
    NewVal = OldVal
").
:- pragma foreign_proc("Erlang",
    partial_deep_copy(_SolutionsHeapPtr::in, OldVal::di, NewVal::uo),
    [will_not_call_mercury, thread_safe],
"
    NewVal = OldVal
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
    // For the C# back-end, we don't have a separate `solutions heap'.
    // Hence this operation is a NOP.
").

:- pragma foreign_proc("Java",
    reset_solutions_heap(_SolutionsHeapPtr::in),
    [will_not_call_mercury, thread_safe],
"
    // As above, we take no action.
").

:- pragma foreign_proc("Erlang",
    reset_solutions_heap(_SolutionsHeapPtr::in),
    [will_not_call_mercury, thread_safe],
"
    % As above, we take no action.
    void
").

:- impure pred start_all_soln_neg_context is det.

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

start_all_soln_neg_context.

:- impure pred end_all_soln_neg_context_more is det.

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

end_all_soln_neg_context_more.

:- impure pred end_all_soln_neg_context_no_more is det.

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

end_all_soln_neg_context_no_more.

%---------------------------------------------------------------------------%
:- end_module solutions.
%---------------------------------------------------------------------------%
