%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The compiler currently cannot handle properly situations in which
% a predicate has more than one clause that defines the type_info for
% an existentially typed output variable.
%
% When these two clauses are Mercury clauses, we have long generated
% an error message for such situations. When these two clauses are
% foreign_procs, the process of generating that error message used to result
% in a compiler abort. This test case checks for the absence of that
% compiler abort, by checking that we get pretty much the same error message
% for two foreign clauses as for two Mercury clauses.
%
% We test the wording of that error message for both a predicate and a
% function.
%
%---------------------------------------------------------------------------%

:- module foreign_procs_exist_type.
:- interface.

:- import_module bool.

:- inst bool_no for bool/0
    --->    no.
:- inst bool_yes for bool/0
    --->    yes.

:- some [T] pred mercury_test(bool, T).
:- mode mercury_test(in(bool_no), out) is det.
:- mode mercury_test(in(bool_yes), out) is det.

:- some [T] pred foreign_pred_test(bool, T).
:- mode foreign_pred_test(in(bool_no), out) is det.
:- mode foreign_pred_test(in(bool_yes), out) is det.

:- some [T] func foreign_func_test(bool) = T.
:- mode foreign_func_test(in(bool_no)) = out is det.
:- mode foreign_func_test(in(bool_yes)) = out is det.

%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

mercury_test(no, T1) :-
    make_t(T1).
mercury_test(yes, T2) :-
    make_t(T2).

:- some [T] pred make_t(T::out) is det.

make_t(42).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    foreign_pred_test(Bool::in(bool_no), T1::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T1 */
").

:- pragma foreign_proc("C",
    foreign_pred_test(Bool::in(bool_yes), T2::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T2 */
").

:- pragma foreign_proc("Java",
    foreign_pred_test(Bool::in(bool_no), T1::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T1 */
").

:- pragma foreign_proc("Java",
    foreign_pred_test(Bool::in(bool_yes), T2::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T2 */
").

:- pragma foreign_proc("C#",
    foreign_pred_test(Bool::in(bool_no), T1::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T1 */
").

:- pragma foreign_proc("C#",
    foreign_pred_test(Bool::in(bool_yes), T2::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T2 */
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    foreign_func_test(Bool::in(bool_no)) = (T1::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T1 */
").

:- pragma foreign_proc("C",
    foreign_func_test(Bool::in(bool_yes)) = (T2::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T2 */
").

:- pragma foreign_proc("Java",
    foreign_func_test(Bool::in(bool_no)) = (T1::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T1 */
").

:- pragma foreign_proc("Java",
    foreign_func_test(Bool::in(bool_yes)) = (T2::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T2 */
").

:- pragma foreign_proc("C#",
    foreign_func_test(Bool::in(bool_no)) = (T1::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T1 */
").

:- pragma foreign_proc("C#",
    foreign_func_test(Bool::in(bool_yes)) = (T2::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    /* TypeInfo_for_T, Bool, T2 */
").

%---------------------------------------------------------------------------%
