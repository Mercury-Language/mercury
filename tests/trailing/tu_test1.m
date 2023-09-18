%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tu_test1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

%---------------------------------------------------------------------------%

main(!IO) :-
    solutions(test(1), Solns),
    io.write(Solns, !IO),
    io.nl(!IO).

:- pred test(int::in, int::out) is nondet.

test(X, Y) :-
    promise_pure (
        between(X, 10, Y),
        impure save_ref_on_trail(Y),
        is_correct(Y)
    ).

:- pred is_correct(int::in) is semidet.

is_correct(5).
is_correct(7).

:- pred between(int::in, int::in, int::out) is multi.

between(N, _, N).
between(L, U, N) :-
    L < U,
    between(L + 1, U, N).

:- impure pred save_ref_on_trail(int::in) is det.
:- pragma foreign_proc("C",
    save_ref_on_trail(I::in),
    [will_not_call_mercury, may_modify_trail],
"
    MR_trail_function(print_entry, (void *)I);
").

:- pragma foreign_decl("C", "
    #include <stdio.h>
    #include <stdlib.h>
    extern void print_entry(void *, MR_untrail_reason);
").

:- pragma foreign_code("C", "
void
print_entry(void *value, MR_untrail_reason reason)
{
    switch (reason) {
    case MR_undo:
        printf(
    \"undo %\" MR_INTEGER_LENGTH_MODIFIER \"d\\n\",
    (MR_Integer)value);
        break;

    case MR_solve:
        printf(
    \"solve (soft commit) %\" MR_INTEGER_LENGTH_MODIFIER \"d\\n\",
    (MR_Integer)value);
        break;

    case MR_commit:
        printf(
    \"*** unexpected (hard) commit %\" MR_INTEGER_LENGTH_MODIFIER \"d\\n\",
    (MR_Integer)value);
        break;

    case MR_retry:
        printf(
    \"*** unexpected retry %\" MR_INTEGER_LENGTH_MODIFIER \"d\\n\",
    (MR_Integer)value);
        break;

    case MR_exception:
        printf(
    \"*** unexpected exception %\" MR_INTEGER_LENGTH_MODIFIER \"d\\n\",
    (MR_Integer)value);
        break;

    default:
        printf(\"*** Unexpected call to print_entry\");
    }
}
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
