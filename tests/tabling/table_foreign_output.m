%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test checks whether we process correctly the memoization of predicates
% that return foreign types that do not fit into a word.

:- module table_foreign_output.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module assoc_list.
:- import_module list.
:- import_module pair.
:- import_module solutions.

:- pragma require_feature_set([memo]).

main(!IO) :-
    solutions(test([1 - "one", 2 - "two", 2 - "two", 1 - "one"]),
        Solns),
    solutions(test_memo([1 - "one", 2 - "two", 2 - "two", 1 - "one"]),
        MemoSolns),
    % solutions sorts foreign types as C pointers, so the order of the
    % solutions depends on memory layout. This means the order is not
    % predictable enough to let us print the solutions themselves.
    list.map(extract_str, Solns, Strs),
    list.map(extract_str, MemoSolns, MemoStrs),
    io.write_int(list.length(Strs), !IO),
    io.nl(!IO),
    io.write_int(list.length(MemoStrs), !IO),
    io.nl(!IO).

:- type foreign ---> foreign.
:- pragma foreign_type("C", foreign, "Foreign").

:- pragma foreign_decl("C",
"
typedef struct {
    MR_Integer  i;
    MR_String   s;
} Foreign;
").

:- pred test(assoc_list(int, string)::in, foreign::out) is nondet.

test(Pairs, F) :-
    list.member(N - S, Pairs),
    make_foreign(N, S, F).

:- pred test_memo(assoc_list(int, string)::in, foreign::out) is nondet.

test_memo(Pairs, F) :-
    list.member(N - S, Pairs),
    make_foreign_memo(N, S, F).

:- pred make_foreign(int::in, string::in, foreign::out) is det.

:- pragma foreign_proc("C",
    make_foreign(N::in, S::in, F::out),
    [will_not_call_mercury, promise_pure],
"
    Foreign *p;

    printf(""make_foreign(%"" MR_INTEGER_LENGTH_MODIFIER ""d, %s)\\n"", N, S);
    p = (Foreign *) malloc(sizeof(Foreign));
    if (p == NULL) {
        exit(1);
    }

    p->i = N;
    p->s = S;
    F = *p;
").

:- pred make_foreign_memo(int::in, string::in, foreign::out) is det.
:- pragma memo(make_foreign_memo/3).

:- pragma foreign_proc("C",
    make_foreign_memo(N::in, S::in, F::out),
    [will_not_call_mercury, promise_pure],
"
    Foreign *p;

    printf(""make_foreign_memo(%"" MR_INTEGER_LENGTH_MODIFIER ""d, %s)\\n"",
        N, S);
    p = (Foreign *) malloc(sizeof(Foreign));
    if (p == NULL) {
        exit(1);
    }

    p->i = N;
    p->s = S;
    F = *p;
").

:- pred extract_str(foreign::in, string::out) is det.

:- pragma foreign_proc("C",
    extract_str(F::in, S::out),
    [will_not_call_mercury, promise_pure],
"
    S = F.s;
").
