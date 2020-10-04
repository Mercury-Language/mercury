%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Author: Mark Brown <mark@csse.unimelb.edu.au>.
%
% Simulate nogood pruning using the backjump module.
%
%---------------------------------------------------------------------------%

:- module backjump_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module backjump.
:- import_module list.
:- import_module solutions.
:- import_module string.

main(!IO) :-
    solutions(problem, Sols),
    (
        Sols = [],
        io.write_string("No solutions!\n", !IO)
    ;
        Sols = [_ | _],
        io.write_string("Solutions:\n", !IO),
        io.write_list(Sols, ",\n", io.write, !IO),
        io.nl(!IO)
    ).

:- pred problem({int, int, int}::out) is nondet.

problem({A, B, C}) :-
    promise_pure (
        impure label("A", [1, 2], A, PA),
        impure label("B", [1, 2], B, PB),
        impure label("C", [1, 2, 3], C, PC),
        impure check(A, B, C, PA, PB, PC)
    ).

:- impure pred label(string::in, list(int)::in, int::out, choice_id::out)
    is nondet.

label(Name, [N | _], N, P) :-
    impure get_choice_id(P),
    trace [io(!IO)] (
        io.format("label %s = %d, (%d)\n", [s(Name), i(N), i(to_int(P))], !IO),
        true
    ).
label(Name, [_ | Ns], N, P) :-
    impure label(Name, Ns, N, P).

:- impure pred check(int::in, int::in, int::in, choice_id::in, choice_id::in,
    choice_id::in) is semidet.

check(A, B, C, PA, PB, PC) :-
    ( if is_nogood(A, B, C, PA, PB, PC, P) then
        trace [io(!IO)] (
            io.format("backjump (%d)\n", [i(to_int(P))], !IO)
        ),
        impure backjump(P)
    else
        is_solution(A, B, C),
        trace [io(!IO)] (
            io.format("solution %d, %d, %d\n", [i(A), i(B), i(C)], !IO)
        )
    ).

:- pred is_nogood(int::in, int::in, int::in, choice_id::in, choice_id::in,
    choice_id::in, choice_id::out) is semidet.

is_nogood(1, 1, 2, _, _, P, P).
is_nogood(1, 2, 1, P, _, _, P).
is_nogood(2, 1, 2, _, P, _, P).

:- pred is_solution(int::in, int::in, int::in) is semidet.

is_solution(1, 1, 3).
is_solution(2, 1, 1).
is_solution(2, 2, 2).
