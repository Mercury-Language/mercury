%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module mode_error_arg_number_ho.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    % Since we report only one mode error per conjunction,
    % we need four separate conjunctions for four error messages.
    % We put each into its own predicate.
    test1(A),
    test2(B),
    test3(C),
    test4(D),
    io.format("A = %d, B = %d, C = %d, D = %d\n",
        [i(A), i(B), i(C), i(D)], !IO).

:- pred test1(int::out) is det.

test1(A) :-
    P = add_pred,
    _ShutUp = [A1],     % shut up singleton variable warnings
    call(P, 42, A1, A).

:- pred test2(int::out) is det.

test2(B) :-
    P = add_pred,
    _ShutUp = [B1],     % shut up singleton variable warnings
    P(42, B1, B).

:- pred test3(int::out) is det.

test3(C) :-
    F = add_func,
    _ShutUp = [C1],     % shut up singleton variable warnings
    C = apply(F, 42, C1).

:- pred test4(int::out) is det.

test4(D) :-
    F = add_func,
    _ShutUp = [D1],     % shut up singleton variable warnings
    D = F(42, D1).

%---------------------------------------------------------------------------%

:- pred add_pred(int::in, int::in, int::out) is det.

add_pred(A, B, C) :-
    C = A + B.

:- func add_func(int, int) = int.

add_func(A, B) = C :-
    C = A + B.
