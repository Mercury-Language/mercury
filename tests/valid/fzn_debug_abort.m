%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This is a regression test for Mantis bug #44.
%
% rotd-2008-02-07 aborts with the following assertion failure
%
% Uncaught Mercury exception:
% Software Error: llds_out.m: Unexpected: stack var out of range
%
% when this program is compiled in a (decl)debug grade.
% (This test case is derived from r4597 of the G12 FlatZinc interpreter.)
%
%---------------------------------------------------------------------------%

:- module fzn_debug_abort.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module exception.

%---------------------------------------------------------------------------%

main(!IO) :-
    MaybeItems3 = no,
    SAT_Solver = msat,
    do_io_stage(flatzinc_program,
        evaluate_flatzinc_ast(flatzinc_sat_solver(SAT_Solver)),
        MaybeItems3, _MaybeItems4, !IO).

:- pred evaluate_flatzinc_ast(Solver::in, ast::in, ast::out,
    io::di, io::uo) is det <= flatzinc_solver(Solver, Var).

evaluate_flatzinc_ast(_, _, ast, !IO).

:- func flatzinc_program = string.

flatzinc_program = "flatzinc".

%---------------------------------------------------------------------------%

:- type ast ---> ast.

:- pred do_io_stage(string::in,
    pred(A, B, io, io)::in(pred(in, out, di, uo) is det),
    maybe(A)::in,
    maybe(B)::out,
    io::di, io::uo) is det.

do_io_stage(_, _, _, no, !IO).

%---------------------------------------------------------------------------%

:- typeclass flatzinc_solver(Solver, Var) <= (Solver -> Var) where [].

:- type flatzinc_sat_solver(S) ---> flatzinc_sat_solver(S).
:- type flatzinc_sat_var(L) ---> flatzinc_sat_var.

:- instance flatzinc_solver(flatzinc_sat_solver(S), flatzinc_sat_var(L))
    <= clausal_sat_solver(S, L) where [].

:- typeclass clausal_sat_solver(S, L) <= (S -> L) where [].

%---------------------------------------------------------------------------%

:- solver type msat_literal where representation is int.
:- type msat_solver ---> msat_solver.
:- func msat = msat_solver.

msat = msat_solver.

:- instance clausal_sat_solver(msat_solver, msat_literal) where [].

%---------------------------------------------------------------------------%
:- end_module fzn_debug_abort.
%---------------------------------------------------------------------------%
