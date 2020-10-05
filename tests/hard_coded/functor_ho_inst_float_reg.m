%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test a higher-order argument with an inst with the float_regs pass.
%

:- module functor_ho_inst_float_reg.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

main(!IO) :-
    JobsGeneric = [job_generic(j1)],
    list.foldl(run_generic, JobsGeneric, !IO),

    JobsFloat = [job_float(j1)],
    list.foldl(run_float, JobsFloat, !IO).

%---------------------------------------------------------------------------%

:- type job_generic(T)
    --->    job_generic(pred(T::out, io::di, io::uo) is det).

:- type job_float
    --->    job_float(pred(float::out, io::di, io::uo) is det).

:- pred run_generic(job_generic(T)::in, io::di, io::uo) is det.

run_generic(Job, !IO) :-
    Job = job_generic(Pred),
    Pred(Result, !IO),
    io.write_line(Result, !IO).

:- pred run_float(job_float::in, io::di, io::uo) is det.

run_float(Job, !IO) :-
    Job = job_float(Pred),
    Pred(Result, !IO),
    io.write_line(Result, !IO).

:- pred j1(float::out, io::di, io::uo) is det.

j1(3.141529, !IO).
