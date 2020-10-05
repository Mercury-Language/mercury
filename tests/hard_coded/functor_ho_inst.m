%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Use a functor with a higher-order argument with an inst.
%

:- module functor_ho_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- import_module list.

main(!IO) :-
    Jobs = [job(j1), job(j2)],
    list.foldl(run, Jobs, !IO).

%---------------------------------------------------------------------------%

:- type job
    --->    job(pred(job_result::out, io::di, io::uo) is det).

:- type job_result
    --->    job_result_ok
    ;       job_result_failed.

:- pred run(job::in, io::di, io::uo) is det.

run(Job, !IO) :-
    Job = job(Pred),
    Pred(Result, !IO),
    io.write_line(Result, !IO).

:- pred j1(job_result::out, io::di, io::uo) is det.

j1(job_result_ok, !IO).

:- pred j2(job_result::out, io::di, io::uo) is det.

j2(job_result_failed, !IO).
