%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Try to use a functor with a higher-order argument with an inst that
% doesn't match the supplied one.
%

:- module functor_ho_inst_bad_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- type job
    --->    job(pred(job_result::in, io::di, io::uo) is det).

:- type job_result
    --->    job_result_ok
    ;       job_result_failed.

:- pred run(job::in, io::di, io::uo) is det.

run(Job, !IO) :-
    Job = job(Pred),
    Pred(Result, !IO),
    io.write_line(Result, !IO).

:- pred jbad(job_result::in, io::di, io::uo) is det.

jbad(_, !IO).

main(!IO) :-
    Jobs = [job(jbad)],
    list.foldl(run, Jobs, !IO).
