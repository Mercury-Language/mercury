%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Try to use a functor with a higher-order argument with a term that has
% lost its higher-order inst and become ground.
%

:- module functor_ho_inst_bad_3.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type job
    ---> job(pred(job_result::out, io::di, io::uo) is det).

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

main(!IO) :-
    get(J),
    Job = job(J),
    run(Job, !IO).

:- pred get(pred(job_result, io, io)::out) is det.

get(j1).
