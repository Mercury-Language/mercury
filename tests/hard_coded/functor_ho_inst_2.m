%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Same as functor_ho_inst except the extra functor argument makes it a
% proper du functor rather than a notag functor.
%

:- module functor_ho_inst_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- import_module list.

main(!IO) :-
    Jobs = [job(1, j1), job(2, j2)],
    list.foldl(run, Jobs, !IO).

%---------------------------------------------------------------------------%

:- type job
    --->    job(int, pred(job_result::out, io::di, io::uo) is det).

:- type job_result
    --->    job_result_ok
    ;       job_result_failed.

:- pred run(job::in, io::di, io::uo) is det.

run(Job, !IO) :-
    Job = job(_, Pred),
    Pred(Result, !IO),
    io.write_line(Result, !IO).

:- pred j1(job_result::out, io::di, io::uo) is det.

j1(job_result_ok, !IO).

:- pred j2(job_result::out, io::di, io::uo) is det.

j2(job_result_failed, !IO).
