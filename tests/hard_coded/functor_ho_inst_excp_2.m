%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Same as functor_ho_inst_excp except the extra functor argument makes it a
% proper du functor rather than a notag functor.
%

:- module functor_ho_inst_excp_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module construct.
:- import_module exception.
:- import_module list.
:- import_module type_desc.
:- import_module univ.

main(!IO) :-
    Jobs = [bad_job],
    list.foldl(run, Jobs, !IO).

%---------------------------------------------------------------------------%

:- type job
    ---> job(int, pred(int::in, string::out) is det).

:- pred run(job::in, io::di, io::uo) is det.

run(Job, !IO) :-
    Job = job(_, Pred),
    Pred(5, Result),
    io.write_string("Printing result:\n", !IO),
    io.write_string(Result, !IO),
    io.write_string("\nEnd of result.\n", !IO).

:- pred semi_job(int::in, string::out) is semidet.

semi_job(1, "singular sensation").

:- func bad_job = job.

bad_job = Job :-
    Args = [univ(1), univ(semi_job)],
    ( if univ_to_type(construct(type_of(Job), 0, Args), Job0) then
        Job = Job0
    else
        throw("unable to create job")
    ).
