%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests the readability of the error message we generate
% when a value whose type is higher order has no higher order inst info.
% The message should be more useful than the message we generated
% before the addition of this test case to the test suite, which was:
%
% no_ho_inst.m:044: In clause for `run_loop(in, in, out, di, uo)':
% no_ho_inst.m:044:   in argument 1 (i.e. the predicate term) of higher-order
% no_ho_inst.m:044:   predicate call:
% no_ho_inst.m:044:   mode error: context requires a predicate of arity 4, but
% no_ho_inst.m:044:   the inst of AppHandler is not a higher order inst.
% For more information, recompile with `-E'.
%
% The origin of this test code is an email to the mercury-users list
% by Sean Charles.
%

:- module no_ho_inst.

:- interface.
:- import_module io.

:- type callback == (pred(world, world, io, io)).
:- inst callback == (pred(in, out, di, uo) is det).

:- type world
    --->    world_1
    ;       world_2.

:- pred run_loop(callback::in, world::in, world::out, io::di, io::uo) is det.

:- implementation.

run_loop(AppHandler, !W, !IO) :-
    (
        !.W = world_1,
        io.write_string("stopped\n", !IO)
    ;
        !.W = world_2,
        io.write_string("before invoking AppHandler\n", !IO),
        AppHandler(!W, !IO),
        io.write_string("after invoking AppHandler\n", !IO),
        run_loop(AppHandler, !W, !IO)
    ).
