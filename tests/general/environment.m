%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Short series of tests for io.get_environment_var and
% io.set_environment_var.
%
% Author: bromage
%
% The .exp file is for backends where io.set_environment_var/4 is supported.
% The .exp2 file is for Java backend where io.set_environment_var/4 is
% *not* supported.
%
%---------------------------------------------------------------------------%

:- module environment.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module io.environment.
:- import_module list.
:- import_module maybe.
:- import_module string.

main(!IO) :-
    % PATH should be set on all Unix systems but may differ
    % on the different machines that generate .exp and .out files
    test("PATH", yes, no, !IO),

    % This one probably isn't. :-)
    test("SHOULD_NOT_BE_SET", no, yes, !IO),

    ( try [io(!IO)] (
        % So set it...
        io.environment.set_environment_var("SHOULD_NOT_BE_SET",
            "Hello World!", !IO)
    )
    then
        % Did that work?
        environment.test("SHOULD_NOT_BE_SET", yes, yes, !IO)
    catch_any E ->
        io.write_string("Cannot modify environment on this platform:\n", !IO),
        io.write_line(E, !IO)
    ).

:- pred environment.test(string::in, bool::in, bool::in,
    io::di, io::uo) is det.

test(Var, ShouldBeSet, ShouldBePrinted, !IO) :-
    io.environment.get_environment_var(Var, MaybeValue, !IO),
    io.write_strings(["Variable \"", Var, "\" is set "], !IO),
    (
        MaybeValue = yes(Value),
        (
            ShouldBePrinted = yes,
            io.write_strings(["to \"", Value, "\" "], !IO)
        ;
            ShouldBePrinted = no
        ),
        Ok = ShouldBeSet
    ;
        MaybeValue = no,
        io.write_string("not set ", !IO),
        bool.not(ShouldBeSet, Ok)
    ),
    (
        Ok = yes,
        io.write_string("(passed)\n", !IO)
    ;
        Ok = no,
        io.write_string("(failed)\n", !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module environment.
%---------------------------------------------------------------------------%
