%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007-2008, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mdprof_procrep.m.
% Author: zs.
%
% A program to test the reading of program representations.
%
%-----------------------------------------------------------------------------%

:- module mdprof_procrep.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module program_representation_utils.

:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        MaybeModules = no
    ;
        Args = [_ | _],
        MaybeModules = yes(Args)
    ),
    read_prog_rep_file("Deep.procrep", ProgRepRes, !IO),
    (
        ProgRepRes = ok(ProgRep),
        ProgRep = prog_rep(ModuleReps),
        print_selected_modules(ModuleReps, MaybeModules, !IO)
    ;
        ProgRepRes = error(Error),
        io.error_message(Error, Msg),
        io.format("mdprof_procrep: %s\n", [s(Msg)], !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred print_selected_modules(module_map::in, maybe(list(string))::in,
    io::di, io::uo) is det.

print_selected_modules(ModuleReps, MaybeModules, !IO) :-
    (
        MaybeModules = no,
        map.foldl((pred(_::in, ModuleRep::in, IO0::di, IO::uo) is det :-
                print_module(ModuleRep, IO0, IO)
            ), ModuleReps, !IO)
    ;
        MaybeModules = yes(Modules),
        list.foldl((pred(ModuleName::in, IO0::di, IO::uo) is det :-
            ( map.search(ModuleReps, ModuleName, ModuleRep) ->
                print_module(ModuleRep, IO0, IO)
            ;
                IO = IO0
            )), Modules, !IO)
    ).

:- pred print_module(module_rep::in, io::di, io::uo) is det.

print_module(ModuleRep, !IO) :-
    print_module_to_strings(ModuleRep, Strings),
    io.write_string(string.append_list(list(Strings)), !IO).

%-----------------------------------------------------------------------------%
:- end_module mdprof_procrep.
%-----------------------------------------------------------------------------%
