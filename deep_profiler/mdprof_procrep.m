%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
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
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.rtti_access.
:- import_module program_representation_utils.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
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

:- pred print_selected_modules(list(module_rep)::in, maybe(list(string))::in,
    io::di, io::uo) is det.

print_selected_modules([], _, !IO).
print_selected_modules([ModuleRep | ModuleReps], MaybeModules, !IO) :-
    ModuleRep = module_rep(ModuleName, _StringTable, _ProcReps),
    (
        MaybeModules = no,
        print_module(ModuleRep, !IO)
    ;
        MaybeModules = yes(Modules),
        ( list.member(ModuleName, Modules) ->
            print_module(ModuleRep, !IO)
        ;
            true
        )
    ),
    print_selected_modules(ModuleReps, MaybeModules, !IO).

:- pred print_module(module_rep::in, io::di, io::uo) is det.

print_module(ModuleRep, !IO) :-
    print_module_to_strings(ModuleRep, Strings),
    io.write_string(string.append_list(list(Strings)), !IO).

%-----------------------------------------------------------------------------%
:- end_module mdprof_procrep.
%-----------------------------------------------------------------------------%
