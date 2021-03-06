%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007-2008, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mdprof_procrep.m.
% Author: zs.
%
% A program to test the reading of program representations.
%
%---------------------------------------------------------------------------%

:- module mdprof_procrep.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module program_representation_utils.

:- import_module char.
:- import_module cord.
:- import_module getopt.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.progname_base("mdprof_procrep", ProgName, !IO),
    io.command_line_arguments(Args0, !IO),
    getopt.process_options(option_ops_multi(short, long, defaults),
        Args0, Args, MaybeOptions),
    io.stderr_stream(StdErr, !IO),
    (
        MaybeOptions = error(Error),
        Msg = option_error_to_string(Error),
        io.format(StdErr, "%s: error parsing options: %s\n",
            [s(ProgName), s(Msg)], !IO),
        io.write_string(StdErr, help_message(ProgName), !IO),
        io.set_exit_status(1, !IO)
    ;
        MaybeOptions = ok(Options),
        lookup_string_option(Options, filename, FileName),
        read_prog_rep_file(FileName, ProgRepRes, !IO),
        (
            ProgRepRes = error(Error),
            io.error_message(Error, Msg),
            io.format(StdErr, "%s: %s\n", [s(ProgName), s(Msg)], !IO)
        ;
            ProgRepRes = ok(ProgRep),
            ProgRep = prog_rep(ModuleReps),
            (
                Args = [],
                MaybeModules = no
            ;
                Args = [_ | _],
                MaybeModules = yes(Args)
            ),
            io.stdout_stream(StdOut, !IO),
            print_selected_modules(StdOut, ModuleReps, MaybeModules, !IO)
        )
    ).

:- func help_message(string) = string.

help_message(ProgName) =
    string.format(
        "Usage: %s [-f procrepfilename] [module1, module2, ...]\n",
        [s(ProgName)]).

:- type option
    --->    filename
    ;       dummy.      % This is needed to shut up a warning about defaults
                        % being det instead of multi.

:- pred short(char::in, option::out) is semidet.

short('f', filename).

:- pred long(string::in, option::out) is semidet.

long("file", filename).

:- pred defaults(option::out, option_data::out) is multi.

defaults(filename, string("Deep.procrep")).
defaults(dummy, string("dummy")).

%---------------------------------------------------------------------------%

:- pred print_selected_modules(io.text_output_stream::in, module_map::in,
    maybe(list(string))::in, io::di, io::uo) is det.

print_selected_modules(OutputStream, ModuleReps, MaybeModules, !IO) :-
    (
        MaybeModules = no,
        map.foldl(
            ( pred(_::in, ModuleRep::in, IO0::di, IO::uo) is det :-
                print_module(OutputStream, ModuleRep, IO0, IO)
            ), ModuleReps, !IO)
    ;
        MaybeModules = yes(Modules),
        list.foldl(
            ( pred(ModuleName::in, IO0::di, IO::uo) is det :-
                ( if map.search(ModuleReps, ModuleName, ModuleRep) then
                    print_module(OutputStream, ModuleRep, IO0, IO)
                else
                    IO = IO0
                )
            ), Modules, !IO)
        ).

:- pred print_module(io.text_output_stream::in, module_rep::in,
    io::di, io::uo) is det.

print_module(OutputStream, ModuleRep, !IO) :-
    print_module_to_strings(ModuleRep, Strings),
    io.write_string(OutputStream, string.append_list(cord.list(Strings)), !IO).

%---------------------------------------------------------------------------%
:- end_module mdprof_procrep.
%---------------------------------------------------------------------------%
