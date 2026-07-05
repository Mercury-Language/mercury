%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.int_opt.m.
%
% Build interface and optimization files.
%
%---------------------------------------------------------------------------%

:- module make.int_opt.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- type build_what
    --->    build_int3s
    ;       build_int3s_int0s
    ;       build_all_ints
    ;       build_all_ints_opts.

:- pred build_int_opt_files(io.text_output_stream::in, globals::in,
    build_what::in, list(module_name)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module make.build.
:- import_module make.order.
:- import_module make.util.

:- import_module bool.

%---------------------------------------------------------------------------%

build_int_opt_files(ProgressStream, Globals, BuildWhat, AllModules0,
        Succeeded, !Info, !IO) :-
    get_nonnested_and_parent_modules(ProgressStream, Globals, AllModules0,
        NonnestedModules, ParentModules, !Info, !IO),

    Int3s = make_target_id_list(NonnestedModules, module_target_int3),
    Int0s = make_target_id_list(ParentModules,    module_target_int0),
    Int1s = make_target_id_list(NonnestedModules, module_target_int1),
    globals.get_any_intermod(Globals, AnyIntermod),
    (
        AnyIntermod = yes,
        Opts = make_target_id_list(NonnestedModules, module_target_opt)
    ;
        AnyIntermod = no,
        Opts = []
    ),
    KeepGoing = make_info_get_keep_going(!.Info),
    % .int0 files need to be made before building .int files in parallel,
    % otherwise two processes may try to build the same .int0 file.
    foldl2_make_module_targets_maybe_parallel(KeepGoing, [],
        ProgressStream, Globals, Int3s, Succeeded0, !Info, !IO),
    ( if
        ( Succeeded0 = did_not_succeed
        ; BuildWhat = build_int3s
        )
    then
        Succeeded = Succeeded0
    else
        foldl2_make_module_targets(KeepGoing, [],
            ProgressStream, Globals, Int0s, Succeeded1, !Info, !IO),
        ( if
            ( Succeeded1 = did_not_succeed
            ; BuildWhat = build_int3s_int0s
            )
        then
            Succeeded = Succeeded1
        else
            foldl2_make_module_targets_maybe_parallel(KeepGoing, [],
                ProgressStream, Globals, Int1s, Succeeded2, !Info, !IO),
            ( if
                ( Succeeded2 = did_not_succeed
                ; BuildWhat = build_all_ints
                )
            then
                Succeeded = Succeeded2
            else
                foldl2_make_module_targets_maybe_parallel(KeepGoing, [],
                    ProgressStream, Globals, Opts, Succeeded, !Info, !IO)
            )
        )
    ).

%---------------------------------------------------------------------------%
:- end_module make.int_opt.
%---------------------------------------------------------------------------%
