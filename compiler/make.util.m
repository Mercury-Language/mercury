%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.util.m.
% Authors: stayl, wangp.
%
% Assorted predicates used to implement `mmc --make'.
%
%---------------------------------------------------------------------------%

:- module make.util.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.file_names.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%
%
% Remove file a file, deleting the cached timestamp.
% The removal is reported to the user if the given boolean option is set.
% In general the option given should be `--very-verbose' when making a
% `.clean' or `.realclean target', and `--verbose-make' when cleaning
% after an interrupted build.
%

    % Remove the target file and the corresponding timestamp file.
    %
:- pred remove_make_target_file(io.text_output_stream::in, globals::in,
    string::in, option::in, target_file::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Remove the target file and the corresponding timestamp file.
    %
:- pred remove_make_target_file_by_name(io.text_output_stream::in, globals::in,
    string::in, option::in, module_name::in, module_target_type::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % remove_module_file_for_make(ProgressStream, Globals, VerboseOption,
    %   ModuleName, Extension, !Info, !IO).
    %
:- pred remove_module_file_for_make(io.text_output_stream::in, globals::in,
    option::in, module_name::in, ext::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

:- pred remove_file_for_make(io.text_output_stream::in, globals::in,
    option::in, file_name::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func make_target_file_list(list(module_name), module_target_type) =
    list(target_file).

:- func make_dependency_list(list(module_name), module_target_type)
    = list(dependency_file).

%---------------------------------------------------------------------------%

:- pred target_is_grade_or_arch_dependent(module_target_type::in) is semidet.

%---------------------------------------------------------------------------%
%
% Debugging, progress, and error messages.
%

:- pred debug_make_msg(globals::in, pred(string)::in(pred(out) is det),
    string::out) is det.

%---------------------%

    % verbose_make_N_part_msg(Globals, Part1, ..., Msg):
    %
    % If `--verbose-make' is set, return a message consisting of the
    % given input strings (with spaces between them) and a newline.
    % Otherwise, return the empty string.
    %
:- pred verbose_make_one_part_msg(globals::in,
    string::in, string::out) is det.
:- pred verbose_make_two_part_msg(globals::in,
    string::in, string::in, string::out) is det.
:- pred verbose_make_three_part_msg(globals::in,
    string::in, string::in, string::in, string::out) is det.
:- pred verbose_make_four_part_msg(globals::in,
    string::in, string::in, string::in, string::in, string::out) is det.

    % option_set_N_part_msg(Globals, Option, Part1, ..., Msg):
    %
    % If the given option is set, return a message consisting of the
    % given input strings (with spaces between them) and a newline.
    % Otherwise, return the empty string.
    %
:- pred option_set_one_part_msg(globals::in, option::in,
    string::in, string::out) is det.
:- pred option_set_two_part_msg(globals::in, option::in,
    string::in, string::in, string::out) is det.
:- pred option_set_three_part_msg(globals::in, option::in,
    string::in, string::in, string::in, string::out) is det.
:- pred option_set_four_part_msg(globals::in, option::in,
    string::in, string::in, string::in, string::in, string::out) is det.

    % If `--verbose-make' is set, return a progress message saying that
    % the compiler is "Making <filename>". Otherwise, return the empty string.
    %
:- pred maybe_making_filename_msg(globals::in, file_name::in,
    string::out) is det.

    % If `--verbose-make' is set, return a progress message saying that
    % the compiler is reanalysing invalid/suboptimal modules.
    % Otherwise, return the empty string.
    %
:- pred maybe_reanalyse_modules_msg(globals::in, string::out) is det.

    % Return an error nessage saying there was an error making the given
    % target, which has the given filename.
    %
:- pred file_error_msg(file_name::in, string::out) is det.

    % If
    % - the warn_up_to_date option is set, and
    % - the given target was specified on the command line, and
    % - the given targe has not yet been deleted from that list of
    %   command line targets,
    % then return a warning about it already being up to date.
    % Otherwise, return the empty string. In both cases, this predicate
    % will delete ths given target from the list of command line targets,
    % which prevents duplicate "up to date" messages.
    %
    % XXX The list of command line targets is never used for any purpose
    % other than this last test. I (zs) am not sure that it is actually useful
    % for this purpose, because
    %
    % - the two places other than maybe_warn_up_to_date_target_msg that can
    %   delete targets from the list of command line targets, in
    %   make.module_target.m and make.program_target.m respectively,
    %   both do so only if the target is NOT up to date, and
    %
    % - it should not require this make_info field to prevent duplicate
    %   calls to this predicate if the target IS up to date.
    %
:- pred maybe_warn_up_to_date_target_msg(globals::in, top_target_file::in,
    file_name::in, make_info::in, make_info::out, string::out) is det.

    % Write a message "Made symlink/copy of <filename>"
    % if `--verbose-make' is set.
    %
:- pred maybe_symlink_or_copy_linked_target_msg(globals::in, file_name::in,
    string::out) is det.

%---------------------%
%
% All messages from the make-like functionality of "mmc --make" should be
% written out using a single call to io.write_string, and then a flush
% of the stream. This should allow messages from different processes
% during parallel builds to be interleaved *only* on message boundaries.
%
% XXX This is only really true for the "locked" variants, and even then,
% see the other XXX just below.
%

    % Write out a message from mmc --make, if the message is not empty.
    %
:- pred maybe_write_msg(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

    % Write out a message from mmc --make, if the message is not empty,
    % while locking stdout.
    %
    % XXX The current stream, or the specified stream, may be a stream
    % OTHER THAN stdout.
    %
:- pred maybe_write_msg_locked(io.text_output_stream::in, make_info::in,
    string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Timing.
%

:- pred get_real_milliseconds(int::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module make.build.
:- import_module make.file_names.
:- import_module make.timestamp.

:- import_module bool.
:- import_module io.file.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

remove_make_target_file(ProgressStream, Globals, From, VerboseOption, Target,
        !Info, !IO) :-
    Target = target_file(ModuleName, TargetType),
    remove_make_target_file_by_name(ProgressStream, Globals, From,
        VerboseOption, ModuleName, TargetType, !Info, !IO).

%---------------------%

remove_make_target_file_by_name(ProgressStream, Globals, From, VerboseOption,
        ModuleName, TargetType, !Info, !IO) :-
    module_target_to_file_name(Globals, From, TargetType,
        ModuleName, FileName, !IO),
    remove_file_for_make(ProgressStream, Globals, VerboseOption, FileName,
        !Info, !IO),
    ( if timestamp_extension(TargetType, TimestampExt) then
        remove_module_file_for_make(ProgressStream, Globals, VerboseOption,
            ModuleName, TimestampExt, !Info, !IO)
    else
        true
    ).

%---------------------%

remove_module_file_for_make(ProgressStream, Globals, VerboseOption,
        ModuleName, Ext, !Info, !IO) :-
    module_name_to_file_name(Globals, $pred, Ext, ModuleName, FileName),
    remove_file_for_make(ProgressStream, Globals, VerboseOption,
        FileName, !Info, !IO).

%---------------------%

remove_file_for_make(ProgressStream, Globals, VerboseOption, FileName,
        !Info, !IO) :-
    option_set_two_part_msg(Globals, VerboseOption,
        "Removing", FileName, RemovingMsg),
    maybe_write_msg(ProgressStream, RemovingMsg, !IO),
    io.file.remove_file_recursively(FileName, _, !IO),
    FileTimestamps0 = make_info_get_file_timestamps(!.Info),
    map.delete(FileName, FileTimestamps0, FileTimestamps),
    make_info_set_file_timestamps(FileTimestamps, !Info),
    % For simplicity, clear out all target file timestamps.
    make_info_set_target_file_timestamps(init_target_file_timestamps, !Info).

%---------------------------------------------------------------------------%

make_target_file_list(ModuleNames, TargetType) =
    list.map((func(ModuleName) = target_file(ModuleName, TargetType)),
        ModuleNames).

make_dependency_list(ModuleNames, TargetType) =
    list.map((func(Module) = dep_target(target_file(Module, TargetType))),
        ModuleNames).

%---------------------------------------------------------------------------%

target_is_grade_or_arch_dependent(Target) :-
    is_target_grade_or_arch_dependent(Target) = yes.

:- func is_target_grade_or_arch_dependent(module_target_type) = bool.

is_target_grade_or_arch_dependent(Target) = IsDependent :-
    (
        ( Target = module_target_source
        ; Target = module_target_errors
        ; Target = module_target_int0
        ; Target = module_target_int1
        ; Target = module_target_int2
        ; Target = module_target_int3
        ; Target = module_target_c_header(header_mh)
        ; Target = module_target_xml_doc
        ),
        IsDependent = no
    ;
        ( Target = module_target_opt
        ; Target = module_target_analysis_registry
        ; Target = module_target_track_flags
        ; Target = module_target_c_header(header_mih)
        ; Target = module_target_c_code
        ; Target = module_target_csharp_code
        ; Target = module_target_java_code
        ; Target = module_target_java_class_code
        ; Target = module_target_object_code(_)
        ; Target = module_target_foreign_object(_, _)
        ; Target = module_target_fact_table_object(_, _)
        ),
        IsDependent = yes
    ).

%---------------------------------------------------------------------------%
%
% Debugging, progress, and error messages.
%

debug_make_msg(Globals, MakeMsgPred, Msg) :-
    globals.lookup_bool_option(Globals, debug_make, DebugMake),
    (
        DebugMake = no,
        Msg = ""
    ;
        DebugMake = yes,
        MakeMsgPred(Msg)
    ).

%---------------------%

verbose_make_one_part_msg(Globals, Part1, Msg) :-
    option_set_one_part_msg(Globals, verbose_make, Part1, Msg).

verbose_make_two_part_msg(Globals, Part1, Part2, Msg) :-
    option_set_two_part_msg(Globals, verbose_make, Part1, Part2, Msg).

verbose_make_three_part_msg(Globals, Part1, Part2, Part3, Msg) :-
    option_set_three_part_msg(Globals, verbose_make, Part1, Part2, Part3, Msg).

verbose_make_four_part_msg(Globals, Part1, Part2, Part3, Part4, Msg) :-
    option_set_four_part_msg(Globals, verbose_make, Part1, Part2, Part3,
        Part4, Msg).

option_set_one_part_msg(Globals, Option, Part1, Msg) :-
    globals.lookup_bool_option(Globals, Option, OptionValue),
    (
        OptionValue = no,
        Msg = ""
    ;
        OptionValue = yes,
        string.format("%s\n", [s(Part1)], Msg)
    ).

option_set_two_part_msg(Globals, Option, Part1, Part2, Msg) :-
    globals.lookup_bool_option(Globals, Option, OptionValue),
    (
        OptionValue = no,
        Msg = ""
    ;
        OptionValue = yes,
        string.format("%s %s\n", [s(Part1), s(Part2)], Msg)
    ).

option_set_three_part_msg(Globals, Option, Part1, Part2, Part3, Msg) :-
    globals.lookup_bool_option(Globals, Option, OptionValue),
    (
        OptionValue = no,
        Msg = ""
    ;
        OptionValue = yes,
        string.format("%s %s %s\n", [s(Part1), s(Part2), s(Part3)], Msg)
    ).

option_set_four_part_msg(Globals, Option, Part1, Part2, Part3, Part4, Msg) :-
    globals.lookup_bool_option(Globals, Option, OptionValue),
    (
        OptionValue = no,
        Msg = ""
    ;
        OptionValue = yes,
        string.format("%s %s %s %s\n",
            [s(Part1), s(Part2), s(Part3), s(Part4)], Msg)
    ).

%---------------------%

maybe_making_filename_msg(Globals, FileName, Msg) :-
    globals.lookup_bool_option(Globals, verbose_make, VerboseMake),
    (
        VerboseMake = no,
        Msg = ""
    ;
        VerboseMake = yes,
        string.format("Making %s\n", [s(FileName)], Msg)
    ).

maybe_reanalyse_modules_msg(Globals, Msg) :-
    globals.lookup_bool_option(Globals, verbose_make, VerboseMake),
    (
        VerboseMake = no,
        Msg = ""
    ;
        VerboseMake = yes,
        Msg = "Reanalysing invalid/suboptimal modules\n"
    ).

file_error_msg(FileName, Msg) :-
    string.format("** Error making `%s'.\n", [s(FileName)], Msg).
    % with_locked_stdout(Info, io.write_string(Msg), !IO).

maybe_warn_up_to_date_target_msg(Globals, Target, FileName, !Info, Msg) :-
    globals.lookup_bool_option(Globals, warn_up_to_date, Warn),
    CmdLineTargets0 = make_info_get_command_line_targets(!.Info),
    (
        Warn = yes,
        ( if set.member(Target, CmdLineTargets0) then
            string.format("** Nothing to be done for `%s'.\n", [s(FileName)],
                Msg)
        else
            Msg = ""
        )
    ;
        Warn = no,
        Msg = ""
    ),
    set.delete(Target, CmdLineTargets0, CmdLineTargets),
    make_info_set_command_line_targets(CmdLineTargets, !Info).

maybe_symlink_or_copy_linked_target_msg(Globals, FileName, Msg) :-
    globals.lookup_bool_option(Globals, verbose_make, VerboseMake),
    (
        VerboseMake = no,
        Msg = ""
    ;
        VerboseMake = yes,
        string.format("Made symlink/copy of %s\n", [s(FileName)], Msg)
    ).

%---------------------%

maybe_write_msg(OutStream, Msg, !IO) :-    
    ( if Msg = "" then
        true
    else
        io.write_string(OutStream, Msg, !IO),
        io.flush_output(OutStream, !IO)
    ).

maybe_write_msg_locked(OutStream, Info, Msg, !IO) :-    
    ( if Msg = "" then
        true
    else
        MaybeLock = make_info_get_maybe_stdout_lock(Info),
        (
            MaybeLock = yes(Lock),
            lock_stdout(Lock, !IO),
            io.write_string(OutStream, Msg, !IO),
            io.flush_output(OutStream, !IO),
            unlock_stdout(Lock, !IO)
        ;
            MaybeLock = no,
            io.write_string(OutStream, Msg, !IO),
            io.flush_output(OutStream, !IO)
        )
    ).

%---------------------------------------------------------------------------%
%
% Timing.
%

:- pragma foreign_proc("C",
    get_real_milliseconds(Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Time = MR_get_real_milliseconds();
").

:- pragma foreign_proc("C#",
    get_real_milliseconds(Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Time = System.Environment.TickCount;
").

:- pragma foreign_proc("Java",
    get_real_milliseconds(Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    // The loss of precision is acceptable for mmc --make.
    Time = (int) System.currentTimeMillis();
").

get_real_milliseconds(_, _, _) :-
    sorry($file, $pred).

%---------------------------------------------------------------------------%
:- end_module make.util.
%---------------------------------------------------------------------------%
