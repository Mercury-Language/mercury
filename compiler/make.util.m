%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: make.util.m.
% Main author: stayl.
%
% Assorted predicates used to implement `mmc --make'.
%
%-----------------------------------------------------------------------------%

:- module make.util.
:- interface.

%-----------------------------------------------------------------------------%
%
% Versions of foldl which stop if the supplied predicate returns `no'
% for any element of the list.
%

    % foldl2_pred_with_status(T, Succeeded, !Info).
    %
:- type foldl2_pred_with_status(T, Info, IO) ==
    pred(T, bool, Info, Info, IO, IO).
:- inst foldl2_pred_with_status == (pred(in, out, in, out, di, uo) is det).

    % foldl2_maybe_stop_at_error(KeepGoing, P, List, Succeeded, !Info).
    %
:- pred foldl2_maybe_stop_at_error(bool::in,
    foldl2_pred_with_status(T, Info, IO)::in(foldl2_pred_with_status),
    list(T)::in, bool::out, Info::in, Info::out, IO::di, IO::uo) is det.

    % foldl3_pred_with_status(T, Succeeded, !Acc, !Info).
    %
:- type foldl3_pred_with_status(T, Acc, Info, IO) ==
    pred(T, bool, Acc, Acc, Info, Info, IO, IO).
:- inst foldl3_pred_with_status ==
    (pred(in, out, in, out, in, out, di, uo) is det).

    % foldl3_maybe_stop_at_error(KeepGoing, P, List, Succeeded, !Acc,
    %   !Info).
    %
:- pred foldl3_maybe_stop_at_error(bool::in,
    foldl3_pred_with_status(T, Acc, Info, IO)::in(foldl3_pred_with_status),
    list(T)::in, bool::out, Acc::in, Acc::out, Info::in, Info::out,
    IO::di, IO::uo) is det.

%-----------------------------------------------------------------------------%

:- type build(T, Info1, Info2) == pred(T, bool, Info1, Info2, io, io).
:- type build(T, Info) == build(T, Info, Info).
:- type build(T) == build(T, make_info).
:- inst build == (pred(in, out, in, out, di, uo) is det).

    % build_with_module_options(ModuleName, ExtraArgs, Builder, Succeeded,
    %   !Info).
    %
    % Perform the given closure after updating the option_table in the globals
    % in the io.state to contain the module-specific options for the specified
    % module and the extra options given in the ExtraArgs.
    % Adds `--invoked-by-mmc-make' and `--use-subdirs' to the option list.
    % The old option table will be restored afterwards.
    %
:- pred build_with_module_options(module_name::in,
    list(string)::in, build(list(string))::in(build), bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % build_with_module_options_args(ModuleName, OptionsVariables,
    %   OptionArgs, ExtraArgs, Builder, Succeeded, !Info).
    %
    % Perform the given closure after updating the option_table in the globals
    % in the io.state to contain the module-specific options for the specified
    % module and the extra options given in ExtraArgs and OptionArgs.
    % Does not add `--invoked-by-mmc-make' and `--use-subdirs' to the
    % option list. The old option table will be restored afterwards.
    %
:- pred build_with_module_options_args(module_name::in, options_variables::in,
    list(string)::in, list(string)::in,
    build(list(string), Info1, Info2)::in(build),
    bool::out, Info1::in, maybe(Info2)::out, io::di, io::uo) is det.

    % Perform the given closure with an output stream created
    % to append to the error file for the given module.
    %
:- pred build_with_output_redirect(module_name::in,
    build(io.output_stream)::in(build), bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Produce an output stream which writes to the error file
    % for the given module.
    %
:- pred redirect_output(module_name::in, maybe(io.output_stream)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Close the module error output stream.
    %
:- pred unredirect_output(module_name::in, io.output_stream::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type build2(T, U) == pred(T, U, bool, make_info, make_info, io, io).
:- inst build2 == (pred(in, in, out, in, out, di, uo) is det).

:- pred build_with_module_options_and_output_redirect(module_name::in,
    list(string)::in, build2(list(string), io.output_stream)::in(build2),
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Timestamp handling
%

    % Find the timestamp updated when a target is produced.
    %
:- pred get_timestamp_file_timestamp(target_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % Find the timestamp for the given dependency file.
    %
:- pred get_dependency_timestamp(dependency_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % get_target_timestamp(Search, TargetFile, Timestamp)
    %
    % Find the timestamp for the given target file.
    % `Search' should be `yes' if the file could be part of an
    % installed library.
    %
:- pred get_target_timestamp(bool::in, target_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % get_file_name(Search, TargetFile, FileName).
    %
    % Compute a file name for the given target file.
    % `Search' should be `yes' if the file could be part of an
    % installed library.
    %
:- pred get_file_name(bool::in, target_file::in, file_name::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Find the timestamp of the first file matching the given
    % file name in one of the given directories.
    %
:- pred get_file_timestamp(list(dir_name)::in, file_name::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % Return the oldest of the timestamps if both are of the form
    % `ok(Timestamp)', returning `error(Error)' otherwise.
    %
:- func find_oldest_timestamp(maybe_error(timestamp),
    maybe_error(timestamp)) = maybe_error(timestamp).

%-----------------------------------------------------------------------------%
%
% Remove file a file, deleting the cached timestamp
%

    % Remove the target file and the corresponding timestamp file.
    %
:- pred make_remove_target_file(target_file::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % Remove the target file and the corresponding timestamp file.
    %
:- pred make_remove_target_file(module_name::in, module_target_type::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % make_remove_file(ModuleName, Extension, !Info).
    %
:- pred make_remove_file(module_name::in, string::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

:- pred make_remove_file(file_name::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- func make_target_list(list(K), V) = assoc_list(K, V).

:- func make_target_file_list(list(module_name), module_target_type) = 
    list(target_file).

:- func make_dependency_list(list(module_name), module_target_type)
    = list(dependency_file).

:- func target_extension(globals, module_target_type) = maybe(string).
:- mode target_extension(in, in) = out is det.
:- mode target_extension(in, out) = in(bound(yes(ground))) is nondet.

:- pred linked_target_file_name(module_name::in, linked_target_type::in,
    file_name::out, io::di, io::uo) is det.

    % Find the extension for the timestamp file for the
    % given target type, if one exists.
    %
:- func timestamp_extension(globals, module_target_type) = string is semidet.

:- pred target_is_grade_or_arch_dependent(module_target_type::in) is semidet.

%-----------------------------------------------------------------------------%
%
% Debugging, verbose and error messages
%

    % Apply the given predicate if `--debug-make' is set.
    %
:- pred debug_msg(pred(io, io)::(pred(di, uo) is det), io::di, io::uo) is det.

    % Apply the given predicate if `--verbose-make' is set.
    %
:- pred verbose_msg(pred(io, io)::(pred(di, uo) is det),
    io::di, io::uo) is det.

    % Write a debugging message relating to a given target file.
    %
:- pred debug_file_msg(target_file::in, string::in, io::di, io::uo) is det.

:- pred write_dependency_file(dependency_file::in, io::di, io::uo) is det.

:- pred write_target_file(target_file::in, io::di, io::uo) is det.

    % Write a message "Making <filename>" if `--verbose-make' is set.
    %
:- pred maybe_make_linked_target_message(file_name::in, io::di, io::uo) is det.

    % Write a message "Making <filename>" if `--verbose-make' is set.
    %
:- pred maybe_make_target_message(target_file::in, io::di, io::uo) is det.

:- pred maybe_make_target_message_to_stream(io.output_stream::in,
    target_file::in, io::di, io::uo) is det.

    % Write a message "Reanalysing invalid/suboptimal modules" if
    % `--verbose-make' is set.
    %
:- pred maybe_reanalyse_modules_message(io::di, io::uo) is det.

    % Write a message "** Error making <filename>".
    %
:- pred target_file_error(target_file::in, io::di, io::uo) is det.

    % Write a message "** Error making <filename>".
    %
:- pred file_error(file_name::in, io::di, io::uo) is det.

    % If the given target was specified on the command line, warn that it
    % was already up to date.
    %
:- pred maybe_warn_up_to_date_target(pair(module_name, target_type)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Write a message "Making symlink/copy of <filename>" if
    % `--verbose-message' is set.
    %
:- pred maybe_symlink_or_copy_linked_target_message(
    pair(module_name, target_type)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module libs.compiler_util.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

%-----------------------------------------------------------------------------%

foldl2_maybe_stop_at_error(KeepGoing, MakeTarget, Targets, Success,
        !Info, !IO) :-
    foldl2_maybe_stop_at_error_2(KeepGoing, MakeTarget, Targets, yes, Success,
        !Info, !IO).

:- pred foldl2_maybe_stop_at_error_2(bool::in,
    foldl2_pred_with_status(T, Info, IO)::in(foldl2_pred_with_status),
    list(T)::in, bool::in, bool::out, Info::in, Info::out,
    IO::di, IO::uo) is det.

foldl2_maybe_stop_at_error_2(_KeepGoing, _P, [], !Success, !Info, !IO).
foldl2_maybe_stop_at_error_2(KeepGoing, P, [T | Ts], !Success, !Info, !IO) :-
    P(T, NewSuccess, !Info, !IO),
    (
        ( NewSuccess = yes
        ; KeepGoing = yes
        )
    ->
        !:Success = !.Success `and` NewSuccess,
        foldl2_maybe_stop_at_error_2(KeepGoing, P, Ts, !Success, !Info, !IO)
    ;
        !:Success = no
    ).

foldl3_maybe_stop_at_error(KeepGoing, P, Ts, Success, !Acc, !Info, !IO) :-
    foldl3_maybe_stop_at_error_2(KeepGoing, P, Ts, yes, Success,
        !Acc, !Info, !IO).

:- pred foldl3_maybe_stop_at_error_2(bool::in,
    foldl3_pred_with_status(T, Acc, Info, IO)::in(foldl3_pred_with_status),
    list(T)::in, bool::in, bool::out, Acc::in, Acc::out,
    Info::in, Info::out, IO::di, IO::uo) is det.

foldl3_maybe_stop_at_error_2(_KeepGoing, _P, [], !Success, !Acc, !Info, !IO).
foldl3_maybe_stop_at_error_2(KeepGoing, P, [T | Ts],
        !Success, !Acc, !Info, !IO) :-
    P(T, NewSuccess, !Acc, !Info, !IO),
    (
        ( NewSuccess = yes
        ; KeepGoing = yes
        )
    ->
        !:Success = !.Success `and` NewSuccess,
        foldl3_maybe_stop_at_error_2(KeepGoing, P, Ts, !Success, !Acc,
            !Info, !IO)
    ;
        !:Success = no
    ).

%-----------------------------------------------------------------------------%

build_with_module_options_and_output_redirect(ModuleName, ExtraOptions,
        Build, Succeeded, !Info, !IO) :-
    build_with_module_options(ModuleName, ExtraOptions,
        build_with_module_options_and_output_redirect_2(ModuleName, Build),
        Succeeded, !Info, !IO).

:- pred build_with_module_options_and_output_redirect_2(module_name::in,
    build2(list(string), io.output_stream)::in(build2), list(string)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

build_with_module_options_and_output_redirect_2(ModuleName, Build, AllOptions,
        Succeeded, !Info, !IO) :-
    build_with_output_redirect(ModuleName,
        build_with_module_options_and_output_redirect_3(AllOptions, Build),
        Succeeded, !Info, !IO).

:- pred build_with_module_options_and_output_redirect_3(list(string)::in,
    build2(list(string), io.output_stream)::in(build2),
    io.output_stream::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_with_module_options_and_output_redirect_3(AllOptions, Build,
        ErrorStream, Succeeded, !Info, !IO) :-
    Build(AllOptions, ErrorStream, Succeeded, !Info, !IO).

build_with_output_redirect(ModuleName, Build, Succeeded, !Info, !IO) :-
    redirect_output(ModuleName, RedirectResult, !Info, !IO),
    (
        RedirectResult = no,
        Succeeded = no
    ;
        RedirectResult = yes(ErrorStream),
        Build(ErrorStream, Succeeded, !Info, !IO),
        unredirect_output(ModuleName, ErrorStream, !Info, !IO)
    ).

build_with_module_options(ModuleName, ExtraOptions, Build, Succeeded,
        !Info, !IO) :-
    build_with_module_options_args_invoked(yes, ModuleName,
        !.Info ^ options_variables, !.Info ^ option_args, ExtraOptions, Build,
        Succeeded, !.Info, MaybeInfo, !IO),
    (
        MaybeInfo = yes(!:Info)
    ;
        MaybeInfo = no
    ).

build_with_module_options_args(ModuleName, OptionVariables,
        OptionArgs, ExtraOptions, Build, Succeeded, !Info, !IO) :-
    build_with_module_options_args_invoked(no, ModuleName, OptionVariables,
        OptionArgs, ExtraOptions, Build, Succeeded, !Info, !IO).

:- pred build_with_module_options_args_invoked(bool::in, module_name::in,
    options_variables::in, list(string)::in, list(string)::in,
    build(list(string), Info1, Info2)::in(build),
    bool::out, Info1::in, maybe(Info2)::out, io::di, io::uo) is det.

build_with_module_options_args_invoked(InvokedByMmcMake, ModuleName,
        OptionVariables, OptionArgs, ExtraOptions, Build, Succeeded,
        Info0, MaybeInfo, !IO) :-
    lookup_mmc_module_options(OptionVariables, ModuleName, OptionsResult, !IO),
    (
        OptionsResult = no,
        MaybeInfo = no,
        Succeeded = no
    ;
        OptionsResult = yes(ModuleOptionArgs),
        globals.io_get_globals(Globals, !IO),

        % --invoked-by-mmc-make disables reading DEFAULT_MCFLAGS
        % from the environment (DEFAULT_MCFLAGS is included in
        % OptionArgs) and generation of `.d' files.
        % --use-subdirs is needed because the code to install
        % libraries uses `--use-grade-subdirs' and assumes the
        % interface files were built with `--use-subdirs'.
        (
            InvokedByMmcMake = yes,
            UseSubdirs = ["--use-subdirs"],
            InvokedByMake = ["--invoked-by-mmc-make"]
        ;
            InvokedByMmcMake = no,
            UseSubdirs = [],
            InvokedByMake = []
        ),

        AllOptionArgs = list.condense([InvokedByMake, ModuleOptionArgs,
            OptionArgs, ExtraOptions, UseSubdirs]),
        handle_options(AllOptionArgs, OptionsErrors, _, _, _, !IO),
        (
            OptionsErrors = [_ | _],
            Succeeded = no,
            MaybeInfo = no,
            usage_errors(OptionsErrors, !IO)
        ;
            OptionsErrors = [],
            Build(AllOptionArgs, Succeeded, Info0, Info, !IO),
            MaybeInfo = yes(Info),
            globals.io_set_globals(unsafe_promise_unique(Globals), !IO)
        )
    ).

redirect_output(_ModuleName, MaybeErrorStream, !Info, !IO) :-
    % Write the output to a temporary file first, so it's easy to just print
    % the part of the error file that relates to the current command. It will
    % be appended to the error file later.

    io.make_temp(ErrorFileName, !IO),
    io.open_output(ErrorFileName, ErrorFileRes, !IO),
    (
        ErrorFileRes = ok(ErrorOutputStream),
        MaybeErrorStream = yes(ErrorOutputStream)
    ;
        ErrorFileRes = error(IOError),
        MaybeErrorStream = no,
        io.write_string("** Error opening `", !IO),
        io.write_string(ErrorFileName, !IO),
        io.write_string("' for output: ", !IO),
        io.error_message(IOError, Msg),
        io.write_string(Msg, !IO),
        io.nl(!IO)
    ).

unredirect_output(ModuleName, ErrorOutputStream, !Info, !IO) :-
    io.output_stream_name(ErrorOutputStream, TmpErrorFileName, !IO),
    io.close_output(ErrorOutputStream, !IO),

    io.open_input(TmpErrorFileName, TmpErrorInputRes, !IO),
    (
        TmpErrorInputRes = ok(TmpErrorInputStream),
        module_name_to_file_name(ModuleName, ".err", yes, ErrorFileName, !IO),
        ( set.member(ModuleName, !.Info ^ error_file_modules) ->
            io.open_append(ErrorFileName, ErrorFileRes, !IO)
        ;
            io.open_output(ErrorFileName, ErrorFileRes, !IO)
        ),
        (
            ErrorFileRes = ok(ErrorFileOutputStream),
            globals.io_lookup_int_option(output_compile_error_lines,
                LinesToWrite, !IO),
            io.output_stream(CurrentOutputStream, !IO),
            io.input_stream_foldl2_io(TmpErrorInputStream,
                write_error_char(ErrorFileOutputStream, CurrentOutputStream),
                LinesToWrite, TmpFileInputRes, !IO),
            (
                TmpFileInputRes = ok(_)
            ;
                TmpFileInputRes = error(_, TmpFileInputError),
                io.write_string("Error reading `", !IO),
                io.write_string(TmpErrorFileName, !IO),
                io.write_string("': ", !IO),
                io.write_string(io.error_message(TmpFileInputError), !IO),
                io.nl(!IO)
            ),

            io.close_output(ErrorFileOutputStream, !IO),

            !:Info = !.Info ^ error_file_modules :=
                set.insert(!.Info ^ error_file_modules, ModuleName)
        ;
            ErrorFileRes = error(Error),
            io.write_string("Error opening `", !IO),
            io.write_string(TmpErrorFileName, !IO),
            io.write_string("': ", !IO),
            io.write_string(io.error_message(Error), !IO),
            io.nl(!IO)
        ),
        io.close_input(TmpErrorInputStream, !IO)
    ;
        TmpErrorInputRes = error(Error),
        io.write_string("Error opening `", !IO),
        io.write_string(TmpErrorFileName, !IO),
        io.write_string("': ", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO)
    ),
    io.remove_file(TmpErrorFileName, _, !IO).

:- pred write_error_char(io.output_stream::in, io.output_stream::in,
    char::in, int::in, int::out, io::di, io::uo) is det.

write_error_char(FullOutputStream, PartialOutputStream, Char,
        !LinesRemaining, !IO) :-
    io.write_char(FullOutputStream, Char, !IO),
    ( !.LinesRemaining > 0 ->
        io.write_char(PartialOutputStream, Char, !IO),
        ( Char = '\n' ->
            !:LinesRemaining = !.LinesRemaining - 1
        ;
            true
        )
    ; !.LinesRemaining = 0 ->
        io.output_stream_name(FullOutputStream, FullOutputFileName, !IO),
        io.write_string(PartialOutputStream, "... error log truncated, see `",
            !IO),
        io.write_string(PartialOutputStream, FullOutputFileName, !IO),
        io.write_string(PartialOutputStream, "' for the complete log.\n", !IO),
        % Only write the above message once.
        !:LinesRemaining = -1
    ;
        true
    ).

%-----------------------------------------------------------------------------%

get_timestamp_file_timestamp(target_file(ModuleName, FileType),
        MaybeTimestamp, !Info, !IO) :-
    globals.io_get_globals(Globals, !IO),
    ( TimestampExt = timestamp_extension(Globals, FileType) ->
        module_name_to_file_name(ModuleName, TimestampExt, no, FileName, !IO)
    ;
        module_target_to_file_name(ModuleName, FileType, no, FileName, !IO)
    ),

    % We should only ever look for timestamp files
    % in the current directory. Timestamp files are
    % only used when processing a module, and only
    % modules in the current directory are processed.
    SearchDirs = [dir.this_directory],
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO).

get_dependency_timestamp(dep_file(FileName, MaybeOption), MaybeTimestamp,
        !Info, !IO) :-
    (
        MaybeOption = yes(Option),
        globals.io_lookup_accumulating_option(Option, SearchDirs, !IO)
    ;
        MaybeOption = no,
        SearchDirs = [dir.this_directory]
    ),
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO).
get_dependency_timestamp(dep_target(Target), MaybeTimestamp, !Info, !IO) :-
    get_target_timestamp(yes, Target, MaybeTimestamp0, !Info, !IO),
    (
        Target = target_file(_, module_target_c_header(header_mih)),
        MaybeTimestamp0 = ok(_)
    ->
        % Don't rebuild the `.o' file if an irrelevant part of a
        % `.mih' file has changed. If a relevant part of a `.mih'
        % file changed, the interface files of the imported module
        % must have changed in a way that would force the `.c' and
        % `.o' files of the current module to be rebuilt.
        MaybeTimestamp = ok(oldest_timestamp)
    ;
        MaybeTimestamp = MaybeTimestamp0
    ).

get_target_timestamp(Search, Target, MaybeTimestamp, !Info, !IO) :-
    ( Target = target_file(ModuleName, module_target_analysis_registry) ->
        get_target_timestamp_analysis_registry(Search, ModuleName,
            MaybeTimestamp, !Info, !IO)
    ;
        get_target_timestamp_2(Search, Target, MaybeTimestamp, !Info, !IO)
    ).

    % Special treatment for `.analysis' files.  If the `.analysis' file is
    % valid then we look at the corresponding `.analysis_date' file to get the
    % last time that the module was actually analysed (the file may have been
    % rewritten or had it's status changed while analysing other modules).
    % If the `.analysis' file is invalid then we treat it as out of date.
    %
:- pred get_target_timestamp_analysis_registry(bool::in, module_name::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

get_target_timestamp_analysis_registry(Search, ModuleName, MaybeTimestamp,
        !Info, !IO) :-
    ModuleId = module_name_to_module_id(ModuleName),
    analysis.read_module_overall_status(mmc, ModuleId, MaybeStatus, !IO),
    (
        MaybeStatus = yes(Status),
        (
            ( Status = optimal
            ; Status = suboptimal
            ),
            get_timestamp_file_timestamp(
                target_file(ModuleName, module_target_analysis_registry),
                MaybeTimestamp0, !Info, !IO),
            (
                MaybeTimestamp0 = ok(_),
                MaybeTimestamp = MaybeTimestamp0
            ;
                MaybeTimestamp0 = error(_),
                % If the `.analysis' file exists with status `optimal' or
                % `suboptimal' but there is no `.analysis_date' file, then the
                % `.analysis' file must just have been created while analysing
                % a different module.
                MaybeTimestamp = ok(oldest_timestamp)
            )
        ;
            Status = invalid,
            MaybeTimestamp = error("invalid module")
        )
    ;
        MaybeStatus = no,
        get_target_timestamp_2(Search,
            target_file(ModuleName, module_target_analysis_registry),
            MaybeTimestamp, !Info, !IO)
    ).

:- pred get_target_timestamp_2(bool::in, target_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

get_target_timestamp_2(Search, TargetFile, MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, FileType),
    get_file_name(Search, TargetFile, FileName, !Info, !IO),
    (
        Search = yes,
        get_search_directories(FileType, SearchDirs, !IO)
    ;
        Search = no,
        SearchDirs = [dir.this_directory]
    ),
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp0, !Info, !IO),
    (
        MaybeTimestamp0 = error(_),
        ( FileType = module_target_intermodule_interface
        ; FileType = module_target_analysis_registry
        )
    ->
        % If a `.opt' file in another directory doesn't exist,
        % it just means that a library wasn't compiled with
        % `--intermodule-optimization'.
        % Similarly for `.analysis' files.

        get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
        (
            MaybeImports = yes(Imports),
            Imports ^ module_dir \= dir.this_directory
        ->
            MaybeTimestamp = ok(oldest_timestamp),
            !:Info = !.Info ^ file_timestamps ^ elem(FileName)
                := MaybeTimestamp
        ;
            MaybeTimestamp = MaybeTimestamp0
        )
    ;
        MaybeTimestamp = MaybeTimestamp0
    ).

get_file_name(Search, TargetFile, FileName, !Info, !IO) :-
    TargetFile = target_file(ModuleName, FileType),
    ( FileType = module_target_source ->
        % In some cases the module name won't match the file name
        % (module mdb.parse might be in parse.m or mdb.m), so we need to
        % look up the file name here.
        get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
        (
            MaybeImports = yes(Imports),
            FileName = Imports ^ source_file_name
        ;
            MaybeImports = no,

            % Something has gone wrong generating the dependencies,
            % so just take a punt (which probably won't work).
            module_name_to_file_name(ModuleName, ".m", no, FileName, !IO)
        )
    ;
        globals.io_get_globals(Globals, !IO),
        MaybeExt = target_extension(Globals, FileType),
        (
            MaybeExt = yes(Ext),
            (
                Search = yes,
                module_name_to_search_file_name(ModuleName, Ext, FileName, !IO)
            ;
                Search = no,
                module_name_to_file_name(ModuleName, Ext, no, FileName, !IO)
            )
        ;
            MaybeExt = no,
            module_target_to_file_name_maybe_search(ModuleName, FileType, no,
                Search, FileName, !IO)
        )
    ).

get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO) :-
    ( MaybeTimestamp0 = !.Info ^ file_timestamps ^ elem(FileName) ->
        MaybeTimestamp = MaybeTimestamp0
    ;
        io.input_stream(OldInputStream, !IO),
        search_for_file(SearchDirs, FileName, SearchResult, !IO),
        ( SearchResult = ok(_) ->
            io.input_stream_name(FullFileName, !IO),
            io.set_input_stream(OldInputStream, FileStream, !IO),
            io.close_input(FileStream, !IO),
            io.file_modification_time(FullFileName, TimeTResult, !IO),
            (
                TimeTResult = ok(TimeT),
                Timestamp = time_t_to_timestamp(TimeT),
                MaybeTimestamp = ok(Timestamp)
            ;
                TimeTResult = error(Error),
                MaybeTimestamp = error(io.error_message(Error))
            ),
            !:Info = !.Info ^ file_timestamps ^ elem(FileName)
                := MaybeTimestamp
        ;
            MaybeTimestamp = error("file `" ++ FileName ++ "' not found")
        )
    ).

:- pred get_search_directories(module_target_type::in, list(dir_name)::out,
    io::di, io::uo) is det.

get_search_directories(FileType, SearchDirs, !IO) :-
    MaybeOpt = search_for_file_type(FileType),
    (
        MaybeOpt = yes(SearchDirOpt),
        globals.io_lookup_accumulating_option(SearchDirOpt, SearchDirs0, !IO),
        % Make sure the current directory is searched
        % for C headers and libraries.
        SearchDirs =
            ( list.member(dir.this_directory, SearchDirs0) ->
                SearchDirs0
            ;
                [dir.this_directory | SearchDirs0]
            )
    ;
        MaybeOpt = no,
        SearchDirs = [dir.this_directory]
    ).

find_oldest_timestamp(error(_) @ MaybeTimestamp, _) = MaybeTimestamp.
find_oldest_timestamp(ok(_), error(_) @ MaybeTimestamp) = MaybeTimestamp.
find_oldest_timestamp(ok(Timestamp1), ok(Timestamp2)) = ok(Timestamp) :-
    ( compare((<), Timestamp1, Timestamp2) ->
        Timestamp = Timestamp1
    ;
        Timestamp = Timestamp2
    ).

%-----------------------------------------------------------------------------%

make_remove_target_file(target_file(ModuleName, FileType), !Info, !IO) :-
    make_remove_target_file(ModuleName, FileType, !Info, !IO).

make_remove_target_file(ModuleName, FileType, !Info, !IO) :-
    globals.io_get_globals(Globals, !IO),
    module_target_to_file_name(ModuleName, FileType, no, FileName, !IO),
    make_remove_file(FileName, !Info, !IO),
    ( TimestampExt = timestamp_extension(Globals, FileType) ->
        make_remove_file(ModuleName, TimestampExt, !Info, !IO)
    ;
        true
    ).

make_remove_file(ModuleName, Ext, !Info, !IO) :-
    module_name_to_file_name(ModuleName, Ext, no, FileName, !IO),
    make_remove_file(FileName, !Info, !IO).

make_remove_file(FileName, !Info, !IO) :-
    verbose_msg(report_remove_file(FileName), !IO),
    io.remove_file(FileName, _, !IO),
    !:Info = !.Info ^ file_timestamps :=
        map.delete(!.Info ^ file_timestamps, FileName).

:- pred report_remove_file(string::in, io::di, io::uo) is det.

report_remove_file(FileName, !IO) :-
    io.write_string("Removing ", !IO),
    io.write_string(FileName, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%

make_target_list(Ks, V) = list.map((func(K) = K - V), Ks).

make_target_file_list(ModuleNames, FileType) = 
    list.map((func(ModuleName) = target_file(ModuleName, FileType)),
        ModuleNames).

make_dependency_list(ModuleNames, FileType) =
    list.map((func(Module) = dep_target(target_file(Module, FileType))),
        ModuleNames).

target_extension(_, module_target_source) = yes(".m").
target_extension(_, module_target_errors) = yes(".err").
target_extension(_, module_target_private_interface) = yes(".int0").
target_extension(_, module_target_long_interface) = yes(".int").
target_extension(_, module_target_short_interface) = yes(".int2").
target_extension(_, module_target_unqualified_short_interface) = yes(".int3").
target_extension(_, module_target_intermodule_interface) = yes(".opt").
target_extension(_, module_target_analysis_registry) = yes(".analysis").
target_extension(_, module_target_c_header(header_mih)) = yes(".mih").
target_extension(_, module_target_c_header(header_mh)) = yes(".mh").
target_extension(_, module_target_c_code) = yes(".c").
target_extension(_, module_target_il_code) = yes(".il").

    % XXX ".exe" if the module contains main.
target_extension(_, module_target_il_asm) = yes(".dll").
target_extension(_, module_target_java_code) = yes(".java").
target_extension(_, module_target_asm_code(non_pic)) = yes(".s").
target_extension(_, module_target_asm_code(link_with_pic)) = yes(".s").
target_extension(_, module_target_asm_code(pic)) = yes(".pic_s").
target_extension(Globals, module_target_object_code(PIC)) = yes(Ext) :-
    maybe_pic_object_file_extension(Globals, PIC, Ext).
target_extension(_, module_target_xml_doc) = yes(".xml").

    % These all need to be handled as special cases.
target_extension(_, module_target_foreign_object(_, _)) = no.
target_extension(_, module_target_foreign_il_asm(_)) = no.
target_extension(_, module_target_fact_table_object(_, _)) = no.

linked_target_file_name(ModuleName, executable, FileName, !IO) :-
    globals.io_lookup_string_option(executable_file_extension, Ext, !IO),
    module_name_to_file_name(ModuleName, Ext, no, FileName, !IO).
linked_target_file_name(ModuleName, static_library, FileName, !IO) :-
    globals.io_lookup_string_option(library_extension, Ext, !IO),
    module_name_to_lib_file_name("lib", ModuleName, Ext, no, FileName, !IO).
linked_target_file_name(ModuleName, shared_library, FileName, !IO) :-
    globals.io_lookup_string_option(shared_library_extension, Ext, !IO),
    module_name_to_lib_file_name("lib", ModuleName, Ext, no, FileName, !IO).
linked_target_file_name(ModuleName, java_archive, FileName, !IO) :-
    module_name_to_file_name(ModuleName, ".jar", no, FileName, !IO).

:- pred module_target_to_file_name(module_name::in, module_target_type::in,
    bool::in, file_name::out, io::di, io::uo) is det.

module_target_to_file_name(ModuleName, TargetType, MkDir, FileName, !IO) :-
    module_target_to_file_name_maybe_search(ModuleName, TargetType, MkDir, no,
        FileName, !IO).

:- pred module_target_to_search_file_name(module_name::in,
    module_target_type::in, file_name::out, io::di, io::uo) is det.

module_target_to_search_file_name(ModuleName, TargetType, FileName, !IO) :-
    module_target_to_file_name_maybe_search(ModuleName, TargetType, no, yes,
        FileName, !IO).

:- pred module_target_to_file_name_maybe_search(module_name::in,
    module_target_type::in, bool::in, bool::in, file_name::out,
    io::di, io::uo) is det.

module_target_to_file_name_maybe_search(ModuleName, TargetType, MkDir, Search,
        FileName, !IO) :-
    globals.io_get_globals(Globals, !IO),
    target_extension(Globals, TargetType) = MaybeExt,
    (
        MaybeExt = yes(Ext),
        (
            Search = yes,
            module_name_to_search_file_name(ModuleName, Ext, FileName, !IO)
        ;
            Search = no,
            module_name_to_file_name(ModuleName, Ext, MkDir, FileName, !IO)
        )
    ;
        MaybeExt = no,
        ( TargetType = module_target_foreign_object(PIC, Lang) ->
            (
                ForeignModuleName =
                    foreign_language_module_name(ModuleName, Lang)
            ->
                module_target_to_file_name_maybe_search(ForeignModuleName,
                    module_target_object_code(PIC), MkDir, Search, FileName,
                    !IO)
            ;
                unexpected(this_file, "module_target_to_file_name_2")
            )
        ; TargetType = module_target_foreign_il_asm(Lang) ->
            (
                ForeignModuleName =
                    foreign_language_module_name(ModuleName, Lang)
            ->
                module_target_to_file_name_maybe_search(ForeignModuleName,
                    module_target_il_asm, MkDir, Search, FileName, !IO)
            ;
                unexpected(this_file, "module_target_to_file_name_2")
            )
        ; TargetType = module_target_fact_table_object(PIC, FactFile) ->
            maybe_pic_object_file_extension(PIC, Ext, !IO),
            fact_table_file_name(ModuleName, FactFile, Ext, MkDir, FileName,
                !IO)
        ;
            unexpected(this_file, "module_target_to_file_name_2")
        )
    ).

    % Note that we need a timestamp file for `.err' files because
    % errors are written to the `.err' file even when writing interfaces.
    % The timestamp is only updated when compiling to target code.
    %
    % We need a timestamp file for `.analysis' files because they
    % can be modified in the process of analysing _another_ module.
    % The timestamp is only updated after actually analysing the module that
    % the `.analysis' file corresponds to.
timestamp_extension(_, module_target_errors) = ".err_date".
timestamp_extension(_, module_target_private_interface) = ".date0".
timestamp_extension(_, module_target_long_interface) = ".date".
timestamp_extension(_, module_target_short_interface) = ".date".
timestamp_extension(_, module_target_unqualified_short_interface) = ".date3".
timestamp_extension(_, module_target_intermodule_interface) = ".optdate".
timestamp_extension(_, module_target_analysis_registry) = ".analysis_date".
timestamp_extension(_, module_target_c_code) = ".c_date".
timestamp_extension(Globals, module_target_c_header(_)) = Ext :-
    globals.get_target(Globals, Target),
    Ext = timestamp_extension(Globals,
        (Target = target_asm ->
            module_target_asm_code(non_pic) ; module_target_c_code)).
timestamp_extension(_, module_target_il_code) = ".il_date".
timestamp_extension(_, module_target_java_code) = ".java_date".
timestamp_extension(_, module_target_asm_code(non_pic)) = ".s_date".
timestamp_extension(_, module_target_asm_code(pic)) = ".pic_s_date".

:- func search_for_file_type(module_target_type) = maybe(option).

search_for_file_type(module_target_source) = no.
search_for_file_type(module_target_errors) = no.
    % XXX only for inter-module optimization.
search_for_file_type(module_target_private_interface) =
        yes(search_directories).
search_for_file_type(module_target_long_interface) = yes(search_directories).
search_for_file_type(module_target_short_interface) = yes(search_directories).
search_for_file_type(module_target_unqualified_short_interface) =
        yes(search_directories).
search_for_file_type(module_target_intermodule_interface) =
        yes(intermod_directories).
search_for_file_type(module_target_analysis_registry) =
        yes(intermod_directories).
search_for_file_type(module_target_c_header(_)) = yes(c_include_directory).
search_for_file_type(module_target_c_code) = no.
search_for_file_type(module_target_il_code) = no.
search_for_file_type(module_target_il_asm) = no.
search_for_file_type(module_target_java_code) = no.
search_for_file_type(module_target_asm_code(_)) = no.
search_for_file_type(module_target_object_code(_)) = no.
search_for_file_type(module_target_foreign_object(_, _)) = no.
search_for_file_type(module_target_foreign_il_asm(_)) = no.
search_for_file_type(module_target_fact_table_object(_, _)) = no.
search_for_file_type(module_target_xml_doc) = no.

target_is_grade_or_arch_dependent(Target) :-
    is_target_grade_or_arch_dependent(Target) = yes.

:- func is_target_grade_or_arch_dependent(module_target_type) = bool.

is_target_grade_or_arch_dependent(Target) = IsDependent :-
    (
        ( Target = module_target_source
        ; Target = module_target_errors
        ; Target = module_target_private_interface
        ; Target = module_target_long_interface
        ; Target = module_target_short_interface
        ; Target = module_target_unqualified_short_interface
        ; Target = module_target_c_header(header_mh)
        ; Target = module_target_xml_doc
        ),
        IsDependent = no
    ;
        ( Target = module_target_intermodule_interface
        ; Target = module_target_analysis_registry
        ; Target = module_target_c_header(header_mih)
        ; Target = module_target_c_code
        ; Target = module_target_il_code
        ; Target = module_target_il_asm
        ; Target = module_target_java_code
        ; Target = module_target_asm_code(_)
        ; Target = module_target_object_code(_)
        ; Target = module_target_foreign_object(_, _)
        ; Target = module_target_foreign_il_asm(_)
        ; Target = module_target_fact_table_object(_, _)
        ),
        IsDependent = yes
    ).

%-----------------------------------------------------------------------------%

debug_msg(P, !IO) :-
    globals.io_lookup_bool_option(debug_make, Debug, !IO),
    (
        Debug = yes,
        P(!IO),
        io.flush_output(!IO)
    ;
        Debug = no
    ).

verbose_msg(P, !IO) :-
    globals.io_lookup_bool_option(verbose_make, Verbose, !IO),
    (
        Verbose = yes,
        P(!IO),
        io.flush_output(!IO)
    ;
        Verbose = no
    ).

debug_file_msg(TargetFile, Msg, !IO) :-
    debug_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            write_target_file(TargetFile, !IO),
            io.write_string(": ", !IO),
            io.write_string(Msg, !IO),
            io.nl(!IO)
        ), !IO).

write_dependency_file(dep_target(TargetFile), !IO) :-
    write_target_file(TargetFile, !IO).
write_dependency_file(dep_file(FileName, _), !IO) :-
    io.write_string(FileName, !IO).

write_target_file(TargetFile, !IO) :-
    TargetFile = target_file(ModuleName, FileType),
    module_target_to_file_name(ModuleName, FileType, no, FileName, !IO),
    io.write_string(FileName, !IO).

maybe_make_linked_target_message(TargetFile, !IO) :-
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Making ", !IO),
            io.write_string(TargetFile, !IO),
            io.nl(!IO)
        ), !IO).

maybe_make_target_message(TargetFile, !IO) :-
    io.output_stream(OutputStream, !IO),
    maybe_make_target_message_to_stream(OutputStream, TargetFile, !IO).

maybe_make_target_message_to_stream(OutputStream, TargetFile, !IO) :-
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.set_output_stream(OutputStream, OldOutputStream, !IO),
            io.write_string("Making ", !IO),
            write_target_file(TargetFile, !IO),
            io.nl(!IO),
            io.set_output_stream(OldOutputStream, _, !IO)
        ), !IO).

maybe_reanalyse_modules_message(!IO) :-
    io.output_stream(OutputStream, !IO),
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.set_output_stream(OutputStream, OldOutputStream, !IO),
            io.write_string("Reanalysing invalid/suboptimal modules\n", !IO),
            io.set_output_stream(OldOutputStream, _, !IO)
        ), !IO).

target_file_error(TargetFile, !IO) :-
    io.write_string("** Error making `", !IO),
    write_target_file(TargetFile, !IO),
    io.write_string("'.\n", !IO).

file_error(TargetFile, !IO) :-
    io.write_string("** Error making `", !IO),
    io.write_string(TargetFile, !IO),
    io.write_string("'.\n", !IO).

maybe_warn_up_to_date_target(Target, !Info, !IO) :-
    globals.io_lookup_bool_option(warn_up_to_date, Warn, !IO),
    (
        Warn = yes,
        ( set.member(Target, !.Info ^ command_line_targets) ->
            io.write_string("** Nothing to be done for `", !IO),
            write_module_or_linked_target(Target, !IO),
            io.write_string("'.\n", !IO)
        ;
            true
        )
    ;
        Warn = no
    ),
    !:Info = !.Info ^ command_line_targets :=
        set.delete(!.Info ^ command_line_targets, Target).

maybe_symlink_or_copy_linked_target_message(Target, !IO) :-
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Making symlink/copy of ", !IO),
            write_module_or_linked_target(Target, !IO),
            io.write_string("\n", !IO)
        ), !IO).

:- pred write_module_or_linked_target(pair(module_name, target_type)::in,
    io::di, io::uo) is det.

write_module_or_linked_target(ModuleName - FileType, !IO) :-
    (
        FileType = module_target(ModuleTargetType),
        TargetFile = target_file(ModuleName, ModuleTargetType),
        write_target_file(TargetFile, !IO)
    ;
        FileType = linked_target(LinkedTargetType),
        linked_target_file_name(ModuleName, LinkedTargetType, FileName, !IO),
        io.write_string(FileName, !IO)
    ;
        FileType = misc_target(_),
        unexpected(this_file,
            "maybe_warn_up_to_date_target: misc_target")
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "make.util.m".

%-----------------------------------------------------------------------------%
:- end_module make.util.
%-----------------------------------------------------------------------------%
