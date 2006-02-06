%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: make.util.m.
% Main author: stayl.

% Assorted predicates used to implement `mmc --make'.

%-----------------------------------------------------------------------------%

:- module make__util.
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

    % build_with_module_options(ModuleName, ExtraArgs, Builder,
    %   Succeeded, !Info).
    %
    % Perform the given closure after updating the option_table in
    % the globals in the io__state to contain the module-specific
    % options for the specified module and the extra options given
    % in the ExtraArgs.
    % Adds `--invoked-by-mmc-make' and `--use-subdirs' to the option
    % list.
    % The old option table will be restored afterwards.
    %
:- pred build_with_module_options(module_name::in,
    list(string)::in, build(list(string))::in(build), bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % build_with_module_options(ModuleName, OptionsVariables,
    %   OptionArgs, ExtraArgs, Builder, Succeeded, !Info).
    %
    % Perform the given closure after updating the option_table in
    % the globals in the io__state to contain the module-specific
    % options for the specified module and the extra options given
    % in ExtraArgs and OptionArgs
    % Does not add `--invoked-by-mmc-make' and `--use-subdirs'
    % to the option list.
    % The old option table will be restored afterwards.
    %
:- pred build_with_module_options(module_name::in, options_variables::in,
    list(string)::in, list(string)::in,
    build(list(string), Info1, Info2)::in(build),
    bool::out, Info1::in, maybe(Info2)::out, io::di, io::uo) is det.

    % Perform the given closure with an output stream created
    % to append to the error file for the given module.
    %
:- pred build_with_output_redirect(module_name::in,
    build(io__output_stream)::in(build), bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Produce an output stream which writes to the error file
    % for the given module.
    %
:- pred redirect_output(module_name::in, maybe(io__output_stream)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Close the module error output stream.
    %
:- pred unredirect_output(module_name::in, io__output_stream::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type build2(T, U) == pred(T, U, bool, make_info, make_info, io, io).
:- inst build2 == (pred(in, in, out, in, out, di, uo) is det).

:- pred build_with_module_options_and_output_redirect(module_name::in,
    list(string)::in, build2(list(string), io__output_stream)::in(build2),
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Timestamp handling.
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
% Remove file a file, deleting the cached timestamp.
%

    % Remove the target file and the corresponding timestamp file.
    %
:- pred remove_target_file(target_file::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % Remove the target file and the corresponding timestamp file.
    %
:- pred remove_target_file(module_name::in, module_target_type::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % remove_file(ModuleName, Extension, !Info).
    %
:- pred remove_file(module_name::in, string::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

:- pred remove_file(file_name::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- func make_target_list(list(K), V) = assoc_list(K, V).

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
% Debugging, verbose and error messages.
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

:- pred maybe_make_target_message(io__output_stream::in, target_file::in,
    io::di, io::uo) is det.

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

    % If the given target was specified on the command
    % line, warn that it was already up to date.
    %
:- pred maybe_warn_up_to_date_target(pair(module_name, target_type)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

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
    build2(list(string), io__output_stream)::in(build2), list(string)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

build_with_module_options_and_output_redirect_2(ModuleName, Build, AllOptions,
        Succeeded, !Info, !IO) :-
    build_with_output_redirect(ModuleName,
        build_with_module_options_and_output_redirect_3(AllOptions, Build),
        Succeeded, !Info, !IO).

:- pred build_with_module_options_and_output_redirect_3(list(string)::in,
    build2(list(string), io__output_stream)::in(build2),
    io__output_stream::in, bool::out, make_info::in, make_info::out,
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
    build_with_module_options(yes, ModuleName, !.Info ^ options_variables,
        !.Info ^ option_args, ExtraOptions, Build, Succeeded,
        !.Info, MaybeInfo, !IO),
    (
        MaybeInfo = yes(!:Info)
    ;
        MaybeInfo = no
    ).

build_with_module_options(ModuleName, OptionVariables,
        OptionArgs, ExtraOptions, Build, Succeeded, !Info, !IO) :-
    build_with_module_options(no, ModuleName, OptionVariables,
        OptionArgs, ExtraOptions, Build, Succeeded, !Info, !IO).

:- pred build_with_module_options(bool::in, module_name::in,
    options_variables::in, list(string)::in, list(string)::in,
    build(list(string), Info1, Info2)::in(build),
    bool::out, Info1::in, maybe(Info2)::out, io::di, io::uo) is det.

build_with_module_options(InvokedByMmcMake, ModuleName, OptionVariables,
        OptionArgs, ExtraOptions, Build, Succeeded, Info0, MaybeInfo, !IO) :-
    lookup_mmc_module_options(OptionVariables, ModuleName, OptionsResult, !IO),
    (
        OptionsResult = no,
        MaybeInfo = no,
        Succeeded = no
    ;
        OptionsResult = yes(ModuleOptionArgs),
        globals__io_get_globals(Globals, !IO),

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

        AllOptionArgs = list__condense([InvokedByMake, ModuleOptionArgs,
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
            globals__io_set_globals(unsafe_promise_unique(Globals), !IO)
        )
    ).

redirect_output(_ModuleName, MaybeErrorStream, !Info, !IO) :-
    % Write the output to a temporary file first, so it's easy to just print
    % the part of the error file that relates to the current command. It will
    % be appended to the error file later.

    io__make_temp(ErrorFileName, !IO),
    io__open_output(ErrorFileName, ErrorFileRes, !IO),
    (
        ErrorFileRes = ok(ErrorOutputStream),
        MaybeErrorStream = yes(ErrorOutputStream)
    ;
        ErrorFileRes = error(IOError),
        MaybeErrorStream = no,
        io__write_string("** Error opening `", !IO),
        io__write_string(ErrorFileName, !IO),
        io__write_string("' for output: ", !IO),
        io__error_message(IOError, Msg),
        io__write_string(Msg, !IO),
        io__nl(!IO)
    ).

unredirect_output(ModuleName, ErrorOutputStream, !Info, !IO) :-
    io__output_stream_name(ErrorOutputStream, TmpErrorFileName, !IO),
    io__close_output(ErrorOutputStream, !IO),

    io__open_input(TmpErrorFileName, TmpErrorInputRes, !IO),
    (
        TmpErrorInputRes = ok(TmpErrorInputStream),
        module_name_to_file_name(ModuleName, ".err", yes, ErrorFileName, !IO),
        ( set__member(ModuleName, !.Info ^ error_file_modules) ->
            io__open_append(ErrorFileName, ErrorFileRes, !IO)
        ;
            io__open_output(ErrorFileName, ErrorFileRes, !IO)
        ),
        (
            ErrorFileRes = ok(ErrorFileOutputStream),
            globals__io_lookup_int_option(output_compile_error_lines,
                LinesToWrite, !IO),
            io__output_stream(CurrentOutputStream, !IO),
            io__input_stream_foldl2_io(TmpErrorInputStream,
                write_error_char(ErrorFileOutputStream, CurrentOutputStream),
                LinesToWrite, TmpFileInputRes, !IO),
            (
                TmpFileInputRes = ok(_)
            ;
                TmpFileInputRes = error(_, TmpFileInputError),
                io__write_string("Error reading `", !IO),
                io__write_string(TmpErrorFileName, !IO),
                io__write_string("': ", !IO),
                io__write_string(io__error_message(TmpFileInputError), !IO),
                io__nl(!IO)
            ),

            io__close_output(ErrorFileOutputStream, !IO),

            !:Info = !.Info ^ error_file_modules :=
                set__insert(!.Info ^ error_file_modules, ModuleName)
        ;
            ErrorFileRes = error(Error),
            io__write_string("Error opening `", !IO),
            io__write_string(TmpErrorFileName, !IO),
            io__write_string("': ", !IO),
            io__write_string(io__error_message(Error), !IO),
            io__nl(!IO)
        ),
        io__close_input(TmpErrorInputStream, !IO)
    ;
        TmpErrorInputRes = error(Error),
        io__write_string("Error opening `", !IO),
        io__write_string(TmpErrorFileName, !IO),
        io__write_string("': ", !IO),
        io__write_string(io__error_message(Error), !IO),
        io__nl(!IO)
    ),
    io__remove_file(TmpErrorFileName, _, !IO).

:- pred write_error_char(io__output_stream::in, io__output_stream::in,
    char::in, int::in, int::out, io::di, io::uo) is det.

write_error_char(FullOutputStream, PartialOutputStream, Char,
        !LinesRemaining, !IO) :-
    io__write_char(FullOutputStream, Char, !IO),
    ( !.LinesRemaining > 0 ->
        io__write_char(PartialOutputStream, Char, !IO),
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

get_timestamp_file_timestamp(ModuleName - FileType, MaybeTimestamp,
        !Info, !IO) :-
    globals__io_get_globals(Globals, !IO),
    ( TimestampExt = timestamp_extension(Globals, FileType) ->
        module_name_to_file_name(ModuleName, TimestampExt, no, FileName, !IO)
    ;
        module_target_to_file_name(ModuleName, FileType, no, FileName, !IO)
    ),

    % We should only ever look for timestamp files
    % in the current directory. Timestamp files are
    % only used when processing a module, and only
    % modules in the current directory are processed.
    SearchDirs = [dir__this_directory],
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO).

get_dependency_timestamp(file(FileName, MaybeOption), MaybeTimestamp,
        !Info, !IO) :-
    (
        MaybeOption = yes(Option),
        globals__io_lookup_accumulating_option(Option, SearchDirs, !IO)
    ;
        MaybeOption = no,
        SearchDirs = [dir__this_directory]
    ),
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO).
get_dependency_timestamp(target(Target), MaybeTimestamp, !Info, !IO) :-
    get_target_timestamp(yes, Target, MaybeTimestamp0, !Info, !IO),
    (
        Target = _ - c_header(mih),
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
    ( Target = ModuleName - analysis_registry ->
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
            get_timestamp_file_timestamp(ModuleName - analysis_registry,
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
        get_target_timestamp_2(Search, ModuleName - analysis_registry,  
            MaybeTimestamp, !Info, !IO)
    ).

:- pred get_target_timestamp_2(bool::in, target_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

get_target_timestamp_2(Search, ModuleName - FileType, MaybeTimestamp,
        !Info, !IO) :-
    get_file_name(Search, ModuleName - FileType, FileName, !Info, !IO),
    (
        Search = yes,
        get_search_directories(FileType, SearchDirs, !IO)
    ;
        Search = no,
        SearchDirs = [dir__this_directory]
    ),
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp0, !Info, !IO),
    (
        MaybeTimestamp0 = error(_),
        ( FileType = intermodule_interface
        ; FileType = analysis_registry
        )
    ->
        % If a `.opt' file in another directory doesn't exist,
        % it just means that a library wasn't compiled with
        % `--intermodule-optimization'.
        % Similarly for `.analysis' files.

        get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
        (
            MaybeImports = yes(Imports),
            Imports ^ module_dir \= dir__this_directory
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

get_file_name(Search, ModuleName - FileType, FileName, !Info, !IO) :-
    ( FileType = source ->
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
        globals__io_get_globals(Globals, !IO),
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
            module_target_to_file_name(ModuleName, FileType, no, Search,
                FileName, !IO)
        )
    ).

get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO) :-
    ( MaybeTimestamp0 = !.Info ^ file_timestamps ^ elem(FileName) ->
        MaybeTimestamp = MaybeTimestamp0
    ;
        io__input_stream(OldInputStream, !IO),
        search_for_file(SearchDirs, FileName, SearchResult, !IO),
        ( SearchResult = ok(_) ->
            io__input_stream_name(FullFileName, !IO),
            io__set_input_stream(OldInputStream, FileStream, !IO),
            io__close_input(FileStream, !IO),
            io__file_modification_time(FullFileName, TimeTResult, !IO),
            (
                TimeTResult = ok(TimeT),
                Timestamp = time_t_to_timestamp(TimeT),
                MaybeTimestamp = ok(Timestamp)
            ;
                TimeTResult = error(Error),
                MaybeTimestamp = error(io__error_message(Error))
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
        globals__io_lookup_accumulating_option(SearchDirOpt, SearchDirs0, !IO),
        % Make sure the current directory is searched
        % for C headers and libraries.
        SearchDirs =
            ( list__member(dir__this_directory, SearchDirs0) ->
                SearchDirs0
            ;
                [dir__this_directory | SearchDirs0]
            )
    ;
        MaybeOpt = no,
        SearchDirs = [dir__this_directory]
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

remove_target_file(ModuleName - FileType, !Info, !IO) :-
    remove_target_file(ModuleName, FileType, !Info, !IO).

remove_target_file(ModuleName, FileType, !Info, !IO) :-
    globals__io_get_globals(Globals, !IO),
    module_target_to_file_name(ModuleName, FileType, no, FileName, !IO),
    remove_file(FileName, !Info, !IO),
    ( TimestampExt = timestamp_extension(Globals, FileType) ->
        remove_file(ModuleName, TimestampExt, !Info, !IO)
    ;
        true
    ).

remove_file(ModuleName, Ext, !Info, !IO) :-
    module_name_to_file_name(ModuleName, Ext, no, FileName, !IO),
    remove_file(FileName, !Info, !IO).

remove_file(FileName, !Info, !IO) :-
    verbose_msg(report_remove_file(FileName), !IO),
    io__remove_file(FileName, _, !IO),
    !:Info = !.Info ^ file_timestamps :=
        map__delete(!.Info ^ file_timestamps, FileName).

:- pred report_remove_file(string::in, io::di, io::uo) is det.

report_remove_file(FileName, !IO) :-
    io__write_string("Removing ", !IO),
    io__write_string(FileName, !IO),
    io__nl(!IO).

%-----------------------------------------------------------------------------%

make_target_list(Ks, V) = list__map((func(K) = K - V), Ks).

make_dependency_list(ModuleNames, FileType) =
    list__map((func(Module) = target(Module - FileType)), ModuleNames).

target_extension(_, source) = yes(".m").
target_extension(_, errors) = yes(".err").
target_extension(_, private_interface) = yes(".int0").
target_extension(_, long_interface) = yes(".int").
target_extension(_, short_interface) = yes(".int2").
target_extension(_, unqualified_short_interface) = yes(".int3").
target_extension(_, intermodule_interface) = yes(".opt").
target_extension(_, analysis_registry) = yes(".analysis").
target_extension(_, aditi_code) = yes(".rlo").
target_extension(_, c_header(mih)) = yes(".mih").
target_extension(_, c_header(mh)) = yes(".mh").
target_extension(_, c_code) = yes(".c").
target_extension(_, il_code) = yes(".il").

    % XXX ".exe" if the module contains main.
target_extension(_, il_asm) = yes(".dll").
target_extension(_, java_code) = yes(".java").
target_extension(_, asm_code(non_pic)) = yes(".s").
target_extension(_, asm_code(link_with_pic)) = yes(".s").
target_extension(_, asm_code(pic)) = yes(".pic_s").
target_extension(Globals, object_code(PIC)) = yes(Ext) :-
    maybe_pic_object_file_extension(Globals, PIC, Ext).

    % These all need to be handled as special cases.
target_extension(_, foreign_object(_, _)) = no.
target_extension(_, foreign_il_asm(_)) = no.
target_extension(_, fact_table_object(_, _)) = no.

linked_target_file_name(ModuleName, executable, FileName) -->
    globals__io_lookup_string_option(executable_file_extension, Ext),
    module_name_to_file_name(ModuleName, Ext, no, FileName).
linked_target_file_name(ModuleName, static_library, FileName) -->
    globals__io_lookup_string_option(library_extension, Ext),
    module_name_to_lib_file_name("lib", ModuleName, Ext, no, FileName).
linked_target_file_name(ModuleName, shared_library, FileName) -->
    globals__io_lookup_string_option(shared_library_extension, Ext),
    module_name_to_lib_file_name("lib", ModuleName, Ext, no, FileName).
linked_target_file_name(ModuleName, java_archive, FileName) -->
    module_name_to_file_name(ModuleName, ".jar", no, FileName).

:- pred module_target_to_file_name(module_name::in, module_target_type::in,
    bool::in, file_name::out, io::di, io::uo) is det.

module_target_to_file_name(ModuleName, TargetType, MkDir, FileName, !IO) :-
    module_target_to_file_name(ModuleName, TargetType, MkDir, no,
        FileName, !IO).

:- pred module_target_to_search_file_name(module_name::in,
    module_target_type::in, file_name::out, io::di, io::uo) is det.

module_target_to_search_file_name(ModuleName, TargetType, FileName, !IO) :-
    module_target_to_file_name(ModuleName, TargetType, no, yes, FileName, !IO).

:- pred module_target_to_file_name(module_name::in, module_target_type::in,
    bool::in, bool::in, file_name::out, io::di, io::uo) is det.

module_target_to_file_name(ModuleName, TargetType, MkDir, Search, FileName,
        !IO) :-
    globals__io_get_globals(Globals, !IO),
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
        ( TargetType = foreign_object(PIC, Lang) ->
            (
                ForeignModuleName =
                    foreign_language_module_name(ModuleName, Lang)
            ->
                module_target_to_file_name(ForeignModuleName, object_code(PIC),
                    MkDir, Search, FileName, !IO)
            ;
                unexpected(this_file, "module_target_to_file_name_2")
            )
        ; TargetType = foreign_il_asm(Lang) ->
            (
                ForeignModuleName =
                    foreign_language_module_name(ModuleName, Lang)
            ->
                module_target_to_file_name(ForeignModuleName, il_asm, MkDir,
                    Search, FileName, !IO)
            ;
                unexpected(this_file, "module_target_to_file_name_2")
            )
        ; TargetType = fact_table_object(PIC, FactFile) ->
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
timestamp_extension(_, errors) = ".err_date".
timestamp_extension(_, private_interface) = ".date0".
timestamp_extension(_, long_interface) = ".date".
timestamp_extension(_, short_interface) = ".date".
timestamp_extension(_, unqualified_short_interface) = ".date3".
timestamp_extension(_, intermodule_interface) = ".optdate".
timestamp_extension(_, analysis_registry) = ".analysis_date".
timestamp_extension(_, c_code) = ".c_date".
timestamp_extension(Globals, c_header(_)) = Ext :-
    globals__get_target(Globals, Target),
    Ext = timestamp_extension(Globals,
        (Target = asm -> asm_code(non_pic) ; c_code)).
timestamp_extension(_, il_code) = ".il_date".
timestamp_extension(_, java_code) = ".java_date".
timestamp_extension(_, asm_code(non_pic)) = ".s_date".
timestamp_extension(_, asm_code(pic)) = ".pic_s_date".

:- func search_for_file_type(module_target_type) = maybe(option).

search_for_file_type(source) = no.
search_for_file_type(errors) = no.
    % XXX only for inter-module optimization.
search_for_file_type(private_interface) = yes(search_directories).
search_for_file_type(long_interface) = yes(search_directories).
search_for_file_type(short_interface) = yes(search_directories).
search_for_file_type(unqualified_short_interface) = yes(search_directories).
search_for_file_type(intermodule_interface) = yes(intermod_directories).
search_for_file_type(analysis_registry) = yes(intermod_directories).
search_for_file_type(aditi_code) = no.
search_for_file_type(c_header(_)) = yes(c_include_directory).
search_for_file_type(c_code) = no.
search_for_file_type(il_code) = no.
search_for_file_type(il_asm) = no.
search_for_file_type(java_code) = no.
search_for_file_type(asm_code(_)) = no.
search_for_file_type(object_code(_)) = no.
search_for_file_type(foreign_object(_, _)) = no.
search_for_file_type(foreign_il_asm(_)) = no.
search_for_file_type(fact_table_object(_, _)) = no.

target_is_grade_or_arch_dependent(Target) :-
    target_is_grade_or_arch_dependent(Target, yes).

:- pred target_is_grade_or_arch_dependent(module_target_type::in,
    bool::out) is det.

target_is_grade_or_arch_dependent(source, no).
target_is_grade_or_arch_dependent(errors, no).
target_is_grade_or_arch_dependent(private_interface, no).
target_is_grade_or_arch_dependent(long_interface, no).
target_is_grade_or_arch_dependent(short_interface, no).
target_is_grade_or_arch_dependent(unqualified_short_interface, no).
target_is_grade_or_arch_dependent(intermodule_interface, yes).
target_is_grade_or_arch_dependent(analysis_registry, yes).
target_is_grade_or_arch_dependent(aditi_code, no).
target_is_grade_or_arch_dependent(c_header(mh), no).
target_is_grade_or_arch_dependent(c_header(mih), yes).
target_is_grade_or_arch_dependent(c_code, yes).
target_is_grade_or_arch_dependent(il_code, yes).
target_is_grade_or_arch_dependent(il_asm, yes).
target_is_grade_or_arch_dependent(java_code, yes).
target_is_grade_or_arch_dependent(asm_code(_), yes).
target_is_grade_or_arch_dependent(object_code(_), yes).
target_is_grade_or_arch_dependent(foreign_object(_, _), yes).
target_is_grade_or_arch_dependent(foreign_il_asm(_), yes).
target_is_grade_or_arch_dependent(fact_table_object(_, _), yes).

%-----------------------------------------------------------------------------%

debug_msg(P, !IO) :-
    globals__io_lookup_bool_option(debug_make, Debug, !IO),
    (
        Debug = yes,
        P(!IO),
        io__flush_output(!IO)
    ;
        Debug = no
    ).

verbose_msg(P, !IO) :-
    globals__io_lookup_bool_option(verbose_make, Verbose, !IO),
    (
        Verbose = yes,
        P(!IO),
        io__flush_output(!IO)
    ;
        Verbose = no
    ).

debug_file_msg(TargetFile, Msg, !IO) :-
    debug_msg(
        (pred(di, uo) is det -->
            write_target_file(TargetFile),
            io__write_string(": "),
            io__write_string(Msg),
            io__nl
        ), !IO).

write_dependency_file(target(TargetFile), !IO) :-
    write_target_file(TargetFile, !IO).
write_dependency_file(file(FileName, _), !IO) :-
    io__write_string(FileName, !IO).

write_target_file(ModuleName - FileType, !IO) :-
    module_target_to_file_name(ModuleName, FileType, no, FileName, !IO),
    io__write_string(FileName, !IO).

maybe_make_linked_target_message(TargetFile, !IO) :-
    verbose_msg(
        (pred(di, uo) is det -->
            io__write_string("Making "),
            io__write_string(TargetFile),
            io__nl
        ), !IO).

maybe_make_target_message(TargetFile, !IO) :-
    io__output_stream(OutputStream, !IO),
    maybe_make_target_message(OutputStream, TargetFile, !IO).

maybe_make_target_message(OutputStream, TargetFile, !IO) :-
    verbose_msg(
        (pred(di, uo) is det -->
            io__set_output_stream(OutputStream, OldOutputStream),
            io__write_string("Making "),
            write_target_file(TargetFile),
            io__nl,
            io__set_output_stream(OldOutputStream, _)
        ), !IO).

maybe_reanalyse_modules_message(!IO) :-
    io__output_stream(OutputStream, !IO),
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io__set_output_stream(OutputStream, OldOutputStream, !IO),
            io__write_string("Reanalysing invalid/suboptimal modules\n", !IO),
            io__set_output_stream(OldOutputStream, _, !IO)
        ), !IO).

target_file_error(TargetFile, !IO) :-
    io__write_string("** Error making `", !IO),
    write_target_file(TargetFile, !IO),
    io__write_string("'.\n", !IO).

file_error(TargetFile, !IO) :-
    io__write_string("** Error making `", !IO),
    io__write_string(TargetFile, !IO),
    io__write_string("'.\n", !IO).

maybe_warn_up_to_date_target(Target @ (ModuleName - FileType), !Info, !IO) :-
    globals__io_lookup_bool_option(warn_up_to_date, Warn, !IO),
    (
        Warn = yes,
        ( set__member(Target, !.Info ^ command_line_targets) ->
            io__write_string("** Nothing to be done for `", !IO),
            (
                FileType = module_target(ModuleTargetType),
                write_target_file(ModuleName - ModuleTargetType, !IO)
            ;
                FileType = linked_target(LinkedTargetType),
                linked_target_file_name(ModuleName, LinkedTargetType, FileName,
                    !IO),
                io__write_string(FileName, !IO)
            ;
                FileType = misc_target(_),
                unexpected(this_file,
                    "maybe_warn_up_to_date_target: misc_target")
            ),
            io__write_string("'.\n", !IO)
        ;
            true
        )
    ;
        Warn = no
    ),
    !:Info = !.Info ^ command_line_targets :=
        set__delete(!.Info ^ command_line_targets, Target).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "make.util.m".

%-----------------------------------------------------------------------------%
:- end_module make.util.
%-----------------------------------------------------------------------------%
