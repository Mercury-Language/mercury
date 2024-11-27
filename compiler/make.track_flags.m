%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.track_flags.m.
%
% This module keeps track of which options were used to compile which modules.
%
%---------------------------------------------------------------------------%

:- module make.track_flags.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.

    % Generate the .track_flags files for local modules reachable from the
    % target module. The files contain hashes of the options which are set for
    % that particular module (deliberately ignoring some options), and are only
    % updated if they have changed since the last --make run. We use hashes
    % as the full option tables are quite large.
    %
:- pred make_track_flags_files(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.handle_options.
:- import_module libs.md5.
:- import_module libs.options.
:- import_module make.find_local_modules.
:- import_module make.options_file.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module getopt.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.

%---------------------------------------------------------------------------%

:- type last_hash
    --->    last_hash(
                lh_maybe_stdlib_dirs    :: maybe1(list(string)),
                lh_options              :: list(string),
                lh_hash                 :: string
            ).

make_track_flags_files(ErrorStream, ProgressStream, Globals, ModuleName,
        Succeeded, !Info, !IO) :-
    find_reachable_local_modules(ProgressStream, Globals, ModuleName,
        Succeeded0, ModuleNames, !Info, !IO),
    (
        Succeeded0 = succeeded,
        DummyLastHash = last_hash(error1([]), [], ""),
        foldl3_make_track_flags_for_modules_loop(ErrorStream, ProgressStream,
            Globals, set.to_sorted_list(ModuleNames),
            Succeeded, DummyLastHash, _LastHash, !Info, !IO)
    ;
        Succeeded0 = did_not_succeed,
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

:- pred foldl3_make_track_flags_for_modules_loop(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    list(module_name)::in, maybe_succeeded::out, last_hash::in, last_hash::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

foldl3_make_track_flags_for_modules_loop(_ErrorStream, _ProgressStream,
        _Globals, [], succeeded, !LastHash, !Info, !IO).
foldl3_make_track_flags_for_modules_loop(ErrorStream, ProgressStream,
        Globals, [ModuleName | ModuleNames], Succeeded,
        !LastHash, !Info, !IO) :-
    make_track_flags_files_for_module(ErrorStream, ProgressStream,
        Globals, ModuleName, Succeeded0, !LastHash, !Info, !IO),
    (
        Succeeded0 = succeeded,
        foldl3_make_track_flags_for_modules_loop(ErrorStream, ProgressStream,
            Globals, ModuleNames, Succeeded, !LastHash, !Info, !IO)
    ;
        Succeeded0 = did_not_succeed,
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

:- pred make_track_flags_files_for_module(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, module_name::in,
    maybe_succeeded::out, last_hash::in, last_hash::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_track_flags_files_for_module(ErrorStream, ProgressStream, Globals,
        ModuleName, Succeeded, !LastHash, !Info, !IO) :-
    EnvOptFileVariables = make_info_get_env_optfile_variables(!.Info),
    lookup_mmc_module_options(EnvOptFileVariables, ModuleName,
        MaybeModuleOptionArgs),
    lookup_mercury_stdlib_dir(EnvOptFileVariables, MaybeStdLibDirs),
    (
        MaybeModuleOptionArgs = ok1(ModuleOptionArgs),
        DetectedGradeFlags = make_info_get_detected_grade_flags(!.Info),
        EnvVarArgs = make_info_get_env_var_args(!.Info),
        OptionArgs = make_info_get_option_args(!.Info),
        AllOptionArgs = DetectedGradeFlags ++ ModuleOptionArgs ++
            EnvVarArgs ++ OptionArgs,

        % The set of options from one module to the next is usually identical,
        % so we can easily avoid running handle_options and stringifying and
        % hashing the option table, all of which can contribute to an annoying
        % delay when mmc --make starts.
        ( if
            !.LastHash = last_hash(MaybeStdLibDirs, AllOptionArgs, HashPrime)
        then
            Hash = HashPrime
        else
            get_default_options(Globals, DefaultOptionTable),
            option_table_hash(ProgressStream, DefaultOptionTable,
                MaybeStdLibDirs, AllOptionArgs, Hash, !IO),
            !:LastHash = last_hash(MaybeStdLibDirs, AllOptionArgs, Hash)
        ),

        % XXX LEGACY
        module_name_to_file_name_create_dirs(Globals, $pred,
            ext_cur_ngs_gs(ext_cur_ngs_gs_misc_track_flags),
            ModuleName, HashFileName, _HashFileNameProposed, !IO),
        compare_hash_file(ProgressStream, Globals, HashFileName,
            Hash, Same, !IO),
        (
            Same = yes,
            Succeeded = succeeded
        ;
            Same = no,
            write_hash_file(ProgressStream, HashFileName, Hash, Succeeded, !IO)
        )
    ;
        MaybeModuleOptionArgs = error1(LookupSpecs),
        % XXX ErrorStream is the error stream for the top target,
        % which may or may NOT be ModuleName. It is not clear to me (zs)
        % whether the right place to write LookupSpecs is ErrorStream,
        % or a stream wrting to the .err file of ModuleName.
        % I also don't think we have a good basis for deciding the answer
        % to that question until we have practical experience with
        % compiler invocations that lead execution to this spot.
        write_error_specs(ErrorStream, Globals, LookupSpecs, !IO),
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

:- pred option_table_hash(io.text_output_stream::in, option_table::in,
    maybe1(list(string))::in, list(string)::in, string::out,
    io::di, io::uo) is det.

option_table_hash(ProgressStream, DefaultOptionTable, MaybeStdLibDirs,
        AllOptionArgs, Hash, !IO) :-
    % This code is part of the --track-flags implementation. We hash the
    % options in the updated globals because they include module-specific
    % options. The hash is then compared with the hash stored in the
    % <module_name>.track_flags file, which is updated if it differs.
    % The new timestamp will later force the module to be recompiled
    % if necessary, but that's later. We are not compiling the module
    % immediately, so this is the only use we have for AllOptionArgsGlobals
    % here.
    %
    % XXX This algorithm processes every option in the option table, even
    % though it does not include all of them in the hash. Virtually all
    % of these options will have their default values, which makes
    % most of that work effectively wasted.
    %
    % A more elegant approach would be to invoke getopt.record_arguments on
    % AllOptionArgs, and hash the resulting list of option_values. This would
    % require hashing special options (such as -ON) as well as non-special
    % values, but the cost of that would be trivial.
    %
    % This approach would have two principal differences from the current one.
    %
    % - First, the current approach computes a different hash if AllOptionArgs
    %   has not changed, but the default value of an (consequential) option
    %   has changed, or if a new consequential option has been added.
    %   The recompilation that this forces will be needed after some changes
    %   to the option defaults, but not after others.
    %
    %   However, this consideration never applies only to a single module;
    %   if the default set of option values changes, it applies for all
    %   modules. Therefore it would be enough to record a hash of the
    %   default values of all options (or a timestamp when the option database
    %   was last modified, see below) *once* in a global file that is relevant
    %   to all modules in a directory, named maybe "Mercury.track_flags",
    %   which, if it changes, invalidates *every* <module_name>.track_flags
    %   file in that directory.
    %
    % - Second, it is possible for some changes in AllOptionArgs to yield
    %   the same final AllOptionArgsGlobals, if some option in the old
    %   AllOptionArgs implies the value of some other option, and the new
    %   AllOptionArgs explicitly sets this option to the implied value
    %   (or vice versa). In such cases, the new approach would force a
    %   recompilation, while the old one would not. However, the absence
    %   of a recompilation in such an instance could be more worrysome
    %   than welcome for users who do not know about that option implication,
    %   or who do not appreciate its significance.
    %
    % Note also that while the old approach forces a recompilation
    % both when the set of options changes and when the default value
    % of an option changes, it does *not* force a recompilation
    % if the *meaning* of an option is redefined. We could consider
    % all three of these kinds of changes grounds for updating a timestamp
    % of when the option database last changed.
    %
    % XXX A somewhat more backwards-compatible version of the above approach
    % would be to hash both DefaultOptionTable AND the result invoking
    % getopt.record_arguments, after filtering inconsequential options
    % out of both. This would effectively hash the *input* of the call to
    % handle_given_options, rather than its output. It would also be
    % logically cleaner, since in some cases, handle_given_options does
    % updates on mutables (e.g. for disabling smart recompilation)
    % whose effects persist *beyond* the processing of a given module.
    %
    handle_given_options(ProgressStream, DefaultOptionTable, MaybeStdLibDirs,
        AllOptionArgs, _, _, OptionsErrors, AllOptionArgsGlobals, !IO),
    (
        OptionsErrors = []
    ;
        OptionsErrors = [_ | _],
        unexpected($file, $pred ++ ": " ++
            "handle_options returned with errors")
    ),
    globals.get_options(AllOptionArgsGlobals, OptionTable),
    map.to_sorted_assoc_list(OptionTable, OptionList),
    inconsequential_options(InconsequentialOptions),
    InconsequentialOptionsSet = set_tree234.from_set(InconsequentialOptions),
    list.filter(include_option_in_hash(InconsequentialOptionsSet),
        OptionList, HashOptionList),
    globals.get_opt_tuple(AllOptionArgsGlobals, OptTuple),
    Hash = md5sum(string({HashOptionList, OptTuple})).

:- pred include_option_in_hash(set_tree234(option)::in,
    pair(option, option_data)::in) is semidet.

include_option_in_hash(InconsequentialOptionsSet, Option - OptionData) :-
    require_complete_switch [OptionData]
    (
        ( OptionData = bool(_)
        ; OptionData = int(_)
        ; OptionData = string(_)
        ; OptionData = maybe_int(_)
        ; OptionData = maybe_string(_)
        ; OptionData = accumulating(_)
        ),
        % XXX Reconsider if a lot of these options really should be ignored.
        not set_tree234.contains(InconsequentialOptionsSet, Option)
    ;
        ( OptionData = special
        ; OptionData = bool_special
        ; OptionData = int_special
        ; OptionData = string_special
        ; OptionData = maybe_string_special
        ; OptionData = file_special
        ),
        % There is no point hashing special options as the option_data is
        % always the same.
        fail
    ).

:- pred compare_hash_file(io.text_output_stream::in, globals::in,
    string::in, string::in, bool::out, io::di, io::uo) is det.

compare_hash_file(ProgressStream, Globals, FileName, Hash, Same, !IO) :-
    io.open_input(FileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.read_line_as_string(Stream, ReadResult, !IO),
        (
            ReadResult = ok(Line),
            ( if Line = Hash then
                Same = yes
            else
                Same = no
            )
        ;
            ReadResult = eof,
            Same = no
        ;
            ReadResult = error(_),
            Same = no
        ),
        io.close_input(Stream, !IO)
    ;
        OpenResult = error(_),
        % Probably missing file.
        Same = no
    ),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        Verbose = yes,
        (
            Same = yes,
            io.format(ProgressStream, "%% %s does not need updating.\n",
                [s(FileName)], !IO)
        ;
            Same = no,
            io.format(ProgressStream, "%% %s will be UPDATED.\n",
                [s(FileName)], !IO)
        )
    ;
        Verbose = no
    ).

:- pred write_hash_file(io.text_output_stream::in, string::in, string::in,
    maybe_succeeded::out, io::di, io::uo) is det.

write_hash_file(ProgressStream, FileName, Hash, Succeeded, !IO) :-
    io.open_output(FileName, OpenResult, !IO),
    (
        OpenResult = ok(Stream),
        io.write_string(Stream, Hash, !IO),
        io.close_output(Stream, !IO),
        Succeeded = succeeded
    ;
        OpenResult = error(Error),
        io.format(ProgressStream, "Error creating `%s': %s\n",
            [s(FileName), s(io.error_message(Error))], !IO),
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
:- end_module make.track_flags.
%---------------------------------------------------------------------------%
