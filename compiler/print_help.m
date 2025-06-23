%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: print_help.m.
% Main author: zs.

:- module libs.print_help.
:- interface.

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type print_what_help
    --->    print_public_help
    ;       print_public_and_private_help.

    % Display short usage message.
    %
:- pred short_usage(io.text_output_stream::in, io::di, io::uo) is det.

    % Display long usage message for help.
    %
:- pred long_usage(io.text_output_stream::in, print_what_help::in,
    io::di, io::uo) is det.

:- pred write_copyright_notice(io.text_output_stream::in, io::di, io::uo)
    is det.

%---------------------------------------------------------------------------%

:- pred list_optimization_options(io.text_output_stream::in, maybe(int)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.optdb_help.
:- import_module libs.optimization_options.
:- import_module libs.options.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

:- mutable(already_printed_usage, bool, no, ground,
    [untrailed, attach_to_io_state]).

short_usage(ProgressStream, !IO) :-
    % short_usage is called from many places; ensure that we don't print the
    % duplicate copies of the message.
    % XXX The above doesn't seem to be true anymore.
    get_already_printed_usage(AlreadyPrinted, !IO),
    (
        AlreadyPrinted = no,
        io.write_strings(ProgressStream, [
            "Usage: mmc [<options>] <arguments>\n",
            "Use `mmc --help' for more information.\n"
        ], !IO),
        set_already_printed_usage(yes, !IO)
    ;
        AlreadyPrinted = yes
    ).

long_usage(ProgressStream, What, !IO) :-
    % long_usage is called from only one place, so can't print duplicate
    % copies of the long usage message. We can print both a short and along
    % usage message, but there is no simple way to avoid that.
    Lines = [compiler_id_line | copyright_notice_lines] ++
        long_usage_header_lines,
    io.write_strings(ProgressStream, Lines, !IO),
    options_help(ProgressStream, What, !IO).

write_copyright_notice(Stream, !IO) :-
    io.write_strings(Stream, copyright_notice_lines, !IO).

%---------------------------------------------------------------------------%

:- func compiler_id_line = string.

compiler_id_line = "Name: mmc - Melbourne Mercury Compiler\n".

:- func copyright_notice_lines = list(string).

copyright_notice_lines = [
    "Copyright (C) 1993-2012 The University of Melbourne\n",
    "Copyright (C) 2013-2025 The Mercury team\n"
].

:- func long_usage_header_lines = list(string).

long_usage_header_lines = [
    "Usage: mmc [<options>] <arguments>\n",
    "\n",
    "Arguments:\n",
    "    Arguments ending in `.m' are assumed to be source file names.\n",
    "    Arguments that do not end in `.m' are assumed to be module names.\n",
    "    Arguments of the form `@file' are replaced with " ++
        "the contents of the file.\n",
    "\n",
    "Options:\n"
].

%---------------------------------------------------------------------------%

:- pred options_help(io.text_output_stream::in, print_what_help::in,
    io::di, io::uo) is det.

options_help(Stream, What, !IO) :-
    % We check whether we have covered all possible option categories
    %
    % - getting a set containing all of those categories;
    % - deleting each category from the set as it is handled; and then
    % - checking that what remains is the empty set.
    CategoryPred =
        ( pred(Cat::out) is multi :-
            option_categories(Cat, _)
        ),
    solutions_set(CategoryPred, AllCategoriesSet),
    list.foldl2(acc_help_chapter(What), all_chapters,
        AllCategoriesSet, UndoneCategoriesSet, cord.init, LineCord),
    set.to_sorted_list(UndoneCategoriesSet, UndoneCategories),
    (
        UndoneCategories = []
    ;
        UndoneCategories = [_ | _],
        unexpected($pred, "undone: " ++ string(UndoneCategories))
    ),
    Lines = cord.list(LineCord),
    write_lines(Stream, Lines, !IO).

%---------------------------------------------------------------------------%

:- type help_chapter
    --->    one_level_chapter(
                chapter_section             :: help_section
            )
    ;       two_level_chapter(
                chapter_name                :: string,
                chapter_comment_lines       :: list(string),
                chapter_sections            :: list(help_section)
            ).

:- type help_section
    --->    help_section(
                section_name                :: string,
                section_comment_lines       :: list(string),
                section_categories          :: list(option_category)
            ).

%---------------------------------------------------------------------------%

:- func all_chapters = list(help_chapter).

all_chapters = AllChapters :-
    ChapterHelp = one_level_chapter(
        help_section("Help options",
        [], [oc_help])),

    ChapterCmdLine = one_level_chapter(
        help_section("Options for modifying the command line",
        [], [oc_cmdline])),

    ChapterOpmode = one_level_chapter(
        help_section("Options that give the compiler its overall task",
        [], [oc_opmode])),

    ChapterGrade = one_level_chapter(
        help_section("Grade options",
        [], [oc_grade])),

    ChapterInfer = one_level_chapter(
        help_section("Options that control inference",
        [], [oc_infer])),

    ChapterSemantics = one_level_chapter(
        help_section("Options specifying the intended semantics",
        [], [oc_semantics])),

    ChapterVerbosity = one_level_chapter(
        help_section("Verbosity options",
        [], [oc_verbosity, oc_verb_dev, oc_verb_dbg])),

    SectionDiagGen = help_section("Options that control diagnostics",
        [], [oc_diag_gen]),
    SectionDiagColor = help_section(
        "Options that control color in diagnostics",
        [], [oc_diag_color, oc_diag_int]),
    ChapterDiag = two_level_chapter("Diagnostics options",
        [],
        [SectionDiagGen, SectionDiagColor]),

    SectionWarnDodgy = help_section("Warnings about possible incorrectness",
        [], [oc_warn_dodgy]),
    SectionWarnPerf = help_section(
        "Warnings about possible performance issues",
        [], [oc_warn_perf, oc_warn_perf_c]),
    SectionWarnStyle = help_section("Warnings about programming style",
        [], [oc_warn_style, oc_warn_style_c]),
    SectionWarnCtrl = help_section("Options that control warnings",
        [], [oc_warn_ctrl]),
    SectionWarnHalt = help_section("Options about halting for warnings",
        [], [oc_warn_halt]),
    ChapterWarn = two_level_chapter("Warning options",
        [],
        [SectionWarnDodgy, SectionWarnPerf, SectionWarnStyle,
        SectionWarnCtrl, SectionWarnHalt]),

    ChapterInform = one_level_chapter(
        help_section("Options that request information",
        [], [oc_inform])),

    % ZZZ cf ChapterInform
    ChapterFileReq = one_level_chapter(
        help_section("Options that ask for informational files",
        [], [oc_file_req])),

    ChapterTraceGoal = one_level_chapter(
        help_section("Controlling trace goals",
        [], [oc_tracegoal])),

    ChapterDebug = one_level_chapter(
        help_section("Preparing code for mdb debugging",
        [], [oc_mdb, oc_mdb_dev])),

    ChapterProfiling = one_level_chapter(
        help_section("Preparing code for mdprof profiling",
        [], [oc_mdprof])),

    SectionOptCtrl = help_section("Overall control of optimizations",
        [], [oc_opt_ctrl]),
    SectionOptHH = help_section("Source-to-source optimizations",
        [], [oc_opt_hh]),
    SectionOptHHE = help_section("Experimental source-to-Source optimizations",
        [], [oc_opt_hh_exp]),
    SectionOptHLM = help_section("Optimizations during code generation",
        [], [oc_opt_hlm]),
    % ZZZ should these be separate sections?
    SectionOptMM = help_section("Optimizations specific to high level code",
        [], [oc_opt_hm, oc_opt_mm]),
    % ZZZ should these be separate sections?
    SectionOptLL = help_section("Optimizations specific to low level code",
        [], [oc_opt_hl, oc_opt_ll, oc_opt_lc]),
    ChapterOpt = two_level_chapter("Optimization options",
        [], [SectionOptCtrl, SectionOptHH, SectionOptHHE, SectionOptHLM,
            SectionOptMM, SectionOptLL]),

    ChapterTransOpt = one_level_chapter(
        help_section(
            "Options that control transitive intermodule optimization",
        [], [oc_trans_opt])),

    ChapterAnalysis = one_level_chapter(
        help_section("Options that control program analyses",
        [], [oc_analysis])),

    ChapterModOutput = one_level_chapter(
        help_section("Options that ask for modified output",
        [], [oc_output_mod, oc_output_dev])),

    ChapterMmcMake = one_level_chapter(
        help_section("Options for controlling mmc --make",
        [], [oc_make])),

    SectionCompileGen = help_section(
        "General options for compiling target language code",
        [], [oc_target_comp]),
    SectionCompileC = help_section("Options for compiling C code",
        [], [oc_target_c]),
    SectionCompileJava = help_section("Options for compiling Java code",
        [], [oc_target_java]),
    SectionCompileCsharp = help_section("Options for compiling C# code",
        [], [oc_target_csharp]),
    ChapterCompile = two_level_chapter(
        "Options for target language compilation",
        [], [SectionCompileGen, SectionCompileC, SectionCompileJava,
            SectionCompileCsharp]),

    SectionLinkGen = help_section("General options for linking",
        [], [oc_link_c_cs_j]),
    SectionLinkCCsharp = help_section("Options for linking C or C# code",
        [], [oc_link_c_cs]),
    SectionLinkC = help_section("Options for linking just C code",
        [], [oc_link_c]),
    SectionLinkCsharp = help_section("Options for linking just C# code",
        [], [oc_link_csharp]),
    SectionLinkJava = help_section("Options for linking just Java code",
        [], [oc_link_java]),
    ChapterLink = two_level_chapter(
        "Options for linking",
        [], [SectionLinkGen, SectionLinkCCsharp, SectionLinkC, SectionLinkJava,
            SectionLinkCsharp]),

    ChapterFileSearch = one_level_chapter(
        help_section("Options controlling searches for files",
        [], [oc_search])),

    ChapterBuild = one_level_chapter(
        help_section("Options controlling the library installation process",
        [], [oc_buildsys])),

    ChapterEnv = one_level_chapter(
        help_section("Options specifying properties of the environment",
        [], [oc_env])),

    ChapterConfig = one_level_chapter(
        help_section("Options that record autoconfigured parameters",
        [], [oc_config])),

    ChapterMconfig = one_level_chapter(
        help_section("Options reserved for Mercury.config files",
        [], [oc_mconfig])),

    SectionDevCtrl = help_section(
        "Operation selection options for developers only",
        [], [oc_dev_ctrl]),
    SectionDevDebug = help_section(
        "Options that can help debug the compiler",
        [], [oc_dev_debug]),
    SectionDevDump = help_section(
        "Options for dumping internal compiler data structures",
        [], [oc_dev_dump]),
    SectionDevInternal =
        help_section("Options intended for internal use by the compiler only",
        [], [oc_internal]),
    ChapterDev = two_level_chapter(
        "Options for developers only",
        [], [SectionDevCtrl, SectionDevDebug, SectionDevDump,
            SectionDevInternal]),

    ChapterUnused = one_level_chapter(
        help_section("Now-unused former options kept for compatibility",
        [], [oc_unused])),

    AllChapters = [
        ChapterHelp,
        ChapterCmdLine,
        ChapterOpmode,
        ChapterGrade,
        ChapterInfer,
        ChapterSemantics,
        ChapterVerbosity,

        ChapterDiag,
        ChapterWarn,
        ChapterInform,
        ChapterFileReq,

        ChapterTraceGoal,
        ChapterDebug,
        ChapterProfiling,

        ChapterOpt,
        ChapterTransOpt,
        ChapterAnalysis,

        ChapterModOutput,
        ChapterMmcMake,

        ChapterCompile,
        ChapterLink,
        ChapterFileSearch,
        ChapterBuild,

        ChapterEnv,
        ChapterConfig,
        ChapterMconfig,

        ChapterDev,
        ChapterUnused
    ].

%---------------------------------------------------------------------------%

:- pred acc_help_chapter(print_what_help::in, help_chapter::in,
    set(option_category)::in, set(option_category)::out,
    cord(string)::in, cord(string)::out) is det.

acc_help_chapter(What, HelpChapter, !Categories, !LineCord) :-
    (
        HelpChapter = one_level_chapter(HelpSection),
        acc_help_section(What, "", HelpSection, !Categories, !LineCord)
    ;
        HelpChapter = two_level_chapter(ChapterName, ChapterCommentLines,
            SubSections),
        list.foldl2(acc_help_section(What, option_name_indent),
            SubSections, !Categories, cord.init, SectionsLineCord),
        ( if cord.is_empty(SectionsLineCord) then
            true
        else
            cord.snoc("", !LineCord),
            cord.snoc(ChapterName, !LineCord),
            (
                ChapterCommentLines = []
            ;
                ChapterCommentLines = [_ | _],
                cord.snoc("", !LineCord),
                list.foldl(acc_prefixed_line(""),
                    ChapterCommentLines, !LineCord)
            ),
            !:LineCord = !.LineCord ++ SectionsLineCord
        )
    ).

:- pred acc_help_section(print_what_help::in, string::in, help_section::in,
    set(option_category)::in, set(option_category)::out,
    cord(string)::in, cord(string)::out) is det.

acc_help_section(What, SectionNameIndent, HelpSection,
        !Categories, !LineCord) :-
    HelpSection = help_section(SectionName, SectionCommentLines,
        SectionCategories),
    set.det_remove_list(SectionCategories, !Categories),
    list.map(get_optdb_records_in_category,
        SectionCategories, OptdbRecordSets),
    OptdbRecordSet = set.union_list(OptdbRecordSets),

    list.foldl(acc_help_message(What), set.to_sorted_list(OptdbRecordSet),
        cord.init, HelpTextLinesCord),
    ( if cord.is_empty(HelpTextLinesCord) then
        true
    else
        cord.snoc("", !LineCord),
        cord.snoc(SectionNameIndent ++ SectionName, !LineCord),
        (
            SectionCommentLines = []
        ;
            SectionCommentLines = [_ | _],
            cord.snoc("", !LineCord),
            list.foldl(acc_prefixed_line(SectionNameIndent),
                SectionCommentLines, !LineCord)
        ),
        !:LineCord = !.LineCord ++ HelpTextLinesCord
    ).

:- pred get_optdb_records_in_category(option_category::in,
    set(optdb_record)::out) is det.

get_optdb_records_in_category(Cat, OptdbRecordSet) :-
    OptdbPred =
        ( pred(OptdbRecord::out) is multi :-
            optdb(Cat, Opt, OptData, Help),
            OptdbRecord = optdb_record(Cat, Opt, OptData, Help)
        ),
    solutions_set(OptdbPred, OptdbRecordSet).

%---------------------------------------------------------------------------%

:- type optdb_record
    --->    optdb_record(
                option_category,
                option,
                option_data,
                libs.optdb_help.help
            ).

:- type maybe_expect_arg
    --->    do_not_expect_arg
    ;       expect_arg.

:- type maybe_negate
    --->    do_not_negate
    ;       negate.

:- type maybe_add_negative
    --->    no_negative_version
    ;       add_negative_version.

:- type option_params
    --->    option_params(
                op_expect       :: maybe_expect_arg,
                op_negate       :: maybe_negate,
                op_add_negative :: maybe_add_negative
            ).

%---------------------------------------------------------------------------%

:- pred get_optdb_record_params(optdb_record::in, option_params::out) is det.

get_optdb_record_params(OptdbRecord, Params) :-
    OptdbRecord = optdb_record(_Cat, _Option, OptionData, _Help),
    (
        OptionData = bool(Bool),
        MaybeExpectArg = do_not_expect_arg,
        ( Bool = no,  MaybeNegate = do_not_negate
        ; Bool = yes, MaybeNegate = negate
        ),
        MaybeAddNegVersion = no_negative_version
    ;
        OptionData = bool_special,
        MaybeExpectArg = do_not_expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersion = no_negative_version
    ;
        ( OptionData = int(_)
        ; OptionData = string(_)
        ; OptionData = int_special
        ; OptionData = string_special
        ),
        MaybeExpectArg = expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersion = no_negative_version
    ;
        OptionData = special,
        MaybeExpectArg = do_not_expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersion = no_negative_version
    ;
        OptionData = file_special,
        MaybeExpectArg = expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersion = no_negative_version
    ;
        ( OptionData = accumulating(_)
        ; OptionData = maybe_int(_)
        ; OptionData = maybe_string(_)
        ; OptionData = maybe_string_special
        ),
        MaybeExpectArg = expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersion = add_negative_version
    ),
    Params = option_params(MaybeExpectArg, MaybeNegate, MaybeAddNegVersion).

%---------------------------------------------------------------------------%

:- pred acc_help_message(print_what_help::in, optdb_record::in,
    cord(string)::in, cord(string)::out) is det.

acc_help_message(What, OptdbRecord, !EffectiveLinesCord) :-
    get_optdb_record_params(OptdbRecord, Params),
    % XXX We could automatically add "(This option is not for general use.)"
    % to the start of the description of every private option, to save
    % the repetition of including it in help_private structures.
    %
    % We currently handle this message quite badly. First, we do not include it
    % in many help_private structures (which, to be fair, won't matter
    % until we implement callers that specify print_public_and_private_help.
    % Second, we *do* include it in a few help_public structures, in which
    % cases it gives non-developer readers useless information. To make
    % the message useful, the message would have to say *in what situations*
    % the option may be relevant to non-developers.
    OptdbRecord = optdb_record(_Cat, Option, OptionData, Help),
    some [!LineCord]
    (
        !:LineCord = cord.init,
        (
            Help = no_help,
            PublicOrPrivate = help_private,
            DescLines = []
        ;
            Help = xunnamed_help(DescLines),
            % XXX It is quite likely that many options that do not have entries
            % in the long_table predicate, which therefore should be in optdb
            % with unnamed_help, are there with some other help structure,
            % such as priv_help.
            PublicOrPrivate = help_private,
            string.format("%sUNNAMED OPTION %s",
                [s(option_name_indent), s(string(Option))], NameLine),
            cord.snoc(NameLine, !LineCord)
        ;
            Help = xgen_help(ShortNames, LongName, AltLongNames,
                PublicOrPrivate, DescLines),
            acc_short_option_names(Params, Option, no_arg, no_align,
                ShortNames, !LineCord),
            acc_long_option_name(Params, Option, no_arg, no_align,
                LongName, !LineCord),
            acc_long_option_names(Params, Option, no_arg, no_align,
                AltLongNames, !LineCord)
        ;
            (
                Help = xhelp(LongName, DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_public
            ;
                Help = xarg_help(LongName, ArgName, DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_public
            ;
                Help = xpriv_help(LongName, DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_private
            ;
                Help = xpriv_arg_help(LongName, ArgName, DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_private
            ),
            acc_long_option_name(Params, Option, MaybeArg, no_align,
                LongName, !LineCord)
        ;
            (
                Help = xalt_help(LongName, AltLongNames, DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_public
            ;
                Help = xalt_arg_help(LongName, AltLongNames, ArgName,
                    DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_public
            ;
                Help = xpriv_alt_help(LongName, AltLongNames, DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_private
            ;
                Help = xpriv_alt_arg_help(LongName, AltLongNames, ArgName,
                    DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_private
            ),
            acc_long_option_name(Params, Option, MaybeArg, no_align,
                LongName, !LineCord),
            acc_long_option_names(Params, Option, MaybeArg, no_align,
                AltLongNames, !LineCord)
        ;
            (
                Help = xshort_help(ShortName, LongName, AltLongNames,
                    DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_public
            ;
                Help = xshort_arg_help(ShortName, LongName, AltLongNames,
                    ArgName, DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_public
            ;
                Help = xpriv_short_help(ShortName, LongName, AltLongNames,
                    DescLines),
                MaybeArg = no_arg,
                PublicOrPrivate = help_private
            ;
                Help = xpriv_short_arg_help(ShortName, LongName, AltLongNames,
                    ArgName, DescLines),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_private
            ),
            acc_short_option_name(Params, Option, MaybeArg, no_align,
                ShortName, !LineCord),
            acc_long_option_name(Params, Option, MaybeArg, no_align,
                LongName, !LineCord),
            acc_long_option_names(Params, Option, MaybeArg, no_align,
                AltLongNames, !LineCord)
        ;
            (
                Help = xalt_align_help(LongName, AltLongNames,
                    AlignedText, DescLines),
                PublicOrPrivate = help_public
            ;
                Help = xpriv_alt_align_help(LongName, AltLongNames,
                    AlignedText, DescLines),
                PublicOrPrivate = help_private
            ),
            MaybeArg = no_arg,
            Align = aligned_text(AlignedText),
            acc_long_option_name(Params, Option, MaybeArg, Align,
                LongName, !LineCord),
            % The aligned text is added only to the first option name line.
            acc_long_option_names(Params, Option, MaybeArg, no_align,
                AltLongNames, !LineCord)
        ;
            Help = xshort_alt_align_help(ShortName, LongName, AltLongNames,
                AlignedText, DescLines),
            PublicOrPrivate = help_public,
            acc_short_option_name(Params, Option, no_arg,
                aligned_text(AlignedText), ShortName, !LineCord),
            % The aligned text is added only to the first option name line.
            acc_long_option_name(Params, Option, no_arg, no_align,
                LongName, !LineCord),
            acc_long_option_names(Params, Option, no_arg, no_align,
                AltLongNames, !LineCord)
        ;
            Help = xno_align_help(LongName, AlignedText, NoAlignedText,
                DescLines),
            PublicOrPrivate = help_public,
            expect(is_bool(OptionData), $pred,
                "unexpected use of xno_align_help"),
            ParamsNN = Params ^ op_negate := do_not_negate,
            FirstLine0 = long_option_name_line(ParamsNN, Option, no_arg,
                LongName),
            SecondLine0 = long_negated_option_name_line(LongName),
            % In this case, we add *different* aligned text to each line.
            add_aligned_text(AlignedText, FirstLine0, FirstLine),
            add_aligned_text(NoAlignedText, SecondLine0, SecondLine),
            cord.snoc(FirstLine, !LineCord),
            cord.snoc(SecondLine, !LineCord)
        ;
            Help = xalt_arg_align_help(LongName, ArgAligns, DescLines),
            PublicOrPrivate = help_public,
            % In this case, we add *different* aligned text to each line.
            list.foldl(acc_arg_align_text(Params, Option, LongName),
                ArgAligns, !LineCord)
        ),
        ( if
            (
                PublicOrPrivate = help_public
            ;
                PublicOrPrivate = help_private,
                What = print_public_and_private_help
            )
        then
            ( if
                cord.is_empty(!.LineCord),
                DescLines = []
            then
                true
            else
                DescPrefix = option_desc_indent,
                (
                    DescLines = [],
                    acc_prefixed_line(DescPrefix,
                        "There is no help text available.", !LineCord)
                ;
                    DescLines = [_ | _],
                    % ZZZ 71
                    reflow_lines(71, DescLines, ReflowLines),
                    list.foldl(acc_prefixed_line(DescPrefix), ReflowLines,
                        !LineCord)
                ),
                BlankLineCord = cord.singleton(""),
                (
                    PublicOrPrivate = help_public,
                    PrivatePrefixCord = cord.init
                ;
                    PublicOrPrivate = help_private,
                    PrivatePrefixCord =
                        cord.singleton(option_name_indent ++ "PRIVATE OPTION")
                ),
                !:EffectiveLinesCord = !.EffectiveLinesCord ++
                    BlankLineCord ++ PrivatePrefixCord ++ !.LineCord
            )
        else
            true
        )
    ).

:- pred acc_arg_align_text(option_params::in, option::in, string::in,
    arg_align::in, cord(string)::in, cord(string)::out) is det.

acc_arg_align_text(Params, Option, LongName, ArgAlign, !LineCord) :-
    Params = option_params(MaybeExpectArg, MaybeNegate, MaybeAddNegVersion),
    expect(unify(MaybeExpectArg, expect_arg), $pred,
        "unexpected MaybeExpectArg"),
    expect(unify(MaybeNegate, do_not_negate), $pred,
        "unexpected MaybeNegate"),
    expect(unify(MaybeAddNegVersion, no_negative_version), $pred,
        "unexpected MaybeAddNegVersion"),

    ArgAlign = arg_align(ArgName, AlignedText),
    Line0 = long_option_name_line(Params, Option, arg_name(ArgName), LongName),
    add_aligned_text(AlignedText, Line0, Line),
    cord.snoc(Line, !LineCord).

:- pred is_bool(option_data::in) is semidet.

is_bool(bool(_)).

%---------------------------------------------------------------------------%

:- type maybe_arg_name
    --->    no_arg
    ;       arg_name(string).

:- type maybe_aligned_text
    --->    no_align
    ;       aligned_text(string).

% The next two predicates next are needed because folds over
% acc_{long,short}_option_name do not preserve the inst of the
% maybe_aligned_text argument.

:- pred acc_long_option_names(option_params, option, maybe_arg_name,
    maybe_aligned_text, list(string), cord(string), cord(string)).
:- mode acc_long_option_names(in, in, in, in(bound(no_align)),
    in, in, out) is det.
:- mode acc_long_option_names(in, in, in, in(bound(aligned_text(ground))),
    in, in, out) is det.

acc_long_option_names(_, _, _, _, [], !LineCord).
acc_long_option_names(Params, Option, MaybeArgName, MaybeAlignedText,
        [LongName | LongNames], !LineCord) :-
    acc_long_option_name(Params, Option, MaybeArgName, MaybeAlignedText,
        LongName, !LineCord),
    acc_long_option_names(Params, Option, MaybeArgName, MaybeAlignedText,
        LongNames, !LineCord).

:- pred acc_short_option_names(option_params, option, maybe_arg_name,
    maybe_aligned_text, list(char), cord(string), cord(string)).
:- mode acc_short_option_names(in, in, in, in(bound(no_align)),
    in, in, out) is det.
:- mode acc_short_option_names(in, in, in, in(bound(aligned_text(ground))),
    in, in, out) is det.

acc_short_option_names(_, _, _, _, [], !LineCord).
acc_short_option_names(Params, Option, MaybeArgName, MaybeAlignedText,
        [ShortName | ShortNames], !LineCord) :-
    acc_short_option_name(Params, Option, MaybeArgName, MaybeAlignedText,
        ShortName, !LineCord),
    acc_short_option_names(Params, Option, MaybeArgName, MaybeAlignedText,
        ShortNames, !LineCord).

%---------------------------------------------------------------------------%

:- pred acc_long_option_name(option_params, option, maybe_arg_name,
    maybe_aligned_text, string, cord(string), cord(string)).
:- mode acc_long_option_name(in, in, in, in(bound(no_align)),
    in, in, out) is det.
:- mode acc_long_option_name(in, in, in, in(bound(aligned_text(ground))),
    in, in, out) is det.

acc_long_option_name(Params, Option, MaybeArgName, MaybeAlignedText,
        LongName, !LineCord) :-
    FirstLine0 = long_option_name_line(Params, Option, MaybeArgName, LongName),
    (
        MaybeAlignedText = no_align,
        FirstLine = FirstLine0
    ;
        MaybeAlignedText = aligned_text(AlignedText),
        add_aligned_text(AlignedText, FirstLine0, FirstLine)
    ),
    cord.snoc(FirstLine, !LineCord),
    Params = option_params(_MaybeExpectArg, _MaybeNegate, MaybeAddNegVersion),
    (
        MaybeAddNegVersion = no_negative_version
    ;
        MaybeAddNegVersion = add_negative_version,
        SecondLine = long_negated_option_name_line(LongName),
        cord.snoc(SecondLine, !LineCord)
    ).

:- pred acc_short_option_name(option_params, option, maybe_arg_name,
    maybe_aligned_text, char, cord(string), cord(string)).
:- mode acc_short_option_name(in, in, in, in(bound(no_align)),
    in, in, out) is det.
:- mode acc_short_option_name(in, in, in, in(bound(aligned_text(ground))),
    in, in, out) is det.

acc_short_option_name(Params, Option, MaybeArgName, MaybeAlignedText,
        ShortName, !LineCord) :-
    FirstLine0 = short_option_name_line(Params, Option, MaybeArgName,
        ShortName),
    (
        MaybeAlignedText = no_align,
        FirstLine = FirstLine0
    ;
        MaybeAlignedText = aligned_text(AlignedText),
        add_aligned_text(AlignedText, FirstLine0, FirstLine)
    ),
    cord.snoc(FirstLine, !LineCord),
    Params = option_params(_MaybeExpectArg, _MaybeNegate, MaybeAddNegVersion),
    (
        MaybeAddNegVersion = no_negative_version
    ;
        MaybeAddNegVersion = add_negative_version,
        SecondLine = short_negated_option_name_line(ShortName),
        cord.snoc(SecondLine, !LineCord)
    ).

%---------------------%

:- func long_option_name_line(option_params, option, maybe_arg_name, string)
    = string.

long_option_name_line(Params, Option, MaybeArgName, LongName0) = Line :-
    Indent = option_name_indent,
    Params = option_params(MaybeExpectArg, MaybeNegate, _MaybeAddNegVersion),
    (
        MaybeNegate = negate,
        maybe_have_arg(MaybeExpectArg, Option, MaybeArgName,
            LongName0, LongName),
        Line = long_negated_option_name_line(LongName)
    ;
        MaybeNegate = do_not_negate,
        (
            MaybeArgName = no_arg,
            have_no_arg(MaybeExpectArg, Option, LongName0, LongName),
            string.format("%s--%s", [s(Indent), s(LongName)], Line)
        ;
            MaybeArgName = arg_name(ArgName),
            have_arg(MaybeExpectArg, Option, LongName0, LongName),
            MaybeWrappedArgName = maybe_wrap_arg_name(Option, ArgName),
            string.format("%s--%s %s",
                [s(Indent), s(LongName), s(MaybeWrappedArgName)], Line)
        )
    ).

:- func short_option_name_line(option_params, option, maybe_arg_name, char)
    = string.

short_option_name_line(Params, Option, MaybeArgName, ShortName0) = Line :-
    Indent = option_name_indent,
    Params = option_params(MaybeExpectArg, MaybeNegate, _MaybeAddNegVersion),
    (
        MaybeNegate = negate,
        maybe_have_arg(MaybeExpectArg, Option, MaybeArgName,
            ShortName0, ShortName),
        Line = short_negated_option_name_line(ShortName)
    ;
        MaybeNegate = do_not_negate,
        (
            MaybeArgName = no_arg,
            have_no_arg(MaybeExpectArg, Option, ShortName0, ShortName),
            string.format("%s-%c", [s(Indent), c(ShortName)], Line)
        ;
            MaybeArgName = arg_name(ArgName),
            have_arg(MaybeExpectArg, Option, ShortName0, ShortName),
            MaybeWrappedArgName = maybe_wrap_arg_name(Option, ArgName),
            string.format("%s-%c %s",
                [s(Indent), c(ShortName), s(MaybeWrappedArgName)], Line)
        )
    ).

%---------------------%

:- func long_negated_option_name_line(string) = string.

long_negated_option_name_line(LongName) = Line :-
    Indent = option_name_indent,
    string.format("%s--no-%s", [s(Indent), s(LongName)], Line).

:- func short_negated_option_name_line(char) = string.

short_negated_option_name_line(ShortName) = Line :-
    Indent = option_name_indent,
    string.format("%s-%c-", [s(Indent), c(ShortName)], Line).

%---------------------%

:- func maybe_wrap_arg_name(option, string) = string.

maybe_wrap_arg_name(Option, ArgName) = MaybeWrappedArgName :-
    ( if
        ArgName = ""
    then
        unexpected($pred, string(Option) ++ " has empty arg name")
    else if
        % Do not put <>s around argument "names" that are actually not names,
        % but instead are either
        %
        % - sets of the allowed values wrapped in {}s, or
        % - default optimization levels, such as -O2.
        ( string.find_first_char(ArgName, '{', _)
        ; string.find_first_char(ArgName, '-', _)
        )
    then
        MaybeWrappedArgName = ArgName
    else
        MaybeWrappedArgName = "<" ++ ArgName ++ ">"
    ).

%---------------------------------------------------------------------------%

:- pred maybe_have_arg(maybe_expect_arg::in, option::in, maybe_arg_name::in,
    T::in, T::out) is det.

maybe_have_arg(MaybeExpectArg, Option, MaybeArgName,
        OptionName0, OptionName) :-
    (
        MaybeArgName = no_arg,
        have_no_arg(MaybeExpectArg, Option, OptionName0, OptionName)
    ;
        MaybeArgName = arg_name(_),
        have_arg(MaybeExpectArg, Option, OptionName0, OptionName)
    ).

:- pred have_no_arg(maybe_expect_arg::in, option::in, T::in, T::out) is det.

have_no_arg(MaybeExpectArg, Option, OptionName0, OptionName) :-
    (
        MaybeExpectArg = do_not_expect_arg,
        OptionName = OptionName0
    ;
        MaybeExpectArg = expect_arg,
        string.format("missing arg for %s", [s(string(Option))], Msg),
        unexpected($pred, Msg)
    ).

:- pred have_arg(maybe_expect_arg::in, option::in, T::in, T::out) is det.

have_arg(MaybeExpectArg, Option, OptionName0, OptionName) :-
    (
        MaybeExpectArg = do_not_expect_arg,
        string.format("unexpected arg for %s", [s(string(Option))], Msg),
        unexpected($pred, Msg)
    ;
        MaybeExpectArg = expect_arg,
        OptionName = OptionName0
    ).

%---------------------------------------------------------------------------%

:- pred add_aligned_text(string::in, string::in, string::out) is det.

add_aligned_text(AlignedText, Line0, Line) :-
    string.format("%-39s %s", [s(Line0), s(AlignedText)], Line).

%---------------------------------------------------------------------------%

:- func option_name_indent = string.
:- func option_desc_indent = string.

option_name_indent = "    ".
option_desc_indent = "        ".

:- pred acc_prefixed_line(string::in, string::in,
    cord(string)::in, cord(string)::out) is det.

acc_prefixed_line(Prefix, LineBody, !LineCord) :-
    Line = Prefix ++ LineBody,
    cord.snoc(Line, !LineCord).

%---------------------------------------------------------------------------%

:- pred reflow_lines(int::in, list(string)::in, list(string)::out) is det.

reflow_lines(LineLen, InitialLines, FinishedLines) :-
    % string.count_code_points(IndentStr, IndentLen),
    % AvailLen = LineLen - IndentLen,
    reflow_lines_loop_over_lines(LineLen, cord.init, 0, InitialLines,
        cord.init, FinishedLineCord),
    FinishedLines = cord.list(FinishedLineCord).

    % The pieces of the current line.
    % This will NOT contain the initial indent string.
    % This WILL contain both the words put on this line so far, *and*
    % the spaces between them.
:- type cur_line == cord(string).

    % The reflowed lines we have already constructed.
:- type finished_lines == cord(string).

:- pred reflow_lines_loop_over_lines(int::in, cur_line::in, int::in,
    list(string)::in, finished_lines::in, finished_lines::out) is det.

reflow_lines_loop_over_lines(LineLen, !.CurLine, !.CurLineLen, Lines,
        !FinishedLineCord) :-
    (
        Lines = [],
        finish_cur_line(!.CurLine, !FinishedLineCord)
    ;
        Lines = [HeadLine | TailLines0],
        ( if HeadLine = "QUOTE" then
            % Delete the "QUOTE" word, and do NOT break the next line, if any.
            (
                TailLines0 = [],
                TailLines = []
            ;
                TailLines0 = [ProtectedLine | TailLines],
                add_word(LineLen, !CurLine, !CurLineLen, ProtectedLine,
                    !FinishedLineCord)
            )
        else
            TailLines = TailLines0,
            HeadLineWords = string.words(HeadLine),
            reflow_lines_loop_over_words(LineLen, !CurLine, !CurLineLen,
                HeadLineWords, !FinishedLineCord)
        ),
        reflow_lines_loop_over_lines(LineLen, !.CurLine, !.CurLineLen,
            TailLines, !FinishedLineCord)
    ).

:- pred reflow_lines_loop_over_words(int::in, cur_line::in, cur_line::out,
    int::in, int::out, list(string)::in,
    finished_lines::in, finished_lines::out) is det.

reflow_lines_loop_over_words(LineLen, !CurLine, !CurLineLen, Words,
        !FinishedLineCord) :-
    (
        Words = []
    ;
        Words = [HeadWord | TailWords],
        add_word(LineLen, !CurLine, !CurLineLen, HeadWord, !FinishedLineCord),
        reflow_lines_loop_over_words(LineLen, !CurLine, !CurLineLen,
            TailWords, !FinishedLineCord)
    ).

:- pred add_word(int::in, cur_line::in, cur_line::out, int::in, int::out,
    string::in, finished_lines::in, finished_lines::out) is det.

add_word(LineLen, !CurLine, !CurLineLen, Word, !FinishedLineCord) :-
    string.count_code_points(Word, WordLen),
    ( if !.CurLineLen = 0 then
        !:CurLine = cord.singleton(Word),
        !:CurLineLen = WordLen
    else
        NextLineLen = !.CurLineLen + 1 + WordLen,
        ( if NextLineLen =< LineLen then
            cord.snoc(" ", !CurLine),
            cord.snoc(Word, !CurLine),
            !:CurLineLen = NextLineLen
        else
            finish_cur_line(!.CurLine, !FinishedLineCord),
            !:CurLine = cord.singleton(Word),
            !:CurLineLen = WordLen
        )
    ).

:- pred finish_cur_line(cur_line::in,
    finished_lines::in, finished_lines::out) is det.

finish_cur_line(CurLine, !FinishedLineCord) :-
    FinishedLine = string.append_list(cord.list(CurLine)),
    ( if FinishedLine = "" then
        true
    else
        cord.snoc(FinishedLine, !FinishedLineCord)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

list_optimization_options(Stream, MaybeUpTo, !IO) :-
    acc_optimization_options_loop(MaybeUpTo, 0, cord.init, LineCord),
    write_lines(Stream, cord.list(LineCord), !IO).

:- pred acc_optimization_options_loop(maybe(int)::in, int::in,
    cord(string)::in, cord(string)::out) is det.

acc_optimization_options_loop(MaybeUpTo, CurLevel, !LineCord) :-
    ( if
        (
            MaybeUpTo = no
        ;
            MaybeUpTo = yes(UpTo),
            CurLevel =< UpTo
        ),
        opts_enabled_at_level(CurLevel, LevelDescLines, DocumentedOpts)
    then
        string.format("Optimization level %d:", [i(CurLevel)],
            LevelHeading),
        string.format("%sThe options set at this level are:",
            [s(option_name_indent)], SetAtLevel),
        list.foldl(document_one_optimization_option, DocumentedOpts,
            [], OptLines),
        % Sorting makes options slightly easier to find in a list of options.
        % It also makes the output look more systematic.
        list.sort(OptLines, SortedOptLines),

        ( if CurLevel > 0 then
            cord.snoc("", !LineCord)
        else
            true
        ),
        cord.snoc(LevelHeading, !LineCord),
        (
            LevelDescLines = []
        ;
            LevelDescLines = [_ | _],
            cord.snoc("", !LineCord),
            list.foldl(acc_prefixed_line(option_name_indent), LevelDescLines,
                !LineCord)
        ),
        cord.snoc("", !LineCord),
        cord.snoc(SetAtLevel, !LineCord),
        cord.snoc("", !LineCord),
        list.foldl(acc_prefixed_line(option_desc_indent), SortedOptLines,
            !LineCord),

        acc_optimization_options_loop(MaybeUpTo, CurLevel + 1, !LineCord)
    else
        true
    ).

:- pred document_one_optimization_option(documented_optimization_option::in,
    list(string)::in, list(string)::out) is det.

document_one_optimization_option(DocOpt, !Lines) :-
    DocOpt = doc_oo(_, Option, OptionData),
    get_main_long_name(Option, MaybeLongName),
    (
        MaybeLongName = no
    ;
        MaybeLongName = yes(LongName),
        (
            OptionData = bool(Bool),
            (
                Bool = no,
                string.format("--no-%s", [s(LongName)], Line)
            ;
                Bool = yes,
                string.format("--%s", [s(LongName)], Line)
            ),
            !:Lines = [Line | !.Lines]
        ;
            OptionData = int(Int),
            string.format("--%s=%d", [s(LongName), i(Int)], Line),
            !:Lines = [Line | !.Lines]
        ;
            ( OptionData = string(_)
            ; OptionData = maybe_int(_)
            ; OptionData = maybe_string(_)
            ; OptionData = accumulating(_)
            ; OptionData = special
            ; OptionData = bool_special
            ; OptionData = int_special
            ; OptionData = string_special
            ; OptionData = maybe_string_special
            ; OptionData = file_special
            ),
            % These kinds of options are never set automatically
            % at any optimization level. Some (the special options)
            % literally *cannot* be set there.
            unexpected($pred, string(OptionData))
        )
    ).

:- pred get_main_long_name(option::in, maybe(string)::out) is det.

get_main_long_name(Option, MaybeLongName) :-
    optdb(_, Option, _, Help),
    (
        ( Help = no_help
        ; Help = xunnamed_help(_)
        ),
        MaybeLongName = no
    ;
        ( Help = xgen_help(_, LongName, _, _, _)
        ; Help = xhelp(LongName, _)
        ; Help = xarg_help(LongName, _, _)
        ; Help = xpriv_help(LongName, _)
        ; Help = xpriv_arg_help(LongName, _, _)
        ; Help = xalt_help(LongName, _, _)
        ; Help = xalt_arg_help(LongName, _, _, _)
        ; Help = xpriv_alt_help(LongName, _, _)
        ; Help = xpriv_alt_arg_help(LongName, _, _, _)
        ; Help = xshort_help(_, LongName, _, _)
        ; Help = xshort_arg_help(_, LongName, _, _, _)
        ; Help = xpriv_short_help(_, LongName, _, _)
        ; Help = xpriv_short_arg_help(_, LongName, _, _, _)
        ; Help = xalt_align_help(LongName, _, _, _)
        ; Help = xpriv_alt_align_help(LongName, _, _, _)
        ; Help = xshort_alt_align_help(_, LongName, _, _, _)
        ; Help = xno_align_help(LongName, _, _, _)
        ; Help = xalt_arg_align_help(LongName, _, _)
        ),
        MaybeLongName = yes(LongName)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred write_lines(io.text_output_stream::in, list(string)::in,
    io::di, io::uo) is det.

write_lines(_, [], !IO).
write_lines(Stream, [Line | Lines], !IO) :-
    io.write_string(Stream, Line, !IO),
    io.nl(Stream, !IO),
    write_lines(Stream, Lines, !IO).

%---------------------------------------------------------------------------%
:- end_module libs.print_help.
%---------------------------------------------------------------------------%
