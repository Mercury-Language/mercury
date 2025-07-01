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

:- pred document_options_for_users_guide(io.text_output_stream::in,
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
:- import_module libs.option_categories.
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
        ShortUsageLines = [
            "Usage: mmc [<options>] <arguments>",
            "Use `mmc --help' for more information."
        ],
        write_lines(ProgressStream, ShortUsageLines, !IO),
        set_already_printed_usage(yes, !IO)
    ;
        AlreadyPrinted = yes
    ).

long_usage(ProgressStream, What, !IO) :-
    % long_usage is called from only one place, so can't print duplicate
    % copies of the long usage message. We can print both a short and along
    % usage message, but there is no simple way to avoid that.
    HeaderLines = [compiler_id_line | copyright_notice_lines] ++
        long_usage_header_lines,
    document_requested_options(help_plain_text, What, OptionsLines),
    Lines = HeaderLines ++ OptionsLines,
    write_lines(ProgressStream, Lines, !IO).

document_options_for_users_guide(ProgressStream, !IO) :-
    document_requested_options(help_texinfo, print_public_and_private_help,
        OptionsLines),
    write_lines(ProgressStream, OptionsLines, !IO).

write_copyright_notice(Stream, !IO) :-
    write_lines(Stream, copyright_notice_lines, !IO).

%---------------------------------------------------------------------------%

:- func compiler_id_line = string.

compiler_id_line = "Name: mmc - Melbourne Mercury Compiler".

:- func copyright_notice_lines = list(string).

copyright_notice_lines = [
    "Copyright (C) 1993-2012 The University of Melbourne",
    "Copyright (C) 2013-2025 The Mercury team"
].

:- func long_usage_header_lines = list(string).

long_usage_header_lines = [
    "Usage: mmc [<options>] <arguments>",
    "",
    "Arguments:",
    "    Arguments ending in `.m' are assumed to be source file names.",
    "    Arguments that do not end in `.m' are assumed to be module names.",
    "    Arguments of the form `@file' are replaced with " ++
        "the contents of the file.",
    "",
    "Options:"
].

%---------------------------------------------------------------------------%

:- type help_section
    --->    one_level_section(
                chapter_section             :: help_subsection
            )
    ;       two_level_section(
                chapter_name                :: string,
                chapter_comment_lines       :: list(string),
                chapter_sections            :: list(help_subsection)
            ).

:- type help_subsection
    --->    help_subsection(
                section_name                :: string,
                section_comment_lines       :: list(string),
                section_categories          :: list(option_category)
            ).

%---------------------------------------------------------------------------%

:- func all_chapters = list(help_section).

all_chapters = AllSections :-
    SectionHelp = one_level_section(
        help_subsection("Help options",
        [], [oc_help])),

    SectionCmdLine = one_level_section(
        help_subsection("Options for modifying the command line",
        [], [oc_cmdline])),

    SectionOpmode = one_level_section(
        help_subsection("Options that give the compiler its overall task",
        [], [oc_opmode])),

    SubSectionGradeGen = help_subsection("Grades and grade components",
        [], [oc_grade_gen]),
    SubSectionGradeTarget = help_subsection("Target options",
        [], [oc_grade_target]),
    SubSectionGradeLlds = help_subsection("LLDS backend grade options",
        [], [oc_grade_llds]),
    SubSectionGradeMlds = help_subsection("MLDS backend grade options",
        [], [oc_grade_mlds]),
    SubSectionGradeDbg = help_subsection("Debugging grade options",
        [], [oc_grade_dbg]),
    SubSectionGradeProf = help_subsection("Profiling grade options",
        [], [oc_grade_prof]),
    SubSectionGradeEtc = help_subsection("Optional feature grade options",
        [], [oc_grade_etc]),
    SubSectionGradeDev = help_subsection("Developer grade options",
        [], [oc_grade_dev]),
    SectionGrade = two_level_section("Grade options",
        [],
        [SubSectionGradeGen, SubSectionGradeTarget,
        SubSectionGradeLlds, SubSectionGradeMlds,
        SubSectionGradeDbg, SubSectionGradeProf,
        SubSectionGradeEtc, SubSectionGradeDev]),

    SectionInfer = one_level_section(
        help_subsection("Options that control inference",
        [], [oc_infer])),

    SectionSemantics = one_level_section(
        help_subsection("Options specifying the intended semantics",
        [], [oc_semantics])),

    SectionVerbosity = one_level_section(
        help_subsection("Verbosity options",
        [], [oc_verbosity, oc_verb_dev, oc_verb_dbg])),

    SubSectionDiagGen = help_subsection("Options that control diagnostics",
        [], [oc_diag_gen]),
    SubSectionDiagColor = help_subsection(
        "Options that control color in diagnostics",
        [], [oc_diag_color, oc_diag_int]),
    SectionDiag = two_level_section("Diagnostics options",
        [],
        [SubSectionDiagGen, SubSectionDiagColor]),

    SubSectionWarnDodgy = help_subsection(
        "Warnings about possible incorrectness",
        [], [oc_warn_dodgy]),
    SubSectionWarnPerf = help_subsection(
        "Warnings about possible performance issues",
        [], [oc_warn_perf, oc_warn_perf_c]),
    SubSectionWarnStyle = help_subsection("Warnings about programming style",
        [], [oc_warn_style, oc_warn_style_c]),
    SubSectionWarnCtrl = help_subsection("Options that control warnings",
        [], [oc_warn_ctrl]),
    SubSectionWarnHalt = help_subsection("Options about halting for warnings",
        [], [oc_warn_halt]),
    SectionWarn = two_level_section("Warning options",
        [],
        [SubSectionWarnDodgy, SubSectionWarnPerf, SubSectionWarnStyle,
        SubSectionWarnCtrl, SubSectionWarnHalt]),

    % XXX Should these two chapters instead be two sections in one chapter?
    SectionInform = one_level_section(
        help_subsection("Options that request information",
        [], [oc_inform])),
    SectionFileReq = one_level_section(
        help_subsection("Options that ask for informational files",
        [], [oc_file_req])),

    SectionTraceGoal = one_level_section(
        help_subsection("Controlling trace goals",
        [], [oc_tracegoal])),

    % Once ssdb debugging is publicly documented, we should replace these
    % two chapters (the second of which is not printed for non-developers)
    % with a single two-section chapter.
    SectionDebugMdb = one_level_section(
        help_subsection("Preparing code for mdb debugging",
        [], [oc_mdb, oc_mdb_dev])),
    SectionDebugSsdb = one_level_section(
        help_subsection("Preparing code for ssdb debugging",
        [], [oc_ssdb, oc_ssdb_dev])),

    SectionProfiling = one_level_section(
        help_subsection("Preparing code for mdprof profiling",
        [], [oc_mdprof])),

    SubSectionOptCtrl = help_subsection("Overall control of optimizations",
        [], [oc_opt_ctrl]),
    SubSectionOptHH = help_subsection("Source-to-source optimizations",
        [], [oc_opt_hh]),
    SubSectionOptHHE = help_subsection(
        "Experimental source-to-source optimizations",
        [], [oc_opt_hh_exp]),
    SubSectionOptHLM = help_subsection("Optimizations during code generation",
        [], [oc_opt_hlm]),
    % XXX Should the categories here be separate sections?
    SubSectionOptMM = help_subsection(
        "Optimizations specific to high level code",
        [], [oc_opt_hm, oc_opt_mm]),
    % XXX Should the categories here be separate sections?
    SubSectionOptLL = help_subsection(
        "Optimizations specific to low level code",
        [], [oc_opt_hl, oc_opt_ll, oc_opt_lc]),
    SectionOpt = two_level_section("Optimization options",
        [],
        [SubSectionOptCtrl, SubSectionOptHH, SubSectionOptHHE,
        SubSectionOptHLM, SubSectionOptMM, SubSectionOptLL]),

    SectionTransOpt = one_level_section(
        help_subsection(
            "Options that control transitive intermodule optimization",
        [], [oc_trans_opt])),

    SectionAnalysis = one_level_section(
        help_subsection("Options that control program analyses",
        [], [oc_analysis])),

    SectionModOutput = one_level_section(
        help_subsection("Options that ask for modified output",
        [], [oc_output_mod, oc_output_dev])),

    SectionMmcMake = one_level_section(
        help_subsection("Options for controlling mmc --make",
        [], [oc_make])),

    SubSectionCompileGen = help_subsection(
        "General options for compiling target language code",
        [], [oc_target_comp]),
    SubSectionCompileC = help_subsection("Options for compiling C code",
        [], [oc_target_c]),
    SubSectionCompileJava = help_subsection("Options for compiling Java code",
        [], [oc_target_java]),
    SubSectionCompileCsharp = help_subsection("Options for compiling C# code",
        [], [oc_target_csharp]),
    SectionCompile = two_level_section(
        "Options for target language compilation",
        [],
        [SubSectionCompileGen, SubSectionCompileC, SubSectionCompileJava,
        SubSectionCompileCsharp]),

    SubSectionLinkGen = help_subsection("General options for linking",
        [], [oc_link_c_cs_j]),
    SubSectionLinkCCsharp = help_subsection("Options for linking C or C# code",
        [], [oc_link_c_cs]),
    SubSectionLinkC = help_subsection("Options for linking just C code",
        [], [oc_link_c]),
    SubSectionLinkCsharp = help_subsection("Options for linking just C# code",
        [], [oc_link_csharp]),
    SubSectionLinkJava = help_subsection("Options for linking just Java code",
        [], [oc_link_java]),
    SectionLink = two_level_section("Options for linking",
        [],
        [SubSectionLinkGen, SubSectionLinkCCsharp,
        SubSectionLinkC, SubSectionLinkJava, SubSectionLinkCsharp]),

    SectionFileSearch = one_level_section(
        help_subsection("Options controlling searches for files",
        [], [oc_search])),

    SectionBuild = one_level_section(
        help_subsection("Options controlling the library installation process",
        [], [oc_buildsys])),

    SectionEnv = one_level_section(
        help_subsection("Options specifying properties of the environment",
        [], [oc_env])),

    SectionConfig = one_level_section(
        help_subsection("Options that record autoconfigured parameters",
        [], [oc_config])),

    SectionMconfig = one_level_section(
        help_subsection("Options reserved for Mercury.config files",
        [], [oc_mconfig])),

    SubSectionDevCtrl = help_subsection(
        "Operation selection options for developers only",
        [], [oc_dev_ctrl]),
    SubSectionDevDebug = help_subsection(
        "Options that can help debug the compiler",
        [], [oc_dev_debug]),
    SubSectionDevDump = help_subsection(
        "Options for dumping internal compiler data structures",
        [], [oc_dev_dump]),
    SubSectionDevInternal = help_subsection(
        "Options intended for internal use by the compiler only",
        [], [oc_internal]),
    SectionDev = two_level_section(
        "Options for developers only",
        [],
        [SubSectionDevCtrl, SubSectionDevDebug, SubSectionDevDump,
        SubSectionDevInternal]),

    SectionUnused = one_level_section(
        help_subsection("Now-unused former options kept for compatibility",
        [], [oc_unused])),

    AllSections = [
        SectionHelp,
        SectionCmdLine,
        SectionOpmode,
        SectionGrade,
        SectionInfer,
        SectionSemantics,
        SectionVerbosity,

        SectionDiag,
        SectionWarn,
        SectionInform,
        SectionFileReq,

        SectionTraceGoal,
        SectionDebugMdb,
        SectionDebugSsdb,
        SectionProfiling,

        SectionOpt,
        SectionTransOpt,
        SectionAnalysis,

        SectionModOutput,
        SectionMmcMake,

        SectionCompile,
        SectionLink,
        SectionFileSearch,
        SectionBuild,

        SectionEnv,
        SectionConfig,
        SectionMconfig,

        SectionDev,
        SectionUnused
    ].

%---------------------------------------------------------------------------%

:- type help_format
    --->    help_plain_text
    ;       help_texinfo.

:- inst help_plain_text for help_format/0
    --->    help_plain_text.
:- inst help_texinfo for help_format/0
    --->    help_texinfo.

:- pred document_requested_options(help_format, print_what_help, list(string)).
:- mode document_requested_options(in(help_plain_text), in, out) is det.
:- mode document_requested_options(in(help_texinfo), in, out) is det.

document_requested_options(Format, What, OptionsLines) :-
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
    acc_help_sections(Format, What, all_chapters,
        AllCategoriesSet, UndoneCategoriesSet, cord.init, OptionsLineCord),
    set.to_sorted_list(UndoneCategoriesSet, UndoneCategories),
    (
        UndoneCategories = []
    ;
        UndoneCategories = [_ | _],
        unexpected($pred, "undone: " ++ string(UndoneCategories))
    ),
    OptionsLines = cord.list(OptionsLineCord).

%---------------------------------------------------------------------------%

:- pred acc_help_sections(help_format, print_what_help, list(help_section),
    set(option_category), set(option_category), cord(string), cord(string)).
:- mode acc_help_sections(in(help_plain_text), in, in,
    in, out, in, out) is det.
:- mode acc_help_sections(in(help_texinfo), in, in,
    in, out, in, out) is det.

acc_help_sections(_, _, [], !Categories, !LineCord).
acc_help_sections(Format, What, [Section | Sections],
        !Categories, !LineCord) :-
    acc_help_section(Format, What, Section, !Categories, !LineCord),
    acc_help_sections(Format, What, Sections, !Categories, !LineCord).

:- pred acc_help_section(help_format, print_what_help, help_section,
    set(option_category), set(option_category), cord(string), cord(string)).
:- mode acc_help_section(in(help_plain_text), in, in, in, out, in, out) is det.
:- mode acc_help_section(in(help_texinfo), in, in, in, out, in, out) is det.

acc_help_section(Format, What, Section, !Categories, !LineCord) :-
    (
        Section = one_level_section(SubSection),
        acc_help_subsection(Format, What, "", SubSection,
            !Categories, !LineCord)
    ;
        Section = two_level_section(SectionName, SectionCommentLines,
            SubSections),
        ( Format = help_plain_text, SubSectionIndent = single_indent
        ; Format = help_texinfo,    SubSectionIndent = ""
        ),
        acc_help_subsections(Format, What, SubSectionIndent, SubSections,
            !Categories, cord.init, SubSectionsLineCord),
        ( if cord.is_empty(SubSectionsLineCord) then
            true
        else
            cord.snoc("", !LineCord),
            (
                Format = help_plain_text,
                cord.snoc(SectionName, !LineCord)
            ;
                Format = help_texinfo,
                % ZZZ
                cord.snoc("@node " ++ SectionName, !LineCord),
                cord.snoc("@subsection " ++ SectionName, !LineCord),
                cord.snoc("@cindex " ++ SectionName, !LineCord)
            ),
            (
                SectionCommentLines = []
            ;
                SectionCommentLines = [_ | _],
                cord.snoc("", !LineCord),
                list.foldl(acc_prefixed_line(""),
                    SectionCommentLines, !LineCord)
            ),
            !:LineCord = !.LineCord ++ SubSectionsLineCord
        )
    ).

:- pred acc_help_subsections(help_format, print_what_help, string,
    list(help_subsection), set(option_category), set(option_category),
    cord(string), cord(string)).
:- mode acc_help_subsections(in(help_plain_text), in, in, in,
    in, out, in, out) is det.
:- mode acc_help_subsections(in(help_texinfo), in, in, in,
    in, out, in, out) is det.

acc_help_subsections(_, _, _, [], !Categories, !LineCord).
acc_help_subsections(Format, What, SubSectionNameIndent,
        [SubSection | SubSections], !Categories, !LineCord) :-
    acc_help_subsection(Format, What, SubSectionNameIndent,
        SubSection, !Categories, !LineCord),
    acc_help_subsections(Format, What, SubSectionNameIndent,
        SubSections, !Categories, !LineCord).

:- pred acc_help_subsection(help_format, print_what_help, string,
    help_subsection, set(option_category), set(option_category),
    cord(string), cord(string)).
:- mode acc_help_subsection(in(help_plain_text), in, in, in,
    in, out, in, out) is det.
:- mode acc_help_subsection(in(help_texinfo), in, in, in,
    in, out, in, out) is det.

acc_help_subsection(Format, What, SubSectionNameIndent, SubSection,
        !Categories, !LineCord) :-
    SubSection = help_subsection(SubSectionName, SubSectionCommentLines,
        SubSectionCategories),
    set.det_remove_list(SubSectionCategories, !Categories),
    list.map(get_optdb_records_in_category,
        SubSectionCategories, OptdbRecordSets),
    OptdbRecordSet = set.union_list(OptdbRecordSets),

    acc_help_messages(Format, What, set.to_sorted_list(OptdbRecordSet),
        cord.init, HelpTextLinesCord),
    ( if cord.is_empty(HelpTextLinesCord) then
        true
    else
        cord.snoc("", !LineCord),
        (
            Format = help_plain_text,
            cord.snoc(SubSectionNameIndent ++ SubSectionName, !LineCord)
        ;
            Format = help_texinfo,
            % ZZZ
            cord.snoc("@node " ++ SubSectionName, !LineCord),
            cord.snoc("@subsection " ++ SubSectionName, !LineCord),
            cord.snoc("@cindex " ++ SubSectionName, !LineCord)
        ),
        (
            SubSectionCommentLines = []
        ;
            SubSectionCommentLines = [_ | _],
            cord.snoc("", !LineCord),
            list.foldl(acc_prefixed_line(SubSectionNameIndent),
                SubSectionCommentLines, !LineCord)
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

:- pred acc_help_messages(help_format, print_what_help, list(optdb_record),
    cord(string), cord(string)).
:- mode acc_help_messages(in(help_plain_text), in, in, in, out) is det.
:- mode acc_help_messages(in(help_texinfo), in, in, in, out) is det.

acc_help_messages(_, _, [], !EffectiveLinesCord).
acc_help_messages(Format, What, [OptdbRecord | OptdbRecords],
        !EffectiveLinesCord) :-
    acc_help_message(Format, What, OptdbRecord, !EffectiveLinesCord),
    acc_help_messages(Format, What, OptdbRecords, !EffectiveLinesCord).

:- pred acc_help_message(help_format, print_what_help, optdb_record,
    cord(string), cord(string)).
:- mode acc_help_message(in(help_plain_text), in, in, in, out) is det.
:- mode acc_help_message(in(help_texinfo), in, in, in, out) is det.

acc_help_message(Format, What, OptdbRecord, !EffectiveLinesCord) :-
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
            DescPieces = []
        ;
            Help = unnamed_help(DescPieces),
            % XXX It is quite likely that many options that do not have entries
            % in the long_table predicate, which therefore should be in optdb
            % with unnamed_help, are there with some other help structure,
            % such as priv_help.
            PublicOrPrivate = help_private,
            string.format("%sUNNAMED OPTION %s",
                [s(single_indent), s(string(Option))], NameLine),
            cord.snoc(NameLine, !LineCord)
        ;
            Help = gen_help(ShortNames, LongName, AltLongNames,
                PublicOrPrivate, DescPieces),
            acc_short_option_names(Params, Option, no_arg, no_align,
                ShortNames, !LineCord),
            acc_long_option_name(Params, Option, no_arg, no_align,
                LongName, !LineCord),
            acc_long_option_names(Params, Option, no_arg, no_align,
                AltLongNames, !LineCord)
        ;
            (
                Help = help(LongName, DescPieces),
                MaybeArg = no_arg,
                PublicOrPrivate = help_public
            ;
                Help = arg_help(LongName, ArgName, DescPieces),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_public
            ;
                Help = priv_help(LongName, DescPieces),
                MaybeArg = no_arg,
                PublicOrPrivate = help_private
            ;
                Help = priv_arg_help(LongName, ArgName, DescPieces),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_private
            ),
            acc_long_option_name(Params, Option, MaybeArg, no_align,
                LongName, !LineCord)
        ;
            (
                Help = alt_help(LongName, AltLongNames, DescPieces),
                MaybeArg = no_arg,
                PublicOrPrivate = help_public
            ;
                Help = alt_arg_help(LongName, AltLongNames, ArgName,
                    DescPieces),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_public
            ;
                Help = priv_alt_help(LongName, AltLongNames, DescPieces),
                MaybeArg = no_arg,
                PublicOrPrivate = help_private
            ;
                Help = priv_alt_arg_help(LongName, AltLongNames, ArgName,
                    DescPieces),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_private
            ),
            acc_long_option_name(Params, Option, MaybeArg, no_align,
                LongName, !LineCord),
            acc_long_option_names(Params, Option, MaybeArg, no_align,
                AltLongNames, !LineCord)
        ;
            (
                Help = short_help(ShortName, LongName, AltLongNames,
                    DescPieces),
                MaybeArg = no_arg,
                PublicOrPrivate = help_public
            ;
                Help = short_arg_help(ShortName, LongName, AltLongNames,
                    ArgName, DescPieces),
                MaybeArg = arg_name(ArgName),
                PublicOrPrivate = help_public
            ;
                Help = priv_short_help(ShortName, LongName, AltLongNames,
                    DescPieces),
                MaybeArg = no_arg,
                PublicOrPrivate = help_private
            ;
                Help = priv_short_arg_help(ShortName, LongName, AltLongNames,
                    ArgName, DescPieces),
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
                Help = alt_align_help(LongName, AltLongNames,
                    AlignedText, DescPieces),
                PublicOrPrivate = help_public
            ;
                Help = priv_alt_align_help(LongName, AltLongNames,
                    AlignedText, DescPieces),
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
            Help = short_alt_align_help(ShortName, LongName, AltLongNames,
                AlignedText, DescPieces),
            PublicOrPrivate = help_public,
            acc_short_option_name(Params, Option, no_arg,
                aligned_text(AlignedText), ShortName, !LineCord),
            % The aligned text is added only to the first option name line.
            acc_long_option_name(Params, Option, no_arg, no_align,
                LongName, !LineCord),
            acc_long_option_names(Params, Option, no_arg, no_align,
                AltLongNames, !LineCord)
        ;
            Help = no_align_help(LongName, AlignedText, NoAlignedText,
                DescPieces),
            PublicOrPrivate = help_public,
            expect(is_bool(OptionData), $pred,
                "unexpected use of no_align_help"),
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
            Help = alt_arg_align_help(LongName, ArgAligns, DescPieces),
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
                DescPieces = []
            then
                true
            else
                DescPrefix = double_indent,
                (
                    DescPieces = [],
                    EffDescPieces =
                        [w("There is no help text available.")]
                ;
                    DescPieces = [_ | _],
                    EffDescPieces = DescPieces
                ),
                % ZZZ 71
                reflow_lines(Format, 71, EffDescPieces, ReflowLines),
                BlankLineCord = cord.singleton(""),
                (
                    Format = help_plain_text,
                    list.foldl(acc_prefixed_line(DescPrefix), ReflowLines,
                        !LineCord),
                    (
                        PublicOrPrivate = help_public,
                        PrivatePrefixCord = cord.init
                    ;
                        PublicOrPrivate = help_private,
                        PrivatePrefixCord =
                            cord.singleton(single_indent ++ "PRIVATE OPTION")
                    ),
                    !:EffectiveLinesCord = !.EffectiveLinesCord ++
                        BlankLineCord ++ PrivatePrefixCord ++ !.LineCord
                ;
                    Format = help_texinfo,
                    !:LineCord = !.LineCord ++ cord.from_list(ReflowLines),
                    (
                        PublicOrPrivate = help_public
                    ;
                        PublicOrPrivate = help_private,
                        AddCommentPrefix = ( func(L) = "@c " ++ L ),
                        !:LineCord = cord.map(AddCommentPrefix, !.LineCord)
                    ),
                    !:EffectiveLinesCord = !.EffectiveLinesCord ++
                        BlankLineCord ++ !.LineCord
                )
            )
        else
            true
        )
    ).

    % ZZZ revisit for texinfo
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
    Indent = single_indent,
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
    Indent = single_indent,
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
    Indent = single_indent,
    string.format("%s--no-%s", [s(Indent), s(LongName)], Line).

:- func short_negated_option_name_line(char) = string.

short_negated_option_name_line(ShortName) = Line :-
    Indent = single_indent,
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

:- func single_indent = string.
:- func double_indent = string.

single_indent = "    ".
double_indent = "        ".

:- pred acc_prefixed_line(string::in, string::in,
    cord(string)::in, cord(string)::out) is det.

acc_prefixed_line(Prefix, LineBody, !LineCord) :-
    Line = Prefix ++ LineBody,
    cord.snoc(Line, !LineCord).

%---------------------------------------------------------------------------%

:- pred reflow_lines(help_format, int, list(help_piece), list(string)).
:- mode reflow_lines(in(help_plain_text), in, in, out) is det.
:- mode reflow_lines(in(help_texinfo), in, in, out) is det.

reflow_lines(Format, LineLen, InitialPieces, FinishedLines) :-
    % string.count_code_points(IndentStr, IndentLen),
    % AvailLen = LineLen - IndentLen,
    reflow_lines_loop_over_lines(Format, LineLen, InitialPieces,
        0, _CurLineLen, cord.init, CurLine1, cord.init, FinishedLineCord1),
    finish_cur_line(CurLine1, FinishedLineCord1, FinishedLineCord),
    FinishedLines = cord.list(FinishedLineCord).

    % The pieces of the current line.
    % This will NOT contain the initial indent string.
    % This WILL contain both the words put on this line so far, *and*
    % the spaces between them.
:- type cur_line == cord(string).

    % The reflowed lines we have already constructed.
:- type finished_lines == cord(string).

:- pred reflow_lines_loop_over_lines(help_format, int, list(help_piece),
    int, int, cur_line, cur_line, finished_lines, finished_lines).
:- mode reflow_lines_loop_over_lines(in(help_plain_text), in, in,
    in, out, in, out, in, out) is det.
:- mode reflow_lines_loop_over_lines(in(help_texinfo), in, in,
    in, out, in, out, in, out) is det.

reflow_lines_loop_over_lines(Format, LineLen, Pieces,
        !CurLineLen, !CurLine, !FinishedLineCord) :-
    (
        Pieces = []
    ;
        Pieces = [HeadPiece | TailPieces],
        (
            HeadPiece = w(WordsStr),
            Words = string.words(WordsStr),
            reflow_lines_loop_over_words(LineLen, Words, !CurLine, !CurLineLen,
                !FinishedLineCord)
        ;
            HeadPiece = fixed(FixedStr),
            add_word(LineLen, FixedStr, !CurLine, !CurLineLen,
                !FinishedLineCord)
        ;
            ( HeadPiece = quote(_)
            ; HeadPiece = quote(_, _)
            ; HeadPiece = opt(_)
            ; HeadPiece = opt(_, _)
            ; HeadPiece = arg(_)
            ; HeadPiece = arg(_, _)
            ; HeadPiece = opt_arg(_, _)
            ; HeadPiece = opt_arg(_, _, _)
            ; HeadPiece = samp(_)
            ; HeadPiece = samp(_, _)
            ; HeadPiece = emph(_)
            ; HeadPiece = emph(_, _)
            ; HeadPiece = code(_)
            ; HeadPiece = code(_, _)
            ; HeadPiece = file(_)
            ; HeadPiece = file(_, _)
            ; HeadPiece = var(_)
            ; HeadPiece = var(_, _)
            ; HeadPiece = file_var(_, _)
            ; HeadPiece = file_var(_, _, _)
            ; HeadPiece = ref(_, _, _)
            ; HeadPiece = ref(_, _, _, _)
            ; HeadPiece = xref(_)
            ; HeadPiece = xref(_, _)
            ),
            (
                ( HeadPiece = quote(Text), Suffix = ""
                ; HeadPiece = quote(Text, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("`%s'%s", [s(Text), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("``%s''%s", [s(Text), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = opt(Option), Suffix = ""
                ; HeadPiece = opt(Option, Suffix)
                ),
                string.format("`%s'%s", [s(Option), s(Suffix)], Str)
            ;
                ( HeadPiece = arg(Arg), Suffix = ""
                ; HeadPiece = arg(Arg, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("<%s>%s", [s(Arg), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("@samp{%s}%s", [s(Arg), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = opt_arg(Option, Arg), Suffix = ""
                ; HeadPiece = opt_arg(Option, Arg, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("`%s <%s>'%s",
                        [s(Option), s(Arg), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("`%s @samp{%s}'%s",
                        [s(Option), s(Arg), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = samp(Option), Suffix = ""
                ; HeadPiece = samp(Option, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("`%s'%s", [s(Option), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("@samp{%s}%s", [s(Option), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = emph(Text), Suffix = ""
                ; HeadPiece = emph(Text, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("*%s*%s", [s(Text), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("@emph{%s}%s", [s(Text), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = code(Code), Suffix = ""
                ; HeadPiece = code(Code, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("`%s'%s", [s(Code), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("@code{%s}%s", [s(Code), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = file(Var), Suffix = ""
                ; HeadPiece = file(Var, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("`%s'%s", [s(Var), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("@file{%s}%s", [s(Var), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = var(Var), Suffix = ""
                ; HeadPiece = var(Var, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("%s%s", [s(Var), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("@var{%s}%s", [s(Var), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = file_var(File, Ext), Suffix = ""
                ; HeadPiece = file_var(File, Ext, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("`<%s>.%s'%s",
                        [s(File), s(Ext), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("@file{@var{%s}.%s}%s",
                        [s(File), s(Ext), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = ref(Before0, RefName, After0), Suffix = ""
                ; HeadPiece = ref(Before0, RefName, After0, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("%s\"%s\"%s%s",
                        [s(before_str(Before0)), s(RefName),
                        s(after_str(After0)), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    % We ignore Before0 and After0,
                    % which are for help text only;
                    % for texinfo, the @ref should supply them.
                    string.format("@ref{%s}%s", [s(RefName), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = xref(RefName), Suffix = ""
                ; HeadPiece = xref(RefName, Suffix)
                ),
                (
                    Format = help_plain_text,
                    % xref is a texinfo only piece. It should occur
                    % only inside texinfo_only wrappers.
                    Str = ""
                ;
                    Format = help_texinfo,
                    string.format("@xref{%s}%s", [s(RefName), s(Suffix)], Str)
                )
            ),
            add_word(LineLen, Str, !CurLine, !CurLineLen, !FinishedLineCord)
        ;
            HeadPiece = help_text_only(HelpTextPieces),
            (
                Format = help_plain_text,
                reflow_lines_loop_over_lines(Format, LineLen, HelpTextPieces,
                    !CurLineLen, !CurLine, !FinishedLineCord)
            ;
                Format = help_texinfo
            )
        ;
            HeadPiece = texinfo_only(TexInfoPieces),
            (
                Format = help_plain_text
            ;
                Format = help_texinfo,
                reflow_lines_loop_over_lines(Format, LineLen, TexInfoPieces,
                    !CurLineLen, !CurLine, !FinishedLineCord)
            )
        ;
            HeadPiece = help_text_texinfo(HelpTextPieces, TexInfoPieces),
            (
                Format = help_plain_text,
                reflow_lines_loop_over_lines(Format, LineLen, HelpTextPieces,
                    !CurLineLen, !CurLine, !FinishedLineCord)
            ;
                Format = help_texinfo,
                reflow_lines_loop_over_lines(Format, LineLen, TexInfoPieces,
                    !CurLineLen, !CurLine, !FinishedLineCord)
            )
        ),
        reflow_lines_loop_over_lines(Format, LineLen, TailPieces,
            !CurLineLen, !CurLine, !FinishedLineCord)
    ).

:- func before_str(string) = string.

before_str(BeforeStr0) = BeforeStr :-
    ( if BeforeStr0 = "" then
        BeforeStr = ""
    else
        BeforeStr = BeforeStr0 ++ " "
    ).

:- func after_str(string) = string.

after_str(AfterStr0) = AfterStr :-
    ( if AfterStr0 = "" then
        AfterStr = ""
    else
        AfterStr = " " ++ AfterStr0
    ).

:- pred reflow_lines_loop_over_words(int::in, list(string)::in,
    cur_line::in, cur_line::out, int::in, int::out,
    finished_lines::in, finished_lines::out) is det.

reflow_lines_loop_over_words(LineLen, Words, !CurLine, !CurLineLen,
        !FinishedLineCord) :-
    (
        Words = []
    ;
        Words = [HeadWord | TailWords],
        add_word(LineLen, HeadWord, !CurLine, !CurLineLen, !FinishedLineCord),
        reflow_lines_loop_over_words(LineLen, TailWords, !CurLine, !CurLineLen,
            !FinishedLineCord)
    ).

:- pred add_word(int::in, string::in,
    cur_line::in, cur_line::out, int::in, int::out,
    finished_lines::in, finished_lines::out) is det.

add_word(LineLen, Word, !CurLine, !CurLineLen, !FinishedLineCord) :-
    string.count_code_points(Word, WordLen),
    ( if WordLen = 0 then
        true
    else if !.CurLineLen = 0 then
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
            [s(single_indent)], SetAtLevel),
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
            list.foldl(acc_prefixed_line(single_indent), LevelDescLines,
                !LineCord)
        ),
        cord.snoc("", !LineCord),
        cord.snoc(SetAtLevel, !LineCord),
        cord.snoc("", !LineCord),
        list.foldl(acc_prefixed_line(double_indent), SortedOptLines,
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
        ; Help = unnamed_help(_)
        ),
        MaybeLongName = no
    ;
        ( Help = gen_help(_, LongName, _, _, _)
        ; Help = help(LongName, _)
        ; Help = arg_help(LongName, _, _)
        ; Help = priv_help(LongName, _)
        ; Help = priv_arg_help(LongName, _, _)
        ; Help = alt_help(LongName, _, _)
        ; Help = alt_arg_help(LongName, _, _, _)
        ; Help = priv_alt_help(LongName, _, _)
        ; Help = priv_alt_arg_help(LongName, _, _, _)
        ; Help = short_help(_, LongName, _, _)
        ; Help = short_arg_help(_, LongName, _, _, _)
        ; Help = priv_short_help(_, LongName, _, _)
        ; Help = priv_short_arg_help(_, LongName, _, _, _)
        ; Help = alt_align_help(LongName, _, _, _)
        ; Help = priv_alt_align_help(LongName, _, _, _)
        ; Help = short_alt_align_help(_, LongName, _, _, _)
        ; Help = no_align_help(LongName, _, _, _)
        ; Help = alt_arg_align_help(LongName, _, _)
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
