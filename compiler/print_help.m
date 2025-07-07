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
:- import_module map.
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
    document_requested_options(help_plain_text, What,
        _SectionNames, OptionsLines),
    Lines = HeaderLines ++ OptionsLines,
    write_lines(ProgressStream, Lines, !IO).

document_options_for_users_guide(ProgressStream, !IO) :-
    document_requested_options(help_texinfo, print_public_and_private_help,
        MenuItemsTail, OptionsLines),
    MenuItems = [menu_item("Invocation overview", "") | MenuItemsTail],
    MenuLines = menu_items_to_menu(MenuItems),
    OverviewLines = string.split_at_char('\n', invocation_overview_section),
    % OverviewLines will start and end with blank lines.
    AllLines = MenuLines ++ OverviewLines ++ OptionsLines,
    write_lines(ProgressStream, AllLines, !IO).

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

:- func invocation_overview_section = string.

invocation_overview_section =
"
@node Invocation overview
@section Invocation overview
@findex --no-

@code{mmc} is invoked as
@example
mmc [@var{options}] @var{arguments}
@end example

Arguments can be either module names or file names.
Arguments ending in @samp{.m} are assumed to be file names,
while other arguments are assumed to be module names.
The compiler will convert module names to file names
by looking up the module name in the module-name-to-file-name map
in the @file{Mercury.modules} file it if exists.
(It can be created using a command such as @code{mmc -f *.m}.)
It @file{Mercury.modules} does not exist, then the compiler
will search for a module named e.g. @samp{foo.bar.baz}
in the files @file{foo.bar.baz.m}, @file{bar.baz.m}, and @file{baz.m},
in that order.

Options are either short (single-letter) options preceded by a single @samp{-},
or long options preceded by @samp{--}.
Options are case-sensitive.
We call options that do not take arguments @dfn{flags}.
Single-letter flags may be grouped with a single @samp{-}, e.g.@: @samp{-vVc}.
Single-letter flags may be negated
by appending another trailing @samp{-}, e.g.@: @samp{-v-}.
(You cannot both group @emph{and} negate single-letter flags at the same time.)
Long flags may be negated by preceding them with @samp{no-},
e.g.@: @samp{--no-verbose}.
".

%---------------------------------------------------------------------------%

:- type help_section
    --->    one_level_section(
                section_section             :: help_option_group
            )
    ;       two_level_section(
                section_name                :: string,
                section_menu_desc           :: string,
                section_comment_lines       :: list(string),
                section_sections            :: list(help_option_group)
            ).

:- type help_option_group
    --->    help_option_group(
                hog_name                    :: string,
                hog_menu_desc               :: string,
                hog_comment_lines           :: list(string),
                hog_categories              :: list(option_category)
            ).

:- type section_or_subsection
    --->    sos_section
    ;       sos_subsection.

%---------------------------------------------------------------------------%

:- func all_chapters = list(help_section).

all_chapters = AllSections :-
    SectionHelp = one_level_section(
        help_option_group(
            "Help options",
            "", [], [oc_help])),

    SectionCmdLine = one_level_section(
        help_option_group(
            "Options for modifying the command line",
            "", [], [oc_cmdline])),

    SectionOpmode = one_level_section(
        help_option_group(
            "Options that give the compiler its overall task",
            "", [], [oc_opmode])),

    SubSectionGradeGen = help_option_group(
        "Grades and grade components",
        "Setting the compilation model", [], [oc_grade_gen]),
    SubSectionGradeTarget = help_option_group(
        "Target options",
        "Choosing the target language", [], [oc_grade_target]),
    SubSectionGradeLlds = help_option_group(
        "LLDS backend grade options",
        "For the low-level C backend", [], [oc_grade_llds]),
    SubSectionGradeMlds = help_option_group(
        "MLDS backend grade options",
        "For the high-level C/Java/C# backend", [], [oc_grade_mlds]),
    SubSectionGradeDbg = help_option_group(
        "Debugging grade options",
        "", [], [oc_grade_dbg]),
    SubSectionGradeProf = help_option_group(
        "Profiling grade options",
        "", [], [oc_grade_prof]),
    SubSectionGradeEtc = help_option_group(
        "Optional feature grade options",
        "", [], [oc_grade_etc]),
    SubSectionGradeDev = help_option_group(
        "Developer grade options",
        "Not for general use", [], [oc_grade_dev]),
    SectionGrade = two_level_section("Grade options",
        "", [],
        [SubSectionGradeGen, SubSectionGradeTarget,
        SubSectionGradeLlds, SubSectionGradeMlds,
        SubSectionGradeDbg, SubSectionGradeProf,
        SubSectionGradeEtc, SubSectionGradeDev]),

    SectionInfer = one_level_section(
        help_option_group(
            "Options that control inference",
            "", [], [oc_infer])),

    SectionSemantics = one_level_section(
        help_option_group(
            "Options specifying the intended semantics",
            "", [], [oc_semantics])),

    SectionVerbosity = one_level_section(
        help_option_group(
            "Verbosity options",
            "", [], [oc_verbosity, oc_verb_dev, oc_verb_dbg])),

    SubSectionDiagGen = help_option_group(
        "Options that control diagnostics",
        "", [], [oc_diag_gen]),
    SubSectionDiagColor = help_option_group(
        "Options that control color in diagnostics",
        "", [], [oc_diag_color, oc_diag_int]),
    SectionDiag = two_level_section("Diagnostics options",
        "", [],
        [SubSectionDiagGen, SubSectionDiagColor]),

    SubSectionWarnDodgy = help_option_group(
        "Warnings about possible incorrectness",
        "", [], [oc_warn_dodgy]),
    SubSectionWarnPerf = help_option_group(
        "Warnings about possible performance issues",
        "", [], [oc_warn_perf, oc_warn_perf_c]),
    SubSectionWarnStyle = help_option_group(
        "Warnings about programming style",
        "", [], [oc_warn_style, oc_warn_style_c]),
    SubSectionWarnCtrl = help_option_group(
        "Options that control warnings",
        "", [], [oc_warn_ctrl]),
    SubSectionWarnHalt = help_option_group(
        "Options about halting for warnings",
        "", [], [oc_warn_halt]),
    SectionWarn = two_level_section("Warning options",
        "", [],
        [SubSectionWarnDodgy, SubSectionWarnPerf, SubSectionWarnStyle,
        SubSectionWarnCtrl, SubSectionWarnHalt]),

    % XXX Should these two chapters instead be two sections in one chapter?
    SectionInform = one_level_section(
        help_option_group("Options that request information",
        "", [], [oc_inform])),
    SectionFileReq = one_level_section(
        help_option_group("Options that ask for informational files",
        "", [], [oc_file_req])),

    SectionTraceGoal = one_level_section(
        help_option_group("Controlling trace goals",
        "", [], [oc_tracegoal])),

    % Once ssdb debugging is publicly documented, we should replace these
    % two chapters (the second of which is not printed for non-developers)
    % with a single two-section chapter.
    SectionDebugMdb = one_level_section(
        help_option_group("Preparing code for mdb debugging",
        "", [], [oc_mdb, oc_mdb_dev])),
    SectionDebugSsdb = one_level_section(
        help_option_group("Preparing code for ssdb debugging",
        "", [], [oc_ssdb, oc_ssdb_dev])),

    SectionProfiling = one_level_section(
        help_option_group("Preparing code for mdprof profiling",
        "", [], [oc_mdprof])),

    SubSectionOptCtrl = help_option_group(
        "Overall control of optimizations",
        "", [], [oc_opt_ctrl]),
    SubSectionOptHH = help_option_group(
        "Source-to-source optimizations",
        "", [], [oc_opt_hh]),
    SubSectionOptHHE = help_option_group(
        "Experimental source-to-source optimizations",
        "", [], [oc_opt_hh_exp]),
    SubSectionOptHLM = help_option_group(
        "Optimizations during code generation",
        "", [], [oc_opt_hlm]),
    % XXX Should the categories here be separate sections?
    SubSectionOptMM = help_option_group(
        "Optimizations specific to high level code",
        "", [], [oc_opt_hm, oc_opt_mm]),
    % XXX Should the categories here be separate sections?
    SubSectionOptLL = help_option_group(
        "Optimizations specific to low level code",
        "", [], [oc_opt_hl, oc_opt_ll, oc_opt_lc]),
    SectionOpt = two_level_section("Optimization options",
        "", [],
        [SubSectionOptCtrl, SubSectionOptHH, SubSectionOptHHE,
        SubSectionOptHLM, SubSectionOptMM, SubSectionOptLL]),

    SectionTransOpt = one_level_section(
        help_option_group(
            "Options that control transitive intermodule optimization",
            "", [], [oc_trans_opt])),

    SectionAnalysis = one_level_section(
        help_option_group(
            "Options that control program analyses",
            "", [], [oc_analysis])),

    SectionModOutput = one_level_section(
        help_option_group(
            "Options that ask for modified output",
            "", [], [oc_output_mod, oc_output_dev])),

    SectionMmcMake = one_level_section(
        help_option_group(
            "Options for controlling mmc --make",
            "", [], [oc_make])),

    SubSectionCompileGen = help_option_group(
        "General options for compiling target language code",
        "", [], [oc_target_comp]),
    SubSectionCompileC = help_option_group(
        "Options for compiling C code",
        "", [], [oc_target_c]),
    SubSectionCompileJava = help_option_group(
        "Options for compiling Java code",
        "", [], [oc_target_java]),
    SubSectionCompileCsharp = help_option_group(
        "Options for compiling C# code",
        "", [], [oc_target_csharp]),
    SectionCompile = two_level_section(
        "Options for target language compilation",
        "", [],
        [SubSectionCompileGen, SubSectionCompileC, SubSectionCompileJava,
        SubSectionCompileCsharp]),

    SubSectionLinkGen = help_option_group(
        "General options for linking",
        "", [], [oc_link_c_cs_j]),
    SubSectionLinkCCsharp = help_option_group(
        "Options for linking C or C# code",
        "", [], [oc_link_c_cs]),
    SubSectionLinkC = help_option_group(
        "Options for linking just C code",
        "", [], [oc_link_c]),
    SubSectionLinkCsharp = help_option_group(
        "Options for linking just C# code",
        "", [], [oc_link_csharp]),
    SubSectionLinkJava = help_option_group(
        "Options for linking just Java code",
        "", [], [oc_link_java]),
    SectionLink = two_level_section(
        "Options for linking",
        "", [],
        [SubSectionLinkGen, SubSectionLinkCCsharp,
        SubSectionLinkC, SubSectionLinkJava, SubSectionLinkCsharp]),

    SectionFileSearch = one_level_section(
        help_option_group(
            "Options controlling searches for files",
            "", [], [oc_search])),

    SectionBuild = one_level_section(
        help_option_group(
            "Options controlling the library installation process",
            "", [], [oc_buildsys])),

    SectionEnv = one_level_section(
        help_option_group(
            "Options specifying properties of the environment",
            "", [], [oc_env])),

    SectionConfig = one_level_section(
        help_option_group(
            "Options that record autoconfigured parameters",
            "", [], [oc_config])),

    SectionMconfig = one_level_section(
        help_option_group(
            "Options reserved for Mercury.config files",
            "", [], [oc_mconfig])),

    SubSectionDevCtrl = help_option_group(
        "Operation selection options for developers only",
        "", [], [oc_dev_ctrl]),
    SubSectionDevDebug = help_option_group(
        "Options that can help debug the compiler",
        "", [], [oc_dev_debug]),
    SubSectionDevDump = help_option_group(
        "Options for dumping internal compiler data structures",
        "", [], [oc_dev_dump]),
    SubSectionDevInternal = help_option_group(
        "Options intended for internal use by the compiler only",
        "", [], [oc_internal]),
    SectionDev = two_level_section("Options for developers only",
        "", [],
        [SubSectionDevCtrl, SubSectionDevDebug, SubSectionDevDump,
        SubSectionDevInternal]),

    SectionUnused = one_level_section(
        help_option_group(
            "Now-unused former options kept for compatibility",
            "", [], [oc_unused])),

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

:- pred document_requested_options(help_format, print_what_help,
    list(menu_item), list(string)).
:- mode document_requested_options(in(help_plain_text), in, out, out) is det.
:- mode document_requested_options(in(help_texinfo), in, out, out) is det.

document_requested_options(Format, What, SectionNames, OptionsLines) :-
    bool_option_initial_n_y(InitialNoOptions, InitialYesOptions),
    map.init(InitialValueMap0),
    list.foldl(insert_initial(no), InitialNoOptions,
        InitialValueMap0, InitialValueMap1),
    list.foldl(insert_initial(yes), InitialYesOptions,
        InitialValueMap1, InitialValueMap),

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
    acc_help_sections(InitialValueMap, Format, What, all_chapters,
        AllCategoriesSet, UndoneCategoriesSet,
        cord.init, SectionNameCord, cord.init, OptionsLineCord),
    set.to_sorted_list(UndoneCategoriesSet, UndoneCategories),
    (
        UndoneCategories = []
    ;
        UndoneCategories = [_ | _],
        unexpected($pred, "undone: " ++ string(UndoneCategories))
    ),
    SectionNames = cord.list(SectionNameCord),
    OptionsLines = cord.list(OptionsLineCord).

    % Maps each bool option handled by optimization_options.m
    % to its initial value.
:- type initial_bool_value_map == map(option, bool).

:- pred insert_initial(bool::in, option::in,
    initial_bool_value_map::in, initial_bool_value_map::out) is det.

insert_initial(InitialValue, Option, !InitialValueMap) :-
    map.det_insert(Option, InitialValue, !InitialValueMap).

%---------------------------------------------------------------------------%

:- type menu_item
    --->    menu_item(string, string).
            % The name of the menu item, and its short description, if any.
            % (Nonexistent descriptions are represented by an empty string.)

:- pred acc_help_sections(initial_bool_value_map, help_format, print_what_help,
    list(help_section), set(option_category), set(option_category),
    cord(menu_item), cord(menu_item), cord(string), cord(string)).
:- mode acc_help_sections(in, in(help_plain_text), in, in,
    in, out, in, out, in, out) is det.
:- mode acc_help_sections(in, in(help_texinfo), in, in,
    in, out, in, out, in, out) is det.

acc_help_sections(_, _, _, [], !Categories, !SectionNameCord, !LineCord).
acc_help_sections(InitialValueMap, Format, What, [Section | Sections],
        !Categories, !MenuItemCord, !LineCord) :-
    acc_help_section(InitialValueMap, Format, What, Section,
        !Categories, !MenuItemCord, !LineCord),
    acc_help_sections(InitialValueMap, Format, What, Sections,
        !Categories, !MenuItemCord, !LineCord).

:- pred acc_help_section(initial_bool_value_map, help_format, print_what_help,
    help_section, set(option_category), set(option_category),
    cord(menu_item), cord(menu_item), cord(string), cord(string)).
:- mode acc_help_section(in, in(help_plain_text), in, in,
    in, out, in, out, in, out) is det.
:- mode acc_help_section(in, in(help_texinfo), in, in,
    in, out, in, out, in, out) is det.

acc_help_section(InitialValueMap, Format, What, Section,
        !Categories, !MenuItemCord, !LineCord) :-
    (
        Section = one_level_section(SubSection),
        SubSection = help_option_group(GroupName, MenuDesc, _, _),
        acc_help_option_group(InitialValueMap, Format, What, sos_section,
            SubSection, !Categories, cord.init, _MenuItemCord,
            !LineCord, 0, NumDocOpts),
        ( if NumDocOpts > 0 then
            cord.snoc(menu_item(GroupName, MenuDesc), !MenuItemCord)
        else
            true
        )
    ;
        Section = two_level_section(SectionName, SectionDesc,
            CommentLines, SubSections),
        acc_help_subsections(InitialValueMap, Format, What, SubSections,
            !Categories, cord.init, SubMenuItemCord,
            cord.init, SubSectionsLineCord, 0, NumDocOpts),
        (
            Format = help_plain_text,
            ( if NumDocOpts = 0 then
                true
            else
                some [!GroupLineCord]
                (
                    !:GroupLineCord = cord.init,
                    cord.snoc("", !GroupLineCord),
                    cord.snoc(SectionName, !GroupLineCord),
                    (
                        CommentLines = []
                    ;
                        CommentLines = [_ | _],
                        cord.snoc("", !GroupLineCord),
                        !:GroupLineCord = !.GroupLineCord ++
                            cord.from_list(CommentLines)
                    ),
                    !:GroupLineCord = !.GroupLineCord ++ SubSectionsLineCord,
                    !:LineCord = !.LineCord ++ !.GroupLineCord
                )
            )
        ;
            Format = help_texinfo,
            some [!GroupLineCord]
            (
                !:GroupLineCord = cord.init,
                cord.snoc("", !GroupLineCord),
                add_node_line("@node",    SectionName, !GroupLineCord),
                add_node_line("@section", SectionName, !GroupLineCord),
                add_node_line("@cindex",  SectionName, !GroupLineCord),
                cord.snoc("", !GroupLineCord),
                SubMenuLines = menu_items_to_menu(cord.list(SubMenuItemCord)),
                !:GroupLineCord = !.GroupLineCord ++
                    cord.from_list(SubMenuLines),
                (
                    CommentLines = []
                ;
                    CommentLines = [_ | _],
                    cord.snoc("", !GroupLineCord),
                    !:GroupLineCord = !.GroupLineCord ++
                        cord.from_list(CommentLines)
                ),
                % If we did not gather any non-commented-out option help texts,
                % then comment out the group header as well.
                %
                % Include a menu item for this section only if the section
                % has *some* non-commented-out parts.
                ( if NumDocOpts = 0 then
                    comment_out_texinfo_lines(!GroupLineCord)
                else
                    cord.snoc(menu_item(SectionName, SectionDesc),
                        !MenuItemCord)
                ),
                !:GroupLineCord = !.GroupLineCord ++ SubSectionsLineCord,
                !:LineCord = !.LineCord ++ !.GroupLineCord
            )
        )
    ).

:- pred acc_help_subsections(initial_bool_value_map, help_format,
    print_what_help, list(help_option_group),
    set(option_category), set(option_category),
    cord(menu_item), cord(menu_item), cord(string), cord(string), int, int).
:- mode acc_help_subsections(in, in(help_plain_text), in, in,
    in, out, in, out, in, out, in, out) is det.
:- mode acc_help_subsections(in, in(help_texinfo), in, in,
    in, out, in, out, in, out, in, out) is det.

acc_help_subsections(_, _, _, [],
        !Categories, !MenuItemCord, !LineCord, !NumDocOpts).
acc_help_subsections(InitialValueMap, Format, What, [SubSection | SubSections],
        !Categories, !MenuItemCord, !LineCord, !NumDocOpts) :-
    acc_help_option_group(InitialValueMap, Format, What, sos_subsection,
        SubSection, !Categories, !MenuItemCord, !LineCord, !NumDocOpts),
    acc_help_subsections(InitialValueMap, Format, What,
        SubSections, !Categories, !MenuItemCord, !LineCord, !NumDocOpts).

:- pred acc_help_option_group(initial_bool_value_map, help_format,
    print_what_help, section_or_subsection, help_option_group,
    set(option_category), set(option_category),
    cord(menu_item), cord(menu_item), cord(string), cord(string), int, int).
:- mode acc_help_option_group(in, in(help_plain_text), in, in, in,
    in, out, in, out, in, out, in, out) is det.
:- mode acc_help_option_group(in, in(help_texinfo), in, in, in,
    in, out, in, out, in, out, in, out) is det.

acc_help_option_group(InitialValueMap, Format, What, SubOrNot, Group,
        !Categories, !MenuItemCord, !LineCord, !NumDocOpts) :-
    Group = help_option_group(GroupName, MenuDesc, CommentLines, Categories),
    set.det_remove_list(Categories, !Categories),
    list.map(get_optdb_records_in_category, Categories, OptdbRecordSets),
    OptdbRecordSet = set.union_list(OptdbRecordSets),

    acc_help_messages(InitialValueMap, Format, What,
        set.to_sorted_list(OptdbRecordSet),
        cord.init, HelpTextLinesCord, 0, GroupNumDocOpts),
    !:NumDocOpts = !.NumDocOpts + GroupNumDocOpts,
    (
        Format = help_plain_text,
        ( if GroupNumDocOpts = 0 then
            true
        else
            % Let section names start at the left margin, but
            % indent the names of subsections.
            ( SubOrNot = sos_section,    NameIndent = ""
            ; SubOrNot = sos_subsection, NameIndent = single_indent
            ),
            some [!GroupLineCord]
            (
                !:GroupLineCord = cord.init,
                cord.snoc("", !GroupLineCord),
                cord.snoc(NameIndent ++ GroupName, !GroupLineCord),
                (
                    CommentLines = []
                ;
                    CommentLines = [_ | _],
                    cord.snoc("", !GroupLineCord),
                    list.foldl(acc_prefixed_line(NameIndent),
                        CommentLines, !GroupLineCord)
                ),
                !:GroupLineCord = !.GroupLineCord ++ HelpTextLinesCord,
                !:LineCord = !.LineCord ++ !.GroupLineCord
            )
        )
    ;
        Format = help_texinfo,
        ( SubOrNot = sos_section,    NodeCmd = "@section "
        ; SubOrNot = sos_subsection, NodeCmd = "@subsection "
        ),
        some [!GroupStartLineCord, !GroupEndLineCord]
        (
            !:GroupStartLineCord = cord.init,
            !:GroupEndLineCord = cord.init,
            cord.snoc("", !GroupStartLineCord),
            add_node_line("@node",   GroupName, !GroupStartLineCord),
            add_node_line(NodeCmd,   GroupName, !GroupStartLineCord),
            add_node_line("@cindex", GroupName, !GroupStartLineCord),
            (
                CommentLines = []
            ;
                CommentLines = [_ | _],
                cord.snoc("", !GroupStartLineCord),
                !:GroupStartLineCord = !.GroupStartLineCord ++
                    cord.from_list(CommentLines)
            ),
            cord.snoc("", !GroupStartLineCord),
            cord.snoc("@table @asis", !GroupStartLineCord),

            cord.snoc("", !GroupEndLineCord),
            cord.snoc("@end table", !GroupEndLineCord),

            % If we did not gather any non-commented-out option help texts,
            % then comment out the group header as well.
            ( if GroupNumDocOpts = 0 then
                !:GroupStartLineCord = cord.map(comment_out_texinfo_line,
                    !.GroupStartLineCord),
                !:GroupEndLineCord = cord.map(comment_out_texinfo_line,
                    !.GroupEndLineCord)
            else
                cord.snoc(menu_item(GroupName, MenuDesc), !MenuItemCord)
            ),
            GroupLineCord = !.GroupStartLineCord ++ HelpTextLinesCord ++
                !.GroupEndLineCord,
            !:LineCord = !.LineCord ++ GroupLineCord
        )
    ).

:- pred get_optdb_records_in_category(option_category::in,
    set(optdb_record)::out) is det.

get_optdb_records_in_category(Cat, OptdbRecordSet) :-
    OptdbPred =
        ( pred(OptdbRecord::out) is multi :-
            optdb(Cat, Opt, OptData, Help),
            OptdbRecord = optdb_record(Opt, Cat, OptData, Help)
        ),
    solutions_set(OptdbPred, OptdbRecordSet).

%---------------------------------------------------------------------------%

:- type optdb_record
    --->    optdb_record(
                % Put the option first, so that we can use the order
                % of options in the option type to control the relative
                % ordering of options in the help text even if they are
                % in nominally-different option categories (such as
                % oc_warn_style vs oc_warn_style_c).
                option,
                option_category,
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

:- type index_versions
    --->    index_positive_only
    ;       index_negative_only
    ;       index_positive_and_negative.

:- type option_params
    --->    option_params(
                op_expect_arg           :: maybe_expect_arg,
                op_negate               :: maybe_negate,
                op_add_negative_opt     :: maybe_add_negative,
                op_index_versions       :: index_versions
            ).

%---------------------------------------------------------------------------%

:- pred get_optdb_record_params(initial_bool_value_map::in, optdb_record::in,
    option_params::out) is det.

get_optdb_record_params(InitialValueMap, OptdbRecord, Params) :-
    OptdbRecord = optdb_record(Option, _Cat, OptionData, _Help),
    (
        OptionData = bool(Bool),
        MaybeExpectArg = do_not_expect_arg,
        (
            Bool = no,
            MaybeNegate = do_not_negate,
            % The negative version is the default, so the absence
            % of the option is just as good as its negation.
            % We do not want to create index entries for e.g.
            % --no-help, --no-make-int, etc.
            IndexVersions = index_positive_only
        ;
            Bool = yes,
            MaybeNegate = negate,
            % The negative version is the default, so the absence
            % of the option is just as good as its positive version.
            % Nevertheless, if we have an index entry for -no-xyz,
            % it would look strange to have no index entry for --xyz.
            IndexVersions = index_positive_and_negative
        ),
        MaybeAddNegVersionOpt = no_negative_version
    ;
        OptionData = bool_special,
        MaybeExpectArg = do_not_expect_arg,
        ( if map.search(InitialValueMap, Option, InitialBool) then
            (
                InitialBool = no,
                MaybeNegate = do_not_negate,
                IndexVersions = index_positive_only
            ;
                InitialBool = yes,
                MaybeNegate = negate,
                IndexVersions = index_positive_and_negative
            )
        else
            MaybeNegate = do_not_negate,
            IndexVersions = index_positive_and_negative
        ),
        MaybeAddNegVersionOpt = no_negative_version
    ;
        ( OptionData = int(_)
        ; OptionData = string(_)
        ; OptionData = int_special
        ; OptionData = string_special
        ),
        MaybeExpectArg = expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersionOpt = no_negative_version,
        IndexVersions = index_positive_only
    ;
        OptionData = special,
        MaybeExpectArg = do_not_expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersionOpt = no_negative_version,
        IndexVersions = index_positive_only
    ;
        OptionData = file_special,
        MaybeExpectArg = expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersionOpt = no_negative_version,
        IndexVersions = index_positive_only
    ;
        ( OptionData = accumulating(_)
        ; OptionData = maybe_int(_)
        ; OptionData = maybe_string(_)
        ; OptionData = maybe_string_special
        ),
        MaybeExpectArg = expect_arg,
        MaybeNegate = do_not_negate,
        MaybeAddNegVersionOpt = add_negative_version,
        IndexVersions = index_positive_and_negative
    ),
    Params = option_params(MaybeExpectArg, MaybeNegate,
        MaybeAddNegVersionOpt, IndexVersions).

%---------------------------------------------------------------------------%

:- pred acc_help_messages(initial_bool_value_map, help_format, print_what_help,
    list(optdb_record), cord(string), cord(string), int, int).
:- mode acc_help_messages(in, in(help_plain_text), in, in, in, out, in, out)
    is det.
:- mode acc_help_messages(in, in(help_texinfo), in, in, in, out, in, out)
    is det.

acc_help_messages(_, _, _, [], !EffectiveLinesCord, !NumDocOpts).
acc_help_messages(InitialValueMap, Format, What, [OptdbRecord | OptdbRecords],
        !EffectiveLinesCord, !NumDocOpts) :-
    (
        Format = help_plain_text,
        acc_help_message_plain(InitialValueMap, What, OptdbRecord,
            !EffectiveLinesCord, !NumDocOpts)
    ;
        Format = help_texinfo,
        % For the user guide, we always add documentation for every option,
        % though private ones are commented out.
        acc_help_message_texinfo(InitialValueMap, OptdbRecord,
            !EffectiveLinesCord, !NumDocOpts)
    ),
    acc_help_messages(InitialValueMap, Format, What, OptdbRecords,
        !EffectiveLinesCord, !NumDocOpts).

:- pred acc_help_message_plain(initial_bool_value_map::in, print_what_help::in,
    optdb_record::in,
    cord(string)::in, cord(string)::out, int::in, int::out) is det.

acc_help_message_plain(InitialValueMap, What, OptdbRecord,
        !EffectiveLinesCord, !NumDocOpts) :-
    get_optdb_record_params(InitialValueMap, OptdbRecord, Params),
    OptdbRecord = optdb_record(Option, _Cat, OptionData, Help),
    some [!LineCord]
    (
        !:LineCord = cord.init,
        (
            Help = no_help,
            PublicOrPrivate = help_private,
            DescPieces = []
        ;
            Help = unnamed_help(DescPieces),
            PublicOrPrivate = help_private,
            string.format("%sUNNAMED OPTION %s",
                [s(single_indent), s(string(Option))], NameLine),
            cord.snoc(NameLine, !LineCord)
        ;
            Help = gen_help(ShortNames, LongName, AltLongNames,
                PublicOrPrivate, DescPieces),
            acc_short_option_names_plain(Params, Option, no_arg, no_align,
                ShortNames, !LineCord),
            acc_long_option_name_plain(Params, Option, no_arg, no_align,
                LongName, !LineCord),
            acc_long_option_names_plain(Params, Option, no_arg, no_align,
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
            acc_long_option_name_plain(Params, Option, MaybeArg, no_align,
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
            acc_long_option_name_plain(Params, Option, MaybeArg, no_align,
                LongName, !LineCord),
            acc_long_option_names_plain(Params, Option, MaybeArg, no_align,
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
            acc_short_option_name_plain(Params, Option, MaybeArg, no_align,
                ShortName, !LineCord),
            acc_long_option_name_plain(Params, Option, MaybeArg, no_align,
                LongName, !LineCord),
            acc_long_option_names_plain(Params, Option, MaybeArg, no_align,
                AltLongNames, !LineCord)
        ;
            (
                Help = alt_align_help(LongName, AltLongNames,
                    AlignedText, _, DescPieces),
                PublicOrPrivate = help_public
            ;
                Help = priv_alt_align_help(LongName, AltLongNames,
                    AlignedText, _, DescPieces),
                PublicOrPrivate = help_private
            ),
            MaybeArg = no_arg,
            Align = aligned_text(AlignedText),
            acc_long_option_name_plain(Params, Option, MaybeArg, Align,
                LongName, !LineCord),
            % The aligned text is added only to the first option name line.
            acc_long_option_names_plain(Params, Option, MaybeArg, no_align,
                AltLongNames, !LineCord)
        ;
            Help = short_alt_align_help(ShortName, LongName, AltLongNames,
                AlignedText, _, DescPieces),
            PublicOrPrivate = help_public,
            acc_short_option_name_plain(Params, Option, no_arg,
                aligned_text(AlignedText), ShortName, !LineCord),
            % The aligned text is added only to the first option name line.
            acc_long_option_name_plain(Params, Option, no_arg, no_align,
                LongName, !LineCord),
            acc_long_option_names_plain(Params, Option, no_arg, no_align,
                AltLongNames, !LineCord)
        ;
            Help = no_align_help(LongName, AlignedText, NoAlignedText,
                _, _, DescPieces),
            PublicOrPrivate = help_public,
            expect(is_bool(OptionData), $pred,
                "unexpected use of no_align_help"),
            ParamsNN = Params ^ op_negate := do_not_negate,
            FirstLine0 = long_option_name_line_plain(ParamsNN, Option,
                no_arg, LongName),
            SecondLine0 = long_negated_option_name_line_plain(LongName),
            % In this case, we add *different* aligned text to each line.
            add_aligned_text(AlignedText, FirstLine0, FirstLine),
            add_aligned_text(NoAlignedText, SecondLine0, SecondLine),
            cord.snoc(FirstLine, !LineCord),
            cord.snoc(SecondLine, !LineCord)
        ;
            Help = alt_arg_align_help(LongName, ArgAligns, DescPieces),
            PublicOrPrivate = help_public,
            % In this case, we add *different* aligned text to each line.
            list.foldl(acc_arg_align_text_plain(Params, Option, LongName),
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
            !:NumDocOpts = !.NumDocOpts + 1,
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
                reflow_lines(help_plain_text, 71, EffDescPieces,
                    _CindexTopics, _FindexTopics, ReflowLines),
                BlankLineCord = cord.singleton(""),
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
            )
        else
            true
        )
    ).

%---------------------%

:- pred acc_help_message_texinfo(initial_bool_value_map::in, optdb_record::in,
    cord(string)::in, cord(string)::out, int::in, int::out) is det.

acc_help_message_texinfo(InitialValueMap, OptdbRecord,
        !EffectiveLinesCord, !NumDocOpts) :-
    get_optdb_record_params(InitialValueMap, OptdbRecord, Params),
    OptdbRecord = optdb_record(Option, _Cat, OptionData, Help),
    some [!LineCord, !OptLineCord, !IndexLineCord]
    (
        !:OptLineCord = cord.init,
        !:IndexLineCord = cord.init,
        (
            Help = no_help,
            PublicOrPrivate = help_private,
            string.format("NO_HELP OPTION %s", [s(string(Option))], NameLine),
            cord.snoc(NameLine, !OptLineCord),
            DescPieces = []
        ;
            Help = unnamed_help(DescPieces),
            PublicOrPrivate = help_private,
            string.format("UNNAMED OPTION %s", [s(string(Option))], NameLine),
            cord.snoc(NameLine, !OptLineCord)
        ;
            Help = gen_help(ShortNames, LongName, AltLongNames,
                PublicOrPrivate, DescPieces),
            acc_short_option_names_texinfo(Params, Option, no_arg, no_align,
                ShortNames, !OptLineCord, !IndexLineCord),
            acc_long_option_name_texinfo(Params, Option, no_arg, no_align,
                LongName, !OptLineCord, !IndexLineCord),
            acc_long_option_names_texinfo(Params, Option, no_arg, no_align,
                AltLongNames, !OptLineCord, !IndexLineCord)
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
            acc_long_option_name_texinfo(Params, Option, MaybeArg, no_align,
                LongName, !OptLineCord, !IndexLineCord)
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
            acc_long_option_name_texinfo(Params, Option, MaybeArg, no_align,
                LongName, !OptLineCord, !IndexLineCord),
            acc_long_option_names_texinfo(Params, Option, MaybeArg, no_align,
                AltLongNames, !OptLineCord, !IndexLineCord)
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
            acc_short_option_name_texinfo(Params, Option, MaybeArg, no_align,
                ShortName, !OptLineCord, !IndexLineCord),
            acc_long_option_name_texinfo(Params, Option, MaybeArg, no_align,
                LongName, !OptLineCord, !IndexLineCord),
            acc_long_option_names_texinfo(Params, Option, MaybeArg, no_align,
                AltLongNames, !OptLineCord, !IndexLineCord)
        ;
            (
                Help = alt_align_help(LongName, AltLongNames,
                    _, AlignedText, DescPieces),
                PublicOrPrivate = help_public
            ;
                Help = priv_alt_align_help(LongName, AltLongNames,
                    _, AlignedText, DescPieces),
                PublicOrPrivate = help_private
            ),
            MaybeArg = no_arg,
            Align = aligned_text(AlignedText),
            acc_long_option_name_texinfo(Params, Option, MaybeArg, Align,
                LongName, !OptLineCord, !IndexLineCord),
            % The aligned text is added only to the first option name line.
            acc_long_option_names_texinfo(Params, Option, MaybeArg, no_align,
                AltLongNames, !OptLineCord, !IndexLineCord)
        ;
            Help = short_alt_align_help(ShortName, LongName, AltLongNames,
                _, AlignedText, DescPieces),
            PublicOrPrivate = help_public,
            acc_short_option_name_texinfo(Params, Option, no_arg,
                aligned_text(AlignedText), ShortName,
                !OptLineCord, !IndexLineCord),
            % The aligned text is added only to the first option name line.
            acc_long_option_name_texinfo(Params, Option, no_arg, no_align,
                LongName, !OptLineCord, !IndexLineCord),
            acc_long_option_names_texinfo(Params, Option, no_arg, no_align,
                AltLongNames, !OptLineCord, !IndexLineCord)
        ;
            Help = no_align_help(LongName, _, _, AlignedText, NoAlignedText,
                DescPieces),
            PublicOrPrivate = help_public,
            expect(is_bool(OptionData), $pred,
                "unexpected use of no_align_help"),
            ParamsNN = Params ^ op_negate := do_not_negate,
            long_option_name_lines_texinfo(ParamsNN, Option,
                no_arg, LongName, FirstOptLine0, FirstIndexLine),
            SecondIndexLine = long_negated_option_name_texinfo(LongName),
            string.format("@code{%s}", [s(SecondIndexLine)], SecondOptLine0),
            % In this case, we add *different* aligned text to each line.
            FirstOptLine = FirstOptLine0 ++ " " ++ AlignedText,
            SecondOptLine = SecondOptLine0 ++ " " ++ NoAlignedText,
            add_option_line_texinfo(FirstOptLine, !OptLineCord),
            add_option_line_texinfo(SecondOptLine, !OptLineCord),
            add_findex_line_texinfo(FirstIndexLine, !IndexLineCord),
            add_findex_line_texinfo(SecondIndexLine, !IndexLineCord)
        ;
            Help = alt_arg_align_help(LongName, ArgAligns, DescPieces),
            PublicOrPrivate = help_public,
            % In this case, we add *different* aligned text to each line.
            list.foldl2(acc_arg_align_text_texinfo(Params, Option, LongName),
                ArgAligns, !OptLineCord, !IndexLineCord),
            % The option lines each contain both the option name and
            % one of different arg names, but the index lines contain only
            % the option name. We therefore need to get rid of duplicates.
            AAIndexLines0 = cord.list(!.IndexLineCord),
            list.sort_and_remove_dups(AAIndexLines0, AAIndexLines),
            !:IndexLineCord = cord.from_list(AAIndexLines)
        ),
        (
            DescPieces = [],
            EffDescPieces = [w("There is no help text available.")]
        ;
            DescPieces = [_ | _],
            EffDescPieces = DescPieces
        ),
        % ZZZ 71
        reflow_lines(help_texinfo, 71, EffDescPieces,
            CindexTopics, FindexTopics, ReflowLines),
        list.foldl(add_cindex_line_texinfo, CindexTopics, !IndexLineCord),
        list.foldl(add_findex_line_texinfo, FindexTopics, !IndexLineCord),
        BlankLineCord = cord.singleton(""),
        SpaceLine = cord.singleton("@sp 1"),
        !:LineCord = SpaceLine ++ !.OptLineCord ++ !.IndexLineCord ++
            cord.from_list(ReflowLines),
        (
            PublicOrPrivate = help_public,
            !:NumDocOpts = !.NumDocOpts + 1
        ;
            PublicOrPrivate = help_private,
            comment_out_texinfo_lines(!LineCord)
        ),
        !:EffectiveLinesCord = !.EffectiveLinesCord ++
            BlankLineCord ++ !.LineCord
    ).

%---------------------------------------------------------------------------%

:- pred acc_arg_align_text_plain(option_params::in, option::in, string::in,
    arg_align::in, cord(string)::in, cord(string)::out) is det.

acc_arg_align_text_plain(Params, Option, LongName, ArgAlign, !LineCord) :-
    Params = option_params(MaybeExpectArg, MaybeNegate,
        MaybeAddNegVersionOpt, IndexVersions),
    expect(unify(MaybeExpectArg, expect_arg), $pred,
        "unexpected MaybeExpectArg"),
    expect(unify(MaybeNegate, do_not_negate), $pred,
        "unexpected MaybeNegate"),
    expect(unify(MaybeAddNegVersionOpt, no_negative_version), $pred,
        "unexpected MaybeAddNegVersionOpt"),
    expect(unify(IndexVersions, index_positive_only), $pred,
        "unexpected IndexVersions"),

    ArgAlign = arg_align(ArgName, AlignedText, _),
    Line0 = long_option_name_line_plain(Params, Option,
        arg_name(ArgName), LongName),
    add_aligned_text(AlignedText, Line0, Line),
    cord.snoc(Line, !LineCord).

:- pred acc_arg_align_text_texinfo(option_params::in, option::in, string::in,
    arg_align::in, cord(string)::in, cord(string)::out,
    cord(string)::in, cord(string)::out) is det.

acc_arg_align_text_texinfo(Params, Option, LongName, ArgAlign,
        !OptLineCord, !IndexLineCord) :-
    Params = option_params(MaybeExpectArg, MaybeNegate,
        MaybeAddNegVersionOpt, IndexVersions),
    expect(unify(MaybeExpectArg, expect_arg), $pred,
        "unexpected MaybeExpectArg"),
    expect(unify(MaybeNegate, do_not_negate), $pred,
        "unexpected MaybeNegate"),
    expect(unify(MaybeAddNegVersionOpt, no_negative_version), $pred,
        "unexpected MaybeAddNegVersionOpt"),
    expect(unify(IndexVersions, index_positive_only), $pred,
        "unexpected IndexVersions"),

    ArgAlign = arg_align(ArgName, _, AlignedText),
    long_option_name_lines_texinfo(Params, Option, arg_name(ArgName), LongName,
        OptLine0, IndexLine),
    OptLine = OptLine0 ++ " " ++ AlignedText,
    add_option_line_texinfo(OptLine, !OptLineCord),
    add_findex_line_texinfo(IndexLine, !IndexLineCord).

%---------------------%

:- type maybe_arg_name
    --->    no_arg
    ;       arg_name(string).

:- type maybe_aligned_text
    --->    no_align
    ;       aligned_text(string).

%---------------------%

% The next four predicates next are needed because folds over
% acc_{long,short}_option_name_{plain,texinfo} do not preserve the inst of the
% maybe_aligned_text argument.

:- pred acc_long_option_names_plain(option_params, option, maybe_arg_name,
    maybe_aligned_text, list(string), cord(string), cord(string)).
:- mode acc_long_option_names_plain(in, in, in,
    in(bound(no_align)), in, in, out) is det.
:- mode acc_long_option_names_plain(in, in, in,
    in(bound(aligned_text(ground))), in, in, out) is det.

acc_long_option_names_plain(_, _, _, _, [], !LineCord).
acc_long_option_names_plain(Params, Option, MaybeArgName,
        MaybeAlignedText, [LongName | LongNames], !LineCord) :-
    acc_long_option_name_plain(Params, Option, MaybeArgName,
        MaybeAlignedText, LongName, !LineCord),
    acc_long_option_names_plain(Params, Option, MaybeArgName,
        MaybeAlignedText, LongNames, !LineCord).

:- pred acc_short_option_names_plain(option_params, option, maybe_arg_name,
    maybe_aligned_text, list(char), cord(string), cord(string)).
:- mode acc_short_option_names_plain(in, in, in,
    in(bound(no_align)), in, in, out) is det.
:- mode acc_short_option_names_plain(in, in, in,
    in(bound(aligned_text(ground))), in, in, out) is det.

acc_short_option_names_plain(_, _, _, _, [], !LineCord).
acc_short_option_names_plain(Params, Option, MaybeArgName,
        MaybeAlignedText, [ShortName | ShortNames], !LineCord) :-
    acc_short_option_name_plain(Params, Option, MaybeArgName,
        MaybeAlignedText, ShortName, !LineCord),
    acc_short_option_names_plain(Params, Option, MaybeArgName,
        MaybeAlignedText, ShortNames, !LineCord).

:- pred acc_long_option_names_texinfo(option_params, option, maybe_arg_name,
    maybe_aligned_text, list(string),
    cord(string), cord(string), cord(string), cord(string)).
:- mode acc_long_option_names_texinfo(in, in, in,
    in(bound(no_align)), in, in, out, in, out) is det.
:- mode acc_long_option_names_texinfo(in, in, in,
    in(bound(aligned_text(ground))), in, in, out, in, out) is det.

acc_long_option_names_texinfo(_, _, _, _, [], !OptLineCord, !IndexLineCord).
acc_long_option_names_texinfo(Params, Option, MaybeArgName,
        MaybeAlignedText, [LongName | LongNames],
        !OptLineCord, !IndexLineCord) :-
    acc_long_option_name_texinfo(Params, Option, MaybeArgName,
        MaybeAlignedText, LongName, !OptLineCord, !IndexLineCord),
    acc_long_option_names_texinfo(Params, Option, MaybeArgName,
        MaybeAlignedText, LongNames, !OptLineCord, !IndexLineCord).

:- pred acc_short_option_names_texinfo(option_params, option, maybe_arg_name,
    maybe_aligned_text, list(char),
    cord(string), cord(string), cord(string), cord(string)).
:- mode acc_short_option_names_texinfo(in, in, in,
    in(bound(no_align)), in, in, out, in, out) is det.
:- mode acc_short_option_names_texinfo(in, in, in,
    in(bound(aligned_text(ground))), in, in, out, in, out) is det.

acc_short_option_names_texinfo(_, _, _, _, [], !OptLineCord, !IndexLineCord).
acc_short_option_names_texinfo(Params, Option, MaybeArgName,
        MaybeAlignedText, [ShortName | ShortNames],
        !OptLineCord, !IndexLineCord) :-
    acc_short_option_name_texinfo(Params, Option, MaybeArgName,
        MaybeAlignedText, ShortName, !OptLineCord, !IndexLineCord),
    acc_short_option_names_texinfo(Params, Option, MaybeArgName,
        MaybeAlignedText, ShortNames, !OptLineCord, !IndexLineCord).

%---------------------%

:- pred acc_long_option_name_plain(option_params, option, maybe_arg_name,
    maybe_aligned_text, string, cord(string), cord(string)).
:- mode acc_long_option_name_plain(in, in, in,
    in(bound(no_align)), in, in, out) is det.
:- mode acc_long_option_name_plain(in, in, in,
    in(bound(aligned_text(ground))), in, in, out) is det.

acc_long_option_name_plain(Params, Option, MaybeArgName, MaybeAlignedText,
        LongName, !LineCord) :-
    FirstLine0 = long_option_name_line_plain(Params, Option,
        MaybeArgName, LongName),
    (
        MaybeAlignedText = no_align,
        FirstLine = FirstLine0
    ;
        MaybeAlignedText = aligned_text(AlignedText),
        add_aligned_text(AlignedText, FirstLine0, FirstLine)
    ),
    cord.snoc(FirstLine, !LineCord),
    Params = option_params(_MaybeExpectArg, _MaybeNegate,
        MaybeAddNegVersionOpt, _IndexVersions),
    (
        MaybeAddNegVersionOpt = no_negative_version
    ;
        MaybeAddNegVersionOpt = add_negative_version,
        SecondLine = long_negated_option_name_line_plain(LongName),
        cord.snoc(SecondLine, !LineCord)
    ).

:- pred acc_short_option_name_plain(option_params, option, maybe_arg_name,
    maybe_aligned_text, char, cord(string), cord(string)).
:- mode acc_short_option_name_plain(in, in, in,
    in(bound(no_align)), in, in, out) is det.
:- mode acc_short_option_name_plain(in, in, in,
    in(bound(aligned_text(ground))), in, in, out) is det.

acc_short_option_name_plain(Params, Option, MaybeArgName, MaybeAlignedText,
        ShortName, !LineCord) :-
    FirstLine0 = short_option_name_line_plain(Params, Option, MaybeArgName,
        ShortName),
    (
        MaybeAlignedText = no_align,
        FirstLine = FirstLine0
    ;
        MaybeAlignedText = aligned_text(AlignedText),
        add_aligned_text(AlignedText, FirstLine0, FirstLine)
    ),
    cord.snoc(FirstLine, !LineCord),
    Params = option_params(_MaybeExpectArg, _MaybeNegate,
        MaybeAddNegVersionOpt, _IndexVersions),
    (
        MaybeAddNegVersionOpt = no_negative_version
    ;
        MaybeAddNegVersionOpt = add_negative_version,
        SecondLine = short_negated_option_name_line_plain(ShortName),
        cord.snoc(SecondLine, !LineCord)
    ).

:- pred acc_long_option_name_texinfo(option_params, option, maybe_arg_name,
    maybe_aligned_text, string,
    cord(string), cord(string), cord(string), cord(string)).
:- mode acc_long_option_name_texinfo(in, in, in,
    in(bound(no_align)), in, in, out, in, out) is det.
:- mode acc_long_option_name_texinfo(in, in, in,
    in(bound(aligned_text(ground))), in, in, out, in, out) is det.

acc_long_option_name_texinfo(Params, Option, MaybeArgName, MaybeAlignedText,
        LongName, !OptLineCord, !IndexLineCord) :-
    long_option_name_lines_texinfo(Params, Option, MaybeArgName, LongName,
        FirstOptLine0, _FirstIndexLine),
    (
        MaybeAlignedText = no_align,
        FirstOptLine = FirstOptLine0
    ;
        MaybeAlignedText = aligned_text(AlignedText),
        FirstOptLine = FirstOptLine0 ++ " " ++ AlignedText
    ),
    add_option_line_texinfo(FirstOptLine, !OptLineCord),
    Params = option_params(_MaybeExpectArg, _MaybeNegate,
        MaybeAddNegVersionOpt, IndexVersions),
    NegatedOptionName = long_negated_option_name_texinfo(LongName),
    (
        MaybeAddNegVersionOpt = no_negative_version
    ;
        MaybeAddNegVersionOpt = add_negative_version,
        % ZZZ
        add_option_line_texinfo(NegatedOptionName, !OptLineCord)
    ),
    PosParams = Params ^ op_negate := do_not_negate,
    NegParams = Params ^ op_negate := negate,
    % XXX We *could avoid this call if Params ^ op_negate is do_not_negate,
    % by simply setting PosIndexLine = _FirstIndexLine.
    long_option_name_lines_texinfo(PosParams, Option, MaybeArgName,
        LongName, _, PosIndexLine),
    % We cannot move the calls that create of the NegIndexLine here,
    % because that would lead to an assertion failure for non-negatable
    % options.
    (
        IndexVersions = index_positive_only,
        add_findex_line_texinfo(PosIndexLine, !IndexLineCord)
    ;
        IndexVersions = index_negative_only,
        long_option_name_lines_texinfo(NegParams, Option, MaybeArgName,
            LongName, _, NegIndexLine),
        add_findex_line_texinfo(NegIndexLine, !IndexLineCord)
    ;
        IndexVersions = index_positive_and_negative,
        long_option_name_lines_texinfo(NegParams, Option, MaybeArgName,
            LongName, _, NegIndexLine),
        add_findex_line_texinfo(PosIndexLine, !IndexLineCord),
        add_findex_line_texinfo(NegIndexLine, !IndexLineCord)
    ).

:- pred acc_short_option_name_texinfo(option_params, option, maybe_arg_name,
    maybe_aligned_text, char,
    cord(string), cord(string), cord(string), cord(string)).
:- mode acc_short_option_name_texinfo(in, in, in,
    in(bound(no_align)), in, in, out, in, out) is det.
:- mode acc_short_option_name_texinfo(in, in, in,
    in(bound(aligned_text(ground))), in, in, out, in, out) is det.

acc_short_option_name_texinfo(Params, Option, MaybeArgName, MaybeAlignedText,
        ShortName, !OptLineCord, !IndexLineCord) :-
    short_option_name_lines_texinfo(Params, Option, MaybeArgName, ShortName,
        FirstOptLine0, _FirstIndexLine),
    (
        MaybeAlignedText = no_align,
        FirstOptLine = FirstOptLine0
    ;
        MaybeAlignedText = aligned_text(AlignedText),
        FirstOptLine = FirstOptLine0 ++ " " ++ AlignedText
    ),
    add_option_line_texinfo(FirstOptLine, !OptLineCord),
    Params = option_params(_MaybeExpectArg, _MaybeNegate,
        MaybeAddNegVersionOpt, IndexVersions),
    NegatedOptionName = short_negated_option_name_texinfo(ShortName),
    (
        MaybeAddNegVersionOpt = no_negative_version
    ;
        MaybeAddNegVersionOpt = add_negative_version,
        % ZZZ
        add_option_line_texinfo(NegatedOptionName, !OptLineCord)
    ),
    PosParams = Params ^ op_negate := do_not_negate,
    NegParams = Params ^ op_negate := negate,
    % XXX We *could avoid this call if Params ^ op_negate is do_not_negate,
    % by simply setting PosIndexLine = _FirstIndexLine.
    short_option_name_lines_texinfo(PosParams, Option, MaybeArgName,
        ShortName, _, PosIndexLine),
    % We cannot move the calls that create of the NegIndexLine here,
    % because that would lead to an assertion failure for non-negatable
    % options.
    (
        IndexVersions = index_positive_only,
        add_findex_line_texinfo(PosIndexLine, !IndexLineCord)
    ;
        IndexVersions = index_negative_only,
        short_option_name_lines_texinfo(NegParams, Option, MaybeArgName,
            ShortName, _, NegIndexLine),
        add_findex_line_texinfo(NegIndexLine, !IndexLineCord)
    ;
        IndexVersions = index_positive_and_negative,
        short_option_name_lines_texinfo(NegParams, Option, MaybeArgName,
            ShortName, _, NegIndexLine),
        add_findex_line_texinfo(PosIndexLine, !IndexLineCord),
        add_findex_line_texinfo(NegIndexLine, !IndexLineCord)
    ).

%---------------------%

:- func long_option_name_line_plain(option_params, option, maybe_arg_name,
    string) = string.

long_option_name_line_plain(Params, Option, MaybeArgName, LongName0) = Line :-
    Indent = single_indent,
    Params = option_params(MaybeExpectArg, MaybeNegate,
        _MaybeAddNegVersionOpt, _IndexVersions),
    (
        MaybeNegate = negate,
        expect(unify(MaybeArgName, no_arg), $pred, "MaybeArgName != no_arg"),
        maybe_have_arg(MaybeExpectArg, Option, MaybeArgName,
            LongName0, LongName),
        Line = long_negated_option_name_line_plain(LongName)
    ;
        MaybeNegate = do_not_negate,
        (
            MaybeArgName = no_arg,
            have_no_arg(MaybeExpectArg, Option, LongName0, LongName),
            string.format("%s--%s", [s(Indent), s(LongName)], Line)
        ;
            MaybeArgName = arg_name(ArgName),
            have_arg(MaybeExpectArg, Option, LongName0, LongName),
            MaybeWrappedArgName = maybe_wrap_arg_name_plain(Option, ArgName),
            string.format("%s--%s %s",
                [s(Indent), s(LongName), s(MaybeWrappedArgName)], Line)
        )
    ).

:- func short_option_name_line_plain(option_params, option, maybe_arg_name,
    char) = string.

short_option_name_line_plain(Params, Option, MaybeArgName, ShortName0)
        = Line :-
    Indent = single_indent,
    Params = option_params(MaybeExpectArg, MaybeNegate,
        _MaybeAddNegVersionOpt, _IndexVersions),
    (
        MaybeNegate = negate,
        expect(unify(MaybeArgName, no_arg), $pred, "MaybeArgName != no_arg"),
        maybe_have_arg(MaybeExpectArg, Option, MaybeArgName,
            ShortName0, ShortName),
        Line = short_negated_option_name_line_plain(ShortName)
    ;
        MaybeNegate = do_not_negate,
        (
            MaybeArgName = no_arg,
            have_no_arg(MaybeExpectArg, Option, ShortName0, ShortName),
            string.format("%s-%c", [s(Indent), c(ShortName)], Line)
        ;
            MaybeArgName = arg_name(ArgName),
            have_arg(MaybeExpectArg, Option, ShortName0, ShortName),
            MaybeWrappedArgName = maybe_wrap_arg_name_plain(Option, ArgName),
            string.format("%s-%c %s",
                [s(Indent), c(ShortName), s(MaybeWrappedArgName)], Line)
        )
    ).

:- pred long_option_name_lines_texinfo(option_params::in, option::in,
    maybe_arg_name::in, string::in, string::out, string::out) is det.

long_option_name_lines_texinfo(Params, Option, MaybeArgName, LongName0,
        OptLine, IndexLine) :-
    Params = option_params(MaybeExpectArg, MaybeNegate,
        _MaybeAddNegVersionOpt, _IndexVersions),
    (
        MaybeNegate = negate,
        maybe_have_arg(MaybeExpectArg, Option, MaybeArgName,
            LongName0, LongName),
        IndexLine = long_negated_option_name_texinfo(LongName),
        string.format("@code{--no-%s}", [s(LongName)], OptLine)
    ;
        MaybeNegate = do_not_negate,
        (
            MaybeArgName = no_arg,
            have_no_arg(MaybeExpectArg, Option, LongName0, LongName),
            string.format("--%s", [s(LongName)], IndexLine),
            string.format("@code{--%s}", [s(LongName)], OptLine)
        ;
            MaybeArgName = arg_name(ArgName),
            have_arg(MaybeExpectArg, Option, LongName0, LongName),
            MaybeWrappedArgName = maybe_wrap_arg_name_texinfo(Option, ArgName),
            string.format("--%s", [s(LongName)], IndexLine),
            string.format("@code{--%s %s}",
                [s(LongName), s(MaybeWrappedArgName)], OptLine)
        )
    ).

:- pred short_option_name_lines_texinfo(option_params::in, option::in,
    maybe_arg_name::in, char::in, string::out, string::out) is det.

short_option_name_lines_texinfo(Params, Option, MaybeArgName, ShortName0,
        OptLine, IndexLine) :-
    Params = option_params(MaybeExpectArg, MaybeNegate,
        _MaybeAddNegVersionOpt, _IndexVersions),
    (
        MaybeNegate = negate,
        maybe_have_arg(MaybeExpectArg, Option, MaybeArgName,
            ShortName0, ShortName),
        IndexLine = short_negated_option_name_texinfo(ShortName),
        string.format("@code{-%c}", [c(ShortName)], OptLine)
    ;
        MaybeNegate = do_not_negate,
        (
            MaybeArgName = no_arg,
            have_no_arg(MaybeExpectArg, Option, ShortName0, ShortName),
            string.format("-%c", [c(ShortName)], IndexLine),
            string.format("@code{-%c}", [c(ShortName)], OptLine)
        ;
            MaybeArgName = arg_name(ArgName),
            have_arg(MaybeExpectArg, Option, ShortName0, ShortName),
            MaybeWrappedArgName = maybe_wrap_arg_name_texinfo(Option, ArgName),
            string.format("-%c", [c(ShortName)], IndexLine),
            string.format("@code{-%c %s}",
                [c(ShortName), s(MaybeWrappedArgName)], OptLine)
        )
    ).

%---------------------%

:- func long_negated_option_name_line_plain(string) = string.

long_negated_option_name_line_plain(LongName) = Line :-
    Indent = single_indent,
    string.format("%s--no-%s", [s(Indent), s(LongName)], Line).

:- func short_negated_option_name_line_plain(char) = string.

short_negated_option_name_line_plain(ShortName) = Line :-
    Indent = single_indent,
    string.format("%s-%c-", [s(Indent), c(ShortName)], Line).

:- func long_negated_option_name_texinfo(string) = string.

long_negated_option_name_texinfo(LongName) = NegatedLongName :-
    string.format("--no-%s", [s(LongName)], NegatedLongName).

:- func short_negated_option_name_texinfo(char) = string.

short_negated_option_name_texinfo(ShortName) = NegatedShortName :-
    string.format("-%c-", [c(ShortName)], NegatedShortName).

%---------------------%

:- func maybe_wrap_arg_name_plain(option, string) = string.

maybe_wrap_arg_name_plain(Option, ArgName) = MaybeWrappedArgName :-
    % Do not put <>s around argument "names" that are actually not names,
    % but instead are either
    %
    % - sets of the allowed values wrapped in {}s, or
    % - default optimization levels, such as -O2.
    ( if
        ArgName = ""
    then
        unexpected($pred, string(Option) ++ " has empty arg name")
    else if
        ( string.find_first_char(ArgName, '{', _)
        ; string.find_first_char(ArgName, '-', _)
        )
    then
        MaybeWrappedArgName = ArgName
    else
        MaybeWrappedArgName = "<" ++ ArgName ++ ">"
    ).

:- func maybe_wrap_arg_name_texinfo(option, string) = string.

maybe_wrap_arg_name_texinfo(Option, ArgName) = MaybeWrappedArgName :-
    % Do not put @var{} around argument "names" that are actually not names,
    % but instead are either
    %
    % - sets of the allowed values wrapped in {}s, or
    % - default optimization levels, such as -O2.
    ( if ArgName = "" then
        unexpected($pred, string(Option) ++ " has empty arg name")
    else if string.find_first_char(ArgName, '{', _) then
        string.replace_all(ArgName, "{", "@{", ArgName1),
        string.replace_all(ArgName1, "}", "@}", MaybeWrappedArgName)
    else if string.index(ArgName, 0, '-') then
        MaybeWrappedArgName = ArgName
    else
        string.format("@var{%s}", [s(ArgName)], MaybeWrappedArgName)
    ).

%---------------------------------------------------------------------------%

:- pred add_option_line_texinfo(string::in,
    cord(string)::in, cord(string)::out) is det.

add_option_line_texinfo(OptionStr, !OptLineCord) :-
    ( if cord.is_empty(!.OptLineCord) then
        ItemStr = "@item"
    else
        ItemStr = "@itemx"
    ),
    string.format("%s %s", [s(ItemStr), s(OptionStr)], OptLine),
    cord.snoc(OptLine, !OptLineCord).

:- pred add_findex_line_texinfo(string::in,
    cord(string)::in, cord(string)::out) is det.

add_findex_line_texinfo(OptionStr, !IndexLineCord) :-
    string.format("@findex %s", [s(OptionStr)], IndexLine),
    cord.snoc(IndexLine, !IndexLineCord).

:- pred add_cindex_line_texinfo(string::in,
    cord(string)::in, cord(string)::out) is det.

add_cindex_line_texinfo(Topic, !IndexLineCord) :-
    string.format("@cindex %s", [s(Topic)], IndexLine),
    cord.snoc(IndexLine, !IndexLineCord).

%---------------------------------------------------------------------------%

% The next three predicates all return the last input argument unchanged
% as their only output argument. The jobs of this output argument is simply
% to prevent calls to these predicates from being optimized away.

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

:- pred reflow_lines(help_format, int, list(help_piece),
    list(string), list(string), list(string)).
:- mode reflow_lines(in(help_plain_text), in, in, out, out, out) is det.
:- mode reflow_lines(in(help_texinfo), in, in, out, out, out) is det.

reflow_lines(Format, LineLen, Pieces, CindexTopics, FindexTopics,
        FinishedLines) :-
    % string.count_code_points(IndentStr, IndentLen),
    % AvailLen = LineLen - IndentLen,
    reflow_lines_loop_over_lines(Format, LineLen, Pieces,
        cord.init, CindexCord, cord.init, FindexCord,
        0, _CurLineLen, cord.init, CurLine1, cord.init, FinishedLineCord1),
    CindexTopics = cord.list(CindexCord),
    FindexTopics = cord.list(FindexCord),
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
    cord(string), cord(string), cord(string), cord(string),
    int, int, cur_line, cur_line, finished_lines, finished_lines).
:- mode reflow_lines_loop_over_lines(in(help_plain_text), in, in,
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode reflow_lines_loop_over_lines(in(help_texinfo), in, in,
    in, out, in, out, in, out, in, out, in, out) is det.

reflow_lines_loop_over_lines(Format, LineLen, Pieces, !CindexCord, !FindexCord,
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
            ; HeadPiece = bare_arg(_)
            ; HeadPiece = bare_arg(_, _)
            ; HeadPiece = opt_arg(_, _)
            ; HeadPiece = opt_arg(_, _, _)
            ; HeadPiece = samp(_)
            ; HeadPiece = samp(_, _)
            ; HeadPiece = emph(_)
            ; HeadPiece = emph(_, _)
            ; HeadPiece = env(_)
            ; HeadPiece = env(_, _)
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
                (
                    Format = help_plain_text,
                    string.format("`%s'%s", [s(Option), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("@samp{%s}%s", [s(Option), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = arg(Arg), Suffix = ""
                ; HeadPiece = arg(Arg, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("<%s>%s", [s(Arg), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("@var{%s}%s", [s(Arg), s(Suffix)], Str)
                )
            ;
                ( HeadPiece = bare_arg(Arg), Suffix = ""
                ; HeadPiece = bare_arg(Arg, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("%s%s", [s(Arg), s(Suffix)], Str)
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
                    string.format("@samp{%s @var{%s}}%s",
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
                ( HeadPiece = env(Code), Suffix = ""
                ; HeadPiece = env(Code, Suffix)
                ),
                (
                    Format = help_plain_text,
                    string.format("`%s'%s", [s(Code), s(Suffix)], Str)
                ;
                    Format = help_texinfo,
                    string.format("@env{%s}%s", [s(Code), s(Suffix)], Str)
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
            ),
            add_word(LineLen, Str, !CurLine, !CurLineLen, !FinishedLineCord)
        ;
            ( HeadPiece = ref(_, _, _)
            ; HeadPiece = ref(_, _, _, _)
            ; HeadPiece = xref(_)
            ; HeadPiece = xref(_, _)
            ),
            (
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
            Words = string.words(Str),
            reflow_lines_loop_over_words(LineLen, Words, !CurLine, !CurLineLen,
                !FinishedLineCord)
        ;
            HeadPiece = blank_line,
            finish_cur_line(!.CurLine, !FinishedLineCord),
            (
                Format = help_plain_text,
                cord.snoc("", !FinishedLineCord)
            ;
                Format = help_texinfo,
                cord.snoc("", !FinishedLineCord),
                cord.snoc("@sp 1", !FinishedLineCord)
            ),
            !:CurLine = cord.init,
            !:CurLineLen = 0
        ;
            HeadPiece = help_text_only(HelpTextPieces),
            (
                Format = help_plain_text,
                reflow_lines_loop_over_lines(Format, LineLen, HelpTextPieces,
                    !CindexCord, !FindexCord,
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
                    !CindexCord, !FindexCord,
                    !CurLineLen, !CurLine, !FinishedLineCord)
            )
        ;
            HeadPiece = help_text_texinfo(HelpTextPieces, TexInfoPieces),
            (
                Format = help_plain_text,
                reflow_lines_loop_over_lines(Format, LineLen, HelpTextPieces,
                    !CindexCord, !FindexCord,
                    !CurLineLen, !CurLine, !FinishedLineCord)
            ;
                Format = help_texinfo,
                reflow_lines_loop_over_lines(Format, LineLen, TexInfoPieces,
                    !CindexCord, !FindexCord,
                    !CurLineLen, !CurLine, !FinishedLineCord)
            )
        ;
            HeadPiece = cindex(Topic),
            (
                Format = help_plain_text
            ;
                Format = help_texinfo,
                cord.snoc(Topic, !CindexCord)
            )
        ;
            HeadPiece = findex(Topic),
            (
                Format = help_plain_text
            ;
                Format = help_texinfo,
                cord.snoc(Topic, !FindexCord)
            )
        ),
        reflow_lines_loop_over_lines(Format, LineLen, TailPieces,
            !CindexCord, !FindexCord, !CurLineLen, !CurLine, !FinishedLineCord)
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
        ; Help = alt_align_help(LongName, _, _, _, _)
        ; Help = priv_alt_align_help(LongName, _, _, _, _)
        ; Help = short_alt_align_help(_, LongName, _, _, _, _)
        ; Help = no_align_help(LongName, _, _, _, _, _)
        ; Help = alt_arg_align_help(LongName, _, _)
        ),
        MaybeLongName = yes(LongName)
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

:- pred is_bool(option_data::in) is semidet.

is_bool(bool(_)).

%---------------------------------------------------------------------------%

:- func menu_items_to_menu(list(menu_item)) = list(string).

menu_items_to_menu(MenuItems) = MenuLines :-
    MenuItemLines = list.map(menu_item_to_menu_line, MenuItems),
    MenuLines = ["@menu"] ++ MenuItemLines ++ ["@end menu"].

:- func menu_item_to_menu_line(menu_item) = string.

menu_item_to_menu_line(MenuItem) = Line :-
    MenuItem = menu_item(Name, Desc),
    string.format("* %s:: %s", [s(Name), s(Desc)], Line).

%---------------------------------------------------------------------------%

:- pred add_node_line(string::in, string::in,
    cord(string)::in, cord(string)::out) is det.

add_node_line(NodeCmd, SectionName, !LineCord) :-
    string.format("%-12s %s", [s(NodeCmd), s(SectionName)], Line),
    cord.snoc(Line, !LineCord).

:- pred comment_out_texinfo_lines(cord(string)::in, cord(string)::out) is det.

comment_out_texinfo_lines(!LineCord) :-
    !:LineCord = cord.map(comment_out_texinfo_line, !.LineCord).

:- func comment_out_texinfo_line(string) = string.

comment_out_texinfo_line(Line) = CommentedOutLine :-
    % It is somewhat easier to read commented out sections of the user's guide
    % if lines containing only "@c " do NOT obscure (sub)section boundaries.
    ( if Line = "" then
        CommentedOutLine = ""
    else
        CommentedOutLine = "@c " ++ Line
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
