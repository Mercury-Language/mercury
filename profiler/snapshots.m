%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: snapshots.m.
% Main author: wangp.
%
% This module summarises and outputs the data for memory attribution profiling.
% This is a distinct mode from the rest of the mprof tool.
%
%---------------------------------------------------------------------------%

:- module snapshots.
:- interface.

:- import_module io.

:- pred show_snapshots(io.text_output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module demangle.
:- import_module globals.
:- import_module options.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- type alloc_site_map ==  map(alloc_id, alloc_site).

:- type alloc_id
    --->    alloc_id(int).

:- type alloc_site
            % Field order affects secondary sort order.
    --->    alloc_site(
                alloc_proc          :: string,
                alloc_type          :: string,
                alloc_file_name     :: string,
                alloc_line_number   :: int,
                alloc_words         :: int
                % If non-zero, the fixed number of words allocated at this
                % allocation site.  If zero, the size is unknown or variable.
            ).

:- type alloc_site_counts
            % alloc_site field must be first for secondary sort order.
    --->    alloc_site_counts(
                asc_alloc_site  :: alloc_site,
                asc_num_cells   :: int,
                asc_num_words   :: int
                % The number of words allocated during the profiling run.
                % If possible, we do NOT use this number as is inflated by
                % the extra attribution word plus roundup.
            ).

:- type group
    --->    group(
                g_total_cells   :: int,
                g_total_words   :: int,
                g_representative:: alloc_site,
                g_details       :: list(alloc_site_counts)
            ).

    % size_map[ReqWords] = ActualWords.
    % ReqWords is the number of words that was requested to be allocated.
    % ActualWords is the number of words that Boehm GC would have rounded the
    % request up to.
    %
:- type size_map == list(int).

:- type snapshot_options
    --->    snapshot_options(
                major_axis      :: major_axis,
                brief           :: bool,
                recalc_words    :: bool,
                include_runtime :: bool
            ).

:- type major_axis
    --->    major_axis_proc
    ;       major_axis_type.

%---------------------------------------------------------------------------%

show_snapshots(OutputStream, !IO) :-
    globals.io_lookup_string_option(snapshots_file, SnapshotsFile, !IO),
    globals.io_lookup_bool_option(snapshots_by_type, ByType, !IO),
    globals.io_lookup_bool_option(snapshots_brief, Brief, !IO),
    globals.io_lookup_bool_option(snapshots_recalc_size, RecalcSize, !IO),
    globals.io_lookup_bool_option(snapshots_include_runtime, InclRuntime, !IO),
    (
        ByType = yes,
        MajorAxis = major_axis_type
    ;
        ByType = no,
        MajorAxis = major_axis_proc
    ),
    Options = snapshot_options(MajorAxis, Brief, RecalcSize, InclRuntime),
    io.open_input(SnapshotsFile, OpenDeclRes, !IO),
    (
        OpenDeclRes = ok(DeclStream),
        read_and_parse_alloc_site_decls(DeclStream, AllocSiteMap, SizeMap,
            !IO),
        io.close_input(DeclStream, !IO)
    ;
        OpenDeclRes = error(DeclError),
        DeclErrorStr = "error opening file `" ++ SnapshotsFile ++
            "': " ++ io.error_message(DeclError) ++ "\n",
        error(DeclErrorStr)
    ),
    io.open_input(SnapshotsFile, OpenRes, !IO),
    (
        OpenRes = ok(SnapshotStream),
        show_all_snapshots(SnapshotStream, OutputStream, Options,
            AllocSiteMap, SizeMap, !IO),
        io.close_input(SnapshotStream, !IO)
    ;
        OpenRes = error(Error),
        ErrorStr = "error opening file `" ++ SnapshotsFile ++
            "': " ++ io.error_message(Error) ++ "\n",
        error(ErrorStr)
    ).

%---------------------------------------------------------------------------%

:- pred read_and_parse_alloc_site_decls(io.text_input_stream::in,
    alloc_site_map::out, size_map::out, io::di, io::uo) is det.

read_and_parse_alloc_site_decls(InputStream, AllocSiteMap, SizeMap, !IO) :-
    io.read_line_as_string(InputStream, LineRes, !IO),
    (
        LineRes = ok(Line),
        % Search for the size_map, which indicates the start of allocation site
        % declarations.
        ( if string.prefix(Line, "size_map ") then
            parse_size_map(Line, SizeMap),
            read_and_parse_alloc_site_lines(InputStream,
                map.init, AllocSiteMap, !IO)
        else
            read_and_parse_alloc_site_decls(InputStream, AllocSiteMap, SizeMap,
                !IO)
        )
    ;
        LineRes = eof,
        unexpected($pred, "format error: cannot find declarations")
    ;
        LineRes = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

:- pred parse_size_map(string::in, list(int)::out) is det.

parse_size_map(Line, SizeMap) :-
    ( if
        string.words(Line) = ["size_map" | Words],
        list.map(string.to_int, Words, Ints)
    then
        SizeMap = Ints
    else
        unexpected($pred, "format error: bad size_map line")
    ).

:- pred read_and_parse_alloc_site_lines(io.input_stream::in,
    alloc_site_map::in, alloc_site_map::out, io::di, io::uo) is det.

read_and_parse_alloc_site_lines(InputStream, !AllocSiteMap, !IO) :-
    io.read_line_as_string(InputStream, LineRes, !IO),
    (
        LineRes = ok(Line),
        parse_alloc_site_line(Line, !AllocSiteMap),
        read_and_parse_alloc_site_lines(InputStream, !AllocSiteMap, !IO)
    ;
        LineRes = eof
    ;
        LineRes = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

:- pred parse_alloc_site_line(string::in,
    alloc_site_map::in, alloc_site_map::out) is det.

parse_alloc_site_line(Line0, !AllocSiteMap) :-
    Line = string.chomp(Line0),
    Words = string.split_at_char('\t', Line),
    ( if
        Words = [IdStr, MangledProcName, FileName, LineNumStr, Type,
            NumWordsStr],
        string.to_int(IdStr, Id),
        string.to_int(LineNumStr, LineNum),
        string.to_int(NumWordsStr, NumWords)
    then
        demangle(MangledProcName, ProcName),
        AllocSite = alloc_site(ProcName, Type, FileName, LineNum, NumWords),
        map.det_insert(alloc_id(Id), AllocSite, !AllocSiteMap)
    else
        unexpected($pred, "format error: bad alloc site declaration")
    ).

%---------------------------------------------------------------------------%

:- pred show_all_snapshots(io.text_input_stream::in, io.text_output_stream::in,
    snapshot_options::in, alloc_site_map::in, size_map::in,
    io::di, io::uo) is det.

show_all_snapshots(InputStream, OutputStream, Options, AllocSiteMap, SizeMap,
        !IO) :-
    io.read_line_as_string(InputStream, LineRes, !IO),
    (
        LineRes = ok(Line),
        ( if string.remove_prefix("start ", Line, SnapshotName0) then
            SnapshotName = string.chomp(SnapshotName0),
            output_snapshot_title(OutputStream, SnapshotName, !IO),
            show_single_snapshot(InputStream, OutputStream, Options,
                AllocSiteMap, SizeMap, !IO),
            show_all_snapshots(InputStream, OutputStream, Options,
                AllocSiteMap, SizeMap, !IO)
        else
            true
        )
    ;
        LineRes = eof
    ;
        LineRes = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

:- pred show_single_snapshot(
    io.text_input_stream::in, io.text_output_stream::in, snapshot_options::in,
    alloc_site_map::in, size_map::in, io::di, io::uo) is det.

show_single_snapshot(InputStream, OutputStream, Options,
        AllocSiteMap, SizeMap, !IO) :-
    read_and_parse_snapshot(InputStream, Options, AllocSiteMap, SizeMap,
        AllocCounts, !IO),
    MajorAxis = Options ^ major_axis,
    make_sorted_groups(MajorAxis, AllocCounts, Groups),
    output_snapshot(OutputStream, Options, Groups, !IO).

:- pred read_and_parse_snapshot(io.input_stream::in, snapshot_options::in,
    alloc_site_map::in, size_map::in, list(alloc_site_counts)::out,
    io::di, io::uo) is det.

read_and_parse_snapshot(InputStream, Options, AllocSiteMap, SizeMap,
        AllocCounts, !IO) :-
    io.read_line_as_string(InputStream, LineRes, !IO),
    (
        LineRes = ok(Line),
        ( if
            string.prefix(Line, "end ")
        then
            AllocCounts = []
        else if
            parse_alloc_site(Options, AllocSiteMap, SizeMap, Line, Counts)
        then
            read_and_parse_snapshot(InputStream, Options,
                AllocSiteMap, SizeMap, RestCounts, !IO),
            AllocCounts = [Counts | RestCounts]
        else
            read_and_parse_snapshot(InputStream, Options,
                AllocSiteMap, SizeMap, AllocCounts, !IO)
        )
    ;
        LineRes = eof,
        unexpected($pred, "format error")
    ;
        LineRes = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

:- pred parse_alloc_site(snapshot_options::in, alloc_site_map::in,
    size_map::in, string::in, alloc_site_counts::out) is semidet.

parse_alloc_site(Options, AllocSiteMap, SizeMap, Line, Counts) :-
    string.words(Line) = [IdStr, NumCellsStr, NumWordsStr0],
    string.to_int(NumCellsStr, NumCells),
    string.to_int(NumWordsStr0, NumWords0),
    ( if string.to_int(IdStr, Id) then
        get_alloc_site(AllocSiteMap, alloc_id(Id), AllocSite),
        RecalcSize = Options ^ recalc_words
    else
        (
            IdStr = "runtime",
            Options ^ include_runtime = yes,
            string.format("runtime struct (%d words)", [i(NumWords0)], Type),
            AllocSite = alloc_site("unknown", Type, "unknown", 0, NumWords0),
            RecalcSize = no
        ;
            IdStr = "unknown",
            string.format("unknown (%d words)", [i(NumWords0)], Type),
            AllocSite = alloc_site("unknown", Type, "unknown", 0, NumWords0),
            RecalcSize = no
        )
    ),
    ( if
        RecalcSize = yes,
        WordsPerCell = AllocSite ^ alloc_words,
        WordsPerCell > 0,
        list.index1(SizeMap, WordsPerCell, SizeMapWords)
    then
        NumWords = NumCells * SizeMapWords
    else
        NumWords = NumWords0
    ),
    Counts = alloc_site_counts(AllocSite, NumCells, NumWords).

:- pred get_alloc_site(alloc_site_map::in, alloc_id::in, alloc_site::out)
    is det.

get_alloc_site(AllocSiteMap, AllocId, AllocSite) :-
    ( if map.search(AllocSiteMap, AllocId, AllocSite0) then
        AllocSite = AllocSite0
    else
        AllocSite = alloc_site("unknown", "unknown", "(unknown)", 0, 0)
    ).

%---------------------------------------------------------------------------%

:- pred make_sorted_groups(major_axis::in, list(alloc_site_counts)::in,
    list(group)::out) is det.

make_sorted_groups(MajorAxis, Counts, SortedGroups) :-
    (
        MajorAxis = major_axis_proc,
        Compare = counts_by_proc
    ;
        MajorAxis = major_axis_type,
        Compare = counts_by_type
    ),
    sort(Compare, Counts, SortedCounts),
    make_groups(Compare, SortedCounts, Groups),
    sort(group_by_words, Groups, SortedGroups).

:- pred make_groups(comparison_pred(alloc_site_counts)::in(comparison_pred),
    list(alloc_site_counts)::in, list(group)::out) is det.

make_groups(Compare, Counts, Groups) :-
    (
        Counts = [],
        Groups = []
    ;
        Counts = [_ | _],
        takewhile(Compare, Counts, First, Rest),
        make_group(First, FirstGroup),
        make_groups(Compare, Rest, RestGroups),
        Groups = [FirstGroup | RestGroups]
    ).

:- pred make_group(list(alloc_site_counts)::in, group::out) is det.

make_group(Counts, Group) :-
    % This relies on the alloc_site being the first field of alloc_site_counts,
    % and on the default ordering of alloc_site.
    sort(Counts, SortedCounts0),
    sort(counts_by_words, SortedCounts0, SortedCounts),
    list.foldl2(sum_counts, SortedCounts, 0, TotalCells, 0, TotalWords),
    FirstSite = list.det_head(SortedCounts) ^ asc_alloc_site,
    Group = group(TotalCells, TotalWords, FirstSite, SortedCounts).

%---------------------------------------------------------------------------%

:- pred output_snapshot_title(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

output_snapshot_title(OutputStream, SnapshotName, !IO) :-
    io.write_string(OutputStream, "------ ", !IO),
    io.write_string(OutputStream, SnapshotName, !IO),
    io.write_string(OutputStream, " ------\n", !IO).

:- pred output_snapshot(io.text_output_stream::in, snapshot_options::in,
    list(group)::in, io::di, io::uo) is det.

output_snapshot(OutputStream, Options, Grouped, !IO) :-
    output_column_names(OutputStream, Options, !IO),
    list.foldl2(sum_groups, Grouped, 0, TotalCells, 0, TotalWords),
    io.format(OutputStream, " %7d%17d%14s  %s\n",
        [i(TotalCells), i(TotalWords), s(""), s("total")], !IO),
    Brief = Options ^ brief,
    (
        Brief = yes
    ;
        Brief = no,
        io.nl(OutputStream, !IO)
    ),
    list.foldl2(output_group(OutputStream, Options, TotalCells, TotalWords),
        Grouped, 0, _CumulWords, !IO).

:- pred output_column_names(io.text_output_stream::in, snapshot_options::in,
    io::di, io::uo) is det.

output_column_names(OutputStream, Options, !IO) :-
    MajorAxis = Options ^ major_axis,
    (
        MajorAxis = major_axis_proc,
        RightLabel = "procedure / type (location)"
    ;
        MajorAxis = major_axis_type,
        RightLabel = "type / procedure (location)"
    ),
    io.format(OutputStream, " %7s%17s%14s  %s\n",
        [s("cells"), s("words"), s("cumul"), s(RightLabel)], !IO).

:- pred output_group(io.text_output_stream::in, snapshot_options::in,
    int::in, int::in, group::in, int::in, int::out, io::di, io::uo) is det.

output_group(OutputStream, Options, TotalCells, TotalWords, Group,
        !CumulWords, !IO) :-
    Group = group(NumCells, NumWords, AllocSite, Counts),
    !:CumulWords = !.CumulWords + NumWords,
    CellsPercent = percentage(NumCells, TotalCells),
    WordsPercent = percentage(NumWords, TotalWords),
    CumulPercent = percentage(!.CumulWords, TotalWords),
    ( if
        CellsPercent =< min_percentage_major,
        WordsPercent =< min_percentage_major
    then
        true
    else
        MajorAxis = Options ^ major_axis,
        Brief = Options ^ brief,
        (
            MajorAxis = major_axis_proc,
            RightLabel = AllocSite ^ alloc_proc
        ;
            MajorAxis = major_axis_type,
            RightLabel = AllocSite ^ alloc_type
        ),
        (
            Brief = yes,
            Star = ' '
        ;
            Brief = no,
            Star = ('*')
        ),
        io.format(OutputStream, "%c%7d/%5.1f%% %9d/%5.1f%% %5.1f%%  %s\n",
            [c(Star),
            i(NumCells), f(CellsPercent),
            i(NumWords), f(WordsPercent), f(CumulPercent),
            s(RightLabel)], !IO),
        (
            Brief = yes
        ;
            Brief = no,
            Single = ( if Counts = [_] then yes else no ),
            list.foldl(
                output_site(OutputStream, MajorAxis, TotalCells, TotalWords,
                    Single),
                Counts, !IO),
            io.nl(OutputStream, !IO)
        )
    ).

:- pred output_site(io.text_output_stream::in, major_axis::in,
    int::in, int::in, bool::in, alloc_site_counts::in, io::di, io::uo) is det.

output_site(OutputStream, MajorAxis, TotalCells, TotalWords, Single,
        AllocCounts, !IO) :-
    AllocCounts = alloc_site_counts(AllocSite, NumCells, NumWords),
    AllocSite = alloc_site(Proc, Type, File, LineNum, _),
    CellsPercent = percentage(NumCells, TotalCells),
    WordsPercent = percentage(NumWords, TotalWords),
    (
        MajorAxis = major_axis_proc,
        RightLabel = Type
    ;
        MajorAxis = major_axis_type,
        RightLabel = Proc
    ),
    (
        Single = yes,
        io.format(OutputStream,
            " %38s  %s (%s:%d)\n",
            [s(""), s(RightLabel), s(File), i(LineNum)], !IO)
    ;
        Single = no,
        ( if
            CellsPercent =< min_percentage_major,
            WordsPercent =< min_percentage_major
        then
            true
        else
            io.format(OutputStream,
                " %7d/%5.1f%% %9d/%5.1f%%  %5s  %s (%s:%d)\n",
                [i(NumCells), f(CellsPercent),
                i(NumWords), f(WordsPercent), s(""),
                s(RightLabel), s(File), i(LineNum)], !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred group_by_words(group::in, group::in, comparison_result::out) is det.

group_by_words(GroupA, GroupB, Result) :-
    A = GroupA ^ g_total_words,
    B = GroupB ^ g_total_words,
    compare(Result, B, A).

:- pred counts_by_proc(alloc_site_counts::in, alloc_site_counts::in,
    comparison_result::out) is det.

counts_by_proc(CountsA, CountsB, Result) :-
    A = CountsA ^ asc_alloc_site ^ alloc_proc,
    B = CountsB ^ asc_alloc_site ^ alloc_proc,
    compare(Result, B, A).

:- pred counts_by_type(alloc_site_counts::in, alloc_site_counts::in,
    comparison_result::out) is det.

counts_by_type(CountsA, CountsB, Result) :-
    A = CountsA ^ asc_alloc_site ^ alloc_type,
    B = CountsB ^ asc_alloc_site ^ alloc_type,
    compare(Result, B, A).

:- pred counts_by_words(alloc_site_counts::in, alloc_site_counts::in,
    comparison_result::out) is det.

counts_by_words(CountsA, CountsB, Result) :-
    A = CountsA ^ asc_num_words,
    B = CountsB ^ asc_num_words,
    compare(Result, B, A).

:- pred sum_groups(group::in, int::in, int::out, int::in, int::out) is det.

sum_groups(Group, !TotalCells, !TotalWords) :-
    !:TotalCells = !.TotalCells + Group ^ g_total_cells,
    !:TotalWords = !.TotalWords + Group ^ g_total_words.

:- pred sum_counts(alloc_site_counts::in, int::in, int::out,
    int::in, int::out) is det.

sum_counts(Site, !TotalCells, !TotalWords) :-
    !:TotalCells = !.TotalCells + (Site ^ asc_num_cells),
    !:TotalWords = !.TotalWords + (Site ^ asc_num_words).

%---------------------------------------------------------------------------%

:- func percentage(int, int) = float.

percentage(N, Total) = 100.0 * float(N) / float(Total).

:- func min_percentage_major = float.

min_percentage_major = 0.1.

:- func min_percentage_minor = float.

min_percentage_minor = 0.05.

%---------------------------------------------------------------------------%

:- pred takewhile(comparison_pred(T)::in(comparison_pred),
    list(T)::in, list(T)::out, list(T)::out) is det.

takewhile(Compare, List, Upto, After) :-
    (
        List = [],
        Upto = [],
        After = []
    ;
        List = [_],
        Upto = List,
        After = []
    ;
        List = [A, B | Cs],
        Compare(A, B, Cmp),
        (
            Cmp = (=),
            takewhile(Compare, [B | Cs], Upto0, After),
            Upto = [A | Upto0]
        ;
            ( Cmp = (<)
            ; Cmp = (>)
            ),
            Upto = [A],
            After = [B | Cs]
        )
    ).

%---------------------------------------------------------------------------%
