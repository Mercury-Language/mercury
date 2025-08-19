%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2017, 2019-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: opt_deps_spec.m.
%
%---------------------------------------------------------------------------%

:- module parse_tree.opt_deps_spec.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.

:- import_module digraph.
:- import_module io.
:- import_module list.

:- pred compute_opt_trans_opt_deps_graph(io.text_output_stream::in,
    globals::in, module_name::in, digraph(module_name)::in,
    digraph(module_name)::out, digraph(module_name)::out,
    list(module_name)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.file_util.
:- import_module libs.options.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module mercury_term_parser.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module string.builder.
:- import_module term.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

compute_opt_trans_opt_deps_graph(ProgressStream, Globals, ModuleName,
        ImpDepsGraph, IndirectOptDepsGraph,
        TransOptDepsGraph, TransOptDepsOrdering, !Specs, !IO) :-
    globals.lookup_bool_option(Globals, generate_module_order, OutputOrder),
    (
        OutputOrder = yes,
        % Note that we output the contents of ImpDepsGraph here,
        % but we also output the contents of TransOptDepsGraph below,
        % once we have computed it.
        ImpDepsOrdering = digraph.return_sccs_in_from_to_order(ImpDepsGraph),
        output_module_order(ProgressStream, Globals, ModuleName,
            ext_cur_user_order, ImpDepsOrdering, !IO)
    ;
        OutputOrder = no
    ),

    % Compute the indirect optimization dependencies: indirect
    % dependencies including those via `.opt' or `.trans_opt' files.
    % Actually we cannot compute that, since we don't know
    % which modules the `.opt' files will import!
    % Instead, we need to make a conservative (over-)approximation,
    % and assume that the each module's `.opt' file might import any
    % of that module's implementation dependencies; in actual fact,
    % it will be some subset of that.
    %
    % Note that IndirectOptDepsGraph is needed on only two of the three
    % paths through the nested if-then-else below. This is why we compile
    % this module with --unneeded-code.
    IndirectOptDepsGraph = digraph.transitive_closure(ImpDepsGraph),

    % Compute the trans-opt deps for the purpose of making `.trans_opt'
    % files. This is normally equal to the transitive closure of the
    % indirect dependencies (i.e. IndirectOptDepsGraph) since a module
    % may read the `.trans_opt' file of any directly or indirectly
    % imported module.
    %
    % To deal with cycles in the graph, by default, we impose an arbitrary
    % order on modules so that when making the .trans_opt file for a module
    % "earlier" in the cycle, the compiler may read the .trans_opt files
    % of modules "later" in the cycle, but not vice versa.
    %
    % This has two problems.
    %
    % - Lack of parallelism. The .trans_opt files for modules within a
    %   single SCC have to be made one after another.
    %
    % - The arbitrary ordering is likely to produce sub-optimal
    %   information transfer between .trans_opt files.
    %
    % To help the user fix both problems at least partially,
    % we allow them to specify a list of edges (in a file read in by
    % read_trans_opt_deps_spec) that the code of apply_trans_opt_deps_spec
    % will then remove from the dependency graph. The intention is that
    % this should allow users to break up SCCs in a manner of their
    % choosing.
    %
    % Note that if the removal of the edges specified by the user
    % does not convert the graph into a dag (directed acyclic graph),
    % the compiler will use the default algorithm described above
    % to finish the job.
    globals.lookup_maybe_string_option(Globals, trans_opt_deps_spec,
        MaybeSpecFileName),
    (
        MaybeSpecFileName = yes(SpecFileName),
        read_trans_opt_deps_spec_file(SpecFileName, MaybeEdgesToRemove, !IO),
        (
            MaybeEdgesToRemove = ok1(EdgesToRemove),
            report_unknown_module_names_in_deps_spec(ImpDepsGraph,
                EdgesToRemove, UnknownModuleSpecs),
            !:Specs = UnknownModuleSpecs ++ !.Specs,
            apply_trans_opt_deps_spec(EdgesToRemove,
                ImpDepsGraph, TrimmedImpDepsGraph),
            TransOptDepsGraph = digraph.transitive_closure(TrimmedImpDepsGraph)
        ;
            MaybeEdgesToRemove = error1(EdgeSpecs),
            !:Specs = EdgeSpecs ++ !.Specs,
            TransOptDepsGraph = IndirectOptDepsGraph
        )
    ;
        MaybeSpecFileName = no,
        TransOptDepsGraph = IndirectOptDepsGraph
    ),
    TransOptDepsOrdering0 =
        digraph.return_sccs_in_from_to_order(TransOptDepsGraph),
    (
        OutputOrder = yes,
        output_module_order(ProgressStream, Globals, ModuleName,
            ext_cur_user_order_to, TransOptDepsOrdering0, !IO)
    ;
        OutputOrder = no
    ),
    list.map(set.to_sorted_list, TransOptDepsOrdering0,
        TransOptDepsOrdering1),
    list.condense(TransOptDepsOrdering1, TransOptDepsOrdering).

%---------------------------------------------------------------------------%

:- type trans_opt_deps_spec
    ==  map(module_name, allow_or_disallow_trans_opt_deps).

    % The contexts, and the order of the module names in the second arguments
    % of the module_{allow,disallow}_deps, are needed only for generating
    % meaningful error messages.
:- type allow_or_disallow_trans_opt_deps
    --->    module_allow_deps(
                % The context of the first argument.
                term_context,
                % The module names listed in the second argument, and their
                % contexts.
                assoc_list(term_context, module_name)
            )
    ;       module_disallow_deps(
                % The context of the first argument.
                term_context,
                % The module names listed in the second argument, and their
                % contexts.
                assoc_list(term_context, module_name)
            ).

    % The --trans-opt-deps-spec file shall contain a series of terms
    % of either form:
    %
    %   module_allow_deps(M, [ALLOW]).
    %   module_disallow_deps(M, [DISALLOW]).
    %
    % where M is a Mercury module name,
    % and ALLOW and DISALLOW are comma-separated lists of module names.
    %
    % To make the file less verbose, `builtin' and `private_builtin' are
    % implicitly included in an ALLOW list unless M is itself `builtin'
    % or `private_builtin'.
    %
    % It is an error to provide both a module_allow_deps term and a
    % module_disallow_deps term for the same module M.
    %
    % A module_allow_deps term with a first argument M specifies that
    % in the process of making M.trans_opt, the compiler may read
    % T.trans_opt only if T is in the ALLOW list.
    %
    % A module_disallow_deps term with a first argument M specifies that
    % in the process of making M.trans_opt, the compiler may NOT read
    % T.trans_opt if T is in the DISALLOW list.
    %
:- pred read_trans_opt_deps_spec_file(string::in,
    maybe1(trans_opt_deps_spec)::out, io::di, io::uo) is det.

read_trans_opt_deps_spec_file(FileName, Result, !IO) :-
    io.read_named_file_as_string(FileName, ReadResult, !IO),
    (
        ReadResult = ok(Contents),
        string.length(Contents, ContentsLen),
        StartPos = init_posn,
        parse_trans_opt_deps_spec_file(FileName, Contents, ContentsLen,
            StartPos, _EndPos, map.init, EdgesToRemove, [], FileSpecs0),
        (
            FileSpecs0 = [],
            Result = ok1(EdgesToRemove)
        ;
            FileSpecs0 = [_ | _],
            list.foldl(accumulate_contexts, FileSpecs0,
                set.init, FileSpecContextsSet),
            set.to_sorted_list(FileSpecContextsSet, FileSpecContexts),
            list.reverse(FileSpecContexts, RevFileSpecContexts),
            (
                RevFileSpecContexts = [],
                % Every error_spec parse_trans_opt_deps_spec_file constructs
                % should have a context.
                unexpected($pred, "RevFileSpecContexts = []")
            ;
                RevFileSpecContexts = [LastContext | _]
            ),
            IgnorePieces = [invis_order_default_end(0, ""),
                words("Ignoring"), quote(FileName),
                words("due to the presence of errors."), nl],
            IgnoreSpec = spec($pred, severity_error, phase_read_files,
                LastContext, IgnorePieces),
            FileSpecs = [IgnoreSpec | FileSpecs0],
            Result = error1(FileSpecs)
        )
    ;
        ReadResult = error(Error),
        Pieces = [words("Error: cannot open"), quote(FileName), suffix(":"),
            words(io.error_message(Error)), suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_read_files, Pieces),
        Result = error1([Spec])
    ).

:- pred parse_trans_opt_deps_spec_file(string::in, string::in, int::in,
    posn::in, posn::out, trans_opt_deps_spec::in, trans_opt_deps_spec::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_trans_opt_deps_spec_file(FileName, Contents, ContentsLen,
        !Pos, !EdgesToRemove, !Specs) :-
    read_term_from_substring(FileName, Contents, ContentsLen, !Pos, ReadTerm),
    (
        ReadTerm = eof
    ;
        ReadTerm = error(Error, LineNum),
        Pieces = [words("Read error:"), words(Error), suffix("."), nl],
        Context = context(FileName, LineNum),
        Spec = spec($pred, severity_error, phase_read_files, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ;
        ReadTerm = term(VarSet, Term),
        parse_trans_opt_deps_spec_term(VarSet, Term, !EdgesToRemove, !Specs),
        parse_trans_opt_deps_spec_file(FileName, Contents, ContentsLen,
            !Pos, !EdgesToRemove, !Specs)
    ).

:- pred parse_trans_opt_deps_spec_term(varset::in, term::in,
    trans_opt_deps_spec::in, trans_opt_deps_spec::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_trans_opt_deps_spec_term(VarSet, Term, !EdgesToRemove, !Specs) :-
    ( if
        Term = functor(atom(AtomName), [LeftTerm, RightTerm], _Context),
        (
            AtomName = "module_allow_deps"
        ;
            AtomName = "module_disallow_deps"
        ),
        try_parse_sym_name(LeftTerm, SourceName)
    then
        parse_trans_opt_deps_spec_module_list(VarSet, RightTerm,
            cord.init, TargetCord0, [], EntrySpecs),
        (
            EntrySpecs = [],
            TargetList0 = cord.list(TargetCord0),
            LeftTermContext = get_term_context(LeftTerm),
            (
                AtomName = "module_allow_deps",
                ( if
                    SourceName \= unqualified("builtin"),
                    SourceName \= unqualified("private_builtin")
                then
                    TargetList = [
                        dummy_context - unqualified("builtin"),
                        dummy_context - unqualified("private_builtin") |
                        TargetList0
                    ]
                else
                    TargetList = TargetList0
                ),
                AllowOrDisallow = module_allow_deps(LeftTermContext,
                    TargetList)
            ;
                AtomName = "module_disallow_deps",
                AllowOrDisallow = module_disallow_deps(LeftTermContext,
                    TargetList0)
            ),
            map.search_insert(SourceName, AllowOrDisallow,
                MaybeOldAllowOrDisallow, !EdgesToRemove),
            (
                MaybeOldAllowOrDisallow = no
            ;
                MaybeOldAllowOrDisallow = yes(OldAllowOrDisallow),
                ( OldAllowOrDisallow = module_allow_deps(OldContext, _)
                ; OldAllowOrDisallow = module_disallow_deps(OldContext, _)
                ),
                Pieces1 = [words("Error: duplicate entry for source module"),
                    qual_sym_name(SourceName), suffix("."), nl],
                Pieces2 = [words("The original entry is here."), nl],
                Msg1 = msg(LeftTermContext, Pieces1),
                Msg2 = msg(OldContext, Pieces2),
                Spec = error_spec($pred, severity_error, phase_read_files,
                    [Msg1, Msg2]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            EntrySpecs = [_ | _],
            !:Specs = EntrySpecs ++ !.Specs
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected either"),
            nl_indent_delta(1),
            quote("module_allow_deps(module_name, module_name_list)"),
            nl_indent_delta(-1),
            words("or"),
            nl_indent_delta(1),
            quote("module_disallow_deps(module_name, module_name_list)"),
            suffix(","), nl_indent_delta(-1),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_read_files,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred parse_trans_opt_deps_spec_module_list(varset::in, term::in,
    cord(pair(term_context, module_name))::in,
    cord(pair(term_context, module_name))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_trans_opt_deps_spec_module_list(VarSet, Term, !ModuleNameCord, !Specs) :-
    ( if list_term_to_term_list(Term, TermList) then
        parse_trans_opt_deps_spec_module_names(VarSet, TermList,
            !ModuleNameCord, !Specs)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a list, got"),
            quote(TermStr), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_read_files,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred parse_trans_opt_deps_spec_module_names(varset::in, list(term)::in,
    cord(pair(term_context, module_name))::in,
    cord(pair(term_context, module_name))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_trans_opt_deps_spec_module_names(_VarSet, [], !ModuleNameCord, !Specs).
parse_trans_opt_deps_spec_module_names(VarSet, [Term | Terms],
        !ModuleNameCord, !Specs) :-
    ( if try_parse_sym_name(Term, ModuleName) then
        cord.snoc(get_term_context(Term) - ModuleName, !ModuleNameCord)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a module name, got"),
            quote(TermStr), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_read_files,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    parse_trans_opt_deps_spec_module_names(VarSet, Terms,
        !ModuleNameCord, !Specs).

%---------------------------------------------------------------------------%

:- pred report_unknown_module_names_in_deps_spec(digraph(module_name)::in,
    trans_opt_deps_spec::in, list(error_spec)::out) is det.

report_unknown_module_names_in_deps_spec(Graph, DepsSpec, Specs) :-
    digraph.vertices(Graph, KnownModules),
    map.foldl(report_unknown_module_names_in_allow_disallow(KnownModules),
        DepsSpec, [], Specs).

:- pred report_unknown_module_names_in_allow_disallow(set(module_name)::in,
    module_name::in, allow_or_disallow_trans_opt_deps::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unknown_module_names_in_allow_disallow(KnownModules,
        Module, AllowOrDisallow, !Specs) :-
    (
        AllowOrDisallow = module_allow_deps(Context, TargetModules),
        AoD = "allowed"
    ;
        AllowOrDisallow = module_disallow_deps(Context, TargetModules),
        AoD = "disallowed"
    ),
    ( if set.contains(KnownModules, Module) then
        true
    else
        Pieces = [words("Warning: the module name"), qual_sym_name(Module),
            words("does not occur in the dependency graph."), nl],
        Severity = severity_warning(warn_trans_opt_deps_spec),
        Spec = spec($pred, Severity, phase_read_files, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    report_unknown_module_names_in_module_names(KnownModules, AoD, 1,
        TargetModules, map.init, !Specs).

:- pred report_unknown_module_names_in_module_names(set(module_name)::in,
    string::in, int::in, assoc_list(term_context, module_name)::in,
    map(module_name, int)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unknown_module_names_in_module_names(_, _, _, [], _OrdMap, !Specs).
report_unknown_module_names_in_module_names(KnownModules, AoD, N,
        [Context - Module | ContextModules], !.OrdMap, !Specs) :-
    ( if set.contains(KnownModules, Module) then
        map.search_insert(Module, N, MaybeOldN, !OrdMap),
        (
            MaybeOldN = no
        ;
            MaybeOldN = yes(OldN),
            Pieces = [words("Warning: the"), nth_fixed(N), words(AoD),
                words("module name"), qual_sym_name(Module),
                words("is the same as the"), nth_fixed(OldN), words(AoD),
                words("module name."), nl],
            Severity = severity_warning(warn_trans_opt_deps_spec),
            Spec = spec($pred, Severity, phase_read_files, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    else
        Pieces = [words("Warning: the"), nth_fixed(N), words(AoD),
            words("module name"), qual_sym_name(Module),
            words("does not occur in the dependency graph."), nl],
        Severity = severity_warning(warn_trans_opt_deps_spec),
        Spec = spec($pred, Severity, phase_read_files, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    report_unknown_module_names_in_module_names(KnownModules,
        AoD, N + 1, ContextModules, !.OrdMap, !Specs).

%---------------------------------------------------------------------------%

:- pred apply_trans_opt_deps_spec(trans_opt_deps_spec::in,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_trans_opt_deps_spec(EdgesToRemove, !Graph) :-
    SCCs = set.to_sorted_list(digraph.cliques(!.Graph)),
    list.foldl2(apply_trans_opt_deps_spec_in_scc, SCCs,
        EdgesToRemove, _EdgesToRemove, !Graph).

:- pred apply_trans_opt_deps_spec_in_scc(set(digraph_key(module_name))::in,
    trans_opt_deps_spec::in, trans_opt_deps_spec::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_trans_opt_deps_spec_in_scc(SCC, !EdgesToRemove, !Graph) :-
    set.foldl2(apply_trans_opt_deps_spec_for_module, SCC,
        !EdgesToRemove, !Graph).

:- pred apply_trans_opt_deps_spec_for_module(digraph_key(module_name)::in,
    trans_opt_deps_spec::in, trans_opt_deps_spec::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_trans_opt_deps_spec_for_module(SourceKey,
        EdgesToRemove, EdgesToRemove, !Graph) :-
    % XXX This predicate does not update EdgesToRemove, Should it do so?
    digraph.lookup_vertex(!.Graph, SourceKey, SourceName),
    ( if map.search(EdgesToRemove, SourceName, AllowOrDisallow) then
        digraph.lookup_from(!.Graph, SourceKey, TargetSet),
        (
            AllowOrDisallow = module_allow_deps(_Context, AllowList),
            assoc_list.values(AllowList, AllowModuleList),
            set.list_to_set(AllowModuleList, AllowSet),
            set.foldl(apply_module_allow_deps(AllowSet, SourceKey),
                TargetSet, !Graph)
        ;
            AllowOrDisallow = module_disallow_deps(_Context, DisallowList),
            assoc_list.values(DisallowList, DisallowModuleList),
            set.list_to_set(DisallowModuleList, DisallowSet),
            set.foldl(apply_module_disallow_deps(DisallowSet, SourceKey),
                TargetSet, !Graph)
        )
    else
        true
    ).

:- pred apply_module_allow_deps(set(module_name)::in,
    digraph_key(module_name)::in, digraph_key(module_name)::in,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_module_allow_deps(AllowSet, SourceKey, TargetKey, !Graph) :-
    digraph.lookup_vertex(!.Graph, TargetKey, TargetName),
    ( if set.contains(AllowSet, TargetName) then
        true
    else
        digraph.delete_edge(SourceKey, TargetKey, !Graph)
    ).

:- pred apply_module_disallow_deps(set(module_name)::in,
    digraph_key(module_name)::in, digraph_key(module_name)::in,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_module_disallow_deps(DisallowSet, SourceKey, TargetKey, !Graph) :-
    digraph.lookup_vertex(!.Graph, TargetKey, TargetName),
    ( if set.contains(DisallowSet, TargetName) then
        digraph.delete_edge(SourceKey, TargetKey, !Graph)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred output_module_order(io.text_output_stream::in, globals::in,
    module_name::in, ext_cur::in, list(set(module_name))::in,
    io::di, io::uo) is det.

output_module_order(ProgressStream, Globals, ModuleName, ExtCur,
        DepsOrdering, !IO) :-
    module_name_to_cur_dir_file_name(ExtCur, ModuleName, OrdFileName),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    string.format("%% Creating module order file `%s'...",
        [s(OrdFileName)], CreatingMsg),
    maybe_write_string(ProgressStream, Verbose, CreatingMsg, !IO),
    io.open_output(OrdFileName, OrdResult, !IO),
    (
        OrdResult = ok(OrdStream),
        State0 = string.builder.init,
        add_module_sccs(DepsOrdering, State0, State),
        DepsOrderingStr = string.builder.to_string(State),
        io.write_string(OrdStream, DepsOrderingStr, !IO),
        io.close_output(OrdStream, !IO),
        maybe_write_string(ProgressStream, Verbose, " done.\n", !IO)
    ;
        OrdResult = error(IOError),
        maybe_write_string(ProgressStream, Verbose, " failed.\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        io.error_message(IOError, IOErrorMessage),
        string.format("error opening file `%s' for output: %s",
            [s(OrdFileName), s(IOErrorMessage)], OrdMessage),
        report_error(ProgressStream, OrdMessage, !IO)
    ).

:- pred add_module_sccs(list(set(module_name))::in,
    string.builder.state::di, string.builder.state::uo) is det.

add_module_sccs([], !SB).
add_module_sccs([HeadSCC | TailSCCs], !SB) :-
    add_module_sccs_lag(HeadSCC, TailSCCs, !SB).

:- pred add_module_sccs_lag(set(module_name)::in, list(set(module_name))::in,
    string.builder.state::di, string.builder.state::uo) is det.

add_module_sccs_lag(HeadSCC, TailSCCs, !SB) :-
    add_module_scc(HeadSCC, !SB),
    (
        TailSCCs = []
    ;
        TailSCCs = [HeadTailSCC | TailTailSCCs],
        % Add a blank line between SCCs.
        append_char('\n', !SB),
        add_module_sccs_lag(HeadTailSCC, TailTailSCCs, !SB)
    ).

:- pred add_module_scc(set(module_name)::in,
    string.builder.state::di, string.builder.state::uo) is det.

add_module_scc(SCC0, !SB) :-
    set.to_sorted_list(SCC0, SCC),
    list.foldl(add_module_name, SCC, !SB).

:- pred add_module_name(module_name::in,
    string.builder.state::di, string.builder.state::uo) is det.

add_module_name(ModuleName, !SB) :-
    format_escaped_sym_name(string.builder.handle, ModuleName, !SB),
    append_char('\n', !SB).

%---------------------------------------------------------------------------%
:- end_module parse_tree.opt_deps_spec.
%---------------------------------------------------------------------------%
