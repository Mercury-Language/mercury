%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 1993-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io.m.
% Main authors: fjh, zs.
%
% This module defines predicates for parsing Mercury programs.
%
% In some ways the representation of programs here is considerably
% more complex than is necessary for the compiler.
% The basic reason for this is that it was designed to preserve
% as much information about the source code as possible, so that
% this representation could also be used for other tools such
% as pretty-printers.
% Currently the only information that is lost is that comments and
% whitespace are stripped, any redundant parenthesization
% are lost, distinctions between different spellings of the same
% operator (eg "\+" vs "not") are lost, and DCG clauses get expanded.
% It would be a good idea to preserve all those too (well, maybe not
% the redundant parentheses), but right now it's not worth the effort.
%
% So that means that this phase of compilation is purely parsing.
% No simplifications are done (other than DCG expansion).
% The results of this phase specify
% basically the same information as is contained in the source code,
% but in a parse tree rather than a flat file.
% Simplifications are done only by make_hlds.m, which transforms
% the parse tree which we built here into the HLDS.
%
% Some of this code is a rather bad example of cut-and-paste style reuse.
% It should be cleaned up to eliminate most of the duplication.
% But that task really needs to wait until we implement higher-order
% predicates.  For the moment, just be careful that any changes
% you make are reflected correctly in all similar parts of this file.
%
% Implication and equivalence implemented by squirrel, who would also
% like to get her hands on this file and give it a good clean up and
% put it into good clean "mercury" style!
%
% Wishlist:
%
% 1.  implement importing/exporting operators with a particular fixity
%     eg. :- import_op prefix(+). % only prefix +, not infix
%     (not important, but should be there for reasons of symmetry.)
% 2.  improve the handling of type and inst parameters
% 3.  improve the error reporting (most of the semidet preds should
%     be det and should return a meaningful indication of where an
%     error occurred).

%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io.
:- interface.

:- import_module libs.file_util.
:- import_module libs.timestamp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_util.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

% This module (prog_io) exports the following predicates:

:- type module_error
    --->    no_module_errors        % no errors
    ;       some_module_errors      % some syntax errors
    ;       fatal_module_errors.    % couldn't open the file

:- type maybe_return_timestamp
    --->    do_return_timestamp
    ;       do_not_return_timestamp.

    % read_module(OpenFile, FileName, DefaultModuleName, ReturnTimestamp,
    %   MaybeFileInfo, ActualModuleName, Program, Specs, Error,
    %   MaybeModuleTimestamp, !IO):
    %
    % Reads and parses the file opened by OpenFile using the default module
    % name DefaultModuleName. If ReturnTimestamp is `yes', attempt to return
    % the modification timestamp in MaybeModuleTimestamp. Error is
    % `fatal_module_errors' if the file coudn't be opened, `some_module_errors'
    % if a syntax error was detected, and `no_module_errors' otherwise.
    % MaybeFileInfo is the information about the file (usually the file or
    % directory name) returned by OpenFile. ActualModuleName is the module name
    % specified in the `:- module' declaration, if any, or the
    % DefaultModuleName if there is no `:- module' declaration.
    % Specs is a list of warning/error messages. Program is the parse tree.
    %
:- pred read_module(open_file(FileInfo)::in(open_file),
    module_name::in, maybe_return_timestamp::in, maybe(FileInfo)::out,
    module_name::out, list(item)::out, list(error_spec)::out,
    module_error::out, maybe(io.res(timestamp))::out, io::di, io::uo) is det.

:- pred read_module_if_changed(open_file(FileInfo)::in(open_file),
    module_name::in, timestamp::in, maybe(FileInfo)::out, module_name::out,
    list(item)::out, list(error_spec)::out, module_error::out,
    maybe(io.res(timestamp))::out, io::di, io::uo) is det.

    % Same as read_module, but use intermod_directories instead of
    % search_directories when searching for the file.
    % Also report an error if the actual module name doesn't match
    % the expected module name.
    %
:- pred read_opt_file(file_name::in, module_name::in, list(item)::out,
    list(error_spec)::out, module_error::out, io::di, io::uo) is det.

    % check_module_has_expected_name(FileName, ExpectedName, ActualName):
    %
    % Check that two module names are equal, and report an error if they
    % aren't.
    %
:- pred check_module_has_expected_name(file_name::in, module_name::in,
    module_name::in, io::di, io::uo) is det.

    % search_for_module_source(Dirs, InterfaceDirs, ModuleName,
    %   FoundSourceFileName, !IO):
    %
    % Look for the source for ModuleName in Dirs. This will also search for
    % files matching partially qualified versions of ModuleName, but only if
    % a more qualified `.m' or `.int' file doesn't exist in InterfaceDirs.
    % in InterfaceDirs. For example, module foo.bar.baz can be found in
    % foo.bar.m, bar.baz.m or bar.m.
    %
:- pred search_for_module_source(list(dir_name)::in, list(dir_name)::in,
    module_name::in, maybe_error(file_name)::out, io::di, io::uo) is det.

    % Read the first item from the given file to find the module name.
    %
:- pred find_module_name(file_name::in, maybe(module_name)::out,
    io::di, io::uo) is det.

    % parse_item(ModuleName, VarSet, Term, MaybeItem):
    %
    % Parse Term. If successful, MaybeItem is bound to the parsed item,
    % otherwise it is bound to an appropriate error message. Qualify
    % appropriate parts of the item, with ModuleName as the module name.
    %
:- pred parse_item(module_name::in, varset::in, term::in, maybe1(item)::out)
    is det.

    % parse_decl(ModuleName, VarSet, Term, Result):
    %
    % Parse Term as a declaration. If successful, Result is bound to the
    % parsed item, otherwise it is bound to an appropriate error message.
    % Qualify appropriate parts of the item, with ModuleName as the module
    % name.
    %
:- pred parse_decl(module_name::in, varset::in, term::in, maybe1(item)::out)
    is det.

    % parse_type_defn_head(ModuleName, VarSet, Head, HeadResult):
    %
    % Check the head of a type definition for errors.
    %
:- pred parse_type_defn_head(module_name::in, varset::in, term::in,
    maybe2(sym_name, list(type_param))::out) is det.

    % parse_type_decl_where_part_if_present(TypeSymName, Arity,
    %   IsSolverType, Inst, ModuleName, Term0, Term, Result):
    %
    % Checks if Term0 is a term of the form `<body> where <attributes>'.
    % If so, returns the `<body>' in Term and the parsed `<attributes>'
    % in Result. If not, returns Term = Term0 and Result = no.
    %
:- pred parse_type_decl_where_part_if_present(is_solver_type::in,
    module_name::in, varset::in, term::in, term::out,
    maybe2(maybe(solver_type_details), maybe(unify_compare))::out) is det.

%-----------------------------------------------------------------------------%

% A QualifiedTerm is one of
%   Name(Args)
%   Module.Name(Args)
% (or if Args is empty, one of
%   Name
%   Module.Name)
% where Module is a SymName. For backwards compatibility, we allow `__'
% as an alternative to `.'.

    % Sym_name_and_args takes a term and returns a sym_name that is its
    % top function symbol, and a list of its argument terms. It fails
    % if the input is not valid syntax for a QualifiedTerm.
    %
:- pred sym_name_and_args(term(T)::in, sym_name::out, list(term(T))::out)
    is semidet.

    % parse_qualified_term(Term, _ContainingTerm, VarSet, ContextPieces,
    %   Result):
    %
    % Parse Term into a sym_name that is its top function symbol and a
    % list of its argument terms, and if successful return them in Result.
    % (parse_qualified_term thus does the same job as sym_name_and_args
    % if it succeeds.) However, in case it does not succced,
    % parse_qualified_term also takes as input Varset (from which the variables
    % in Term are taken), the term containing Term, and a format_component
    % list describing the context from which it was called, e.g.
    % "In clause head:". XXX Currently, _ContainingTerm isn't used;
    % maybe it should be deleted.
    %
    % Note: parse_qualified_term is used for places where a symbol is _used_,
    % where no default module name exists for the sym_name. For places
    % where a symbol is _defined_, use parse_implicitly_qualified_term.
    %
    % If you care only about the case where Result = ok2(SymName, Args),
    % use sym_name_and_args.
    %
:- pred parse_qualified_term(term(T)::in, term(T)::in, varset::in,
    list(format_component)::in, maybe_functor(T)::out) is det.

    % parse_implicitly_qualified_term(ModuleName, Term, _ContainingTerm,
    %   VarSet, ContextPieces, Result):
    %
    % Parse Term into a sym_name that is its top function symbol and a
    % list of its argument terms, and if successful return them in Result.
    % This predicate thus does almost the same job as the predicate
    % parse_implicitly_qualified_term above, the difference being that
    % that if the sym_name is qualified, then we check whether it is qualified
    % with ModuleName, and if it isn't qualified, then we qualify it with
    % Modulename (unless ModuleName is root_module_name). This is the
    % right thing to do for clause heads, which is the intended job of
    % parse_implicitly_qualified_term.
    %
:- pred parse_implicitly_qualified_term(module_name::in, term(T)::in,
    term(T)::in, varset::in, list(format_component)::in, maybe_functor(T)::out)
    is det.

%-----------------------------------------------------------------------------%

    % Replace all occurrences of inst_var(I) with
    % constrained_inst_var(I, ground(shared, none)).
    %
:- pred constrain_inst_vars_in_mode(mer_mode::in, mer_mode::out) is det.

    % Replace all occurrences of inst_var(I) with
    % constrained_inst_var(I, Inst) where I -> Inst is in the inst_var_sub.
    % If I is not in the inst_var_sub, default to ground(shared, none).
    %
:- pred constrain_inst_vars_in_mode(inst_var_sub::in,
    mer_mode::in, mer_mode::out) is det.

%-----------------------------------------------------------------------------%

    % Check that for each constrained_inst_var all occurrences have the
    % same constraint.
    %
:- pred inst_var_constraints_are_self_consistent_in_modes(list(mer_mode)::in)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io_dcg.
:- import_module parse_tree.prog_io_goal.
:- import_module parse_tree.prog_io_pragma.
:- import_module parse_tree.prog_io_typeclass.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module assoc_list.
:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module parser.
:- import_module set.
:- import_module string.
:- import_module term_io.
:- import_module unit.

%-----------------------------------------------------------------------------%

read_module(OpenFile, DefaultModuleName, ReturnTimestamp, FileData,
        ModuleName, Items, Specs, Error, MaybeModuleTimestamp, !IO) :-
    read_module_2(OpenFile, DefaultModuleName, no, ReturnTimestamp,
        FileData, ModuleName, Items, Specs, Error, MaybeModuleTimestamp, !IO).

read_module_if_changed(OpenFile, DefaultModuleName, OldTimestamp, FileData,
        ModuleName, Items, Specs, Error, MaybeModuleTimestamp, !IO) :-
    read_module_2(OpenFile, DefaultModuleName, yes(OldTimestamp),
        do_return_timestamp,
        FileData, ModuleName, Items, Specs, Error,MaybeModuleTimestamp, !IO).

read_opt_file(FileName, DefaultModuleName, Items, Specs, Error, !IO) :-
    globals.io_lookup_accumulating_option(intermod_directories, Dirs, !IO),
    read_module_2(search_for_file(Dirs, FileName), DefaultModuleName, no,
        do_not_return_timestamp, _, ModuleName, Items, Specs, Error, _, !IO),
    check_module_has_expected_name(FileName, DefaultModuleName, ModuleName,
        !IO).

check_module_has_expected_name(FileName, ExpectedName, ActualName, !IO) :-
    ( ActualName \= ExpectedName ->
        Pieces = [words("Error: file"), quote(FileName),
            words("contains the wrong module."), nl,
            words("Expected module"), sym_name(ExpectedName), suffix(","),
            words("found module"), sym_name(ActualName), suffix("."), nl],
        write_error_pieces_plain(Pieces, !IO),
        io.set_exit_status(1, !IO)
    ;
        true
    ).

    % This implementation uses io.read_term to read in the program one term
    % at a time, and then converts those terms into clauses and declarations,
    % checking for errors as it goes. Note that rather than using difference
    % lists, we just build up the lists of items and messages in reverse order
    % and then reverse them afterwards. (Using difference lists would require
    % late-input modes.)
    %
:- pred read_module_2(open_file(T)::in(open_file), module_name::in,
    maybe(timestamp)::in, maybe_return_timestamp::in, maybe(T)::out,
    module_name::out, list(item)::out, list(error_spec)::out,
    module_error::out, maybe(io.res(timestamp))::out, io::di, io::uo) is det.

read_module_2(OpenFile, DefaultModuleName, MaybeOldTimestamp, ReturnTimestamp,
        MaybeFileData, ModuleName, Items, Specs, Error,
        MaybeModuleTimestamp, !IO) :-
    io.input_stream(OldInputStream, !IO),
    OpenFile(OpenResult, !IO),
    (
        OpenResult = ok(FileData),
        MaybeFileData = yes(FileData),
        (
            ReturnTimestamp = do_return_timestamp,
            io.input_stream_name(InputStreamName, !IO),
            io.file_modification_time(InputStreamName, TimestampResult, !IO),
            (
                TimestampResult = ok(Timestamp),
                MaybeModuleTimestamp = yes(ok(time_t_to_timestamp(Timestamp)))
            ;
                TimestampResult = error(IOError),
                MaybeModuleTimestamp = yes(error(IOError))
            )
        ;
            ReturnTimestamp = do_not_return_timestamp,
            MaybeModuleTimestamp = no
        ),
        (
            MaybeOldTimestamp = yes(OldTimestamp),
            MaybeModuleTimestamp = yes(ok(OldTimestamp))
        ->
            % XXX Currently smart recompilation won't work
            % if ModuleName \= DefaultModuleName.
            % In that case, smart recompilation will be disabled
            % and read_module should never be passed an old timestamp.

            ModuleName = DefaultModuleName,
            Items = [],
            Specs = [],
            Error = no_module_errors
        ;
            read_all_items(DefaultModuleName, ModuleName, Items,
                Specs, Error, !IO)
        ),
        io.set_input_stream(OldInputStream, ModuleInputStream, !IO),
        io.close_input(ModuleInputStream, !IO)
    ;
        OpenResult = error(ErrorMsg),
        MaybeFileData = no,
        ModuleName = DefaultModuleName,
        Items = [],
        MaybeModuleTimestamp = no,

        io.progname_base("mercury_compile", Progname, !IO),
        Pieces = [fixed(Progname), suffix(":"), words(ErrorMsg), nl],
        Spec = error_spec(severity_error, phase_read_files,
            [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
        Specs = [Spec],
        Error = fatal_module_errors
    ).

search_for_module_source(Dirs, InterfaceDirs, ModuleName, MaybeFileName,
        !IO) :-
    search_for_module_source_2(Dirs, ModuleName, ModuleName,
        MaybeFileName0, !IO),
    (
        MaybeFileName0 = ok(SourceFileName),
        (
            string.remove_suffix(dir.basename(SourceFileName),
                ".m", SourceFileBaseName),
            file_name_to_module_name(SourceFileBaseName, SourceFileModuleName),
            ModuleName \= SourceFileModuleName
        ->
            % The module name doesn't match the file name. Return an error
            % if there is a more qualified matching `.m' or `.int' file in
            % the interface search path. This avoids having a file `read.m'
            % in the current directory prevent the compiler from finding
            % `bit_buffer.read.int' in the standard library.

            io.input_stream(SourceStream, !IO),
            search_for_module_source_2(InterfaceDirs, ModuleName,
                ModuleName, MaybeFileName2, !IO),
            ( MaybeFileName2 = ok(_) ->
                io.seen(!IO)
            ;
                true
            ),
            (
                MaybeFileName2 = ok(SourceFileName2),
                SourceFileName2 \= SourceFileName,
                string.remove_suffix(dir.basename(SourceFileName2), ".m",
                    SourceFileBaseName2),
                file_name_to_module_name(SourceFileBaseName2,
                    SourceFileModuleName2),
                match_sym_name(SourceFileModuleName, SourceFileModuleName2)
            ->
                io.close_input(SourceStream, !IO),
                MaybeFileName = error(find_source_error(ModuleName,
                    Dirs, yes(SourceFileName2)))
            ;
                module_name_to_file_name(ModuleName, ".int",
                    do_not_create_dirs, IntFile, !IO),
                search_for_file_returning_dir(InterfaceDirs, IntFile,
                    MaybeIntDir, !IO),
                ( MaybeIntDir = ok(_) ->
                    io.seen(!IO)
                ;
                    true
                ),
                (
                    MaybeIntDir = ok(IntDir),
                    IntDir \= dir.this_directory
                ->
                    io.close_input(SourceStream, !IO),
                    MaybeFileName = error(find_source_error(ModuleName,
                        Dirs, yes(IntDir/IntFile)))
                ;
                    io.set_input_stream(SourceStream, _, !IO),
                    MaybeFileName = MaybeFileName0
                )
            )
        ;
            MaybeFileName = MaybeFileName0
        )
    ;
        MaybeFileName0 = error(_),
        MaybeFileName = MaybeFileName0
    ).

:- func find_source_error(module_name, list(dir_name), maybe(file_name))
    = string.

find_source_error(ModuleName, Dirs, MaybeBetterMatch) = Msg :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    Msg0 = "cannot find source for module `" ++ ModuleNameStr ++
        "' in directories " ++ string.join_list(", ", Dirs),
    (
        MaybeBetterMatch = no,
        Msg = Msg0
    ;
        MaybeBetterMatch = yes(BetterMatchFile),
        Msg = Msg0 ++ ", but found " ++ BetterMatchFile ++
            " in interface search path"
    ).

:- pred search_for_module_source_2(list(dir_name)::in, module_name::in,
    module_name::in, maybe_error(file_name)::out, io::di, io::uo) is det.

search_for_module_source_2(Dirs, ModuleName, PartialModuleName, Result, !IO) :-
    module_name_to_file_name(PartialModuleName, ".m", do_not_create_dirs,
        FileName, !IO),
    search_for_file(Dirs, FileName, Result0, !IO),
    (
        Result0 = ok(_),
        Result = Result0
    ;
        Result0 = error(_),
        ( PartialModuleName1 = drop_one_qualifier(PartialModuleName) ->
            search_for_module_source_2(Dirs, ModuleName, PartialModuleName1,
                Result, !IO)
        ;
            Result = error(find_source_error(ModuleName, Dirs, no))
        )
    ).

:- func drop_one_qualifier(module_name) = module_name is semidet.

drop_one_qualifier(qualified(ParentQual, ChildName)) =
    drop_one_qualifier_2(ParentQual, ChildName).

:- func drop_one_qualifier_2(module_name, string) = module_name.

drop_one_qualifier_2(ParentQual, ChildName) =  PartialQual :-
    (
        ParentQual = unqualified(_ParentName),
        PartialQual = unqualified(ChildName)
    ;
        ParentQual = qualified(GrandParentQual, ParentName),
        PartialGrandParentQual = drop_one_qualifier_2(GrandParentQual,
            ParentName),
        PartialQual = qualified(PartialGrandParentQual, ChildName)
    ).

%-----------------------------------------------------------------------------%

:- type module_end
    --->    module_end_no
    ;       module_end_yes(module_name, prog_context).

    % Extract the final `:- end_module' declaration if any.
    %
:- pred get_end_module(module_name::in, list(item)::in, list(item)::out,
    module_end::out) is det.

get_end_module(ModuleName, RevItems0, RevItems, EndModule) :-
    (
        % Note: if the module name in the end_module declaration does not match
        % what we expect, given the source file name, then we assume that it is
        % for a nested module, and so we leave it alone. If it is not for a
        % nested module, the error will be caught by make_hlds.

        RevItems0 = [Item | RevItemsPrime],
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
        ModuleDefn = md_end_module(ModuleName)
    ->
        RevItems = RevItemsPrime,
        EndModule = module_end_yes(ModuleName, Context)
    ;
        RevItems = RevItems0,
        EndModule = module_end_no
    ).

%-----------------------------------------------------------------------------%

    % Check that the module starts with a :- module declaration,
    % and that the end_module declaration (if any) is correct,
    % and construct the final parsing result.
    %
:- pred check_end_module(module_end::in, list(item)::in, list(item)::out,
    list(error_spec)::in, list(error_spec)::out,
    module_error::in, module_error::out) is det.

check_end_module(EndModule, !Items, !Specs, !Error) :-
    % Double-check that the first item is a `:- module ModuleName' declaration,
    % and remove it from the front of the item list.
    (
        !.Items = [Item | !:Items],
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(md_module(ModuleName1), _)
    ->
        % Check that the end module declaration (if any) matches
        % the begin module declaration.
        (
            EndModule = module_end_yes(EndModuleName, EndModuleContext),
            ModuleName1 \= EndModuleName
        ->
            Pieces = [words("Error:"),
                quote(":- end_module"), words("declaration"),
                words("does not match"),
                quote(":- module"), words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(EndModuleContext, [always(Pieces)])]),
            !:Specs = [Spec | !.Specs],
            !:Error = some_module_errors
        ;
            true
        )
    ;
        % If there's no `:- module' declaration at this point, it is
        % an internal error -- read_first_item should have inserted one.
        unexpected(this_file,
            "check_end_module: no `:- module' declaration")
    ).

%-----------------------------------------------------------------------------%

    % Create a dummy term. Used for error messages that are not associated
    % with any particular term or context.
    %
:- pred dummy_term(term::out) is det.

dummy_term(Term) :-
    term.context_init(Context),
    dummy_term_with_context(Context, Term).

    % Create a dummy term with the specified context.
    % Used for error messages that are associated with some specific
    % context, but for which we don't want to print out the term
    % (or for which the term isn't available to be printed out).
    %
:- pred dummy_term_with_context(term.context::in, term::out) is det.

dummy_term_with_context(Context, Term) :-
    Term = term.functor(term.atom(""), [], Context).

%-----------------------------------------------------------------------------%

find_module_name(FileName, MaybeModuleName, !IO) :-
    io.open_input(FileName, OpenRes, !IO),
    (
        OpenRes = ok(InputStream),
        io.set_input_stream(InputStream, OldInputStream, !IO),
        ( string.remove_suffix(FileName, ".m", PartialFileName0) ->
            PartialFileName = PartialFileName0
        ;
            PartialFileName = FileName
        ),
        ( dir.basename(PartialFileName, BaseName0) ->
            BaseName = BaseName0
        ;
            BaseName = ""
        ),
        file_name_to_module_name(BaseName, DefaultModuleName),
        read_first_item(DefaultModuleName, FileName, _,
            ModuleName, _, _, Specs, _, !IO),
        MaybeModuleName = yes(ModuleName),
        % XXX _NumErrors
        globals.io_get_globals(Globals, !IO),
        write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors, !IO),
        io.set_input_stream(OldInputStream, _, !IO),
        io.close_input(InputStream, !IO)
    ;
        OpenRes = error(Error),
        ErrorMsg = io.error_message(Error),
        io.progname_base("mercury_compile", Progname, !IO),
        Pieces = [fixed(Progname), suffix(":"), words("error opening"),
            quote(FileName), suffix(":"), words(ErrorMsg), suffix("."), nl],
        Spec = error_spec(severity_error, phase_read_files,
            [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
        globals.io_get_globals(Globals, !IO),
        % XXX _NumErrors
        write_error_spec(Spec, Globals, 0, _NumWarnings, 0, _NumErrors, !IO),
        MaybeModuleName = no
    ).

    % Read a source file from standard in, first reading in
    % the input term by term and then parsing those terms and producing
    % a high-level representation.
    % Parsing is actually a 3-stage process instead of the
    % normal two-stage process:
    %   lexical analysis (chars -> tokens),
    %   parsing stage 1 (tokens -> terms),
    %   parsing stage 2 (terms -> items).
    % The final stage produces a list of program items, each of which
    % may be a declaration or a clause.
    %
    % We use a continuation-passing style here.
    %
:- pred read_all_items(module_name::in, module_name::out,
    list(item)::out, list(error_spec)::out, module_error::out,
    io::di, io::uo) is det.

read_all_items(DefaultModuleName, ModuleName, Items, Specs, Error, !IO) :-
    % Read all the items (the first one is handled specially).
    io.input_stream(Stream, !IO),
    io.input_stream_name(Stream, SourceFileName0, !IO),
    read_first_item(DefaultModuleName, SourceFileName0, SourceFileName,
        ModuleName, RevItems0, MaybeSecondTerm, Specs0, Error0, !IO),
    (
        MaybeSecondTerm = yes(SecondTerm),
        % XXX Should this be SourceFileName instead of SourceFileName0?
        read_term_to_item_result(ModuleName, SourceFileName0, SecondTerm,
            MaybeSecondItem),

        read_items_loop_2(MaybeSecondItem, ModuleName, SourceFileName,
            RevItems0, RevItems1, Specs0, Specs1, Error0, Error1, !IO)
    ;
        MaybeSecondTerm = no,
        read_items_loop(ModuleName, SourceFileName,
            RevItems0, RevItems1, Specs0, Specs1, Error0, Error1, !IO)
    ),

    % Get the end_module declaration (if any), check that it matches
    % the initial module declaration (if any), and remove both of them
    % from the final item list.
    get_end_module(ModuleName, RevItems1, RevItems, EndModule),
    check_end_module(EndModule, Items0, Items, Specs1, Specs, Error1, Error),
    list.reverse(RevItems, Items0).

    % We need to jump through a few hoops when reading the first item,
    % to allow the initial `:- module' declaration to be optional.
    % The reason is that in order to parse an item, we need to know
    % which module it is defined in (because we do some module
    % qualification and checking of module qualifiers at parse time),
    % but the initial `:- module' declaration and the declaration
    % that follows it occur in different scopes, so we need to know
    % what it is that we're parsing before we can parse it!
    % We solve this dilemma by first parsing it in the root scope,
    % and then if it turns out to not be a `:- module' declaration
    % we reparse it in the default module scope. Blecchh.
    %
:- pred read_first_item(module_name::in, file_name::in, file_name::out,
    module_name::out, list(item)::out, maybe(read_term)::out,
    list(error_spec)::out, module_error::out, io::di, io::uo) is det.

read_first_item(DefaultModuleName, !SourceFileName, ModuleName,
        Items, MaybeSecondTerm, Specs, Error, !IO) :-
    % Parse the first term, treating it as occurring within the scope
    % of the special "root" module (so that any `:- module' declaration
    % is taken to be a non-nested module unless explicitly qualified).
    parser.read_term_filename(!.SourceFileName, MaybeFirstTerm, !IO),
    root_module_name(RootModuleName),
    read_term_to_item_result(RootModuleName, !.SourceFileName, MaybeFirstTerm,
        MaybeFirstItem),
    (
        % Apply and then skip `pragma source_file' decls, by calling ourselves
        % recursively with the new source file name.
        MaybeFirstItem = read_item_ok(FirstItem),
        FirstItem = item_pragma(FirstItemPragma),
        FirstItemPragma = item_pragma_info(_,
            pragma_source_file(!:SourceFileName), _)
    ->
        read_first_item(DefaultModuleName, !SourceFileName,
            ModuleName, Items, MaybeSecondTerm, Specs, Error, !IO)
    ;
        % Check if the first term was a `:- module' decl.
        MaybeFirstItem = read_item_ok(FirstItem),
        FirstItem = item_module_defn(FirstItemModuleDefn),
        FirstItemModuleDefn = item_module_defn_info(ModuleDefn, FirstContext),
        ModuleDefn = md_module(StartModuleName)
    ->
        % If so, then check that it matches the expected module name,
        % and if not, report a warning.
        ( match_sym_name(StartModuleName, DefaultModuleName) ->
            ModuleName = DefaultModuleName,
            Specs = []
        ; match_sym_name(DefaultModuleName, StartModuleName) ->
            ModuleName = StartModuleName,
            Specs = []
        ;
            % XXX I think this should be an error, not a warning. -zs
            Pieces = [words("Warning: source file"), quote(!.SourceFileName),
                words("contains module named"), sym_name(StartModuleName),
                suffix("."), nl],
            Severity = severity_conditional(warn_wrong_module_name, yes,
                severity_warning, no),
            Msgs = [option_is_set(warn_wrong_module_name, yes,
                [always(Pieces)])],
            Spec = error_spec(Severity, phase_term_to_parse_tree,
                [simple_msg(FirstContext, Msgs)]),
            Specs = [Spec],

            % Which one should we use here? We used to use the default module
            % name (computed from the filename) but now we use the declared
            % one.
            ModuleName = StartModuleName
        ),
        make_module_decl(ModuleName, FirstContext, FixedFirstItem),
        Items = [FixedFirstItem],
        Error = no_module_errors,
        MaybeSecondTerm = no
    ;
        % If the first term was not a `:- module' decl, then issue a warning
        % (if warning enabled), and insert an implicit `:- module ModuleName'
        % decl.
        ( MaybeFirstItem = read_item_ok(FirstItem) ->
            FirstContext = get_item_context(FirstItem)
        ;
            term.context_init(!.SourceFileName, 1, FirstContext)
        ),
        % XXX I think this should be an error, not a warning. -zs
        Pieces = [words("Warning: module should start with a"),
            quote(":- module"), words("declaration."), nl],
        Severity = severity_conditional(warn_missing_module_name, yes,
            severity_warning, no),
        Msgs  = [option_is_set(warn_missing_module_name, yes,
            [always(Pieces)])],
        Spec = error_spec(Severity, phase_term_to_parse_tree,
            [simple_msg(FirstContext, Msgs)]),
        Specs = [Spec],

        ModuleName = DefaultModuleName,
        make_module_decl(ModuleName, FirstContext, FixedFirstItem),

        % Reparse the first term, this time treating it as occuring within
        % the scope of the implicit `:- module' decl rather than in the
        % root module.
        MaybeSecondTerm = yes(MaybeFirstTerm),
        Items = [FixedFirstItem],
        Error = no_module_errors
    ).

:- pred make_module_decl(module_name::in, term.context::in, item::out) is det.

make_module_decl(ModuleName, Context, Item) :-
    ModuleDefn = md_module(ModuleName),
    ItemInfo = item_module_defn_info(ModuleDefn, Context),
    Item = item_module_defn(ItemInfo).

%-----------------------------------------------------------------------------%

    % The code below was carefully optimized to run efficiently in NU-Prolog.
    % We used to call read_item(MaybeItem) - which does all the work for
    % a single item - via io.gc_call/1, which called the goal with
    % garbage collection. But optimizing for NU-Prolog is no longer a concern.

:- pred read_items_loop(module_name::in, file_name::in,
    list(item)::in, list(item)::out,
    list(error_spec)::in, list(error_spec)::out,
    module_error::in, module_error::out, io::di, io::uo) is det.

read_items_loop(ModuleName, SourceFileName, !Items, !Specs, !Error, !IO) :-
    read_item(ModuleName, SourceFileName, MaybeItem, !IO),
    read_items_loop_2(MaybeItem, ModuleName, SourceFileName, !Items,
        !Specs, !Error, !IO).

%-----------------------------------------------------------------------------%

:- pred read_items_loop_2(read_item_result::in, module_name::in,
    file_name::in, list(item)::in, list(item)::out,
    list(error_spec)::in, list(error_spec)::out,
    module_error::in, module_error::out, io::di, io::uo) is det.

read_items_loop_2(MaybeItemOrEOF, !.ModuleName, !.SourceFileName, !Items,
        !Specs, !Error, !IO) :-
    (
        MaybeItemOrEOF = read_item_eof
        % If the next item was end-of-file, then we're done.
    ;
        % If the next item had some errors, then insert them
        % in the list of errors and continue looping.

        MaybeItemOrEOF = read_item_errors(ItemSpecs),
        !:Specs = ItemSpecs ++ !.Specs,
        !:Error = some_module_errors,
        read_items_loop(!.ModuleName, !.SourceFileName, !Items,
            !Specs, !Error, !IO)
    ;
        MaybeItemOrEOF = read_item_ok(Item),
        read_items_loop_ok(Item, !ModuleName, !SourceFileName, !Items,
            !Specs, !Error, !IO),
        read_items_loop(!.ModuleName, !.SourceFileName, !Items,
            !Specs, !Error, !IO)
    ).

:- pred read_items_loop_ok(item::in, module_name::in, module_name::out,
    file_name::in, file_name::out, list(item)::in, list(item)::out,
    list(error_spec)::in, list(error_spec)::out,
    module_error::in, module_error::out, io::di, io::uo) is det.

read_items_loop_ok(Item0, !ModuleName, !SourceFileName, !Items,
        !Specs, !Error, !IO) :-
    (
        Item0 = item_nothing(ItemNothing0),
        ItemNothing0 = item_nothing_info(yes(Warning), Context0)
    ->
        Warning = item_warning(MaybeOption, Msg, Term),
        (
            MaybeOption = yes(Option),
            globals.io_lookup_bool_option(Option, Warn, !IO)
        ;
            MaybeOption = no,
            Warn = yes
        ),
        (
            Warn = yes,
            Pieces = [words("Warning: "), words(Msg), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            !:Specs = [Spec | !.Specs],

            globals.io_lookup_bool_option(halt_at_warn, Halt, !IO),
            (
                Halt = yes,
                !:Error = some_module_errors
            ;
                Halt = no
            )
        ;
            Warn = no
        ),
        ItemNothing = item_nothing_info(no, Context0),
        Item = item_nothing(ItemNothing)
    ;
        Item = Item0
    ),

    % If the next item was a valid item, check whether it was a declaration
    % that affects the current parsing context -- i.e. either a `module' or
    % `end_module' declaration, or a `pragma source_file' declaration.
    % If so, set the new parsing context according. Next, unless the item
    % is a `pragma source_file' declaration, insert it into the item list.
    % Then continue looping.
    (
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(_, PragmaType, _),
        PragmaType = pragma_source_file(NewSourceFileName)
    ->
        !:SourceFileName = NewSourceFileName
    ;
        Item = item_module_defn(ItemModuleDefn)
    ->
        ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
        ( ModuleDefn = md_module(NestedModuleName) ->
            !:ModuleName = NestedModuleName,
            !:Items = [Item | !.Items]
        ; ModuleDefn = md_end_module(NestedModuleName) ->
            root_module_name(RootModuleName),
            sym_name_get_module_name_default(NestedModuleName, RootModuleName,
                ParentModuleName),
            !:ModuleName = ParentModuleName,
            !:Items = [Item | !.Items]
        ; ModuleDefn = md_import(Modules) ->
            ImportItems = list.map(make_pseudo_import_module_decl(Context),
                Modules),
            !:Items = ImportItems ++ !.Items
        ; ModuleDefn = md_use(Modules) ->
            UseItems = list.map(make_pseudo_use_module_decl(Context),
                Modules),
            !:Items = UseItems ++ !.Items
        ; ModuleDefn = md_include_module(Modules) ->
            IncludeItems = list.map(make_pseudo_include_module_decl(Context),
                Modules),
            !:Items = IncludeItems ++ !.Items
        ;
            !:Items = [Item | !.Items]
        )
    ;
        !:Items = [Item | !.Items]
    ).

%-----------------------------------------------------------------------------%

:- type read_item_result
    --->    read_item_eof
    ;       read_item_errors(list(error_spec))
    ;       read_item_ok(item).

    % Read_item/1 reads a single item, and if it is a valid term parses it.
    %
:- pred read_item(module_name::in, file_name::in, read_item_result::out,
    io::di, io::uo) is det.

read_item(ModuleName, SourceFileName, MaybeItem, !IO) :-
    parser.read_term_filename(SourceFileName, MaybeTerm, !IO),
    read_term_to_item_result(ModuleName, SourceFileName, MaybeTerm, MaybeItem).

:- pred read_term_to_item_result(module_name::in, string::in, read_term::in,
    read_item_result::out) is det.

read_term_to_item_result(ModuleName, FileName, ReadTermResult,
        ReadItemResult) :-
    (
        ReadTermResult = eof,
        ReadItemResult = read_item_eof
    ;
        ReadTermResult = error(ErrorMsg, LineNumber),
        % XXX Do we need to add an "Error:" prefix?
        Pieces = [words(ErrorMsg), suffix("."), nl],
        Context = term.context_init(FileName, LineNumber),
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        ReadItemResult = read_item_errors([Spec])
    ;
        ReadTermResult = term(VarSet, Term),
        parse_item(ModuleName, VarSet, Term, MaybeItem),
        (
            MaybeItem = ok1(Item),
            ReadItemResult = read_item_ok(Item)
        ;
            MaybeItem = error1(Specs),
            ReadItemResult = read_item_errors(Specs)
        )
    ).

parse_item(ModuleName, VarSet, Term, Result) :-
    (
        Term = term.functor(term.atom(":-"), [DeclTerm], _DeclContext)
    ->
        % It's a declaration.
        parse_decl(ModuleName, VarSet, DeclTerm, Result)
    ;
        Term = term.functor(term.atom("-->"), [DCG_H_Term, DCG_B_Term],
            DCG_Context)
    ->
        % It's a DCG clause.
        parse_dcg_clause(ModuleName, VarSet, DCG_H_Term, DCG_B_Term,
            DCG_Context, Result)
    ;
        % It's either a fact or a rule.
        (
            Term = term.functor(term.atom(":-"),
                [HeadTermPrime, BodyTermPrime], TermContext)
        ->
            % It's a rule.
            HeadTerm = HeadTermPrime,
            BodyTerm = BodyTermPrime,
            ClauseContext = TermContext
        ;
            % It's a fact.
            HeadTerm = Term,
            ClauseContext = get_term_context(HeadTerm),
            BodyTerm = term.functor(term.atom("true"), [], ClauseContext)
        ),
        varset.coerce(VarSet, ProgVarSet),
        parse_clause(ModuleName, Term, HeadTerm, BodyTerm, ProgVarSet,
            ClauseContext, Result)
    ).

:- pred parse_clause(module_name::in, term::in, term::in, term::in,
    prog_varset::in, term.context::in, maybe1(item)::out) is det.

parse_clause(ModuleName, Term, HeadTerm, BodyTerm0, ProgVarSet0, Context,
        MaybeItem) :-
    GoalContextPieces = [],
    parse_goal(BodyTerm0, GoalContextPieces, MaybeBodyGoal,
        ProgVarSet0, ProgVarSet),
    (
        MaybeBodyGoal = ok1(BodyGoal),
        varset.coerce(ProgVarSet, VarSet),
        (
            HeadTerm = term.functor(term.atom("="),
                [FuncHeadTerm0, FuncResultTerm], _),
            FuncHeadTerm = desugar_field_access(FuncHeadTerm0)
        ->
            HeadContextPieces = [words("In equation head:")],
            parse_implicitly_qualified_term(ModuleName, FuncHeadTerm, HeadTerm,
                VarSet, HeadContextPieces, MaybeFunctor),
            (
                MaybeFunctor = ok2(Name, ArgTerms0),
                list.map(term.coerce, ArgTerms0 ++ [FuncResultTerm],
                    ProgArgTerms),
                ItemClause = item_clause_info(user, ProgVarSet, pf_function,
                    Name, ProgArgTerms, BodyGoal, Context),
                Item = item_clause(ItemClause),
                MaybeItem = ok1(Item)
            ;
                MaybeFunctor = error2(Specs),
                MaybeItem = error1(Specs)
            )
        ;
            HeadContextPieces = [words("In clause head:")],
            parse_implicitly_qualified_term(ModuleName, HeadTerm, Term,
                VarSet, HeadContextPieces, MaybeFunctor),
            (
                MaybeFunctor = ok2(Name, ArgTerms),
                list.map(term.coerce, ArgTerms, ProgArgTerms),
                ItemClause = item_clause_info(user, ProgVarSet, pf_predicate,
                    Name, ProgArgTerms, BodyGoal, Context),
                Item = item_clause(ItemClause),
                MaybeItem = ok1(Item)
            ;
                MaybeFunctor = error2(Specs),
                MaybeItem = error1(Specs)
            )
        )
    ;
        MaybeBodyGoal = error1(Specs),
        MaybeItem = error1(Specs)
    ).

%-----------------------------------------------------------------------------%

:- type decl_attribute
    --->    decl_attr_purity(purity)
    ;       decl_attr_quantifier(quantifier_type, list(var))
    ;       decl_attr_constraints(quantifier_type, term)
            % the term here is the (not yet parsed) list of constraints
    ;       decl_attr_solver_type.

:- type quantifier_type
    --->    quant_type_exist
    ;       quant_type_univ.

    % The term associated with each decl_attribute is the term containing
    % both the attribute and the declaration that that attribute modifies;
    % this term is used when printing out error messages for cases when
    % attributes are used on declarations where they are not allowed.
:- type decl_attrs == assoc_list(decl_attribute, term.context).

parse_decl(ModuleName, VarSet, Term, Result) :-
    parse_attrs_and_decl(ModuleName, VarSet, Term, [], Result).

    % parse_attrs_and_decl(ModuleName, VarSet, Term, Attributes, Result):
    %
    % Succeeds if Term is a declaration and binds Result to a representation
    % of that declaration. Attributes is a list of enclosing declaration
    % attributes, in the order innermost to outermost.
    %
:- pred parse_attrs_and_decl(module_name::in, varset::in, term::in,
    decl_attrs::in, maybe1(item)::out) is det.

parse_attrs_and_decl(ModuleName, VarSet, Term, !.Attributes, MaybeItem) :-
    ( Term = term.functor(term.atom(Functor), Args, Context) ->
        (
            parse_decl_attribute(Functor, Args, Attribute, SubTerm)
        ->
            !:Attributes = [Attribute - Context | !.Attributes],
            parse_attrs_and_decl(ModuleName, VarSet, SubTerm, !.Attributes,
                MaybeItem)
        ;
            parse_attributed_decl(ModuleName, VarSet, Functor, Args,
                !.Attributes, Context, MaybeItemPrime)
        ->
            MaybeItemPrime = MaybeItem
        ;
            TermStr = mercury_term_to_string(VarSet, no, Term),
            Pieces = [words("Error: unrecognized declaration:"), nl,
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ;
        Context = get_term_context(Term),
        Pieces = [words("Error: atom expected after `:-'."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

    % parse_attributed_decl(ModuleName, VarSet, Functor, Args, Attributes,
    %   Context, Result):
    %
    % Succeeds if Atom(Args) is a declaration and binds Result to a
    % representation of that declaration. Attributes is a list of
    % enclosing declaration attributes, in the order outermost to innermost.
    %
:- pred parse_attributed_decl(module_name::in, varset::in, string::in,
    list(term)::in, decl_attrs::in, prog_context::in, maybe1(item)::out)
    is semidet.

parse_attributed_decl(ModuleName, VarSet, Functor, ArgTerms, Attributes,
        Context, MaybeItem) :-
    (
        Functor = "type",
        ArgTerms = [TypeDefnTerm],
        parse_type_defn(ModuleName, VarSet, TypeDefnTerm, Attributes, Context,
            MaybeItem)
    ;
        Functor = "inst",
        ArgTerms = [InstDeclTerm],
        parse_inst_defn(ModuleName, VarSet, InstDeclTerm, Context, Result0),
        check_no_attributes(Result0, Attributes, MaybeItem)
    ;
        Functor = "mode",
        ArgTerms = [SubTerm],
        ( SubTerm = term.functor(term.atom("=="), [HeadTerm, BodyTerm], _) ->
            % This is the definition of a mode.
            parse_condition_suffix(BodyTerm, BeforeCondTerm, Condition),
            parse_mode_defn(ModuleName, VarSet, HeadTerm, BeforeCondTerm,
                ModeDefnResult),
            process_maybe1(make_mode_defn(VarSet, Condition, Context),
                ModeDefnResult, MaybeItem)
        ;
            % This is the declaration of one mode of a predicate or function.
            parse_mode_decl(ModuleName, VarSet, SubTerm, Attributes,
                Context, MaybeItem)
        )
    ;
        (
            Functor = "pred",
            PredOrFunc = pf_predicate
        ;
            Functor = "func",
            PredOrFunc = pf_function
        ),
        ArgTerms = [DeclTerm],
        parse_pred_or_func_decl(PredOrFunc, ModuleName, VarSet, DeclTerm,
            Attributes, Context, MaybeItem)
    ;
        (
            Functor = "import_module",
            Maker = make_import
        ;
            Functor = "use_module",
            Maker = make_use
        ;
            Functor = "export_module",
            Maker = make_export
        ),
        ArgTerms = [ModuleSpecTerm],
        parse_symlist_decl(parse_module_specifier(VarSet), Maker,
            ModuleSpecTerm, Attributes, Context, MaybeItem)
    ;
        (
            Functor = "interface",
            ModuleDefn = md_interface
        ;
            Functor = "implementation",
            ModuleDefn = md_implementation
        ),
        ArgTerms = [],
        ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
        Item = item_module_defn(ItemModuleDefn),
        MaybeItem0 = ok1(Item),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "external",
        (
            ArgTerms = [PredSpecTerm],
            MaybeBackEnd = no
        ;
            ArgTerms = [BackEndArgTerm, PredSpecTerm],
            BackEndArgTerm = term.functor(term.atom(BackEndFunctor), [], _),
            (
                BackEndFunctor = "high_level_backend",
                BackEnd = high_level_backend
            ;
                BackEndFunctor = "low_level_backend",
                BackEnd = low_level_backend
            ),
            MaybeBackEnd = yes(BackEnd)
        ),
        parse_implicitly_qualified_symbol_name_specifier(ModuleName, VarSet,
            PredSpecTerm, MaybeSymSpec),
        process_maybe1(make_external(MaybeBackEnd, Context),
            MaybeSymSpec, MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "module",
        ArgTerms = [ModuleNameTerm],
        parse_module_name(ModuleName, VarSet, ModuleNameTerm,
            MaybeModuleNameSym),
        (
            MaybeModuleNameSym = ok1(ModuleNameSym),
            ModuleDefn = md_module(ModuleNameSym),
            ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
            Item = item_module_defn(ItemModuleDefn),
            MaybeItem0 = ok1(Item)
        ;
            MaybeModuleNameSym = error1(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "include_module",
        ArgTerms = [ModuleNamesTerm],
        parse_list(parse_module_name(ModuleName, VarSet), ModuleNamesTerm,
            MaybeModuleNameSyms),
        (
            MaybeModuleNameSyms = ok1(ModuleNameSyms),
            ModuleDefn = md_include_module(ModuleNameSyms),
            ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
            Item = item_module_defn(ItemModuleDefn),
            MaybeItem0 = ok1(Item)
        ;
            MaybeModuleNameSyms = error1(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "end_module",
        ArgTerms = [ModuleNameTerm],
        % The name in an `end_module' declaration not inside the scope of the
        % module being ended, so the default module name here (ModuleName)
        % is the parent of the previous default module name.

        root_module_name(RootModuleName),
        sym_name_get_module_name_default(ModuleName, RootModuleName,
            ParentOfModuleName),
        parse_module_name(ParentOfModuleName, VarSet, ModuleNameTerm,
            MaybeModuleNameSym),
        (
            MaybeModuleNameSym = ok1(ModuleNameSym),
            ModuleDefn = md_end_module(ModuleNameSym),
            ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
            Item = item_module_defn(ItemModuleDefn),
            MaybeItem0 = ok1(Item)
        ;
            MaybeModuleNameSym = error1(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "pragma",
        parse_pragma(ModuleName, VarSet, ArgTerms, Context, MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        (
            Functor = "promise",
            PromiseType = promise_type_true
        ;
            Functor = "promise_exclusive",
            PromiseType = promise_type_exclusive
        ;
            Functor = "promise_exhaustive",
            PromiseType = promise_type_exhaustive
        ;
            Functor = "promise_exclusive_exhaustive",
            PromiseType = promise_type_exclusive_exhaustive
        ),
        parse_promise(ModuleName, PromiseType, VarSet, ArgTerms, Attributes,
            Context, MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "typeclass",
        parse_typeclass(ModuleName, VarSet, ArgTerms, Context,
            MaybeItemTypeClass),
        (
            MaybeItemTypeClass = ok1(ItemTypeClass),
            MaybeItem0 = ok1(item_typeclass(ItemTypeClass))
        ;
            MaybeItemTypeClass = error1(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "instance",
        parse_instance(ModuleName, VarSet, ArgTerms, Context,
            MaybeItemInstance),
        (
            MaybeItemInstance = ok1(ItemInstance),
            MaybeItem0 = ok1(item_instance(ItemInstance))
        ;
            MaybeItemInstance = error1(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        ( Functor = "initialise"
        ; Functor = "initialize"
        ),
        ArgTerms = [SubTerm],
        parse_initialise_decl(ModuleName, VarSet, SubTerm, Context,
            MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        ( Functor = "finalise"
        ; Functor = "finalize"
        ),
        ArgTerms = [SubTerm],
        parse_finalise_decl(ModuleName, VarSet, SubTerm, Context, MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "mutable",
        parse_mutable_decl(ModuleName, VarSet, ArgTerms, Context, MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "version_numbers",
        process_version_numbers(ModuleName, VarSet, ArgTerms, Attributes,
            Context, MaybeItem)
    ).

:- pred parse_symlist_decl(parser(module_specifier)::parser,
    maker(list(module_specifier), module_defn)::maker,
    term::in, decl_attrs::in, prog_context::in, maybe1(item)::out) is det.

parse_symlist_decl(ParserPred, MakeModuleDefnPred, Term, Attributes, Context,
        MaybeItem) :-
    parse_list(ParserPred, Term, MaybeModuleSpecs),
    process_maybe1(make_module_defn(MakeModuleDefnPred, Context),
        MaybeModuleSpecs, MaybeItem0),
    check_no_attributes(MaybeItem0, Attributes, MaybeItem).

:- pred process_version_numbers(module_name::in, varset::in, list(term)::in,
    decl_attrs::in, prog_context::in, maybe1(item)::out) is semidet.

process_version_numbers(ModuleName, VarSet, ArgTerms, Attributes, Context,
        Result) :-
    ArgTerms = [VersionNumberTerm, ModuleNameTerm, VersionNumbersTerm],
    parse_module_specifier(VarSet, ModuleNameTerm, MaybeModuleName),
    (
        VersionNumberTerm = term.functor(term.integer(VersionNumber), [], _),
        VersionNumber = version_numbers_version_number
    ->
        (
            MaybeModuleName = ok1(ModuleName),
            recompilation.version.parse_version_numbers(VersionNumbersTerm,
                Result0),
            (
                Result0 = ok1(VersionNumbers),
                ModuleDefn = md_version_numbers(ModuleName, VersionNumbers),
                ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
                Item = item_module_defn(ItemModuleDefn),
                Result1 = ok1(Item),
                check_no_attributes(Result1, Attributes, Result)
            ;
                Result0 = error1(Specs),
                Result = error1(Specs)
            )
        ;
            % XXX _Spec
            MaybeModuleName = error1(_Spec),
            Pieces = [words("Error: invalid module name in"),
                quote(":- version_numbers"), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ModuleNameTerm),
                    [always(Pieces)])]),
            Result = error1([Spec])
        )
    ;
        (
            VersionNumberTerm = term.functor(_, _, _VersionNumberContext),
            Msg = "interface file needs to be recreated, " ++
                "the version numbers are out of date",
            dummy_term_with_context(Context, DummyTerm),
            Warning = item_warning(yes(warn_smart_recompilation),
                Msg, DummyTerm),
            ItemNothing = item_nothing_info(yes(Warning), Context),
            Item = item_nothing(ItemNothing),
            Result = ok1(Item)
        ;
            VersionNumberTerm = term.variable(_, VersionNumberContext),
            Pieces = [words("Error: invalid version number in"),
                quote(":- version_numbers"), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(VersionNumberContext, [always(Pieces)])]),
            Result = error1([Spec])
        )
    ).

:- pred parse_decl_attribute(string::in, list(term)::in, decl_attribute::out,
    term::out) is semidet.

parse_decl_attribute(Functor, ArgTerms, Attribute, SubTerm) :-
    (
        Functor = "impure",
        ArgTerms = [SubTerm],
        Attribute = decl_attr_purity(purity_impure)
    ;
        Functor = "semipure",
        ArgTerms = [SubTerm],
        Attribute = decl_attr_purity(purity_semipure)
    ;
        Functor = "<=",
        ArgTerms = [SubTerm, ConstraintsTerm],
        Attribute = decl_attr_constraints(quant_type_univ, ConstraintsTerm)
    ;
        Functor = "=>",
        ArgTerms = [SubTerm, ConstraintsTerm],
        Attribute = decl_attr_constraints(quant_type_exist, ConstraintsTerm)
    ;
        Functor = "some",
        ArgTerms = [TVarsTerm, SubTerm],
        parse_list_of_vars(TVarsTerm, TVars),
        Attribute = decl_attr_quantifier(quant_type_exist, TVars)
    ;
        Functor = "all",
        ArgTerms = [TVarsTerm, SubTerm],
        parse_list_of_vars(TVarsTerm, TVars),
        Attribute = decl_attr_quantifier(quant_type_univ, TVars)
    ;
        Functor = "solver",
        ArgTerms = [SubTerm],
        Attribute = decl_attr_solver_type
    ).

:- pred check_no_attributes(maybe1(T)::in, decl_attrs::in, maybe1(T)::out)
    is det.

check_no_attributes(Result0, Attributes, Result) :-
    (
        Result0 = ok1(_),
        Attributes = [Attr - Context | _]
    ->
        % XXX Shouldn't we mention EVERY element of Attributes?
        Pieces = [words("Error:"), words(attribute_description(Attr)),
            words("not allowed here."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        Result = error1([Spec])
    ;
        Result = Result0
    ).

:- func attribute_description(decl_attribute) = string.

attribute_description(decl_attr_purity(_)) = "purity specifier".
attribute_description(decl_attr_quantifier(quant_type_univ, _)) =
    "universal quantifier (`all')".
attribute_description(decl_attr_quantifier(quant_type_exist, _)) =
    "existential quantifier (`some')".
attribute_description(decl_attr_constraints(quant_type_univ, _)) =
    "type class constraint (`<=')".
attribute_description(decl_attr_constraints(quant_type_exist, _)) =
    "existentially quantified type class constraint (`=>')".
attribute_description(decl_attr_solver_type) = "solver type specifier".

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Parsing type definitions.
%

:- type processed_type_body
    --->    processed_type_body(
                sym_name,
                list(type_param),
                type_defn
            ).

    % parse_type_defn parses the definition of a type.
    %
:- pred parse_type_defn(module_name::in, varset::in, term::in, decl_attrs::in,
    prog_context::in, maybe1(item)::out) is det.

parse_type_defn(ModuleName, VarSet, TypeDefnTerm, Attributes, Context,
        MaybeItem) :-
    (
        TypeDefnTerm = term.functor(term.atom(Name), ArgTerms, _),
        ArgTerms = [HeadTerm, BodyTerm],
        ( Name = "--->"
        ; Name = "=="
        ; Name = "where"
        )
    ->
        parse_condition_suffix(BodyTerm, BeforeCondTerm, Condition),
        (
            Name = "--->",
            parse_du_type_defn(ModuleName, VarSet,
                HeadTerm, BeforeCondTerm, Attributes, MaybeProcessedTypeBody)
        ;
            Name = "==",
            parse_eqv_type_defn(ModuleName, VarSet,
                HeadTerm, BeforeCondTerm, Attributes, MaybeProcessedTypeBody)
        ;
            Name = "where",
            parse_solver_type_defn(ModuleName, VarSet,
                HeadTerm, BeforeCondTerm, Attributes, MaybeProcessedTypeBody)
        )
    ;
        parse_abstract_type_defn(ModuleName, VarSet, TypeDefnTerm,
            Attributes, MaybeProcessedTypeBody),
        Condition = cond_true
    ),
    % We should check the condition for errors (don't bother at the moment,
    % since we ignore conditions anyhow :-).
    process_maybe1(make_type_defn(VarSet, Condition, Context),
        MaybeProcessedTypeBody, MaybeItem).

%-----------------------------------------------------------------------------%
%
% Code dealing with definitions of discriminated union types.
%

    % parse_du_type_defn parses the definition of a discriminated union type.
    %
:- pred parse_du_type_defn(module_name::in, varset::in, term::in, term::in,
    decl_attrs::in, maybe1(processed_type_body)::out) is det.

parse_du_type_defn(ModuleName, VarSet, HeadTerm, BodyTerm, Attributes0,
        MaybeProcessedTypeBody) :-
    get_is_solver_type(IsSolverType, Attributes0, Attributes),
    (
        IsSolverType = solver_type,
        Pieces = [words("Error: a solver type"),
            words("cannot have data constructors."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
        MaybeProcessedTypeBody = error1([Spec])
    ;
        IsSolverType = non_solver_type,
        parse_type_defn_head(ModuleName, VarSet, HeadTerm,
            MaybeTypeCtorAndArgs),
        du_type_rhs_ctors_and_where_terms(BodyTerm, CtorsTerm, MaybeWhereTerm),
        MaybeCtors = parse_constructors(ModuleName, VarSet, CtorsTerm),
        MaybeWhere = parse_type_decl_where_term(non_solver_type,
            ModuleName, VarSet, MaybeWhereTerm),
        % The code to process `where' attributes will return an error
        % if solver attributes are given for a non-solver type. Because
        % this is a du type, if the unification with WhereResult succeeds
        % then _NoSolverTypeDetails is guaranteed to be `no'.
        (
            MaybeTypeCtorAndArgs = ok2(Functor, Params),
            MaybeCtors = ok1(Ctors),
            MaybeWhere = ok2(_NoSolverTypeDetails, MaybeUserEqComp)
        ->
            process_du_ctors(Params, VarSet, BodyTerm, Ctors, [], CtorsSpecs),
            (
                CtorsSpecs = [],
                TypeDefn = parse_tree_du_type(Ctors, MaybeUserEqComp),
                ProcessedTypeBody = processed_type_body(Functor, Params,
                    TypeDefn),
                MaybeProcessedTypeBody0 = ok1(ProcessedTypeBody),
                check_no_attributes(MaybeProcessedTypeBody0, Attributes,
                    MaybeProcessedTypeBody)
            ;
                CtorsSpecs = [_ | _],
                MaybeProcessedTypeBody = error1(CtorsSpecs)
            )
        ;
            Specs = get_any_errors2(MaybeTypeCtorAndArgs) ++
                get_any_errors1(MaybeCtors) ++ get_any_errors2(MaybeWhere),
            MaybeProcessedTypeBody = error1(Specs)
        )
    ).

:- pred du_type_rhs_ctors_and_where_terms(term::in,
    term::out, maybe(term)::out) is det.

du_type_rhs_ctors_and_where_terms(Term, CtorsTerm, MaybeWhereTerm) :-
    (
        Term = term.functor(term.atom("where"), Args, _Context),
        Args = [CtorsTerm0, WhereTerm]
    ->
        CtorsTerm      = CtorsTerm0,
        MaybeWhereTerm = yes(WhereTerm)
    ;
        CtorsTerm      = Term,
        MaybeWhereTerm = no
    ).

    % Convert a list of terms separated by semi-colons (known as a
    % "disjunction", even thought the terms aren't goals in this case)
    % into a list of constructors.
    %
:- func parse_constructors(module_name, varset, term) =
    maybe1(list(constructor)).

parse_constructors(ModuleName, VarSet, Term) = MaybeConstructors :-
    disjunction_to_list(Term, BodyTermList),
    MaybeConstructors = parse_constructors_2(ModuleName, VarSet, BodyTermList).

    % True if the term is a valid list of constructors.
    %
:- func parse_constructors_2(module_name, varset, list(term)) =
    maybe1(list(constructor)).

parse_constructors_2(_ModuleName, _, []) = ok1([]).
parse_constructors_2(ModuleName, VarSet, [Head | Tail]) = MaybeConstructors :-
    MaybeHeadConstructor = parse_constructor(ModuleName, VarSet, Head),
    MaybeTailConstructors = parse_constructors_2(ModuleName, VarSet, Tail),
    (
        MaybeHeadConstructor = ok1(HeadConstructor),
        MaybeTailConstructors = ok1(TailConstructors)
    ->
        Constructors = [HeadConstructor | TailConstructors],
        MaybeConstructors = ok1(Constructors)
    ;
        Specs = get_any_errors1(MaybeHeadConstructor) ++
            get_any_errors1(MaybeTailConstructors),
        MaybeConstructors = error1(Specs)
    ).

:- func parse_constructor(module_name, varset, term) = maybe1(constructor).

parse_constructor(ModuleName, VarSet, Term) = MaybeConstructor :-
    ( Term = term.functor(term.atom("some"), [VarsTerm, SubTerm], _) ->
        ( parse_list_of_vars(VarsTerm, ExistQVars) ->
            list.map(term.coerce_var, ExistQVars, ExistQTVars),
            MaybeConstructor = parse_constructor_2(ModuleName, VarSet,
                ExistQTVars, Term, SubTerm)
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: syntax error in variable list at"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(VarsTerm), [always(Pieces)])]),
            MaybeConstructor = error1([Spec])
        )
    ;
        ExistQVars = [],
        MaybeConstructor = parse_constructor_2(ModuleName, VarSet, ExistQVars,
            Term, Term)
    ).

:- func parse_constructor_2(module_name, varset, list(tvar), term, term) =
    maybe1(constructor).

parse_constructor_2(ModuleName, VarSet, ExistQVars, ContainingTerm, Term)
        = MaybeConstructor :-
    get_existential_constraints_from_term(ModuleName, VarSet, Term,
        BeforeConstraintsTerm, MaybeConstraints),
    (
        MaybeConstraints = error1(Specs),
        MaybeConstructor = error1(Specs)
    ;
        MaybeConstraints = ok1(Constraints),
        (
            % Note that as a special case, one level of curly braces around
            % the constructor are ignored. This is to allow you to define
            % ';'/2 and 'some'/2 constructors.
            BeforeConstraintsTerm = term.functor(term.atom("{}"),
                [InsideBracesTerm], _Context)
        ->
            MainTerm = InsideBracesTerm
        ;
            MainTerm = BeforeConstraintsTerm
        ),
        ContextPieces = [words("In constructor definition:")],
        parse_implicitly_qualified_term(ModuleName, MainTerm, ContainingTerm,
            VarSet, ContextPieces, MaybeFunctorAndArgTerms),
        (
            MaybeFunctorAndArgTerms = error2(Specs),
            MaybeConstructor  = error1(Specs)
        ;
            MaybeFunctorAndArgTerms = ok2(Functor, ArgTerms),
            MaybeConstructorArgs = convert_constructor_arg_list(ModuleName,
                VarSet, ArgTerms),
            (
                MaybeConstructorArgs = error1(Specs),
                MaybeConstructor = error1(Specs)
            ;
                MaybeConstructorArgs = ok1(ConstructorArgs),
                Ctor = ctor(ExistQVars, Constraints, Functor, ConstructorArgs,
                    get_term_context(MainTerm)),
                MaybeConstructor = ok1(Ctor)
            )
        )
    ).

:- pred get_existential_constraints_from_term(module_name::in, varset::in,
    term::in, term::out, maybe1(list(prog_constraint))::out) is det.

get_existential_constraints_from_term(ModuleName, VarSet, !PredTypeTerm,
        MaybeExistentialConstraints) :-
    (
        !.PredTypeTerm = term.functor(term.atom("=>"),
            [!:PredTypeTerm, ExistentialConstraints], _)
    ->
        parse_class_constraints(ModuleName, VarSet, ExistentialConstraints,
            MaybeExistentialConstraints)
    ;
        MaybeExistentialConstraints = ok1([])
    ).

:- func convert_constructor_arg_list(module_name, varset, list(term)) =
    maybe1(list(constructor_arg)).

convert_constructor_arg_list(_, _, []) = ok1([]).
convert_constructor_arg_list(ModuleName, VarSet, [Term | Terms])
        = MaybeConstructorArgs :-
    ( Term = term.functor(term.atom("::"), [NameTerm, TypeTerm], _) ->
        ContextPieces = [words("In field name:")],
        parse_implicitly_qualified_term(ModuleName, NameTerm, Term,
            VarSet, ContextPieces, MaybeSymNameAndArgs),
        (
            MaybeSymNameAndArgs = error2(Specs),
            MaybeConstructorArgs = error1(Specs)
        ;
            MaybeSymNameAndArgs = ok2(SymName, SymNameArgs),
            (
                SymNameArgs = [_ | _],
                % XXX Should we add "... at function symbol ..."?
                Pieces = [words("Error: syntax error in constructor name."),
                    nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                MaybeConstructorArgs = error1([Spec])
            ;
                SymNameArgs = [],
                MaybeFieldName = yes(SymName),
                MaybeConstructorArgs =
                    convert_constructor_arg_list_2(ModuleName,
                        VarSet, MaybeFieldName, TypeTerm, Terms)
            )
        )
    ;
        MaybeFieldName = no,
        TypeTerm = Term,
        MaybeConstructorArgs = convert_constructor_arg_list_2(ModuleName,
            VarSet, MaybeFieldName, TypeTerm, Terms)
    ).

:- func convert_constructor_arg_list_2(module_name, varset, maybe(sym_name),
    term, list(term)) = maybe1(list(constructor_arg)).

convert_constructor_arg_list_2(ModuleName, VarSet, MaybeFieldName,
        TypeTerm, Terms) = Result :-
    ContextPieces = [words("In type definition:")],
    parse_type(TypeTerm, VarSet, ContextPieces, TypeResult),
    (
        TypeResult = ok1(Type),
        Context = get_term_context(TypeTerm),
        Arg = ctor_arg(MaybeFieldName, Type, Context),
        Result0 = convert_constructor_arg_list(ModuleName, VarSet, Terms),
        (
            Result0 = error1(Specs),
            Result  = error1(Specs)
        ;
            Result0 = ok1(Args),
            Result  = ok1([Arg | Args])
        )
    ;
        TypeResult = error1(Specs),
        Result = error1(Specs)
    ).

:- pred process_du_ctors(list(type_param)::in, varset::in, term::in,
    list(constructor)::in, list(error_spec)::in, list(error_spec)::out) is det.

process_du_ctors(_Params, _, _, [], !Specs).
process_du_ctors(Params, VarSet, BodyTerm, [Ctor | Ctors], !Specs) :-
    Ctor = ctor(ExistQVars, Constraints, _CtorName, CtorArgs, _Context),
    (
        % Check that all type variables in the ctor are either explicitly
        % existentially quantified or occur in the head of the type.

        CtorArgTypes = list.map(func(C) = C ^ arg_type, CtorArgs),
        type_vars_list(CtorArgTypes, VarsInCtorArgTypes0),
        list.sort_and_remove_dups(VarsInCtorArgTypes0, VarsInCtorArgTypes),
        list.filter(list.contains(ExistQVars ++ Params), VarsInCtorArgTypes,
            _ExistQOrParamVars, NotExistQOrParamVars),
        NotExistQOrParamVars = [_ | _]
    ->
        % There should be no duplicate names to remove.
        varset.coerce(VarSet, GenericVarSet),
        NotExistQOrParamVarsStr =
            mercury_vars_to_string(GenericVarSet, no, NotExistQOrParamVars),
        Pieces = [words("Error: free type"),
            words(choose_number(NotExistQOrParamVars,
                "parameter", "parameters")),
            words(NotExistQOrParamVarsStr),
            words("in RHS of type definition."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(BodyTerm), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ;
        % Check that all type variables in existential quantifiers do not
        % occur in the head (maybe this should just be a warning, not an error?
        % If we were to allow it, we would need to rename them apart.)

        set.list_to_set(ExistQVars, ExistQVarsSet),
        set.list_to_set(Params, ParamsSet),
        set.intersect(ExistQVarsSet, ParamsSet, ExistQParamsSet),
        set.non_empty(ExistQParamsSet)
    ->
        % There should be no duplicate names to remove.
        set.to_sorted_list(ExistQParamsSet, ExistQParams),
        varset.coerce(VarSet, GenericVarSet),
        ExistQParamVarsStr =
            mercury_vars_to_string(GenericVarSet, no, ExistQParams),
        Pieces = [words("Error:"),
            words(choose_number(ExistQParams,
                "type variable", "type variables")),
            words(ExistQParamVarsStr),
            words(choose_number(ExistQParams, "has", "have")),
            words("overlapping scopes"),
            words("(explicit type quantifier shadows argument type)."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(BodyTerm), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ;
        % Check that all type variables in existential quantifiers occur
        % somewhere in the constructor argument types or constraints.

        CtorArgTypes = list.map(func(C) = C ^ arg_type, CtorArgs),
        type_vars_list(CtorArgTypes, VarsInCtorArgTypes0),
        list.sort_and_remove_dups(VarsInCtorArgTypes0, VarsInCtorArgTypes),
        constraint_list_get_tvars(Constraints, ConstraintTVars),
        list.filter(list.contains(VarsInCtorArgTypes ++ ConstraintTVars),
            ExistQVars, _OccursExistQVars, NotOccursExistQVars),
        NotOccursExistQVars = [_ | _]
    ->
        % There should be no duplicate names to remove.
        varset.coerce(VarSet, GenericVarSet),
        NotOccursExistQVarsStr =
            mercury_vars_to_string(GenericVarSet, no, NotOccursExistQVars),
        Pieces = [words("Error:"),
            words(choose_number(NotOccursExistQVars,
                "type variable", "type variables")),
            words(NotOccursExistQVarsStr),
            words("in existential quantifier"),
            words(choose_number(NotOccursExistQVars,
                "does not occur", "do not occur")),
            words("in arguments or constraints of constructor."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(BodyTerm), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ;
        % Check that all type variables in existential constraints occur in
        % the existential quantifiers.

        ConstraintArgTypeLists =
            list.map(prog_constraint_get_arg_types, Constraints),
        list.condense(ConstraintArgTypeLists, ConstraintArgTypes),
        type_vars_list(ConstraintArgTypes, VarsInCtorArgTypes0),
        list.sort_and_remove_dups(VarsInCtorArgTypes0, VarsInCtorArgTypes),
        list.filter(list.contains(ExistQVars), VarsInCtorArgTypes,
            _ExistQArgTypes, NotExistQArgTypes),
        NotExistQArgTypes = [_ | _]
    ->
        varset.coerce(VarSet, GenericVarSet),
        NotExistQArgTypesStr =
            mercury_vars_to_string(GenericVarSet, no, NotExistQArgTypes),
        Pieces = [words("Error:"),
            words(choose_number(NotExistQArgTypes,
                "type variable", "type variables")),
            words(NotExistQArgTypesStr),
            words("in class constraints,"),
            words(choose_number(NotExistQArgTypes,
                "which was", "which were")),
            words("introduced with"), quote("=>"),
            words("must be explicitly existentially quantified"),
            words("using"), quote("some"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(BodyTerm), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ;
        true
    ),
    process_du_ctors(Params, VarSet, BodyTerm, Ctors, !Specs).

%-----------------------------------------------------------------------------%

    % parse_eqv_type_defn parses the definition of an equivalence type.
    %
:- pred parse_eqv_type_defn(module_name::in, varset::in, term::in, term::in,
    decl_attrs::in, maybe1(processed_type_body)::out) is det.

parse_eqv_type_defn(ModuleName, VarSet, HeadTerm, BodyTerm, Attributes,
        MaybeProcessedTypeBody) :-
    parse_type_defn_head(ModuleName, VarSet, HeadTerm, MaybeNameParams),
    (
        MaybeNameParams = error2(Specs),
        MaybeProcessedTypeBody0 = error1(Specs)
    ;
        MaybeNameParams = ok2(Name, Params),
        % Check that all the variables in the body occur in the head.
        (
            term.contains_var(BodyTerm, Var),
            term.coerce_var(Var, TVar),
            not list.member(TVar, Params)
        ->
            BodyTermStr = describe_error_term(VarSet, BodyTerm),
            Pieces = [words("Error: free type parameter"),
                words("in RHS of type definition:"),
                words(BodyTermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(BodyTerm), [always(Pieces)])]),
            MaybeProcessedTypeBody0 = error1([Spec])
        ;
            % XXX Should pass more correct ContextPieces.
            ContextPieces = [],
            parse_type(BodyTerm, VarSet, ContextPieces, MaybeType),
            (
                MaybeType = ok1(Type),
                MaybeProcessedTypeBody0 = ok1(processed_type_body(Name, Params,
                    parse_tree_eqv_type(Type)))
            ;
                MaybeType = error1(Specs),
                MaybeProcessedTypeBody0 = error1(Specs)
            )
        )
    ),
    check_no_attributes(MaybeProcessedTypeBody0, Attributes,
        MaybeProcessedTypeBody).

%-----------------------------------------------------------------------------%

    % parse_solver_type_defn parses the definition of a solver type.
    %
:- pred parse_solver_type_defn(module_name::in, varset::in, term::in, term::in,
    decl_attrs::in, maybe1(processed_type_body)::out) is det.

parse_solver_type_defn(ModuleName, VarSet, HeadTerm, BodyTerm, Attributes0,
        MaybeProcessedTypeBody) :-
    get_is_solver_type(IsSolverType, Attributes0, Attributes),
    (
        IsSolverType = non_solver_type,
        Pieces = [words("Error: only solver types can be defined"),
            words("by a `where' block alone."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
        MaybeProcessedTypeBody = error1([Spec])
    ;
        IsSolverType = solver_type,
        MaybeWhere = parse_type_decl_where_term(solver_type, ModuleName,
            VarSet, yes(BodyTerm)),
        (
            MaybeWhere = error2(Specs),
            MaybeProcessedTypeBody = error1(Specs)
        ;
            MaybeWhere = ok2(MaybeSolverTypeDetails, MaybeUserEqComp),
            parse_solver_type_base(ModuleName, VarSet, HeadTerm,
                MaybeSolverTypeDetails, MaybeUserEqComp,
                MaybeProcessedTypeBody0),
            check_no_attributes(MaybeProcessedTypeBody0, Attributes,
                MaybeProcessedTypeBody)
        )
    ).

:- pred parse_solver_type_base(module_name::in, varset::in, term::in,
    maybe(solver_type_details)::in, maybe(unify_compare)::in,
    maybe1(processed_type_body)::out) is det.

parse_solver_type_base(ModuleName, VarSet, HeadTerm,
        MaybeSolverTypeDetails, MaybeUserEqComp, MaybeProcessedTypeBody) :-
    (
        MaybeSolverTypeDetails = yes(SolverTypeDetails),
        parse_type_defn_head(ModuleName, VarSet, HeadTerm, MaybeNameParams),
        (
            MaybeNameParams = error2(Specs),
            MaybeProcessedTypeBody = error1(Specs)
        ;
            MaybeNameParams = ok2(Name, Params),
            (
                RepnType = SolverTypeDetails ^ representation_type,
                type_contains_var(RepnType, Var),
                not list.member(Var, Params)
            ->
                HeadTermStr = describe_error_term(VarSet, HeadTerm),
                Pieces = [words("Error: free type variable"),
                    words("in representation type:"),
                    words(HeadTermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeProcessedTypeBody = error1([Spec])
            ;
                MaybeProcessedTypeBody = ok1(processed_type_body(Name, Params,
                    parse_tree_solver_type(SolverTypeDetails,
                        MaybeUserEqComp)))
            )
        )
    ;
        MaybeSolverTypeDetails = no,
        Pieces = [words("Solver type with no solver_type_details."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
        MaybeProcessedTypeBody = error1([Spec])
    ).

%-----------------------------------------------------------------------------%
%
% Parse an abstract type definition.
%

:- pred parse_abstract_type_defn(module_name::in, varset::in, term::in,
    decl_attrs::in, maybe1(processed_type_body)::out) is det.

parse_abstract_type_defn(ModuleName, VarSet, HeadTerm, Attributes0,
        MaybeProcessedTypeBody) :-
    parse_type_defn_head(ModuleName, VarSet, HeadTerm, MaybeTypeCtorAndArgs),
    get_is_solver_type(IsSolverType, Attributes0, Attributes),
    (
        MaybeTypeCtorAndArgs = error2(Specs),
        MaybeProcessedTypeBody0 = error1(Specs)
    ;
        MaybeTypeCtorAndArgs = ok2(Functor, Params),
        MaybeProcessedTypeBody0 = ok1(processed_type_body(Functor, Params,
            parse_tree_abstract_type(IsSolverType)))
    ),
    check_no_attributes(MaybeProcessedTypeBody0, Attributes,
        MaybeProcessedTypeBody).

%-----------------------------------------------------------------------------%
%
% Parse ... where ... clauses in type definitions. These clauses can specify
% type-specific unify and/or compare predicates for discriminated union types
% and solver type details for solver types.
%

    % The optional `where ...' part of the type definition syntax
    % is a comma separated list of special type `attributes'.
    %
    % The possible attributes (in this order) are either
    % - `type_is_abstract_noncanonical' on its own appears only in .int2
    %   files and indicates that the type has user-defined equality and/or
    %   comparison, but that what these predicates are is not known at
    %   this point
    % or
    % - `representation is <<type name>>' (required for solver types)
    % - `initialisation is <<pred name>>' (required for solver types)
    % - `ground is <<inst>>' (required for solver types)
    % - `any is <<inst>>' (required for solver types)
    % - `equality is <<pred name>>' (optional)
    % - `comparison is <<pred name>>' (optional).
    %
parse_type_decl_where_part_if_present(IsSolverType, ModuleName, VarSet,
        Term, BeforeWhereTerm, MaybeWhereDetails) :-
    (
        Term = term.functor(term.atom("where"),
            [BeforeWhereTermPrime, WhereTerm], _)
    ->
        BeforeWhereTerm = BeforeWhereTermPrime,
        MaybeWhereDetails = parse_type_decl_where_term(IsSolverType,
            ModuleName, VarSet, yes(WhereTerm))
    ;
        BeforeWhereTerm = Term,
        MaybeWhereDetails = ok2(no, no)
    ).

    % The maybe2 wrapper allows us to return an error code or a pair
    % of results. Either result half may be empty, hence the maybe
    % wrapper around each of those.
    %
:- func parse_type_decl_where_term(is_solver_type, module_name, varset,
    maybe(term)) = maybe2(maybe(solver_type_details), maybe(unify_compare)).

parse_type_decl_where_term(IsSolverType, ModuleName, VarSet, MaybeTerm0) =
        MaybeWhereDetails :-
    (
        MaybeTerm0 = no,
        MaybeWhereDetails = ok2(no, no)
    ;
        MaybeTerm0 = yes(Term0),
        some [!MaybeTerm] (
            !:MaybeTerm = MaybeTerm0,
            parse_where_attribute(parse_where_type_is_abstract_noncanonical,
                MaybeTypeIsAbstractNoncanonical, !MaybeTerm),
            parse_where_attribute(parse_where_is("representation",
                    parse_where_type_is(ModuleName, VarSet)),
                MaybeRepresentationIs, !MaybeTerm),
            parse_where_attribute(parse_where_initialisation_is(ModuleName,
                    VarSet),
                MaybeInitialisationIs, !MaybeTerm),
            parse_where_attribute(parse_where_is("ground",
                    parse_where_inst_is(ModuleName)),
                MaybeGroundIs, !MaybeTerm),
            parse_where_attribute(parse_where_is("any",
                    parse_where_inst_is(ModuleName)),
                MaybeAnyIs, !MaybeTerm),
            parse_where_attribute(parse_where_is("constraint_store",
                    parse_where_mutable_is(ModuleName)),
                MaybeCStoreIs, !MaybeTerm),
            parse_where_attribute(parse_where_is("equality",
                    parse_where_pred_is(ModuleName, VarSet)),
                MaybeEqualityIs, !MaybeTerm),
            parse_where_attribute(parse_where_is("comparison",
                    parse_where_pred_is(ModuleName, VarSet)),
                MaybeComparisonIs, !MaybeTerm),
            parse_where_end(!.MaybeTerm, MaybeWhereEnd)
        ),
        MaybeWhereDetails = make_maybe_where_details(
            IsSolverType,
            MaybeTypeIsAbstractNoncanonical,
            MaybeRepresentationIs,
            MaybeInitialisationIs,
            MaybeGroundIs,
            MaybeAnyIs,
            MaybeCStoreIs,
            MaybeEqualityIs,
            MaybeComparisonIs,
            MaybeWhereEnd,
            Term0
        )
    ).

    % parse_where_attribute(Parser, Result, MaybeTerm, MaybeTailTerm)
    % handles
    % - where MaybeTerm may contain nothing
    % - where MaybeTerm may be a comma-separated pair
    % - applies Parser to the appropriate (sub)term to obtain Result
    % - sets MaybeTailTerm depending upon whether the Result is an error
    % or not and whether there is more to parse because MaybeTerm
    % was a comma-separated pair.
    %
:- pred parse_where_attribute((func(term) = maybe1(maybe(T)))::in,
    maybe1(maybe(T))::out, maybe(term)::in, maybe(term)::out) is det.

parse_where_attribute(Parser, Result, MaybeTerm, MaybeTailTerm) :-
    (
        MaybeTerm = no,
        MaybeTailTerm = no,
        Result = ok1(no)
    ;
        MaybeTerm = yes(Term),
        (
            Term = term.functor(term.atom(","), [HeadTerm, TailTerm], _)
        ->
            Result = Parser(HeadTerm),
            MaybeTailTermIfYes = yes(TailTerm)
        ;
            Result = Parser(Term),
            MaybeTailTermIfYes = no
        ),
        (
            Result = error1(_),
            MaybeTailTerm = no
        ;
            Result = ok1(no),
            MaybeTailTerm = yes(Term)
        ;
            Result = ok1(yes(_)),
            MaybeTailTerm = MaybeTailTermIfYes
        )
    ).

    % Parser for `where ...' attributes of the form
    % `attributename is attributevalue'.
    %
:- func parse_where_is(string, func(term) = maybe1(T), term) =
    maybe1(maybe(T)).

parse_where_is(Name, Parser, Term) = Result :-
    ( Term = term.functor(term.atom("is"), [LHS, RHS], _) ->
        ( LHS = term.functor(term.atom(Name), [], _) ->
            RHSResult = Parser(RHS),
            (
                RHSResult = ok1(ParsedRHS),
                Result    = ok1(yes(ParsedRHS))
            ;
                RHSResult = error1(Specs),
                Result    = error1(Specs)
            )
        ;
            Result = ok1(no)
        )
    ;
        Pieces = [words("Error: expected"), quote("is"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        Result = error1([Spec])
    ).

:- func parse_where_type_is_abstract_noncanonical(term) = maybe1(maybe(unit)).

parse_where_type_is_abstract_noncanonical(Term) =
    ( Term = term.functor(term.atom("type_is_abstract_noncanonical"), [], _) ->
        ok1(yes(unit))
    ;
        ok1(no)
    ).

:- func parse_where_initialisation_is(module_name, varset, term) =
    maybe1(maybe(sym_name)).

parse_where_initialisation_is(ModuleName, VarSet, Term) = Result :-
    Result0 = parse_where_is("initialisation",
        parse_where_pred_is(ModuleName, VarSet), Term),
    (
        Result0 = ok1(no)
    ->
        Result1 = parse_where_is("initialization",
            parse_where_pred_is(ModuleName, VarSet), Term)
    ;
        Result1 = Result0
    ),
    promise_pure (
        (
            Result1 = ok1(yes(_)),
            semipure
                semipure_get_solver_auto_init_supported(AutoInitSupported),
            (
                AutoInitSupported = yes,
                Result = Result1
            ;
                AutoInitSupported = no,
                Pieces = [words("Error: unknown attribute"),
                    words("in solver type definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                Result = error1([Spec])
            )
        ;
            ( Result1 = ok1(no)
            ; Result1 = error1(_)
            ),
            Result = Result1
        )
    ).

:- func parse_where_pred_is(module_name, varset, term) = maybe1(sym_name).

parse_where_pred_is(ModuleName, VarSet, Term) = Result :-
    parse_implicitly_qualified_symbol_name(ModuleName, VarSet, Term, Result).

:- func parse_where_inst_is(module_name, term) = maybe1(mer_inst).

parse_where_inst_is(_ModuleName, Term) = Result :-
    (
        convert_inst(no_allow_constrained_inst_var, Term, Inst),
        not inst_contains_unconstrained_var(Inst)
    ->
        Result = ok1(Inst)
    ;
        Pieces = [words("Error: expected a ground, unconstrained inst."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        Result = error1([Spec])
    ).

:- func parse_where_type_is(module_name, varset, term) = maybe1(mer_type).

parse_where_type_is(_ModuleName, VarSet, Term) = Result :-
    % XXX We should pass meaningful ContextPieces.
    ContextPieces = [],
    parse_type(Term, VarSet, ContextPieces, Result).

:- func parse_where_mutable_is(module_name, term) = maybe1(list(item)).

parse_where_mutable_is(ModuleName, Term) = MaybeItems :-
    ( Term = term.functor(term.atom("mutable"), _, _) ->
        parse_mutable_decl_term(ModuleName, Term, MaybeItem),
        (
            MaybeItem = ok1(Mutable),
            MaybeItems  = ok1([Mutable])
        ;
            MaybeItem = error1(Specs),
            MaybeItems  = error1(Specs)
        )
    ; list_term_to_term_list(Term, Terms) ->
        map_parser(parse_mutable_decl_term(ModuleName), Terms, MaybeItems)
    ;
        Pieces = [words("Error: expected a mutable declaration"),
            words("or a list of mutable declarations."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeItems = error1([Spec])
    ).

:- pred parse_mutable_decl_term(module_name::in, term::in, maybe1(item)::out)
    is det.

parse_mutable_decl_term(ModuleName, Term, MaybeItem) :-
    (
        Term = term.functor(term.atom("mutable"), Args, Context),
        varset.init(VarSet),
        parse_mutable_decl(ModuleName, VarSet, Args, Context, MaybeItemPrime)
    ->
        MaybeItem = MaybeItemPrime
    ;
        Pieces = [words("Error: expected a mutable declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_where_end(maybe(term)::in, maybe1(maybe(unit))::out) is det.

parse_where_end(no, ok1(yes(unit))).
parse_where_end(yes(Term), error1([Spec])) :-
    Pieces = [words("Error: attributes are either badly ordered"),
        words("or contain an unrecognised attribute."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(get_term_context(Term), [always(Pieces)])]).

:- func make_maybe_where_details(is_solver_type, maybe1(maybe(unit)),
    maybe1(maybe(mer_type)), maybe1(maybe(init_pred)),
    maybe1(maybe(mer_inst)), maybe1(maybe(mer_inst)),
    maybe1(maybe(list(item))),
    maybe1(maybe(equality_pred)), maybe1(maybe(comparison_pred)),
    maybe1(maybe(unit)), term)
    = maybe2(maybe(solver_type_details), maybe(unify_compare)).

make_maybe_where_details(IsSolverType, MaybeTypeIsAbstractNoncanonical,
        MaybeRepresentationIs, MaybeInitialisationIs,
        MaybeGroundIs, MaybeAnyIs, MaybeCStoreIs,
        MaybeEqualityIs, MaybeComparisonIs, MaybeWhereEnd, WhereTerm)
        = MaybeSolverUC :-
    (
        MaybeTypeIsAbstractNoncanonical = ok1(TypeIsAbstractNoncanonical),
        MaybeRepresentationIs = ok1(RepresentationIs),
        MaybeInitialisationIs = ok1(InitialisationIs),
        MaybeGroundIs = ok1(GroundIs),
        MaybeAnyIs = ok1(AnyIs),
        MaybeCStoreIs = ok1(CStoreIs),
        MaybeEqualityIs = ok1(EqualityIs),
        MaybeComparisonIs = ok1(ComparisonIs),
        MaybeWhereEnd = ok1(WhereEnd)
    ->
        MaybeSolverUC = make_maybe_where_details_2(IsSolverType,
            TypeIsAbstractNoncanonical, RepresentationIs, InitialisationIs,
            GroundIs, AnyIs, CStoreIs, EqualityIs, ComparisonIs,
            WhereEnd, WhereTerm)
    ;
        Specs =
            get_any_errors1(MaybeTypeIsAbstractNoncanonical) ++
            get_any_errors1(MaybeRepresentationIs) ++
            get_any_errors1(MaybeInitialisationIs) ++
            get_any_errors1(MaybeGroundIs) ++
            get_any_errors1(MaybeAnyIs) ++
            get_any_errors1(MaybeCStoreIs) ++
            get_any_errors1(MaybeEqualityIs) ++
            get_any_errors1(MaybeComparisonIs) ++
            get_any_errors1(MaybeWhereEnd),
        MaybeSolverUC = error2(Specs)
    ).

:- func make_maybe_where_details_2(is_solver_type, maybe(unit),
    maybe(mer_type), maybe(init_pred), maybe(mer_inst), maybe(mer_inst),
    maybe(list(item)), maybe(equality_pred), maybe(comparison_pred),
    maybe(unit), term)
    = maybe2(maybe(solver_type_details), maybe(unify_compare)).

make_maybe_where_details_2(IsSolverType, TypeIsAbstractNoncanonical,
        RepresentationIs, InitialisationIs, GroundIs, AnyIs, CStoreIs,
        EqualityIs, ComparisonIs, _WhereEnd, WhereTerm) = MaybeSolverUC :-
    (
        TypeIsAbstractNoncanonical = yes(_),
        % rafe: XXX I think this is wrong. There isn't a problem with having
        % the solver_type_details and type_is_abstract_noncanonical.
        (
            RepresentationIs = maybe.no,
            InitialisationIs = maybe.no,
            GroundIs         = maybe.no,
            AnyIs            = maybe.no,
            EqualityIs       = maybe.no,
            ComparisonIs     = maybe.no,
            CStoreIs         = maybe.no
        ->
            MaybeSolverUC =
                ok2(no, yes(abstract_noncanonical_type(IsSolverType)))
        ;
            Pieces = [words("Error:"),
                quote("where type_is_abstract_noncanonical"),
                words("excludes other"), quote("where ..."),
                words("attributes."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(WhereTerm), [always(Pieces)])]),
            MaybeSolverUC = error2([Spec])
        )
    ;
        TypeIsAbstractNoncanonical = maybe.no,
        (
            IsSolverType = solver_type,
            (
                RepresentationIs = yes(RepnType),
                InitialisationIs = MaybeInitialisation,
                GroundIs         = MaybeGroundInst,
                AnyIs            = MaybeAnyInst,
                EqualityIs       = MaybeEqPred,
                ComparisonIs     = MaybeCmpPred,
                CStoreIs         = MaybeMutableItems
            ->
                (
                    MaybeGroundInst = yes(GroundInst)
                ;
                    MaybeGroundInst = no,
                    GroundInst = ground_inst
                ),
                (
                    MaybeAnyInst = yes(AnyInst)
                ;
                    MaybeAnyInst = no,
                    AnyInst = ground_inst
                ),
                (
                    MaybeMutableItems = yes(MutableItems)
                ;
                    MaybeMutableItems = no,
                    MutableItems = []
                ),
                (
                    MaybeInitialisation = yes(InitPred),
                    HowToInit = solver_init_automatic(InitPred)
                ;
                    MaybeInitialisation = no,
                    HowToInit = solver_init_explicit
                ),
                SolverTypeDetails = solver_type_details(
                    RepnType, HowToInit, GroundInst, AnyInst, MutableItems),
                MaybeSolverTypeDetails = yes(SolverTypeDetails),
                (
                    MaybeEqPred = no,
                    MaybeCmpPred = no
                ->
                    MaybeUnifyCompare = no
                ;
                    MaybeUnifyCompare = yes(unify_compare(
                        MaybeEqPred, MaybeCmpPred))
                ),
                MaybeSolverUC = ok2(MaybeSolverTypeDetails, MaybeUnifyCompare)
            ;
                RepresentationIs = no
            ->
                Pieces = [words("Error: solver type definitions must have a"),
                    quote("representation"), words("attribute."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(WhereTerm),
                        [always(Pieces)])]),
                MaybeSolverUC = error2([Spec])
            ;
               unexpected(this_file, "make_maybe_where_details_2: " ++
                    "shouldn't have reached this point! (1)")
            )
        ;
            IsSolverType = non_solver_type,
            (
                ( RepresentationIs = yes(_)
                ; InitialisationIs = yes(_)
                ; GroundIs         = yes(_)
                ; AnyIs            = yes(_)
                ; CStoreIs         = yes(_)
                )
            ->
                Pieces = [words("Error: solver type attribute given"),
                    words("for non-solver type."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(WhereTerm),
                        [always(Pieces)])]),
                MaybeSolverUC = error2([Spec])
            ;
                EqualityIs = MaybeEqPred,
                ComparisonIs = MaybeCmpPred,
                MaybeSolverUC =
                    ok2(no, yes(unify_compare(MaybeEqPred, MaybeCmpPred)))
            )
        )
    ).

%-----------------------------------------------------------------------------%
%
% Predicates useful for parsing several kinds of type definitions.
%

parse_type_defn_head(ModuleName, VarSet, HeadTerm, MaybeTypeCtorAndArgs) :-
    (
        HeadTerm = term.variable(_, Context),
        Pieces = [words("Error: variable on LHS of type definition."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        MaybeTypeCtorAndArgs = error2([Spec])
    ;
        HeadTerm = term.functor(_, _, HeadContext),
        ContextPieces = [words("In type definition:")],
        parse_implicitly_qualified_term(ModuleName, HeadTerm, HeadTerm,
            VarSet, ContextPieces, HeadResult),
        (
            HeadResult = error2(Specs),
            MaybeTypeCtorAndArgs = error2(Specs)
        ;
            HeadResult = ok2(Name, ArgTerms),
            % Check that all the head args are variables.
            ( term_list_to_var_list(ArgTerms, Params0) ->
                % Check that all the head arg variables are distinct.
                (
                    list.member(_, Params0, [Param | OtherParams]),
                    list.member(Param, OtherParams)
                ->
                    Pieces = [words("Error: repeated type parameters"),
                        words("in LHS of type definition."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(HeadContext, [always(Pieces)])]),
                    MaybeTypeCtorAndArgs = error2([Spec])
                ;
                    list.map(term.coerce_var, Params0, Params),
                    MaybeTypeCtorAndArgs = ok2(Name, Params)
                )
            ;
                HeadTermStr = describe_error_term(VarSet, HeadTerm),
                Pieces = [words("Error: type parameters must be variables:"),
                    words(HeadTermStr), suffix(".") ,nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(HeadContext, [always(Pieces)])]),
                MaybeTypeCtorAndArgs = error2([Spec])
            )
        )
    ).

%-----------------------------------------------------------------------------%
%
% Parsing inst definitions.
%

:- type processed_inst_body
    --->    processed_inst_body(
                sym_name,
                list(inst_var),
                inst_defn
            ).

    % Parse a `:- inst <InstDefn>.' declaration.
    %
:- pred parse_inst_defn(module_name::in, varset::in, term::in,
    prog_context::in, maybe1(item)::out) is det.

parse_inst_defn(ModuleName, VarSet, Term, Context, MaybeItem) :-
    % XXX Some of the tests here could be factored out.
    (
        Term = term.functor(term.atom("=="), [HeadTerm, BodyTerm], _)
    ->
        parse_condition_suffix(BodyTerm, BeforeCondTerm, Condition),
        parse_inst_defn_base(ModuleName, VarSet, HeadTerm, BeforeCondTerm,
            MaybeProcessedInstBody),
        process_maybe1(make_inst_defn(VarSet, Condition, Context),
            MaybeProcessedInstBody, MaybeItem)
    ;
        % XXX This is for `abstract inst' declarations,
        % which are not really supported.
        Term = term.functor(term.atom("is"), Args, _),
        Args = [HeadTerm, term.functor(term.atom("private"), [], _)]
    ->
        Condition = cond_true,
        parse_abstract_inst_defn(ModuleName, VarSet, HeadTerm,
            MaybeProcessedInstBody),
        process_maybe1(make_inst_defn(VarSet, Condition, Context),
            MaybeProcessedInstBody, MaybeItem)
    ;
        Term = term.functor(term.atom("--->"), [HeadTerm, BodyTerm], _)
    ->
        parse_condition_suffix(BodyTerm, BeforeCondTerm, Condition),
        BoundBeforeCondTerm =
            term.functor(term.atom("bound"), [BeforeCondTerm], Context),
        parse_inst_defn_base(ModuleName, VarSet, HeadTerm, BoundBeforeCondTerm,
            MaybeProcessedInstBody),
        % We should check the condition for errors. We don't bother
        % at the moment, since we ignore conditions anyhow :-)
        process_maybe1(make_inst_defn(VarSet, Condition, Context),
            MaybeProcessedInstBody, MaybeItem)
    ;
        Pieces = [words("Error:"), quote("=="), words("expected in"),
            quote(":- inst"), words("definition."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_inst_defn_base(module_name::in, varset::in, term::in, term::in,
    maybe1(processed_inst_body)::out) is det.

parse_inst_defn_base(ModuleName, VarSet, HeadTerm, BodyTerm,
        MaybeProcessedInstBody) :-
    ContextPieces = [words("In inst definition:")],
    parse_implicitly_qualified_term(ModuleName, HeadTerm, BodyTerm,
        VarSet, ContextPieces, MaybeNameAndArgs),
    (
        MaybeNameAndArgs = error2(Specs),
        MaybeProcessedInstBody = error1(Specs)
    ;
        MaybeNameAndArgs = ok2(Name, ArgTerms),
        (
            % Check that all the head args are variables.
            term.term_list_to_var_list(ArgTerms, Args)
        ->
            (
                % Check that all the head arg variables are distinct.
                list.member(Arg2, Args, [Arg2 | OtherArgs]),
                list.member(Arg2, OtherArgs)
            ->
                % XXX Should improve the error message here.
                Pieces = [words("Error: repeated inst parameters"),
                    words("in LHS of inst definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeProcessedInstBody = error1([Spec])
            ;
                % Check that all the variables in the body occur in the head.
                term.contains_var(BodyTerm, Var2),
                \+ list.member(Var2, Args)
            ->
                Pieces = [words("Error: free inst parameter"),
                    words("in RHS of inst definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(BodyTerm),
                        [always(Pieces)])]),
                MaybeProcessedInstBody = error1([Spec])
            ;
                % Check that the inst is a valid user-defined inst, i.e. that it
                % does not have the form of one of the builtin insts.
                \+ (
                    convert_inst(no_allow_constrained_inst_var, HeadTerm,
                        UserInst),
                    UserInst = defined_inst(user_inst(_, _))
                )
            ->
                % XXX Name the builtin inst.
                Pieces =
                    [words("Error: attempt to redefine builtin inst."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeProcessedInstBody = error1([Spec])
            ;
                % Should improve the error message here.
                (
                    convert_inst(no_allow_constrained_inst_var, BodyTerm,
                        ConvertedBody)
                ->
                    list.map(term.coerce_var, Args, InstArgs),
                    MaybeProcessedInstBody = ok1(processed_inst_body(Name,
                        InstArgs, eqv_inst(ConvertedBody)))
                ;
                    BodyTermStr = describe_error_term(VarSet, BodyTerm),
                    Pieces = [words("Error: syntax error in inst body at"),
                        words(BodyTermStr), suffix("."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(BodyTerm),
                            [always(Pieces)])]),
                    MaybeProcessedInstBody = error1([Spec])
                )
            )
        ;
            % XXX If term_list_to_var_list returned the non-var's term
            % or context, we could use it here.
            Pieces = [words("Error: inst parameters must be variables."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
            MaybeProcessedInstBody = error1([Spec])
        )
    ).

:- pred parse_abstract_inst_defn(module_name::in, varset::in, term::in,
    maybe1(processed_inst_body)::out) is det.

parse_abstract_inst_defn(ModuleName, VarSet, HeadTerm,
        MaybeProcessedInstBody) :-
    ContextPieces = [words("In inst definition:")],
    parse_implicitly_qualified_term(ModuleName, HeadTerm, HeadTerm,
        VarSet, ContextPieces, MaybeNameAndArgs),
    (
        MaybeNameAndArgs = error2(Specs),
        MaybeProcessedInstBody = error1(Specs)
    ;
        MaybeNameAndArgs = ok2(Name, ArgTerms),
        (
            % Check that all the head args are variables.
            term.term_list_to_var_list(ArgTerms, Args)
        ->
            (
                % Check that all the head arg variables are distinct.
                list.member(Arg2, Args, [Arg2 | OtherArgs]),
                list.member(Arg2, OtherArgs)
            ->
                % XXX We should we list the repeated parameters.
                Pieces = [words("Error: repeated inst parameters"),
                    words("in abstract inst definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeProcessedInstBody = error1([Spec])
            ;
                list.map(term.coerce_var, Args, InstArgs),
                MaybeProcessedInstBody = ok1(processed_inst_body(Name,
                    InstArgs, abstract_inst))
            )
        ;
            % XXX If term_list_to_var_list returned the non-var's term
            % or context, we could use it here.
            Pieces = [words("Error: inst parameters must be variables."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
            MaybeProcessedInstBody = error1([Spec])
        )
    ).

%-----------------------------------------------------------------------------%
%
% Parsing mode definitions.
%

:- type processed_mode_body
    --->    processed_mode_body(
                sym_name,
                list(inst_var),
                mode_defn
            ).

:- pred parse_mode_defn(module_name::in, varset::in, term::in, term::in,
    maybe1(processed_mode_body)::out) is det.

parse_mode_defn(ModuleName, VarSet, HeadTerm, BodyTerm,
        MaybeProcessedModeBody) :-
    ContextPieces = [words("In mode definition:")],
    parse_implicitly_qualified_term(ModuleName, HeadTerm, HeadTerm,
        VarSet, ContextPieces, MaybeModeNameAndArgs),
    (
        MaybeModeNameAndArgs = error2(Specs),
        MaybeProcessedModeBody = error1(Specs)
    ;
        MaybeModeNameAndArgs = ok2(Name, ArgTerms),
        % Check that all the head args are variables.
        ( term.term_list_to_var_list(ArgTerms, Args) ->
            (
                % Check that all the head arg variables are distinct.
                list.member(Arg2, Args, [Arg2 | OtherArgs]),
                list.member(Arg2, OtherArgs)
            ->
                % Check that all the head arg variables are distinct.
                % XXX We should list the duplicated head arg variables.
                Pieces = [words("Error: repeated parameters"),
                    words("in LHS of mode definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeProcessedModeBody = error1([Spec])
            ;
                % Check that all the variables in the body occur in the head.
                term.contains_var(BodyTerm, Var2),
                \+ list.member(Var2, Args)
            ->
                % XXX Shouldn't we be using the BodyTerm's context?
                % XXX We should list the Var2s for which the condition holds.
                Pieces = [words("Error: free inst parameter"),
                    words("in RHS of mode definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeProcessedModeBody = error1([Spec])
            ;
                (
                    convert_mode(no_allow_constrained_inst_var, BodyTerm, Mode)
                ->
                    list.map(term.coerce_var, Args, InstArgs),
                    ProcessedModeBody = processed_mode_body(Name, InstArgs,
                        eqv_mode(Mode)),
                    MaybeProcessedModeBody = ok1(ProcessedModeBody)
                ;
                    % XXX We should improve the error message here.
                    Pieces = [words("Error: syntax error"),
                        words("in mode definition body."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(BodyTerm),
                            [always(Pieces)])]),
                    MaybeProcessedModeBody = error1([Spec])
                )
            )
        ;
            % XXX If term_list_to_var_list returned the non-var's term
            % or context, we could use it here.
            Pieces = [words("Error: mode parameters must be variables."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
            MaybeProcessedModeBody = error1([Spec])
        )
    ).

%-----------------------------------------------------------------------------%
%
% Parsing ":- pred" and ":- func" declarations.
%

    % parse_pred_or_func_decl parses a predicate or function declaration.
    %
:- pred parse_pred_or_func_decl(pred_or_func::in, module_name::in, varset::in,
    term::in, decl_attrs::in, prog_context::in, maybe1(item)::out) is det.

parse_pred_or_func_decl(PredOrFunc, ModuleName, VarSet, Term, Attributes,
        Context, MaybeItem) :-
    parse_condition_suffix(Term, BeforeCondTerm, Condition),
    parse_determinism_suffix(VarSet, BeforeCondTerm, BeforeDetismTerm,
        MaybeMaybeDetism),
    parse_with_inst_suffix(BeforeDetismTerm, BeforeWithInstTerm,
        MaybeWithInst),
    parse_with_type_suffix(VarSet, BeforeWithInstTerm, BeforeWithTypeTerm,
        MaybeWithType),
    BaseTerm = BeforeWithTypeTerm,
    (
        MaybeMaybeDetism = ok1(MaybeDetism),
        MaybeWithInst = ok1(WithInst),
        MaybeWithType = ok1(WithType)
    ->
        (
            MaybeDetism = yes(_),
            WithInst = yes(_)
        ->
            Pieces = [words("Error:"), quote("with_inst"),
                words("and determinism both specified."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(BaseTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        ;
            WithInst = yes(_),
            WithType = no
        ->
            Pieces = [words("Error:"), quote("with_inst"), words("specified"),
                words("without"), quote("with_type"), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(BaseTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        ;
            (
                % Function declarations with `with_type` annotations
                % have the same form as predicate declarations.
                PredOrFunc = pf_function,
                WithType = no
            ->
                parse_func_decl_base(ModuleName, VarSet, BaseTerm, Condition,
                    MaybeDetism, Attributes, Context, MaybeItem)
            ;
                parse_pred_decl_base(PredOrFunc, ModuleName, VarSet, BaseTerm,
                    Condition, WithType, WithInst, MaybeDetism,
                    Attributes, Context, MaybeItem)
            )
        )
    ;
        Specs = get_any_errors1(MaybeMaybeDetism)
            ++ get_any_errors1(MaybeWithInst)
            ++ get_any_errors1(MaybeWithType),
        MaybeItem = error1(Specs)
    ).

    % parse a `:- pred p(...)' declaration or a
    % `:- func f(...) `with_type` t' declaration
    %
:- pred parse_pred_decl_base(pred_or_func::in, module_name::in, varset::in,
    term::in, condition::in, maybe(mer_type)::in, maybe(mer_inst)::in,
    maybe(determinism)::in, decl_attrs::in, prog_context::in,
    maybe1(item)::out) is det.

parse_pred_decl_base(PredOrFunc, ModuleName, VarSet, PredTypeTerm, Condition,
        WithType, WithInst, MaybeDet, Attributes0, Context, MaybeItem) :-
    get_class_context_and_inst_constraints(ModuleName, VarSet,
        Attributes0, Attributes1, MaybeExistClassInstContext),
    (
        MaybeExistClassInstContext = error3(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeExistClassInstContext =
            ok3(ExistQVars, Constraints, InstConstraints),
        ContextPieces = [words("In")] ++ pred_or_func_decl_pieces(PredOrFunc)
            ++ [suffix(":")],
        parse_implicitly_qualified_term(ModuleName, PredTypeTerm, PredTypeTerm,
            VarSet, ContextPieces, MaybePredNameAndArgs),
        (
            MaybePredNameAndArgs = error2(Specs),
            MaybeItem = error1(Specs)
        ;
            MaybePredNameAndArgs = ok2(Functor, ArgTerms),
            ( parse_type_and_mode_list(InstConstraints, ArgTerms, Args) ->
                ( verify_type_and_mode_list(Args) ->
                    (
                        WithInst = yes(_),
                        Args = [type_only(_) | _]
                    ->
                        Pieces = [words("Error:"), quote("with_inst"),
                            words("specified without argument modes."), nl],
                        Spec = error_spec(severity_error,
                            phase_term_to_parse_tree,
                            [simple_msg(get_term_context(PredTypeTerm),
                                [always(Pieces)])]),
                        MaybeItem = error1([Spec])
                    ;
                        WithInst = no,
                        WithType = yes(_),
                        Args = [type_and_mode(_, _) | _]
                    ->
                        Pieces = [words("Error: arguments have modes but"),
                            quote("with_inst"), words("not specified."), nl],
                        Spec = error_spec(severity_error,
                            phase_term_to_parse_tree,
                            [simple_msg(get_term_context(PredTypeTerm),
                                [always(Pieces)])]),
                        MaybeItem = error1([Spec])
                    ;
                        inst_var_constraints_types_modes_self_consistent(Args)
                    ->
                        get_purity(Purity, Attributes1, Attributes),
                        varset.coerce(VarSet, TVarSet),
                        varset.coerce(VarSet, IVarSet),
                        Origin = user,
                        ItemPredDecl = item_pred_decl_info(Origin,
                            TVarSet, IVarSet, ExistQVars, PredOrFunc,
                            Functor, Args, WithType, WithInst,
                            MaybeDet, Condition, Purity, Constraints, Context),
                        Item = item_pred_decl(ItemPredDecl),
                        MaybeItem0 = ok1(Item),
                        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
                    ;
                        PredTypeTermStr =
                            describe_error_term(VarSet, PredTypeTerm),
                        Pieces = [words("Error: inconsistent constraints on"),
                            words("inst variables in")] ++
                            pred_or_func_decl_pieces(PredOrFunc) ++
                            [suffix(":"), nl,
                            words(PredTypeTermStr), suffix("."), nl],
                        Spec = error_spec(severity_error,
                            phase_term_to_parse_tree,
                            [simple_msg(get_term_context(PredTypeTerm),
                                [always(Pieces)])]),
                        MaybeItem = error1([Spec])
                    )
                ;
                    Pieces = [words("Error: some but not all arguments"),
                        words("have modes."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(PredTypeTerm),
                            [always(Pieces)])]),
                    MaybeItem = error1([Spec])
                )
            ;
                PredTypeTermStr = describe_error_term(VarSet, PredTypeTerm),
                Pieces = [words("Error: syntax error in")] ++
                    pred_or_func_decl_pieces(PredOrFunc) ++
                    [words("at"), words(PredTypeTermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(PredTypeTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            )
        )
    ).

    % Parse a `:- func p(...)' declaration *without* a with_type clause.
    %
:- pred parse_func_decl_base(module_name::in, varset::in, term::in,
    condition::in, maybe(determinism)::in, decl_attrs::in, prog_context::in,
    maybe1(item)::out) is det.

parse_func_decl_base(ModuleName, VarSet, Term, Condition, MaybeDet,
        Attributes0, Context, MaybeItem) :-
    get_class_context_and_inst_constraints(ModuleName, VarSet,
        Attributes0, Attributes, MaybeContext),
    (
        MaybeContext = error3(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeContext = ok3(ExistQVars, Constraints, InstConstraints),
        (
            Term = term.functor(term.atom("="),
                [MaybeSugaredFuncTerm, ReturnTerm], _)
        ->
            FuncTerm = desugar_field_access(MaybeSugaredFuncTerm),
            ContextPieces = [words("In"), quote(":- func"),
                words("declaration")],
            parse_implicitly_qualified_term(ModuleName, FuncTerm, Term,
                VarSet, ContextPieces, MaybeFuncNameAndArgs),
            (
                MaybeFuncNameAndArgs = error2(Specs),
                MaybeItem = error1(Specs)
            ;
                MaybeFuncNameAndArgs = ok2(FuncName, ArgTerms),
                (
                    parse_type_and_mode_list(InstConstraints, ArgTerms,
                        ArgsPrime)
                ->
                    MaybeArgs = ok1(ArgsPrime)
                ;
                    FuncTermStr = describe_error_term(VarSet, FuncTerm),
                    ArgsPieces = [words("Error: syntax error in arguments of"),
                        quote(":- func"), words("declaration at"),
                        words(FuncTermStr), suffix("."), nl],
                    ArgsSpec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(FuncTerm),
                            [always(ArgsPieces)])]),
                    MaybeArgs = error1([ArgsSpec])
                ),
                (
                    parse_type_and_mode(InstConstraints, ReturnTerm,
                        ReturnArgPrime)
                ->
                    MaybeReturnArg = ok1(ReturnArgPrime)
                ;
                    ReturnPieces = [words("Error: syntax error"),
                        words("in return type of"), quote(":- func"),
                        words("declaration."), nl],
                    ReturnSpec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(ReturnTerm),
                            [always(ReturnPieces)])]),
                    MaybeReturnArg = error1([ReturnSpec])
                ),
                (
                    MaybeArgs = ok1(Args),
                    MaybeReturnArg = ok1(ReturnArg)
                ->
                    % We use an auxiliary predicate because the code is just
                    % too deeply indented here.
                    parse_func_decl_base_2(FuncName, Args, ReturnArg,
                        FuncTerm, Term, VarSet, MaybeDet, Condition,
                        ExistQVars, Constraints, Attributes,
                        Context, MaybeItem)
                ;
                    Specs = get_any_errors1(MaybeArgs) ++
                        get_any_errors1(MaybeReturnArg),
                    MaybeItem = error1(Specs)
                )
            )
        ;
            Pieces = [words("Error:"), quote("="), words("expected in"),
                quote(":- func"), words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ).

:- pred parse_func_decl_base_2(sym_name::in, list(type_and_mode)::in,
    type_and_mode::in, term::in, term::in, varset::in, maybe(determinism)::in,
    condition::in, existq_tvars::in, prog_constraints::in, decl_attrs::in,
    prog_context::in, maybe1(item)::out) is det.

parse_func_decl_base_2(FuncName, Args, ReturnArg, FuncTerm, Term,
        VarSet, MaybeDet, Condition, ExistQVars, Constraints, Attributes0,
        Context, MaybeItem) :-
    (
        verify_type_and_mode_list(Args)
    ->
        ConsistentArgsSpecs = []
    ;
        ConsistentPieces =
            [words("Error: some but not all arguments have modes."), nl],
        ConsistentSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(FuncTerm),
                [always(ConsistentPieces)])]),
        ConsistentArgsSpecs = [ConsistentSpec]
    ),
    (
        Args = [type_and_mode(_, _) | _],
        ReturnArg = type_only(_)
    ->
        ArgsOnlyPieces = [words("Error: function arguments have modes,"),
            words("but function result does not."), nl],
        ArgsOnlySpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(FuncTerm),
                [always(ArgsOnlyPieces)])]),
        ArgsOnlySpecs = [ArgsOnlySpec]
    ;
        ArgsOnlySpecs = []
    ),
    (
        Args = [type_only(_) | _],
        ReturnArg = type_and_mode(_, _)
    ->
        ReturnOnlyPieces = [words("Error: function result has mode,"),
            words("but function arguments do not."), nl],
        ReturnOnlySpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(FuncTerm),
                [always(ReturnOnlyPieces)])]),
        ReturnOnlySpecs = [ReturnOnlySpec]
    ;
        ReturnOnlySpecs = []
    ),
    ConsistencySpecs = ConsistentArgsSpecs ++ ArgsOnlySpecs ++ ReturnOnlySpecs,
    (
        ConsistencySpecs = [_ | _],
        MaybeItem = error1(ConsistencySpecs)
    ;
        ConsistencySpecs = [],
        get_purity(Purity, Attributes0, Attributes),
        varset.coerce(VarSet, TVarSet),
        varset.coerce(VarSet, IVarSet),
        AllArgs = Args ++ [ReturnArg],
        ( inst_var_constraints_types_modes_self_consistent(AllArgs) ->
            Origin = user,
            ItemPredDecl = item_pred_decl_info(Origin, TVarSet, IVarSet,
                ExistQVars, pf_function, FuncName, AllArgs, no, no,
                MaybeDet, Condition, Purity, Constraints, Context),
            Item = item_pred_decl(ItemPredDecl),
            MaybeItem0 = ok1(Item),
            check_no_attributes(MaybeItem0, Attributes, MaybeItem)
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: inconsistent constraints"),
                words("on inst variables in function declaration:"), nl,
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ).

:- pred parse_type_and_mode_list(inst_var_sub::in, list(term)::in,
    list(type_and_mode)::out) is semidet.

parse_type_and_mode_list(_, [], []).
parse_type_and_mode_list(InstConstraints, [H0 | T0], [H | T]) :-
    parse_type_and_mode(InstConstraints, H0, H),
    parse_type_and_mode_list(InstConstraints, T0, T).

:- pred parse_type_and_mode(inst_var_sub::in, term::in, type_and_mode::out)
    is semidet.

parse_type_and_mode(InstConstraints, Term, MaybeTypeAndMode) :-
    ( Term = term.functor(term.atom("::"), [TypeTerm, ModeTerm], _Context) ->
        maybe_parse_type(TypeTerm, Type),
        convert_mode(allow_constrained_inst_var, ModeTerm, Mode0),
        constrain_inst_vars_in_mode(InstConstraints, Mode0, Mode),
        MaybeTypeAndMode = type_and_mode(Type, Mode)
    ;
        maybe_parse_type(Term, Type),
        MaybeTypeAndMode = type_only(Type)
    ).

    % Verify that among the arguments of a :- pred or :- func declaration,
    % either all arguments specify a mode or none of them do.
    %
:- pred verify_type_and_mode_list(list(type_and_mode)::in) is semidet.

verify_type_and_mode_list([]).
verify_type_and_mode_list([First | Rest]) :-
    verify_type_and_mode_list_2(Rest, First).

:- pred verify_type_and_mode_list_2(list(type_and_mode)::in, type_and_mode::in)
    is semidet.

verify_type_and_mode_list_2([], _).
verify_type_and_mode_list_2([Head | Tail], First) :-
    (
        Head = type_only(_),
        First = type_only(_)
    ;
        Head = type_and_mode(_, _),
        First = type_and_mode(_, _)
    ),
    verify_type_and_mode_list_2(Tail, First).

%-----------------------------------------------------------------------------%
%
% Parsing mode declarations for predicates and functions.
%

:- pred parse_mode_decl(module_name::in, varset::in, term::in,
    decl_attrs::in, prog_context::in, maybe1(item)::out) is det.

parse_mode_decl(ModuleName, VarSet, Term, Attributes, Context, MaybeItem) :-
    parse_condition_suffix(Term, BeforeCondTerm, Condition),
    parse_determinism_suffix(VarSet, BeforeCondTerm, BeforeDetismTerm,
        MaybeMaybeDetism),
    parse_with_inst_suffix(BeforeDetismTerm, BeforeWithInstTerm,
        MaybeWithInst),
    BaseTerm = BeforeWithInstTerm,
    (
        MaybeMaybeDetism = ok1(MaybeDetism),
        MaybeWithInst = ok1(WithInst)
    ->
        (
            MaybeDetism = yes(_),
            WithInst = yes(_)
        ->
            Pieces = [words("Error:"), quote("with_inst"),
                words("and determinism both specified."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(BeforeCondTerm),
                    [always(Pieces)])]),
            MaybeItem = error1([Spec])
        ;
            parse_mode_decl_base(ModuleName, VarSet, BaseTerm, Condition,
                Attributes, WithInst, MaybeDetism, Context, MaybeItem)
        )
    ;
        Specs = get_any_errors1(MaybeMaybeDetism)
            ++ get_any_errors1(MaybeWithInst),
        MaybeItem = error1(Specs)
    ).

:- pred parse_mode_decl_base(module_name::in, varset::in, term::in,
    condition::in, decl_attrs::in, maybe(mer_inst)::in, maybe(determinism)::in,
    prog_context::in, maybe1(item)::out) is det.

parse_mode_decl_base(ModuleName, VarSet, Term, Condition, Attributes, WithInst,
        MaybeDet, Context, MaybeItem) :-
    (
        WithInst = no,
        Term = term.functor(term.atom("="),
            [MaybeSugaredFuncTerm, ReturnTypeTerm], _)
    ->
        FuncTerm = desugar_field_access(MaybeSugaredFuncTerm),
        ContextPieces = [words("In function"), quote(":- mode"),
            words("declaration")],
        parse_implicitly_qualified_term(ModuleName, FuncTerm, Term,
            VarSet, ContextPieces, R),
        parse_func_mode_decl(R, ModuleName, FuncTerm, ReturnTypeTerm,
            Term, VarSet, MaybeDet, Condition, Attributes, Context, MaybeItem)
    ;
        ContextPieces = [words("In"), quote(":- mode"), words("declaration")],
        parse_implicitly_qualified_term(ModuleName, Term, Term,
            VarSet, ContextPieces, R),
        parse_pred_mode_decl(R, ModuleName, Term, VarSet,
            WithInst, MaybeDet, Condition, Attributes, Context, MaybeItem)
    ).

:- pred parse_pred_mode_decl(maybe_functor::in, module_name::in, term::in,
    varset::in, maybe(mer_inst)::in, maybe(determinism)::in, condition::in,
    decl_attrs::in, prog_context::in, maybe1(item)::out) is det.

parse_pred_mode_decl(error2(Specs), _, _, _, _, _, _, _, _, error1(Specs)).
parse_pred_mode_decl(ok2(F, As0), ModuleName, PredModeTerm, VarSet, WithInst,
        MaybeDet, Condition, Attributes0, Context, MaybeItem) :-
    ( convert_mode_list(allow_constrained_inst_var, As0, As1) ->
        get_class_context_and_inst_constraints(ModuleName, VarSet,
            Attributes0, Attributes, MaybeConstraints),
        (
            MaybeConstraints = ok3(_, _, InstConstraints),
            list.map(constrain_inst_vars_in_mode(InstConstraints), As1, As),
            varset.coerce(VarSet, ProgVarSet),
            ( inst_var_constraints_are_self_consistent_in_modes(As) ->
                (
                    WithInst = no,
                    PredOrFunc = yes(pf_predicate)
                ;
                    WithInst = yes(_),
                    % We don't know whether it's a predicate or a function
                    % until we expand out the inst.
                    PredOrFunc = no
                ),
                ItemModeDecl = item_mode_decl_info(ProgVarSet, PredOrFunc,
                    F, As, WithInst, MaybeDet, Condition, Context),
                Item = item_mode_decl(ItemModeDecl),
                MaybeItem0 = ok1(Item)
            ;
                PredModeTermStr = describe_error_term(VarSet, PredModeTerm),
                Pieces = [words("Error: inconsistent constraints"),
                    words("on inst variables"),
                    words("in predicate mode declaration:"), nl,
                    words(PredModeTermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(PredModeTerm),
                        [always(Pieces)])]),
                MaybeItem0 = error1([Spec])
            )
        ;
            MaybeConstraints = error3(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        PredModeTermStr = describe_error_term(VarSet, PredModeTerm),
        Pieces = [words("Error: syntax error in mode declaration at"),
            words(PredModeTermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(PredModeTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_func_mode_decl(maybe_functor::in, module_name::in, term::in,
    term::in, term::in, varset::in, maybe(determinism)::in, condition::in,
    decl_attrs::in, prog_context::in, maybe1(item)::out) is det.

parse_func_mode_decl(error2(Specs), _, _, _, _, _, _, _, _, _, error1(Specs)).
parse_func_mode_decl(ok2(F, As0), ModuleName, FuncMode, RetMode0, FullTerm,
        VarSet, MaybeDet, Condition, Attributes0, Context, MaybeItem) :-
    (
        convert_mode_list(allow_constrained_inst_var, As0, As1)
    ->
        get_class_context_and_inst_constraints(ModuleName, VarSet,
            Attributes0, Attributes, MaybeConstraints),
        (
            MaybeConstraints = ok3(_, _, InstConstraints),
            list.map(constrain_inst_vars_in_mode(InstConstraints), As1, As),
            (
                convert_mode(allow_constrained_inst_var, RetMode0, RetMode1)
            ->
                constrain_inst_vars_in_mode(InstConstraints,
                    RetMode1, RetMode),
                varset.coerce(VarSet, InstVarSet),
                ArgModes = As ++ [RetMode],
                (
                    inst_var_constraints_are_self_consistent_in_modes(ArgModes)
                ->
                    ItemModeDecl = item_mode_decl_info(InstVarSet,
                        yes(pf_function), F, ArgModes, no, MaybeDet, Condition,
                        Context),
                    Item = item_mode_decl(ItemModeDecl),
                    MaybeItem0 = ok1(Item)
                ;
                    FullTermStr = describe_error_term(VarSet, FullTerm),
                    Pieces = [words("Error: inconsistent constraints"),
                        words("on inst variables"),
                        words("in function mode declaration:"), nl,
                        words(FullTermStr), suffix("."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(FullTerm),
                            [always(Pieces)])]),
                    MaybeItem0 = error1([Spec])
                )
            ;
                Pieces = [words("Error: syntax error in return mode"),
                    words("of function mode declaration."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(RetMode0),
                        [always(Pieces)])]),
                MaybeItem0 = error1([Spec])
            )
        ;
            MaybeConstraints = error3(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        % XXX Should say which argument.
        FuncModeStr = describe_error_term(VarSet, FuncMode),
        Pieces = [words("Error: syntax error in arguments of"),
            words("function mode declaration at"),
            words(FuncModeStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(FuncMode), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

%-----------------------------------------------------------------------------%

    % We could perhaps get rid of some code duplication between here and
    % prog_io_typeclass.m?

    % XXX This documentation is out of date.
    % get_class_context_and_inst_constraints(ModuleName, Attributes0,
    %   Attributes, MaybeContext, MaybeInstConstraints):
    %
    % Parse type quantifiers, type class constraints and inst constraints
    % from the declaration attributes in Attributes0.
    % MaybeContext is either bound to the correctly parsed context, or
    % an appropriate error message (if there was a syntax error).
    % MaybeInstConstraints is either bound to a map containing the inst
    % constraints or an appropriate error message (if there was a syntax
    % error).
    % Attributes is bound to the remaining attributes.
    %
:- pred get_class_context_and_inst_constraints(module_name::in, varset::in,
    decl_attrs::in, decl_attrs::out,
    maybe3(existq_tvars, prog_constraints, inst_var_sub)::out) is det.

get_class_context_and_inst_constraints(ModuleName, VarSet, RevAttributes0,
        RevAttributes, MaybeExistClassInstContext) :-
    % Constraints and quantifiers should occur in the following order
    % (outermost to innermost):
    %
    %                               operator        precedence
    %                               --------        ----------
    %   1. universal quantifiers    all             950
    %   2. existential quantifiers  some            950
    %   3. universal constraints    <=              920
    %   4. existential constraints  =>              920 [*]
    %   5. the decl itself          pred or func    800
    %
    % When we reach here, Attributes0 contains declaration attributes
    % in the opposite order -- innermost to outermost -- so we reverse
    % them before we start.
    %
    % [*] Note that the semantic meaning of `=>' is not quite the same
    % as implication; logically speaking it's more like conjunction.
    % Oh well, at least it has the right precedence.
    %
    % In theory it could make sense to allow the order of 2 & 3 to be
    % swapped, or (in the case of multiple constraints & multiple
    % quantifiers) to allow arbitrary interleaving of 2 & 3, but in
    % practice it seems there would be little benefit in allowing that
    % flexibility, so we don't.
    %
    % Universal quantification is the default, so we just ignore
    % universal quantifiers.  (XXX It might be a good idea to check
    % that any universally quantified type variables do actually
    % occur somewhere in the type declaration, and are not also
    % existentially quantified, and if not, issue a warning or
    % error message.)

    list.reverse(RevAttributes0, Attributes0),
    get_quant_vars(quant_type_univ, ModuleName, Attributes0, Attributes1,
        [], _UnivQVars),
    get_quant_vars(quant_type_exist, ModuleName, Attributes1, Attributes2,
        [], ExistQVars0),
    list.map(term.coerce_var, ExistQVars0, ExistQVars),
    get_constraints(quant_type_univ, ModuleName, VarSet, Attributes2,
        Attributes3, MaybeUnivConstraints),
    get_constraints(quant_type_exist, ModuleName, VarSet, Attributes3,
        Attributes, MaybeExistConstraints),
    list.reverse(Attributes, RevAttributes),

    (
        MaybeUnivConstraints = ok2(UnivConstraints, UnivInstConstraints),
        MaybeExistConstraints = ok2(ExistConstraints, ExistInstConstraints)
    ->
        ClassConstraints = constraints(UnivConstraints, ExistConstraints),
        InstConstraints =
            map.old_merge(UnivInstConstraints, ExistInstConstraints),
        MaybeExistClassInstContext = ok3(ExistQVars, ClassConstraints,
            InstConstraints)
    ;
        Specs = get_any_errors2(MaybeUnivConstraints) ++
            get_any_errors2(MaybeExistConstraints),
        MaybeExistClassInstContext = error3(Specs)
    ).

:- pred get_constraints(quantifier_type::in, module_name::in, varset::in,
    decl_attrs::in, decl_attrs::out, maybe_class_and_inst_constraints::out)
    is det.

get_constraints(QuantType, ModuleName, VarSet, !Attributes,
        MaybeClassInstConstraints) :-
    (
        !.Attributes = [
            decl_attr_constraints(QuantType, ConstraintsTerm) - _Term
            | !:Attributes]
    ->
        parse_class_and_inst_constraints(ModuleName, VarSet, ConstraintsTerm,
            MaybeHeadConstraints),
        % there may be more constraints of the same type --
        % collect them all and combine them
        get_constraints(QuantType, ModuleName, VarSet, !Attributes,
            MaybeTailConstraints),
        (
            MaybeHeadConstraints =
                ok2(HeadClassConstraints, HeadInstConstraint),
            MaybeTailConstraints =
                ok2(TailClassConstraints, TailInstConstraint)
        ->
            ClassConstraints = HeadClassConstraints ++ TailClassConstraints,
            InstConstraints =
                map.old_merge(HeadInstConstraint, TailInstConstraint),
            MaybeClassInstConstraints = ok2(ClassConstraints, InstConstraints)
        ;
            Specs = get_any_errors2(MaybeHeadConstraints) ++
                get_any_errors2(MaybeTailConstraints),
            MaybeClassInstConstraints = error2(Specs)
        )
    ;
        MaybeClassInstConstraints = ok2([], map.init)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_promise(module_name::in, promise_type::in, varset::in,
    list(term)::in, decl_attrs::in, prog_context::in, maybe1(item)::out)
    is semidet.

parse_promise(ModuleName, PromiseType, VarSet, [Term], Attributes, Context,
        Result) :-
    varset.coerce(VarSet, ProgVarSet0),
    ContextPieces = [],
    parse_goal(Term, ContextPieces, MaybeGoal0, ProgVarSet0, ProgVarSet),
    (
        MaybeGoal0 = ok1(Goal0),
        % Get universally quantified variables.
        (
            PromiseType = promise_type_true,
            ( Goal0 = all_expr(UnivVars0, AllGoal) - _Context ->
                UnivVars0 = UnivVars,
                Goal = AllGoal
            ;
                UnivVars = [],
                Goal = Goal0
            )
        ;
            ( PromiseType = promise_type_exclusive
            ; PromiseType = promise_type_exhaustive
            ; PromiseType = promise_type_exclusive_exhaustive
            ),
            get_quant_vars(quant_type_univ, ModuleName, Attributes, _,
                [], UnivVars0),
            list.map(term.coerce_var, UnivVars0, UnivVars),
            Goal0 = Goal
        ),
        ItemPromise = item_promise_info(PromiseType, Goal, ProgVarSet,
            UnivVars, Context),
        Item = item_promise(ItemPromise),
        Result = ok1(Item)
    ;
        MaybeGoal0 = error1(Specs),
        Result = error1(Specs)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_initialise_decl(module_name::in, varset::in, term::in,
    prog_context::in, maybe1(item)::out) is det.

parse_initialise_decl(_ModuleName, VarSet, Term, Context, Result) :-
    parse_symbol_name_specifier(VarSet, Term, MaybeSymNameSpecifier),
    (
        MaybeSymNameSpecifier = error1(Specs),
        Result = error1(Specs)
    ;
        MaybeSymNameSpecifier = ok1(SymNameSpecifier),
        (
            SymNameSpecifier = name(_),
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error:"), quote("initialise"),
                words("declaration"), words("requires arity, found"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            Result = error1([Spec])
        ;
            SymNameSpecifier = name_arity(SymName, Arity),
            (
                ( Arity = 0 ; Arity = 2 )
            ->
                ItemInitialise = item_initialise_info(user, SymName, Arity,
                    Context),
                Item = item_initialise(ItemInitialise),
                Result = ok1(Item)
            ;
                TermStr = describe_error_term(VarSet, Term),
                Pieces = [words("Error:"), quote("initialise"),
                    words("declaration specifies a predicate"),
                    words("whose arity is not zero or two:"),
                    words(TermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                Result = error1([Spec])
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred parse_finalise_decl(module_name::in, varset::in, term::in,
    prog_context::in, maybe1(item)::out) is det.

parse_finalise_decl(_ModuleName, VarSet, Term, Context, Result) :-
    parse_symbol_name_specifier(VarSet, Term, MaybeSymNameSpecifier),
    (
        MaybeSymNameSpecifier = error1(Specs),
        Result = error1(Specs)
    ;
        MaybeSymNameSpecifier = ok1(SymNameSpecifier),
        (
            SymNameSpecifier = name(_),
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error:"), quote("finalise"),
                words("declaration"), words("requires arity, found"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            Result = error1([Spec])
        ;
            SymNameSpecifier = name_arity(SymName, Arity),
            (
                ( Arity = 0 ; Arity = 2 )
            ->
                ItemFinalise = item_finalise_info(user, SymName, Arity,
                    Context),
                Item = item_finalise(ItemFinalise),
                Result = ok1(Item)
            ;
                TermStr = describe_error_term(VarSet, Term),
                Pieces = [words("Error:"), quote("finalise"),
                    words("declaration specifies a predicate"),
                    words("whose arity is not zero or two:"),
                    words(TermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                Result = error1([Spec])
            )
        )
    ).

%-----------------------------------------------------------------------------%
%
% Mutable declarations
%
% See prog_mutable.m for implementation details.
%

:- pred parse_mutable_decl(module_name::in, varset::in, list(term)::in,
    prog_context::in, maybe1(item)::out) is semidet.

parse_mutable_decl(_ModuleName, VarSet, Terms, Context, MaybeItem) :-
    Terms = [NameTerm, TypeTerm, ValueTerm, InstTerm | OptMutAttrsTerm],
    parse_mutable_name(NameTerm, MaybeName),
    parse_mutable_type(VarSet, TypeTerm, MaybeType),
    term.coerce(ValueTerm, Value),
    varset.coerce(VarSet, ProgVarSet),
    parse_mutable_inst(VarSet, InstTerm, MaybeInst),

    % The list of attributes is optional.
    (
        OptMutAttrsTerm = [],
        MaybeMutAttrs = ok1(default_mutable_attributes)
    ;
        OptMutAttrsTerm = [MutAttrsTerm],
        parse_mutable_attrs(VarSet, MutAttrsTerm, MaybeMutAttrs)
    ),
    (
        MaybeName = ok1(Name),
        MaybeType = ok1(Type),
        MaybeInst = ok1(Inst),
        MaybeMutAttrs = ok1(MutAttrs)
    ->
        % We *must* attach the varset to the mutable item because if the
        % initial value is non-ground, then the initial value will be a
        % variable and the mutable initialisation predicate will contain
        % references to it.  Ignoring the varset may lead to later compiler
        % passes attempting to reuse this variable when fresh variables are
        % allocated.
        ItemMutable = item_mutable_info(Name, Type, Value, Inst, MutAttrs,
            ProgVarSet, Context),
        Item = item_mutable(ItemMutable),
        MaybeItem = ok1(Item)
    ;
        Specs = get_any_errors1(MaybeName) ++ get_any_errors1(MaybeType) ++
            get_any_errors1(MaybeInst) ++ get_any_errors1(MaybeMutAttrs),
        MaybeItem = error1(Specs)
    ).

:- pred parse_mutable_name(term::in, maybe1(string)::out) is det.

parse_mutable_name(NameTerm, MaybeName) :-
    ( NameTerm = term.functor(atom(Name), [], _) ->
        MaybeName = ok1(Name)
    ;
        Pieces = [words("Error: invalid mutable name."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(NameTerm), [always(Pieces)])]),
        MaybeName = error1([Spec])
    ).

:- pred parse_mutable_type(varset::in, term::in, maybe1(mer_type)::out) is det.

parse_mutable_type(VarSet, TypeTerm, MaybeType) :-
    ( term.contains_var(TypeTerm, _) ->
        TypeTermStr = describe_error_term(VarSet, TypeTerm),
        Pieces = [words("Error: the type in a mutable declaration"),
            words("cannot contain variables:"),
            words(TypeTermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(TypeTerm), [always(Pieces)])]),
        MaybeType = error1([Spec])
    ;
        ContextPieces = [],
        parse_type(TypeTerm, VarSet, ContextPieces, MaybeType)
    ).

:- pred parse_mutable_inst(varset::in, term::in, maybe1(mer_inst)::out) is det.

parse_mutable_inst(VarSet, InstTerm, MaybeInst) :-
    ( term.contains_var(InstTerm, _) ->
        InstTermStr = describe_error_term(VarSet, InstTerm),
        Pieces = [words("Error: the inst in a mutable declaration"),
            words("cannot contain variables:"),
            words(InstTermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(InstTerm), [always(Pieces)])]),
        MaybeInst = error1([Spec])
    ; convert_inst(no_allow_constrained_inst_var, InstTerm, Inst) ->
        MaybeInst = ok1(Inst)
    ;
        Pieces = [words("Error: invalid inst in mutable declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(InstTerm), [always(Pieces)])]),
        MaybeInst = error1([Spec])
    ).

:- type collected_mutable_attribute
    --->    mutable_attr_trailed(mutable_trailed)
    ;       mutable_attr_foreign_name(foreign_name)
    ;       mutable_attr_attach_to_io_state(bool)
    ;       mutable_attr_constant(bool)
    ;       mutable_attr_thread_local(mutable_thread_local).

:- pred parse_mutable_attrs(varset::in, term::in,
    maybe1(mutable_var_attributes)::out) is det.

parse_mutable_attrs(VarSet, MutAttrsTerm, MaybeMutAttrs) :-
    Attributes0 = default_mutable_attributes,
    ConflictingAttributes = [
        mutable_attr_trailed(mutable_trailed) -
            mutable_attr_trailed(mutable_untrailed),
        mutable_attr_trailed(mutable_trailed) -
            mutable_attr_thread_local(mutable_thread_local),
        mutable_attr_constant(yes) - mutable_attr_trailed(mutable_trailed),
        mutable_attr_constant(yes) - mutable_attr_attach_to_io_state(yes),
        mutable_attr_constant(yes) -
            mutable_attr_thread_local(mutable_thread_local)
    ],
    (
        list_term_to_term_list(MutAttrsTerm, MutAttrTerms),
        map_parser(parse_mutable_attr, MutAttrTerms, MaybeAttrList),
        MaybeAttrList = ok1(CollectedMutAttrs)
    ->
        % We check for trailed/untrailed, constant/trailed,
        % trailed/thread_local, constant/attach_to_io_state,
        % constant/thread_local conflicts here and deal with conflicting
        % foreign_name attributes in make_hlds_passes.m.
        (
            list.member(Conflict1 - Conflict2, ConflictingAttributes),
            list.member(Conflict1, CollectedMutAttrs),
            list.member(Conflict2, CollectedMutAttrs)
        ->
            % XXX Should generate more specific error message.
            MutAttrsStr = mercury_term_to_string(VarSet, no, MutAttrsTerm),
            Pieces = [words("Error: conflicting attributes"),
                words("in attribute list:"), nl,
                words(MutAttrsStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(MutAttrsTerm),
                    [always(Pieces)])]),
            MaybeMutAttrs = error1([Spec])
        ;
            list.foldl(process_mutable_attribute, CollectedMutAttrs,
                Attributes0, Attributes),
            MaybeMutAttrs = ok1(Attributes)
        )
    ;
        MutAttrsStr = mercury_term_to_string(VarSet, no, MutAttrsTerm),
        Pieces = [words("Error: malformed attribute list"),
            words("in mutable declaration:"),
            words(MutAttrsStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(MutAttrsTerm), [always(Pieces)])]),
        MaybeMutAttrs = error1([Spec])
    ).

:- pred process_mutable_attribute(collected_mutable_attribute::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

process_mutable_attribute(mutable_attr_trailed(Trailed), !Attributes) :-
    set_mutable_var_trailed(Trailed, !Attributes).
process_mutable_attribute(mutable_attr_foreign_name(ForeignName),
        !Attributes) :-
    set_mutable_add_foreign_name(ForeignName, !Attributes).
process_mutable_attribute(mutable_attr_attach_to_io_state(AttachToIOState),
        !Attributes) :-
    set_mutable_var_attach_to_io_state(AttachToIOState, !Attributes).
process_mutable_attribute(mutable_attr_constant(Constant), !Attributes) :-
    set_mutable_var_constant(Constant, !Attributes),
    (
        Constant = yes,
        set_mutable_var_trailed(mutable_untrailed, !Attributes),
        set_mutable_var_attach_to_io_state(no, !Attributes)
    ;
        Constant = no
    ).
process_mutable_attribute(mutable_attr_thread_local(ThrLocal), !Attributes) :-
    set_mutable_var_thread_local(ThrLocal, !Attributes).

:- pred parse_mutable_attr(term::in,
    maybe1(collected_mutable_attribute)::out) is det.

parse_mutable_attr(MutAttrTerm, MutAttrResult) :-
    (
        MutAttrTerm = term.functor(term.atom(String), [], _),
        (
            String  = "untrailed",
            MutAttr = mutable_attr_trailed(mutable_untrailed)
        ;
            String = "trailed",
            MutAttr = mutable_attr_trailed(mutable_trailed)
        ;
            String  = "attach_to_io_state",
            MutAttr = mutable_attr_attach_to_io_state(yes)
        ;
            String = "constant",
            MutAttr = mutable_attr_constant(yes)
        ;
            String = "thread_local",
            MutAttr = mutable_attr_thread_local(mutable_thread_local)
        )
    ->
        MutAttrResult = ok1(MutAttr)
    ;
        MutAttrTerm = term.functor(term.atom("foreign_name"), Args, _),
        Args = [LangTerm, ForeignNameTerm],
        parse_foreign_language(LangTerm, Lang),
        ForeignNameTerm = term.functor(term.string(ForeignName), [], _)
    ->
        MutAttr = mutable_attr_foreign_name(foreign_name(Lang, ForeignName)),
        MutAttrResult = ok1(MutAttr)
    ;
        Pieces = [words("Error: unrecognised attribute"),
            words("in mutable declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(MutAttrTerm), [always(Pieces)])]),
        MutAttrResult = error1([Spec])
    ).

%-----------------------------------------------------------------------------%

    % parse_condition_suffix(Term, BeforeCondTerm, Condition):
    %
    % Bind Condition to a representation of the 'where' condition of Term,
    % if any, and bind BeforeCondTerm to the other part of Term. If Term
    % does not contain a condition, then set Condition to true.
    %
    % NU-Prolog supported type declarations of the form
    %   :- pred p(T) where p(X) : sorted(X).
    % or
    %   :- type sorted_list(T) = list(T) where X : sorted(X).
    %   :- pred p(sorted_list(T).
    % There is some code here to support that sort of thing, but
    % probably we would now need to use a different syntax, since
    % Mercury now uses `where' for different purposes (e.g. specifying
    % user-defined equality predicates, and also for type classes ...)
    %
:- pred parse_condition_suffix(term::in, term::out, condition::out) is det.

parse_condition_suffix(Term, Term, cond_true).

% parse_condition_suffix(B, Body, Condition) :-
%   (
%       B = term.functor(term.atom("where"), [Body1, Condition1],
%           _Context)
%   ->
%       Body = Body1,
%       Condition = where(Condition1)
%   ;
%       Body = B,
%       Condition = true
%   ).

    % parse_determinism_suffix(VarSet, BodyTerm, BeforeDetismTerm,
    %   MaybeMaybeDetism):
    %
    % Look for a suffix of the form "is <detism>" in Term. If we find one,
    % bind MaybeMaybeDetism to ok1(yes()) wrapped around the determinism,
    % and bind BeforeDetismTerm to the other part of Term. If we don't
    % find, one, then bind MaybeMaybeDetism to ok1(no).
    %
:- pred parse_determinism_suffix(varset::in, term::in, term::out,
    maybe1(maybe(determinism))::out) is det.

parse_determinism_suffix(VarSet, Term, BeforeDetismTerm, MaybeMaybeDetism) :-
    (
        Term = term.functor(term.atom("is"), Args, _),
        Args = [BeforeDetismTermPrime, DetismTerm]
    ->
        BeforeDetismTerm = BeforeDetismTermPrime,
        (
            DetismTerm = term.functor(term.atom(DetismFunctor), [], _),
            standard_det(DetismFunctor, Detism)
        ->
            MaybeMaybeDetism = ok1(yes(Detism))
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: invalid determinism category"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(DetismTerm), [always(Pieces)])]),
            MaybeMaybeDetism = error1([Spec])
        )
    ;
        BeforeDetismTerm = Term,
        MaybeMaybeDetism = ok1(no)
    ).

    % Process the `with_type type` suffix part of a declaration.
    %
:- pred parse_with_type_suffix(varset::in, term::in, term::out,
    maybe1(maybe(mer_type))::out) is det.

parse_with_type_suffix(VarSet, Term, BeforeWithTypeTerm, MaybeWithType) :-
    (
        Term = term.functor(TypeQualifier,
            [BeforeWithTypeTermPrime, TypeTerm], _),
        (
            TypeQualifier = term.atom("with_type")
        ;
            TypeQualifier = term.atom(":")
        )
    ->
        BeforeWithTypeTerm = BeforeWithTypeTermPrime,
        % XXX Should supply more correct ContextPieces.
        ContextPieces = [],
        parse_type(TypeTerm, VarSet, ContextPieces, MaybeType),
        (
            MaybeType = ok1(Type),
            MaybeWithType = ok1(yes(Type))
        ;
            MaybeType = error1(Specs),
            MaybeWithType = error1(Specs)
        )
    ;
        BeforeWithTypeTerm = Term,
        MaybeWithType = ok1(no)
    ).

    % Process the `with_inst inst` suffix part of a declaration.
    %
:- pred parse_with_inst_suffix(term::in, term::out,
    maybe1(maybe(mer_inst))::out) is det.

parse_with_inst_suffix(Term, BeforeWithInstTerm, MaybeWithInst) :-
    (
        Term = term.functor(term.atom("with_inst"),
            [BeforeWithInstTermPrime, InstTerm], _)
    ->
        BeforeWithInstTerm = BeforeWithInstTermPrime,
        ( convert_inst(allow_constrained_inst_var, InstTerm, Inst) ->
            MaybeWithInst = ok1(yes(Inst))
        ;
            Pieces = [words("Error: invalid inst in"), quote("with_inst"),
                suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(InstTerm), [always(Pieces)])]),
            MaybeWithInst = error1([Spec])
        )
    ;
        BeforeWithInstTerm = Term,
        MaybeWithInst = ok1(no)
    ).

%-----------------------------------------------------------------------------%

:- pred get_quant_vars(quantifier_type::in, module_name::in,
    decl_attrs::in, decl_attrs::out, list(var)::in, list(var)::out) is det.

get_quant_vars(QuantType, ModuleName, !Attributes, !Vars) :-
    (
        !.Attributes = [decl_attr_quantifier(QuantType, QuantVars) - _
            | !:Attributes]
    ->
        !:Vars = !.Vars ++ QuantVars,
        get_quant_vars(QuantType, ModuleName, !Attributes, !Vars)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % Perform one of the following field-access syntax rewrites if possible:
    %
    %   A ^ f(B, ...)       --->    f(B, ..., A)
    %   (A ^ f(B, ...) := X)    --->    'f :='(B, ..., A, X)
    %
:- func desugar_field_access(term) = term.

desugar_field_access(Term) = DesugaredTerm :-
    (
        Term = functor(atom("^"), [A, RHS], _),
        RHS  = functor(atom(FieldName), Bs, Context)
    ->
        DesugaredTerm = functor(atom(FieldName), Bs ++ [A], Context)
    ;
        % XXX We shouldn't insist on the context of LHS and RHS being the same.
        Term = functor(atom(":="), [LHS, X], _),
        LHS  = functor(atom("^"), [A, RHS], Context),
        RHS  = functor(atom(FieldName), Bs, Context)
    ->
        FunctionName = FieldName ++ " :=",
        DesugaredTerm = functor(atom(FunctionName), Bs ++ [A, X], Context)
    ;
        DesugaredTerm = Term
    ).

%-----------------------------------------------------------------------------%

constrain_inst_vars_in_mode(Mode0, Mode) :-
    constrain_inst_vars_in_mode(map.init, Mode0, Mode).

constrain_inst_vars_in_mode(InstConstraints, Mode0, Mode) :-
    (
        Mode0 = (I0 -> F0),
        constrain_inst_vars_in_inst(InstConstraints, I0, I),
        constrain_inst_vars_in_inst(InstConstraints, F0, F),
        Mode = (I -> F)
    ;
        Mode0 = user_defined_mode(Name, Args0),
        list.map(constrain_inst_vars_in_inst(InstConstraints), Args0, Args),
        Mode = user_defined_mode(Name, Args)
    ).

:- pred constrain_inst_vars_in_inst(inst_var_sub::in,
    mer_inst::in, mer_inst::out) is det.

constrain_inst_vars_in_inst(InstConstraints, Inst0, Inst) :-
    (
        Inst0 = any(U, none),
        Inst = any(U, none)
    ;
        Inst0 = any(U, higher_order(PredInstInfo0)),
        constrain_inst_vars_in_pred_inst_info(InstConstraints,
            PredInstInfo0, PredInstInfo),
        Inst = any(U, higher_order(PredInstInfo))
    ;
        Inst0 = free,
        Inst = free
    ;
        Inst0 = free(Type),
        Inst = free(Type)
    ;
        Inst0 = bound(U, BIs0),
        list.map(
            (pred(bound_functor(C, Is0)::in, bound_functor(C, Is)::out)
                    is det :-
                list.map(constrain_inst_vars_in_inst(InstConstraints),
                    Is0, Is)),
            BIs0, BIs),
        Inst = bound(U, BIs)
    ;
        Inst0 = ground(U, none),
        Inst = ground(U, none)
    ;
        Inst0 = ground(U, higher_order(PredInstInfo0)),
        constrain_inst_vars_in_pred_inst_info(InstConstraints,
            PredInstInfo0, PredInstInfo),
        Inst = ground(U, higher_order(PredInstInfo))
    ;
        Inst0 = constrained_inst_vars(Vars0, SubInst0),
        constrain_inst_vars_in_inst(InstConstraints, SubInst0, SubInst1),
        ( SubInst1 = constrained_inst_vars(SubVars, SubSubInst) ->
            set.union(Vars0, SubVars, Vars),
            SubInst = SubSubInst
        ;
            Vars = Vars0,
            SubInst = SubInst1
        ),
        Inst = constrained_inst_vars(Vars, SubInst)
    ;
        Inst0 = not_reached,
        Inst = not_reached
    ;
        Inst0 = inst_var(Var),
        ( map.search(InstConstraints, Var, SubInstPrime) ->
            SubInst = SubInstPrime
        ;
            SubInst = ground(shared, none)
        ),
        Inst = constrained_inst_vars(set.make_singleton_set(Var), SubInst)
    ;
        Inst0 = defined_inst(Name0),
        constrain_inst_vars_in_inst_name(InstConstraints, Name0, Name),
        Inst = defined_inst(Name)
    ;
        Inst0 = abstract_inst(InstName, SubInsts0),
        list.map(constrain_inst_vars_in_inst(InstConstraints),
            SubInsts0, SubInsts),
        Inst = abstract_inst(InstName, SubInsts)
    ).

:- pred constrain_inst_vars_in_pred_inst_info(inst_var_sub::in,
    pred_inst_info::in, pred_inst_info::out) is det.

constrain_inst_vars_in_pred_inst_info(InstConstraints, PII0, PII) :-
    PII0 = pred_inst_info(PredOrFunc, Modes0, Det),
    list.map(constrain_inst_vars_in_mode(InstConstraints), Modes0, Modes),
    PII = pred_inst_info(PredOrFunc, Modes, Det).

:- pred constrain_inst_vars_in_inst_name(inst_var_sub::in,
    inst_name::in, inst_name::out) is det.

constrain_inst_vars_in_inst_name(InstConstraints, Name0, Name) :-
    ( Name0 = user_inst(SymName, Args0) ->
        list.map(constrain_inst_vars_in_inst(InstConstraints), Args0, Args),
        Name = user_inst(SymName, Args)
    ;
        Name = Name0
    ).

%-----------------------------------------------------------------------------%

inst_var_constraints_are_self_consistent_in_modes(Modes) :-
    inst_var_constraints_are_consistent_in_modes(Modes, map.init, _).

:- pred inst_var_constraints_are_consistent_in_modes(list(mer_mode)::in,
    inst_var_sub::in, inst_var_sub::out) is semidet.

inst_var_constraints_are_consistent_in_modes(Modes, !Sub) :-
    list.foldl(inst_var_constraints_are_consistent_in_mode, Modes, !Sub).

:- pred inst_var_constraints_types_modes_self_consistent(
    list(type_and_mode)::in) is semidet.

inst_var_constraints_types_modes_self_consistent(TypeAndModes) :-
    list.foldl(inst_var_constraints_type_mode_consistent, TypeAndModes,
        map.init, _).

:- pred inst_var_constraints_type_mode_consistent(type_and_mode::in,
    inst_var_sub::in, inst_var_sub::out) is semidet.

inst_var_constraints_type_mode_consistent(TypeAndMode, !Sub) :-
    (
        TypeAndMode = type_only(_)
    ;
        TypeAndMode = type_and_mode(_, Mode),
        inst_var_constraints_are_consistent_in_mode(Mode, !Sub)
    ).

:- pred inst_var_constraints_are_consistent_in_mode(mer_mode::in,
    inst_var_sub::in, inst_var_sub::out) is semidet.

inst_var_constraints_are_consistent_in_mode(Mode, !Sub) :-
    (
        Mode = (InitialInst -> FinalInst),
        inst_var_constraints_are_consistent_in_inst(InitialInst, !Sub),
        inst_var_constraints_are_consistent_in_inst(FinalInst, !Sub)
    ;
        Mode = user_defined_mode(_, ArgInsts),
        inst_var_constraints_are_consistent_in_insts(ArgInsts, !Sub)
    ).

:- pred inst_var_constraints_are_consistent_in_insts(list(mer_inst)::in,
    inst_var_sub::in, inst_var_sub::out) is semidet.

inst_var_constraints_are_consistent_in_insts(Insts, !Sub) :-
    list.foldl(inst_var_constraints_are_consistent_in_inst, Insts, !Sub).

:- pred inst_var_constraints_are_consistent_in_inst(mer_inst::in,
    inst_var_sub::in, inst_var_sub::out) is semidet.

inst_var_constraints_are_consistent_in_inst(Inst, !Sub) :-
    (
        ( Inst = free
        ; Inst = free(_)
        ; Inst = not_reached
        )
    ;
        Inst = bound(_, BoundInsts),
        list.foldl(
            (pred(bound_functor(_, Insts)::in, in, out) is semidet -->
                inst_var_constraints_are_consistent_in_insts(Insts)),
            BoundInsts, !Sub)
    ;
        ( Inst = ground(_, HOInstInfo)
        ; Inst = any(_, HOInstInfo)
        ),
        (
            HOInstInfo = none
        ;
            HOInstInfo = higher_order(pred_inst_info(_, Modes, _)),
            inst_var_constraints_are_consistent_in_modes(Modes, !Sub)
        )
    ;
        Inst = inst_var(_),
        unexpected(this_file,
            "inst_var_constraints_are_consistent_in_inst: " ++
            "unconstrained inst_var")
    ;
        Inst = defined_inst(InstName),
        ( InstName = user_inst(_, Insts) ->
            inst_var_constraints_are_consistent_in_insts(Insts, !Sub)
        ;
            true
        )
    ;
        Inst = abstract_inst(_, Insts),
        inst_var_constraints_are_consistent_in_insts(Insts, !Sub)
    ;
        Inst = constrained_inst_vars(InstVars, SubInst),
        set.fold(inst_var_constraints_are_consistent_in_inst_var(SubInst),
            InstVars, !Sub),
        inst_var_constraints_are_consistent_in_inst(SubInst, !Sub)
    ).

:- pred inst_var_constraints_are_consistent_in_inst_var(mer_inst::in,
    inst_var::in, inst_var_sub::in, inst_var_sub::out) is semidet.

inst_var_constraints_are_consistent_in_inst_var(SubInst, InstVar, !Sub) :-
    ( map.search(!.Sub, InstVar, InstVarInst) ->
        % Check that the inst_var constraint is consistent with
        % the previous constraint on this inst_var.
        InstVarInst = SubInst
    ;
        map.det_insert(!.Sub, InstVar, SubInst, !:Sub)
    ).

%-----------------------------------------------------------------------------%

    % A ModuleSpecifier is just an sym_name.
    %
:- pred parse_module_specifier(varset::in, term::in,
    maybe1(module_specifier)::out) is det.

parse_module_specifier(VarSet, Term, MaybeModuleSpecifier) :-
    parse_symbol_name(VarSet, Term, MaybeModuleSpecifier).

    % A ModuleName is an implicitly-quantified sym_name.
    %
    % We check for module names starting with capital letters as a special
    % case, so that we can report a better error message for that case.
    %
:- pred parse_module_name(module_name::in, varset::in, term::in,
    maybe1(module_name)::out) is det.

parse_module_name(DefaultModuleName, VarSet, Term, MaybeModule) :-
    (
        Term = term.variable(_, Context),
        Pieces = [words("Error: module names starting with capital letters"),
            words("must be quoted using single quotes"),
            words("(e.g. "":- module 'Foo'."")."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        MaybeModule = error1([Spec])
    ;
        Term = term.functor(_, _, _),
        parse_implicitly_qualified_symbol_name(DefaultModuleName, VarSet,
            Term, MaybeModule)
    ).

%-----------------------------------------------------------------------------%

    % A SymbolNameSpecifier is one of
    %   SymbolName
    %   SymbolName/Arity
    %       Matches only symbols of the specified arity.
    %
:- pred parse_symbol_name_specifier(varset::in, term::in,
    maybe1(sym_name_specifier)::out) is det.

parse_symbol_name_specifier(VarSet, Term, MaybeSymNameSpecifier) :-
    root_module_name(DefaultModule),
    parse_implicitly_qualified_symbol_name_specifier(DefaultModule, VarSet,
        Term, MaybeSymNameSpecifier).

:- pred parse_implicitly_qualified_symbol_name_specifier(module_name::in,
    varset::in, term::in, maybe1(sym_name_specifier)::out) is det.

parse_implicitly_qualified_symbol_name_specifier(DefaultModule, VarSet, Term,
        MaybeSymNameSpecifier) :-
    ( Term = term.functor(term.atom("/"), [NameTerm, ArityTerm], _) ->
        ( ArityTerm = term.functor(term.integer(Arity), [], _) ->
            ( Arity >= 0 ->
                parse_implicitly_qualified_symbol_name(DefaultModule, VarSet,
                    NameTerm, MaybeName),
                process_maybe1(make_name_arity_specifier(Arity),
                    MaybeName, MaybeSymNameSpecifier)
            ;
                Pieces = [words("Error: arity in symbol name specifier"),
                    words("must be a non-negative integer."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                MaybeSymNameSpecifier = error1([Spec])
            )
        ;
            Pieces = [words("Error: arity in symbol name specifier"),
                words("must be an integer."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeSymNameSpecifier = error1([Spec])
        )
    ;
        parse_implicitly_qualified_symbol_name(DefaultModule, VarSet, Term,
            MaybeSymbolName),
        process_maybe1(make_name_specifier, MaybeSymbolName,
            MaybeSymNameSpecifier)
    ).

%-----------------------------------------------------------------------------%

    % A SymbolName is one of
    %   Name
    %       Matches symbols with the specified name in the
    %       current namespace.
    %   Module.Name
    %       Matches symbols with the specified name exported
    %       by the specified module (where Module is itself a SymbolName).
    %
    % We also allow the syntax `Module__Name' as an alternative
    % for `Module.Name'.
    %
:- pred parse_symbol_name(varset(T)::in, term(T)::in, maybe1(sym_name)::out)
    is det.

parse_symbol_name(VarSet, Term, MaybeSymName) :-
    (
        Term = term.functor(term.atom(FunctorName), [ModuleTerm, NameTerm],
            TermContext),
        ( FunctorName = ":"
        ; FunctorName = "."
        )
    ->
        ( NameTerm = term.functor(term.atom(Name), [], _) ->
            parse_symbol_name(VarSet, ModuleTerm, MaybeModule),
            (
                MaybeModule = ok1(Module),
                MaybeSymName = ok1(qualified(Module, Name))
            ;
                MaybeModule = error1(_ModuleResultSpecs),
                % XXX We should say "module name" OR "identifier", not both.
                Pieces = [words("Error: module name identifier"),
                    words("expected before"), quote(FunctorName),
                    words("in qualified symbol name."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                % XXX Should we include _ModuleResultSpecs?
                MaybeSymName = error1([Spec])
            )
        ;
            Pieces = [words("Error: identifier expected after"),
                quote(FunctorName), words("in qualified symbol name."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeSymName = error1([Spec])
        )
    ;
        ( Term = term.functor(term.atom(Name), [], _) ->
            SymName = string_to_sym_name_sep(Name, "__"),
            MaybeSymName = ok1(SymName)
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: symbol name expected at"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeSymName = error1([Spec])
        )
    ).

:- pred parse_implicitly_qualified_symbol_name(module_name::in, varset::in,
    term::in, maybe1(sym_name)::out) is det.

parse_implicitly_qualified_symbol_name(DefaultModName, VarSet, Term,
        MaybeSymName) :-
    parse_symbol_name(VarSet, Term, MaybeSymName0),
    (
        MaybeSymName0 = ok1(SymName),
        (
            root_module_name(DefaultModName)
        ->
            MaybeSymName = MaybeSymName0
        ;
            SymName = qualified(ModName, _),
            \+ match_sym_name(ModName, DefaultModName)
        ->
            Pieces = [words("Error: module qualifier in definition"),
                words("does not match preceding"), quote(":- module"),
                words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeSymName = error1([Spec])

        ;
            UnqualName = unqualify_name(SymName),
            MaybeSymName = ok1(qualified(DefaultModName, UnqualName))
        )
    ;
        MaybeSymName0 = error1(_),
        MaybeSymName = MaybeSymName0
    ).

%-----------------------------------------------------------------------------%

parse_implicitly_qualified_term(DefaultModuleName, Term, ContainingTerm,
        VarSet, ContextPieces, MaybeSymNameAndArgs) :-
    parse_qualified_term(Term, ContainingTerm, VarSet, ContextPieces,
        MaybeSymNameAndArgs0),
    (
        MaybeSymNameAndArgs0 = ok2(SymName, Args),
        (
            root_module_name(DefaultModuleName)
        ->
            MaybeSymNameAndArgs = MaybeSymNameAndArgs0
        ;
            SymName = qualified(ModuleName, _),
            \+ match_sym_name(ModuleName, DefaultModuleName)
        ->
            Pieces = [words("Error: module qualifier in definition"),
                words("does not match preceding"), quote(":- module"),
                words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeSymNameAndArgs = error2([Spec])
        ;
            UnqualName = unqualify_name(SymName),
            QualSymName = qualified(DefaultModuleName, UnqualName),
            MaybeSymNameAndArgs = ok2(QualSymName, Args)
        )
    ;
        MaybeSymNameAndArgs0 = error2(_),
        MaybeSymNameAndArgs = MaybeSymNameAndArgs0
    ).

sym_name_and_args(Term, SymName, Args) :-
    % The values of VarSet and ContextPieces do not matter here, since
    % we succeed only if they aren't used.
    VarSet = varset.init,
    ContextPieces = [],
    parse_qualified_term(Term, Term, VarSet, ContextPieces,
        ok2(SymName, Args)).

parse_qualified_term(Term, _ContainingTerm, VarSet, ContextPieces,
        MaybeSymNameAndArgs) :-
    % XXX We should delete the _ContainingTerm argument.
    (
        Term = term.functor(Functor, FunctorArgs, TermContext),
        Functor = term.atom("."),
        FunctorArgs = [ModuleTerm, NameArgsTerm]
    ->
        ( NameArgsTerm = term.functor(term.atom(Name), Args, _) ->
            varset.coerce(VarSet, GenericVarSet),
            parse_symbol_name(GenericVarSet, ModuleTerm, MaybeModule),
            (
                MaybeModule = ok1(Module),
                MaybeSymNameAndArgs = ok2(qualified(Module, Name), Args)
            ;
                MaybeModule = error1(_),
                ModuleTermStr = describe_error_term(GenericVarSet, ModuleTerm),
                % XXX We should say "module name" OR "identifier", not both.
                Pieces = ContextPieces ++ [lower_case_next_if_not_first,
                    words("Error: module name identifier expected before '.'"),
                    words("in qualified symbol name, not"),
                    words(ModuleTermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeSymNameAndArgs = error2([Spec])
            )
        ;
            varset.coerce(VarSet, GenericVarSet),
            TermStr = describe_error_term(GenericVarSet, Term),
            Pieces = ContextPieces ++ [lower_case_next_if_not_first,
                words("Error: identifier expected after '.'"),
                words("in qualified symbol name, not"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeSymNameAndArgs = error2([Spec])
        )
    ;
        varset.coerce(VarSet, GenericVarSet),
        ( Term = term.functor(term.atom(Name), Args, _) ->
            SymName = string_to_sym_name_sep(Name, "__"),
            MaybeSymNameAndArgs = ok2(SymName, Args)
        ;
            TermStr = describe_error_term(GenericVarSet, Term),
            Pieces = ContextPieces ++ [lower_case_next_if_not_first,
                words("Error: atom expected at"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeSymNameAndArgs = error2([Spec])
        )
    ).

%-----------------------------------------------------------------------------%

    % We use the empty module name ('') as the "root" module name; when adding
    % default module qualifiers in parse_implicitly_qualified_{term,symbol},
    % if the default module is the root module then we don't add any qualifier.
    %
:- pred root_module_name(module_name::out) is det.

root_module_name(unqualified("")).

%-----------------------------------------------------------------------------%

:- pred get_is_solver_type(is_solver_type::out,
    decl_attrs::in, decl_attrs::out) is det.

get_is_solver_type(IsSolverType, !Attributes) :-
    ( !.Attributes = [decl_attr_solver_type - _ | !:Attributes] ->
        IsSolverType = solver_type
    ;
        IsSolverType = non_solver_type
    ).

:- pred get_purity(purity::out, decl_attrs::in, decl_attrs::out) is det.

get_purity(Purity, !Attributes) :-
    ( !.Attributes = [decl_attr_purity(Purity0) - _ | !:Attributes] ->
        Purity = Purity0
    ;
        Purity = purity_pure
    ).

%-----------------------------------------------------------------------------%

:- func pred_or_func_decl_pieces(pred_or_func) = list(format_component).

pred_or_func_decl_pieces(pf_function) =
    [quote(":- func"), words("declaration")].
pred_or_func_decl_pieces(pf_predicate) =
    [quote(":- pred"), words("declaration")].

%-----------------------------------------------------------------------------%

:- type maker(T1, T2) == pred(T1, T2).
:- mode maker == (pred(in, out) is det).

:- pred process_maybe1(maker(T1, T2)::maker, maybe1(T1)::in, maybe1(T2)::out)
    is det.

process_maybe1(Maker, ok1(X), ok1(Y)) :-
    call(Maker, X, Y).
process_maybe1(_, error1(Specs), error1(Specs)).

:- pred process_maybe1_to_t(maker(T1, maybe1(T2))::maker,
    maybe1(T1)::in, maybe1(T2)::out) is det.

process_maybe1_to_t(Maker, ok1(X), Y) :-
    call(Maker, X, Y).
process_maybe1_to_t(_, error1(Specs), error1(Specs)).

%-----------------------------------------------------------------------------%

:- pred make_use(list(module_specifier)::in, module_defn::out) is det.

make_use(Syms, md_use(Syms)).

:- pred make_import(list(module_specifier)::in, module_defn::out) is det.

make_import(Syms, md_import(Syms)).

:- pred make_export(list(module_specifier)::in, module_defn::out) is det.

make_export(Syms, md_export(Syms)).

%-----------------------------------------------------------------------------%

:- pred make_name_arity_specifier(arity::in, sym_name::in,
    sym_name_specifier::out) is det.

make_name_arity_specifier(Arity, Name, name_arity(Name, Arity)).

:- pred make_name_specifier(sym_name::in, sym_name_specifier::out) is det.

make_name_specifier(Name, name(Name)).

%-----------------------------------------------------------------------------%

:- pred make_module_defn(maker(list(module_specifier), module_defn)::maker,
    prog_context::in, list(module_specifier)::in, item::out) is det.

make_module_defn(MakeModuleDefnPred, Context, ModuleSpecs, Item) :-
    call(MakeModuleDefnPred, ModuleSpecs, ModuleDefn),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
    Item = item_module_defn(ItemModuleDefn).

:- func make_pseudo_import_module_decl(prog_context, module_specifier) = item.

make_pseudo_import_module_decl(Context, ModuleSpecifier) = Item :-
    ModuleDefn = md_import([ModuleSpecifier]),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
    Item = item_module_defn(ItemModuleDefn).

:- func make_pseudo_use_module_decl(prog_context, module_specifier) = item.

make_pseudo_use_module_decl(Context, ModuleSpecifier) = Item :-
    ModuleDefn = md_use([ModuleSpecifier]),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
    Item = item_module_defn(ItemModuleDefn).

:- func make_pseudo_include_module_decl(prog_context, module_name) = item.

make_pseudo_include_module_decl(Context, ModuleSpecifier) = Item :-
    ModuleDefn = md_include_module([ModuleSpecifier]),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
    Item = item_module_defn(ItemModuleDefn).

:- pred make_type_defn(varset::in, condition::in, prog_context::in,
    processed_type_body::in, item::out) is det.

make_type_defn(VarSet0, Condition, Context, ProcessedTypeBody, Item) :-
    ProcessedTypeBody = processed_type_body(Name, Args, TypeDefn),
    varset.coerce(VarSet0, VarSet),
    ItemTypeDefn = item_type_defn_info(VarSet, Name, Args, TypeDefn,
        Condition, Context),
    Item = item_type_defn(ItemTypeDefn).

:- pred make_inst_defn(varset::in, condition::in, prog_context::in,
    processed_inst_body::in, item::out) is det.

make_inst_defn(VarSet0, Condition, Context, ProcessedInstBody, Item) :-
    ProcessedInstBody = processed_inst_body(Name, Params, InstDefn),
    varset.coerce(VarSet0, VarSet),
    ItemInstDefn = item_inst_defn_info(VarSet, Name, Params, InstDefn,
        Condition, Context),
    Item = item_inst_defn(ItemInstDefn).

:- pred make_mode_defn(varset::in, condition::in, prog_context::in,
    processed_mode_body::in, item::out) is det.

make_mode_defn(VarSet0, Condition, Context, ProcessedModeBody, Item) :-
    ProcessedModeBody = processed_mode_body(Name, Params, ModeDefn),
    varset.coerce(VarSet0, VarSet),
    ItemModeDefn = item_mode_defn_info(VarSet, Name, Params, ModeDefn,
        Condition, Context),
    Item = item_mode_defn(ItemModeDefn).

:- pred make_external(maybe(backend)::in, prog_context::in,
    sym_name_specifier::in, item::out) is det.

make_external(MaybeBackend, Context, SymSpec, Item) :-
    ModuleDefn = md_external(MaybeBackend, SymSpec),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context),
    Item = item_module_defn(ItemModuleDefn).

%-----------------------------------------------------------------------------%
%
% You can uncomment this section for debugging.
%

% :- interface.
%
% :- pred write_item_to_stream(io.output_stream::in, item::in, io::di, io::uo)
%     is det.
%
% :- pred write_item_to_stdout(item::in, io::di, io::uo) is det.
%
% :- pred write_items_to_file(string::in, list(item)::in, io::di, io::uo)
%     is det.
%
% :- implementation.
%
% :- import_module pretty_printer.
%
% write_item_to_stream(Stream, Item, !IO) :-
%     write_doc(Stream, format(Item), !IO),
%     io.nl(Stream, !IO).
%
% write_item_to_stdout(Item, !IO) :-
%     write_item_to_stream(io.stdout_stream, Item, !IO).
%
% write_items_to_file(FileName, Items, !IO) :-
%     io.open_output(FileName, Result, !IO),
%     (
%         Result = ok(Stream),
%         list.foldl(write_item_to_stream(Stream), Items, !IO)
%     ;
%         Result = error(_)
%     ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_io.m".

%-----------------------------------------------------------------------------%
