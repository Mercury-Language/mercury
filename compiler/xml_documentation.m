%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: xml_documentation.m.
% Main authors: petdr.
%
% This module outputs an XML representation of a module,
% which can then be transformed by a stylesheet into some other
% documentation format.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.xml_documentation.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module io.

    % Output a representation of the module in XML which can be used
    % to document the module.
    %
:- pred xml_documentation(module_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_class.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.source_file_map.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_to_xml.
:- import_module varset.

    % Record all the locations of comments in a file.
    %
:- type comments
    --->    comments(
                % For each line record what is on the line.
                line_types  :: map(int, line_type)
            ).

:- type line_type
    --->    blank
            % A line containing only whitespace.

    ;       comment(string)
            % A line containing just a comment.

    ;       code_and_comment(string)
            % A line which contains both a comment and code.

    ;       code.
            % A line containing code.

%-----------------------------------------------------------------------------%

xml_documentation(ModuleInfo, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, do_create_dirs, ".xml",
        ModuleName, FileName, !IO),

    lookup_module_source_file(ModuleName, MaybeSrcFileName, !IO),
    (
        MaybeSrcFileName = yes(SrcFileName),
        io.open_input(SrcFileName, SrcResult, !IO),
        (
            SrcResult = ok(SrcStream),
            build_comments(SrcStream, comments(map.init), Comments, !IO),

            % XXX We should find the ":- module " declaration
            % and get the comment from there.
            ModuleComment = get_comment_forwards(Comments, 1),

            io.open_output(FileName, OpenResult, !IO),
            (
                OpenResult = ok(Stream),
                MIXmlDoc = module_info_xml_doc(Comments, ModuleComment,
                    ModuleInfo),
                write_xml_doc(Stream, MIXmlDoc, !IO)
            ;
                OpenResult = error(Err),
                unable_to_open_file(FileName, Err, !IO)
            )
        ;
            SrcResult = error(SrcErr),
            unable_to_open_file(SrcFileName, SrcErr, !IO)
        )
    ;
        MaybeSrcFileName = no,
        unexpected($pred, "no source file name")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Given the input_stream build the comments datastructure which
    % represents this stream.
    %
:- pred build_comments(io.input_stream::in, comments::in, comments::out,
    io::di, io::uo) is det.

build_comments(S, comments(!.C), comments(!:C), !IO) :-
    io.get_line_number(S, LineNumber, !IO),
    io.read_line(S, LineResult, !IO),
    (
        LineResult = ok(Line),
        map.set(LineNumber, line_type(Line), !C),
        build_comments(S, comments(!.C), comments(!:C), !IO)
    ;
        LineResult = eof
    ;
        LineResult = error(E),
        % XXX we should recover more gracefully from this error.
        unexpected($pred, io.error_message(E))
    ).

    % Given a list of characters representing one line,
    % return the type of the line.
    %
    % Note this predicate is pretty stupid at the moment.
    % It only recognizes lines which contains % comments.
    % It also is confused by % characters in strings, etc. etc.
    %
:- func line_type(list(character)) = line_type.

line_type(Line) = LineType :-
    list.drop_while(char.is_whitespace, Line, Rest),
    list.take_while(is_not_comment_char, Rest, Decl, Comment),
    (
        Rest = [],
        LineType = blank
    ;
        Rest = [_ | _],
        (
            Comment = [],
            LineType = code
        ;
            Comment = [_ | _],
            (
                Decl = [],
                LineType = comment(string.from_char_list(Comment))
            ;
                Decl = [_ | _],
                LineType = code_and_comment(string.from_char_list(Comment))
            )
        )
    ).

:- pred is_not_comment_char(char::in) is semidet.

is_not_comment_char(C) :-
    C \= '%'.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% Comment selection strategies

    % If the prog_context given has a comment associated with it
    % add a child element which contains the comment to the given XML.
    %
:- func maybe_add_comment(comments, prog_context, xml) = xml.

maybe_add_comment(Comments, Context, Xml) =
    ( if Xml = elem(N, As, Cs) then
        ( if Comment = get_comment(Comments, Context), Comment \= "" then
            elem(N, As, [elem("comment", [], [data(Comment)]) | Cs])
        else
            Xml
        )
    else
        unexpected($pred, "not an element")
    ).

    % Get the comment string associated with the given prog_context.
    %
:- func get_comment(comments, prog_context) = string.

get_comment(Comments, context(_, Line)) =
    % XXX At a later date this hard-coded strategy should be made
    % more flexible. What I imagine is that the user would pass a string
    % saying in what order they wish to search for comments.
    ( if comment_on_current_line(Comments, Line, C) then
        C
    else if comment_directly_above(Comments, Line, C) then
        C
    else
        ""
    ).

%-----------------------------------------------------------------------------%

    % Succeeds if the current line has a comment.
    % The comment is extended with all the lines following
    % the current line which just contain a comment.
    %
:- pred comment_on_current_line(comments::in, int::in, string::out) is semidet.

comment_on_current_line(Comments, Line, Comment) :-
    map.search(Comments ^ line_types, Line, code_and_comment(Comment0)),
    RestComment = get_comment_forwards(Comments, Line + 1),
    Comment = Comment0 ++ RestComment.

    % Succeeds if the comment is directly above the current line.
    % The comment above ends when we find a line above the current line
    % which doesn't just contain a comment.
    %
:- pred comment_directly_above(comments::in, int::in, string::out) is semidet.

comment_directly_above(Comments, Line, Comment) :-
    map.search(Comments ^ line_types, Line - 1, comment(_)),
    Comment = get_comment_backwards(Comments, Line - 1).

    % Return the string which represents the comment starting at the given
    % line.  The comment ends when a line which is not a plain comment line
    % is found.
    %
:- func get_comment_forwards(comments, int) = string.

get_comment_forwards(Comments, Line) = Comment :-
    ( if map.search(Comments ^ line_types, Line, LineType) then
        (
            LineType = comment(CurrentComment),
            CommentBelow = get_comment_forwards(Comments, Line + 1),
            Comment = CurrentComment ++ CommentBelow
        ;
            ( LineType = blank
            ; LineType = code
            ; LineType = code_and_comment(_)
            ),
            Comment = ""
        )
    else
        Comment = ""
    ).

    % Return the string which represents the comment ending at the given line.
    % The comment extends backwards until the line above the given line is not
    % a comment only line.
    %
:- func get_comment_backwards(comments, int) = string.

get_comment_backwards(Comments, Line) = Comment :-
    ( if map.search(Comments ^ line_types, Line, LineType) then
        (
            LineType = comment(CurrentComment),
            CommentAbove = get_comment_backwards(Comments, Line - 1),
            Comment = CommentAbove ++ CurrentComment
        ;
            ( LineType = blank
            ; LineType = code
            ; LineType = code_and_comment(_)
            ),
            Comment = ""
        )
    else
        Comment = ""
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type module_info_xml_doc
    --->     module_info_xml_doc(comments, string, module_info).

:- instance xmlable(module_info_xml_doc) where [
    (to_xml(module_info_xml_doc(Comments, ModuleComment, ModuleInfo)) = Xml :-
        CommentXml = elem("comment", [], [data(ModuleComment)]),

        module_info_get_avail_module_map(ModuleInfo, AvailModuleMap),
        BuiltinModuleNames = set.list_to_set(all_builtin_modules),
        map.foldl(maybe_add_import_documentation(BuiltinModuleNames),
            AvailModuleMap, [], ImportsXml),
        ImportXml = elem("imports", [], ImportsXml),

        module_info_get_type_table(ModuleInfo, TypeTable),
        foldl_over_type_ctor_defns(type_documentation(Comments), TypeTable,
            [], TypeXmls),
        TypeXml = elem("types", [], TypeXmls),

        module_info_get_preds(ModuleInfo, PredTable),
        map.foldl(pred_documentation(Comments), PredTable, [], PredXmls),
        PredXml = elem("preds", [], PredXmls),

        module_info_get_class_table(ModuleInfo, ClassTable),
        map.foldl(class_documentation(Comments, PredTable), ClassTable,
            [], ClassXmls),
        ClassXml = elem("typeclasses", [], ClassXmls),

        Children = [CommentXml, ImportXml, TypeXml, PredXml, ClassXml],
        Xml = elem("module", [], Children)
    )
].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Output the documentation for one import_module or use_module declaration.
    %
:- pred maybe_add_import_documentation(set(module_name)::in,
    module_name::in, avail_module_entry::in, list(xml)::in, list(xml)::out)
    is det.

maybe_add_import_documentation(BuiltinModuleNames, ModuleName, AvailEntry,
        !Xmls) :-
    ( if set.member(ModuleName, BuiltinModuleNames) then
        true
    else
        XmlName = name_to_xml(ModuleName),
        AvailEntry = avail_module_entry(Section, ImportOrUse, _Avails),
        (
            Section = ms_interface,
            XmlVisibility = tagged_string("visibility", "interface")
        ;
            Section = ms_implementation,
            XmlVisibility = tagged_string("visibility", "implementation")
        ),
        (
            ImportOrUse = import_decl,
            ImportOrUseWord = "import"
        ;
            ImportOrUse = use_decl,
            ImportOrUseWord = "use"
        ),
        Xml = elem(ImportOrUseWord, [], [XmlName, XmlVisibility]),
        !:Xmls = [Xml | !.Xmls]
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Output the documentation of one type.
    %
:- pred type_documentation(comments::in, type_ctor::in, hlds_type_defn::in,
    list(xml)::in, list(xml)::out) is det.

type_documentation(C, type_ctor(TypeName, TypeArity), TypeDefn, !Xmls) :-
    get_type_defn_status(TypeDefn, TypeStatus),
    DefinedInThisModule = type_status_defined_in_this_module(TypeStatus),
    (
        DefinedInThisModule = yes,
        get_type_defn_body(TypeDefn, TypeBody),
        get_type_defn_tvarset(TypeDefn, TVarset),
        get_type_defn_context(TypeDefn, Context),
        get_type_defn_tparams(TypeDefn, TParams),

        XmlName = name_to_xml(TypeName),
        XmlTypeParams = xml_list("type_params", type_param_to_xml(TVarset),
            TParams),
        XmlVisibility = type_visibility_to_xml(TypeStatus),

        Tag = type_xml_tag(TypeBody),
        Id = attr("id", sym_name_and_arity_to_id("type", TypeName, TypeArity)),
        Children = [XmlName, XmlTypeParams, XmlVisibility,
                prog_context_to_xml(Context) |
                type_body_to_xml(C, TVarset, TypeBody)],
        Xml0 = elem(Tag, [Id], Children),
        Xml = maybe_add_comment(C, Context, Xml0),

        !:Xmls = [Xml | !.Xmls]
    ;
        DefinedInThisModule = no
    ).

:- func type_xml_tag(hlds_type_body) = string.

type_xml_tag(hlds_du_type(_, _, _, _)) = "du_type".
type_xml_tag(hlds_eqv_type(_)) = "eqv_type".
type_xml_tag(hlds_foreign_type(_)) = "foreign_type".
type_xml_tag(hlds_solver_type(_)) = "solver_type".
type_xml_tag(hlds_abstract_type(_)) = "abstract_type".

:- func type_param_to_xml(tvarset, type_param) = xml.

type_param_to_xml(TVarset, TVar) = Xml :-
    TVarName = varset.lookup_name(TVarset, TVar),
    Xml = tagged_string("type_variable", TVarName).

:- func type_body_to_xml(comments, tvarset, hlds_type_body) = list(xml).

type_body_to_xml(C, TVarSet, TypeDefnBody) = Xmls :-
    (
        TypeDefnBody = hlds_du_type(OoMCtors, _, _, _),
        Ctors = one_or_more_to_list(OoMCtors),
        Xmls =
            [xml_list("constructors", constructor_to_xml(C, TVarSet), Ctors)]
    ;
        TypeDefnBody = hlds_eqv_type(EqvType),
        Xmls = [elem("equivalent_type", [],
            [mer_type_to_xml(TVarSet, EqvType)])]
    ;
        TypeDefnBody = hlds_foreign_type(_),
        % XXX TODO
        Xmls = [nyi("hlds_foreign_type")]
    ;
        TypeDefnBody = hlds_solver_type(_),
        % XXX TODO
        Xmls = [nyi("hlds_solver_type")]
    ;
        TypeDefnBody = hlds_abstract_type(_),
        % XXX TODO
        Xmls = [nyi("hlds_abstract_type")]
    ).

:- func constructor_to_xml(comments, tvarset, constructor) = xml.

constructor_to_xml(C, TVarset, Ctor) = Xml :-
    Ctor = ctor(_Ordinal, MaybeExistConstraints, Name, Args, Arity, Context),
    (
        MaybeExistConstraints = no_exist_constraints,
        ExistQVars = [],
        Constraints = []
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints =
            cons_exist_constraints(ExistQVars, Constraints, _, _)
    ),
    Id = attr("id", sym_name_and_arity_to_id("ctor", Name, Arity)),
    XmlName = name_to_xml(Name),
    XmlContext = prog_context_to_xml(Context),
    XmlArgs = xml_list("ctor_args", constructor_arg_to_xml(C, TVarset), Args),
    XmlExistQVars = xml_list("ctor_exist_vars", type_param_to_xml(TVarset),
        ExistQVars),
    XmlConstraints =
        xml_list("ctor_constraints", prog_constraint_to_xml(TVarset),
            Constraints),
    Xml0 = elem("constructor", [Id],
        [XmlName, XmlContext, XmlArgs, XmlExistQVars, XmlConstraints]),
    Xml = maybe_add_comment(C, Context, Xml0).

:- func constructor_arg_to_xml(comments, tvarset, constructor_arg) = xml.

constructor_arg_to_xml(C, TVarset, CtorArg) = Xml :-
    CtorArg = ctor_arg(MaybeCtorFieldName, Type, Context),
    XmlType = elem("arg_type", [], [mer_type_to_xml(TVarset, Type)]),
    XmlContext = prog_context_to_xml(Context),
    (
        MaybeCtorFieldName = yes(ctor_field_name(FieldName, _FieldNameCtxt)),
        Id = attr("id", sym_name_to_id("field", FieldName)),
        XmlMaybeFieldName = [elem("field", [Id], [name_to_xml(FieldName)])]
    ;
        MaybeCtorFieldName = no,
        XmlMaybeFieldName = []
    ),
    Xml0 = elem("ctor_arg", [], [XmlType, XmlContext | XmlMaybeFieldName]),
    Xml = maybe_add_comment(C, Context, Xml0).

:- func mer_type_to_xml(tvarset, mer_type) = xml.

mer_type_to_xml(TVarset, type_variable(TVar, _)) =
    type_param_to_xml(TVarset, TVar).
mer_type_to_xml(TVarset, defined_type(TypeName, Args, _)) = Xml :-
    Ref = attr("ref",
        sym_name_and_arity_to_id("type", TypeName, length(Args))),
    XmlName = name_to_xml(TypeName),
    XmlArgs = xml_list("type_args", mer_type_to_xml(TVarset), Args),
    Xml = elem("type", [Ref], [XmlName, XmlArgs]).
mer_type_to_xml(_, builtin_type(builtin_type_int(IntType))) = Xml :-
    int_type_to_string(IntType, IntTypeString),
    Xml = elem(IntTypeString, [], []).
mer_type_to_xml(_, builtin_type(builtin_type_float)) = elem("float", [], []).
mer_type_to_xml(_, builtin_type(builtin_type_string)) = elem("string", [], []).
mer_type_to_xml(_, builtin_type(builtin_type_char)) =
    elem("character", [], []).
mer_type_to_xml(TVarset, higher_order_type(PorF, Types, _, _, _)) = Xml :-
    (
        PorF = pf_predicate,
        XmlTypes = xml_list("higher_order_type_args", mer_type_to_xml(TVarset),
            Types),
        XmlChildren = [XmlTypes]
    ;
        PorF = pf_function,
        list.det_split_last(Types, ArgTypes, ResultType),
        XmlTypes = xml_list("higher_order_type_args", mer_type_to_xml(TVarset),
            ArgTypes),
        XmlReturn = elem("return_type", [],
            [mer_type_to_xml(TVarset, ResultType)]),
        XmlChildren = [XmlTypes, XmlReturn]
    ),
    Xml = elem("higher_order_type", [], XmlChildren).
mer_type_to_xml(TVarset, tuple_type(Types, _)) = Xml :-
    XmlArgs = xml_list("tuple_types", mer_type_to_xml(TVarset), Types),
    Xml = elem("tuple", [], [XmlArgs]).
mer_type_to_xml(_, apply_n_type(_, _, _)) = nyi("apply_n_type").
mer_type_to_xml(_, kinded_type(_, _)) = nyi("kinded_type").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred pred_documentation(comments::in, pred_id::in, pred_info::in,
    list(xml)::in, list(xml)::out) is det.

pred_documentation(C, _PredId, PredInfo, !Xml) :-
    pred_info_get_status(PredInfo, PredStatus),
    pred_info_get_origin(PredInfo, Origin),
    pred_info_get_markers(PredInfo, Markers),

    ( if
        pred_status_defined_in_this_module(PredStatus) = yes,
        Origin = origin_user(_),
        not check_marker(Markers, marker_class_method)
    then
        Xml = predicate_documentation(C, PredInfo),
        !:Xml = [Xml | !.Xml]
    else
        true
    ).

:- func predicate_documentation(comments, pred_info) = xml.

predicate_documentation(C, PredInfo) = Xml :-
    pred_info_get_typevarset(PredInfo, TVarset),
    pred_info_get_exist_quant_tvars(PredInfo, Exists),

    IsPredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    PredName = qualified(Module, Name),
    Arity = pred_info_orig_arity(PredInfo),
    pred_info_get_status(PredInfo, PredStatus),

    Types = get_orig_arg_types(PredInfo),
    pred_info_get_class_context(PredInfo, Constraints),
    pred_info_get_context(PredInfo, Context),
    (
        IsPredOrFunc = pf_predicate,
        Tag = "predicate"
    ;
        IsPredOrFunc = pf_function,
        Tag = "function"
    ),
    Id = sym_name_and_arity_to_id(Tag, PredName, Arity),

    XmlName = name_to_xml(PredName),
    XmlContext = prog_context_to_xml(Context),
    XmlTypes = xml_list("pred_types", mer_type_to_xml(TVarset), Types),
    XmlExistVars = xml_list("pred_exist_vars", type_param_to_xml(TVarset),
        Exists),
    XmlConstraints = prog_constraints_to_xml(TVarset, Constraints),
    XmlVisibility = pred_visibility_to_xml(PredStatus),

    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl(pred_mode_documentation(C), ProcTable, [], XmlProcs),
    XmlModes = elem("pred_modes", [], XmlProcs),

    Xml0 = elem(Tag, [attr("id", Id)],
        [XmlName, XmlTypes, XmlContext,
        XmlExistVars, XmlConstraints, XmlVisibility, XmlModes]),

    Xml = maybe_add_comment(C, Context, Xml0).

:- func get_orig_arg_types(pred_info) = list(mer_type).

get_orig_arg_types(PredInfo) = Types :-
    pred_info_get_arg_types(PredInfo, Types0),
    Types = keep_last_n(pred_info_orig_arity(PredInfo), Types0).

:- import_module require.

:- func keep_last_n(int, list(T)) = list(T).

keep_last_n(N, L0) =
    ( if list.drop(list.length(L0) - N, L0, L) then
        L
    else
        func_error("keep_last_n")
    ).

:- func prog_constraints_to_xml(tvarset, prog_constraints) = xml.

prog_constraints_to_xml(TVarset, constraints(Univs, Exists)) = Xml :-
    XmlUnivs = xml_list("pred_universal",
        prog_constraint_to_xml(TVarset), Univs),
    XmlExists = xml_list("pred_exist",
        prog_constraint_to_xml(TVarset), Exists),
    Xml = elem("pred_constraints", [], [XmlUnivs, XmlExists]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred pred_mode_documentation(comments::in, proc_id::in, proc_info::in,
    list(xml)::in, list(xml)::out) is det.

pred_mode_documentation(_C, _ProcId, ProcInfo, !Xml) :-
    % XXX do we ever need to remove arguments here?
    proc_info_get_inst_varset(ProcInfo, IVarSet),
    proc_info_declared_argmodes(ProcInfo, Modes),
    proc_info_interface_determinism(ProcInfo, Determinism),

    XmlModes = xml_list("modes", mer_mode_to_xml(IVarSet), Modes),
    XmlDet = determinism_to_xml(Determinism),
    Xml = elem("pred_mode", [], [XmlModes, XmlDet]),

    !:Xml = [Xml | !.Xml].

:- func mer_mode_to_xml(inst_varset, mer_mode) = xml.

mer_mode_to_xml(InstVarSet, Mode) = Xml :-
    (
        Mode = from_to_mode(A, B),
        XmlFrom = xml_list("from", mer_inst_to_xml(InstVarSet), [A]),
        XmlTo = xml_list("to", mer_inst_to_xml(InstVarSet), [B]),
        Xml = elem("inst_to_inst", [], [XmlFrom, XmlTo])
    ;
        Mode = user_defined_mode(Name, Args),
        Ref = attr("ref",
            sym_name_and_arity_to_id("mode", Name, length(Args))),
        XmlArgs = xml_list("mode_args", mer_inst_to_xml(InstVarSet), Args),
        Xml = elem("user_defined_mode", [Ref], [name_to_xml(Name), XmlArgs])
    ).

:- func mer_inst_to_xml(inst_varset, mer_inst) = xml.

mer_inst_to_xml(InstVarSet, Inst) = Xml :-
    (
        Inst = free,
        Xml = elem("free", [], [])
    ;
        Inst = free(_),
        Xml = elem("free", [], [])
    ;
        Inst = bound(U, _, BoundInsts),
        XmlUniq = uniqueness_to_xml(U),
        XmlInsts = xml_list("bound_insts", bound_inst_to_xml(InstVarSet),
            BoundInsts),
        Xml = elem("bound", [], [XmlUniq, XmlInsts])
    ;
        Inst = ground(U, _),
        Xml = elem("ground", [], [uniqueness_to_xml(U)])
    ;
        Inst = any(U, _),
        Xml = elem("any", [], [uniqueness_to_xml(U)])
    ;
        Inst = not_reached,
        Xml = elem("not_reached", [], [])
    ;
        Inst = inst_var(InstVar),
        InstVarName = varset.lookup_name(InstVarSet, InstVar),
        Xml = tagged_string("inst_var", InstVarName)
    ;
        Inst = constrained_inst_vars(_, SubInst),
        % XXX We do we ignore the constraint?
        Xml = mer_inst_to_xml(InstVarSet, SubInst)
    ;
        Inst = defined_inst(Name),
        XmlName = inst_name_to_xml(InstVarSet, Name),
        Xml = elem("defined_inst", [], [XmlName])
    ;
        Inst = abstract_inst(SymName, ArgInsts),
        Xml = mer_inst_to_xml(InstVarSet,
            defined_inst(user_inst(SymName, ArgInsts)))
    ).

:- func inst_name_to_xml(inst_varset, inst_name) = xml.

inst_name_to_xml(InstVarSet, user_inst(Name, Insts)) = Xml :-
    Ref = attr("ref", sym_name_and_arity_to_id("inst", Name, length(Insts))),
    XmlName = name_to_xml(Name),
    XmlInsts = xml_list("inst_args", mer_inst_to_xml(InstVarSet), Insts),
    Xml = elem("user_inst", [Ref], [XmlName, XmlInsts]).
inst_name_to_xml(_, unify_inst(_, _, _, _)) = nyi("unify_inst").
inst_name_to_xml(_, merge_inst(_, _)) = nyi("merge_inst").
inst_name_to_xml(_, ground_inst(_, _, _, _)) = nyi("ground_inst").
inst_name_to_xml(_, any_inst(_, _, _, _)) = nyi("any_inst").
inst_name_to_xml(_, shared_inst(_)) = nyi("shared_inst").
inst_name_to_xml(_, mostly_uniq_inst(_)) = nyi("mostly_uniq_inst").
inst_name_to_xml(_, typed_ground(_, _)) = nyi("typed_ground").
inst_name_to_xml(_, typed_inst(_, _)) = nyi("typed_inst").

:- func uniqueness_to_xml(uniqueness) = xml.

uniqueness_to_xml(U) = tagged_string("uniqueness", string(U)).

:- func bound_inst_to_xml(inst_varset, bound_inst) = xml.

bound_inst_to_xml(InstVarSet, bound_functor(ConsId, Insts)) = Xml :-
    XmlCons = cons_id_to_xml(ConsId),
    XmlInsts = xml_list("insts", mer_inst_to_xml(InstVarSet), Insts),
    Xml = elem("bound_functor", [], [XmlCons, XmlInsts]).

:- func cons_id_to_xml(cons_id) = xml.

cons_id_to_xml(cons(Name, Arity, _)) =
    elem("cons", [], [name_to_xml(Name), arity_to_xml(Arity)]).
% XXX We could do better for tuple_cons and closure_cons.
% The return values here are just a continuation of what we used to do.
cons_id_to_xml(tuple_cons(Arity)) =
    elem("cons", [], [name_to_xml(unqualified("{}")), arity_to_xml(Arity)]).
cons_id_to_xml(int_const(I)) = tagged_int("int", I).
cons_id_to_xml(uint_const(_)) = _ :-
    unexpected($file, $pred, "NYI uint").
cons_id_to_xml(int8_const(_)) = _ :-
    unexpected($file, $pred, "NYI int8").
cons_id_to_xml(uint8_const(_)) = _ :-
    unexpected($file, $pred, "NYI uint8").
cons_id_to_xml(int16_const(_)) = _ :-
    unexpected($file, $pred, "NYI int16").
cons_id_to_xml(uint16_const(_)) = _ :-
    unexpected($file, $pred, "NYI uint16").
cons_id_to_xml(int32_const(_)) = _ :-
    unexpected($file, $pred, "NYI int32").
cons_id_to_xml(uint32_const(_)) = _ :-
    unexpected($file, $pred, "NYI uint32").
cons_id_to_xml(int64_const(_)) = _ :-
    unexpected($file, $pred, "NYI int64").
cons_id_to_xml(uint64_const(_)) = _ :-
    unexpected($file, $pred, "NYI uint64").
cons_id_to_xml(float_const(F)) = tagged_float("float", F).
cons_id_to_xml(char_const(C)) = tagged_char("char", C).
cons_id_to_xml(string_const(S)) = tagged_string("string", S).
cons_id_to_xml(impl_defined_const(_)) = nyi("impl_defined_const").
cons_id_to_xml(closure_cons(_, _)) = nyi("closure_cons").
cons_id_to_xml(type_ctor_info_const(_, _, _)) = nyi("type_ctor_info_const").
cons_id_to_xml(base_typeclass_info_const(_,_,_,_)) =
    nyi("base_typeclass_info_const").
cons_id_to_xml(type_info_cell_constructor(_)) =
    nyi("type_info_cell_constructor").
cons_id_to_xml(typeclass_info_cell_constructor) =
    nyi("typeclass_info_cell_constructor").
cons_id_to_xml(type_info_const(_)) = nyi("type_info_const").
cons_id_to_xml(typeclass_info_const(_)) = nyi("typeclass_info_const").
cons_id_to_xml(ground_term_const(_, _)) = nyi("ground_term_const").
cons_id_to_xml(tabling_info_const(_)) = nyi("tabling_info_const").
cons_id_to_xml(table_io_entry_desc(_)) = nyi("table_io_entry_desc").
cons_id_to_xml(deep_profiling_proc_layout(_)) =
    nyi("deep_profiling_proc_layout").

:- func arity_to_xml(int) = xml.

arity_to_xml(Arity) = tagged_int("arity", Arity).

:- func determinism_to_xml(determinism) = xml.

determinism_to_xml(detism_det) = tagged_string("determinism", "det").
determinism_to_xml(detism_semi) = tagged_string("determinism", "semidet").
determinism_to_xml(detism_multi) = tagged_string("determinism", "multi").
determinism_to_xml(detism_non) = tagged_string("determinism", "nondet").
determinism_to_xml(detism_cc_non) = tagged_string("determinism", "cc_nondet").
determinism_to_xml(detism_cc_multi) = tagged_string("determinism", "cc_multi").
determinism_to_xml(detism_erroneous) =
    tagged_string("determinism", "erroneous").
determinism_to_xml(detism_failure) = tagged_string("determinism", "failure").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred class_documentation(comments::in, pred_table::in,
    class_id::in, hlds_class_defn::in,
    list(xml)::in, list(xml)::out) is det.

class_documentation(C, PredTable, class_id(Name, Arity), ClassDefn, !Xml) :-
    TypeClassStatus = ClassDefn ^ classdefn_status,
    DefinedInThisModule =
        typeclass_status_defined_in_this_module(TypeClassStatus),
    (
        DefinedInThisModule = yes,
        Id = sym_name_and_arity_to_id("class", Name, Arity),

        Context = ClassDefn ^ classdefn_context,
        TVarset = ClassDefn ^ classdefn_tvarset,
        Vars = ClassDefn ^ classdefn_vars,

        XmlName = name_to_xml(Name),
        XmlClassVars = xml_list("class_vars",
            type_param_to_xml(TVarset), Vars),
        XmlSupers = xml_list("superclasses",
            prog_constraint_to_xml(TVarset), ClassDefn ^ classdefn_supers),
        XmlFundeps = xml_list("fundeps",
            fundep_to_xml(TVarset, Vars), ClassDefn ^ classdefn_fundeps),
        XmlMethods = class_methods_to_xml(C, PredTable,
            ClassDefn ^ classdefn_hlds_interface),
        XmlVisibility = typeclass_visibility_to_xml(TypeClassStatus),
        XmlContext = prog_context_to_xml(Context),

        Xml0 = elem("typeclass", [attr("id", Id)],
            [XmlName, XmlClassVars, XmlSupers,
            XmlFundeps, XmlMethods, XmlVisibility, XmlContext]),

        Xml = maybe_add_comment(C, Context, Xml0),

        !:Xml = [Xml | !.Xml]
    ;
        DefinedInThisModule = no
    ).

:- func fundep_to_xml(tvarset, list(tvar), hlds_class_fundep) = xml.

fundep_to_xml(TVarset, Vars, fundep(Domain, Range)) = Xml :-
    XmlDomain = fundep_to_xml_2("domain", TVarset, Vars, Domain),
    XmlRange = fundep_to_xml_2("range", TVarset, Vars, Range),
    Xml = elem("fundep", [], [XmlDomain, XmlRange]).

:- func fundep_to_xml_2(string, tvarset, list(tvar), set(hlds_class_argpos))
    = xml.

fundep_to_xml_2(Tag, TVarset, Vars, Set) =
    xml_list(Tag, type_param_to_xml(TVarset),
        restrict_list_elements(Set, Vars)).

:- func class_methods_to_xml(comments, pred_table, hlds_class_interface) = xml.

class_methods_to_xml(C, PredTable, Methods) = Xml :-
    AllPredIds = list.map(pred_proc_id_project_pred_id, Methods),
    PredIds = list.sort_and_remove_dups(AllPredIds),
    PredInfos = list.map(func(Id) = map.lookup(PredTable, Id), PredIds),
    Xml = xml_list("methods", predicate_documentation(C), PredInfos).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func name_to_xml(sym_name) = xml.

name_to_xml(unqualified(Name)) = tagged_string("unqualified", Name).
name_to_xml(qualified(Module, Name)) =
    elem("qualified", [], [
        tagged_string("module", sym_name_to_string(Module)),
        tagged_string("name", Name)]).

%-----------------------------------------------------------------------------%

:- func prog_context_to_xml(prog_context) = xml.

prog_context_to_xml(context(FileName, LineNumber)) =
    elem("context", [], [
        tagged_string("filename", FileName),
        tagged_int("line", LineNumber)]).

%-----------------------------------------------------------------------------%

:- func prog_constraint_to_xml(tvarset, prog_constraint) = xml.

prog_constraint_to_xml(TVarset, Constraint) = Xml :-
    Constraint = constraint(ClassName, ArgTypes),
    Id = sym_name_and_arity_to_id("constraint", ClassName,
        list.length(ArgTypes)),
    XmlName = name_to_xml(ClassName),
    XmlTypes = xml_list("constraint_types", mer_type_to_xml(TVarset),
        ArgTypes),
    Xml = elem("constraint", [attr("ref", Id)], [XmlName, XmlTypes]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func type_visibility_to_xml(type_status) = xml.

type_visibility_to_xml(Status) = tagged_string("visibility", Visibility) :-
    ( if type_status_defined_in_impl_section(Status) = yes then
        ( if Status = type_status(status_abstract_exported) then
            Visibility = "abstract"
        else
            Visibility = "implementation"
        )
    else
        Visibility = "interface"
    ).

:- func inst_visibility_to_xml(inst_status) = xml.
:- pragma consider_used(inst_visibility_to_xml/1).

inst_visibility_to_xml(Status) = tagged_string("visibility", Visibility) :-
    ( if inst_status_defined_in_impl_section(Status) = yes then
        Visibility = "implementation"
    else
        Visibility = "interface"
    ).

:- func mode_visibility_to_xml(mode_status) = xml.
:- pragma consider_used(mode_visibility_to_xml/1).

mode_visibility_to_xml(Status) = tagged_string("visibility", Visibility) :-
    ( if mode_status_defined_in_impl_section(Status) = yes then
        Visibility = "implementation"
    else
        Visibility = "interface"
    ).

:- func typeclass_visibility_to_xml(typeclass_status) = xml.

typeclass_visibility_to_xml(Status) =
        tagged_string("visibility", Visibility) :-
    ( if typeclass_status_defined_in_impl_section(Status) = yes then
        ( if Status = typeclass_status(status_abstract_exported) then
            Visibility = "abstract"
        else
            Visibility = "implementation"
        )
    else
        Visibility = "interface"
    ).

:- func instance_visibility_to_xml(instance_status) = xml.
:- pragma consider_used(instance_visibility_to_xml/1).

instance_visibility_to_xml(Status) = tagged_string("visibility", Visibility) :-
    ( if instance_status_defined_in_impl_section(Status) = yes then
        ( if Status = instance_status(status_abstract_exported) then
            Visibility = "abstract"
        else
            Visibility = "implementation"
        )
    else
        Visibility = "interface"
    ).

:- func pred_visibility_to_xml(pred_status) = xml.

pred_visibility_to_xml(Status) = tagged_string("visibility", Visibility) :-
    ( if pred_status_defined_in_impl_section(Status) = yes then
        ( if Status = pred_status(status_abstract_exported) then
            Visibility = "abstract"
        else
            Visibility = "implementation"
        )
    else
        Visibility = "interface"
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % sym_name_to_id(P, S) converts the sym_name, S, into
    % a string with prefix, P, prefixed to the generated name.
    %
:- func sym_name_to_id(string, sym_name) = string.

sym_name_to_id(Prefix, Name) = prefixed_sym_name(Prefix, Name).

    % sym_name_to_id(P, S, A) converts the sym_name, S, with
    % arity, A, into a string with prefix, P, prefixed to the
    % generated name.
    %
:- func sym_name_and_arity_to_id(string, sym_name, int) = string.

sym_name_and_arity_to_id(Prefix, Name, Arity) =
    prefixed_sym_name(Prefix, Name) ++ "-" ++ int_to_string(Arity).

:- func prefixed_sym_name(string, sym_name) = string.

prefixed_sym_name(Prefix, Name) = Prefix ++ "." ++ sym_name_to_string(Name).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func tagged_int(string, int) = xml.

tagged_int(E, I) = elem(E, [], [data(int_to_string(I))]).

:- func tagged_float(string, float) = xml.

tagged_float(E, F) = elem(E, [], [data(float_to_string(F))]).

:- func tagged_char(string, char) = xml.

tagged_char(E, C) = elem(E, [], [data(char_to_string(C))]).

:- func tagged_string(string, string) = xml.

tagged_string(E, S) = elem(E, [], [data(S)]).

%-----------------------------------------------------------------------------%

:- func xml_list(string, func(T) = xml, list(T)) = xml.

xml_list(Tag, F, L) = elem(Tag, [], list.map(F, L)).

%-----------------------------------------------------------------------------%

:- func nyi(string) = xml.

nyi(Tag) = tagged_string(Tag, "Not yet implemented!").

%-----------------------------------------------------------------------------%
:- end_module check_hlds.xml_documentation.
%-----------------------------------------------------------------------------%
