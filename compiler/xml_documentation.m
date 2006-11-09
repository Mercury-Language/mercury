%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: xml_documentation.m
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

    %
    % Output a representation of the module in XML which can be used
    % to document the module.
    %
:- pred xml_documentation(module_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.source_file_map.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module term_to_xml.
:- import_module varset.

    %
    % Record all the locations of comments in a file.
    %
:- type comments
    --->    comments(
                    % For each line record what is on the line.
                line_types  :: map(int, line_type)
            ).

:- type line_type
                % A line containing only whitespace.
    --->    blank

                % A line containing just a comment.
    ;       comment(string)

                % A line which contains both a comment and code.
    ;       code_and_comment(string)

                % A line containing code.
    ;       code
    .

%-----------------------------------------------------------------------------%

xml_documentation(ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(ModuleName, ".xml", no, FileName, !IO),

    lookup_module_source_file(ModuleName, SrcFileName, !IO),
    io.open_input(SrcFileName, SrcResult, !IO),
    (
        SrcResult = ok(SrcStream),
        build_comments(SrcStream, comments(map.init), Comments, !IO),

            %
            % XXX We should find the ":- module " declaration
            % and get the comment from there.
            %
        ModuleComment = get_comment_forwards(Comments, 1),

        io.open_output(FileName, OpenResult, !IO),
        (
            OpenResult = ok(Stream),
            MIXmlDoc = module_info_xml_doc(Comments, ModuleComment, ModuleInfo),
            write_xml_doc(Stream, MIXmlDoc, !IO)
        ;
            OpenResult = error(Err),
            unable_to_open_file(FileName, Err, !IO)
        )
    ;
        SrcResult = error(SrcErr),
        unable_to_open_file(SrcFileName, SrcErr, !IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    %
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
        svmap.set(LineNumber, line_type(Line), !C),
        build_comments(S, comments(!.C), comments(!:C), !IO)
    ;
        LineResult = eof,
        true
    ;
        LineResult = error(E),
            % XXX we should recover more gracefully from this error.
        unexpected(this_file, io.error_message(E))
    ).
    
    %
    % Given a list of characters representing one line
    % return the type of the line.
    %
    % Note this predicate is pretty stupid at the moment.
    % It only recognizes lines which contains % comments.
    % It also is confused by % characters in strings, etc. etc.
    %
:- func line_type(list(character)) = line_type.

line_type(Line) = LineType :-
    list.takewhile(char.is_whitespace, Line, _WhiteSpace, Rest),
    list.takewhile(is_not_comment_char, Rest, Decl, Comment),
    ( Rest = [] ->
        LineType = blank
    ; Comment = [_ | _] ->
        ( Decl = [],
            LineType = comment(string.from_char_list(Comment))
        ; Decl = [_ | _],
            LineType = code_and_comment(string.from_char_list(Comment))
        )
    ;
        LineType = code
    ).

:- pred is_not_comment_char(char::in) is semidet.

is_not_comment_char(C) :-
    C \= '%'.
    
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% Comment selection strategies

    %
    % If the prog_context given has a comment associated with it
    % add a child element which contains the comment to the
    % given XML.
    %
:- func maybe_add_comment(comments, prog_context, xml) = xml.

maybe_add_comment(Comments, Context, Xml) =
    ( Xml = elem(N, As, Cs) ->
        ( Comment = get_comment(Comments, Context), Comment \= "" ->
            elem(N, As, [elem("comment", [], [data(Comment)]) | Cs])
        ;
            Xml
        )
    ;
        unexpected(this_file, "maybe_add_comment: not an element")
    ).
    
    %
    % Get the comment string associated with the given prog_context.
    %
:- func get_comment(comments, prog_context) = string.

get_comment(Comments, context(_, Line)) =
        %
        % XXX at a later date this hard-coded strategy should
        % be made more flexible.  What I imagine is that the
        % user would pass a string saying in what order
        % they wish to search for comments.
        %
    ( comment_on_current_line(Comments, Line, C) ->
        C
    ; comment_directly_above(Comments, Line, C) ->
        C
    ;
        ""
    ).

%-----------------------------------------------------------------------------%

    %
    % Succeeds if the current line has a comment.
    % The comment is extended with all the lines following
    % the current line which just contain a comment.
    %
:- pred comment_on_current_line(comments::in, int::in, string::out) is semidet.

comment_on_current_line(Comments, Line, Comment) :-
    map.search(Comments ^ line_types, Line, code_and_comment(Comment0)),
    RestComment = get_comment_forwards(Comments, Line + 1),
    Comment = Comment0 ++ RestComment.

    %
    % Succeeds if the comment is directly above the current line.
    % The comment above ends when we find a line above the current
    % line which doesn't just contain a comment.
    %
:- pred comment_directly_above(comments::in, int::in, string::out) is semidet.

comment_directly_above(Comments, Line, Comment) :-
    map.search(Comments ^ line_types, Line - 1, comment(_)),
    Comment = get_comment_backwards(Comments, Line - 1).

    %
    % Return the string which represents the comment starting at the given
    % line.  The comment ends when a line which is not a plain comment line
    % is found.
    %
:- func get_comment_forwards(comments, int) = string.

get_comment_forwards(Comments, Line) = Comment :-
    LineType = map.lookup(Comments ^ line_types, Line),
    (
        LineType = comment(CurrentComment),
        CommentBelow = get_comment_backwards(Comments, Line + 1),
        Comment = CurrentComment ++ CommentBelow
    ;
        ( LineType = blank
        ; LineType = code
        ; LineType = code_and_comment(_)
        ),
        Comment = ""
    ).

    %
    % Return the string which represents the comment ending at the given line.
    % The comment extends backwards until the the line above the given
    % line is not a comment only line.
    %
:- func get_comment_backwards(comments, int) = string.

get_comment_backwards(Comments, Line) = Comment :-
    LineType = map.lookup(Comments ^ line_types, Line),
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
    ).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type module_info_xml_doc
    --->     module_info_xml_doc(comments, string, module_info).

:- instance xmlable(module_info_xml_doc) where [
    (to_xml(module_info_xml_doc(Comments, ModuleComment, ModuleInfo)) = Xml :-
        CommentXml = elem("comment", [], [data(ModuleComment)]),

        module_info_get_type_table(ModuleInfo, TypeTable),
        map.foldl(type_documentation(Comments), TypeTable, [], TypeXmls),
        TypeXml = elem("types", [], TypeXmls),

        module_info_preds(ModuleInfo, PredTable),
        map.foldl(pred_documentation(Comments), PredTable, [], PredXmls),
        PredXml = elem("preds", [], PredXmls),

        module_info_get_class_table(ModuleInfo, ClassTable),
        map.foldl(class_documentation(Comments, PredTable),
                ClassTable, [], ClassXmls),
        ClassXml = elem("typeclasses", [], ClassXmls),

        Xml = elem("module", [], [CommentXml, TypeXml, PredXml, ClassXml])
    )
].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    %
    % Output the documentation of one type.
    %
:- pred type_documentation(comments::in, type_ctor::in, hlds_type_defn::in,
    list(xml)::in, list(xml)::out) is det.

type_documentation(C, type_ctor(TypeName, TypeArity), TypeDefn, !Xmls) :-
    get_type_defn_status(TypeDefn, ImportStatus),

    ( status_defined_in_this_module(ImportStatus) = yes ->
        get_type_defn_body(TypeDefn, TypeBody),
        get_type_defn_tvarset(TypeDefn, TVarset),
        get_type_defn_context(TypeDefn, Context),
        get_type_defn_tparams(TypeDefn, TParams),

        XmlName = name(TypeName),
        XmlTypeParams = xml_list("type_params", type_param(TVarset), TParams),
        XmlVisibility = visibility(ImportStatus),

        Tag = type_xml_tag(TypeBody),
        Id = attr("id", sym_name_and_arity_to_id("type", TypeName, TypeArity)),
        Children = [XmlName, XmlTypeParams, XmlVisibility,
                prog_context(Context) | type_body(C, TVarset, TypeBody)],
        Xml0 = elem(Tag, [Id], Children),
        Xml = maybe_add_comment(C, Context, Xml0),

        !:Xmls = [Xml | !.Xmls]
    ;
        true
    ).

:- func type_xml_tag(hlds_type_body) = string.

type_xml_tag(hlds_du_type(_, _, _, _, _, _)) = "du_type".
type_xml_tag(hlds_eqv_type(_)) = "eqv_type".
type_xml_tag(hlds_foreign_type(_)) = "foreign_type".
type_xml_tag(hlds_solver_type(_, _)) = "solver_type".
type_xml_tag(hlds_abstract_type(_)) = "abstract_type".

:- func type_param(tvarset, type_param) = xml.

type_param(TVarset, TVar) = Xml :-
    TVarName = varset.lookup_name(TVarset, TVar),
    Xml = tagged_string("type_variable", TVarName).

:- func type_body(comments, tvarset, hlds_type_body) = list(xml).

type_body(C, TVarset, hlds_du_type(Ctors, _, _, _, _, _)) =
    [xml_list("constructors", constructor(C, TVarset), Ctors)].
type_body(_, TVarset, hlds_eqv_type(Type)) =
    [elem("equivalent_type", [], [mer_type(TVarset, Type)])].

    % XXX TODO
type_body(_, _, hlds_foreign_type(_)) = [nyi("hlds_foreign_type")].
type_body(_, _, hlds_solver_type(_, _)) = [nyi("hlds_solver_type")].
type_body(_, _, hlds_abstract_type(_)) = [nyi("hlds_abstract_type")].


:- func constructor(comments, tvarset, constructor) = xml.

constructor(C, TVarset,
        ctor(Exists, Constraints, Name, Args, Context)) = Xml :-
    Id = attr("id", sym_name_and_arity_to_id("ctor", Name, length(Args))),
    XmlName = name(Name),
    XmlContext = prog_context(Context),
    XmlArgs = xml_list("ctor_args", constructor_arg(C, TVarset), Args),
    XmlExistVars = xml_list("ctor_exist_vars", type_param(TVarset), Exists),
    XmlConstraints =
            xml_list("ctor_constraints", prog_constraint(TVarset), Constraints),
    Xml0 = elem("constructor", [Id],
            [XmlName, XmlContext, XmlArgs, XmlExistVars, XmlConstraints]),
    Xml = maybe_add_comment(C, Context, Xml0).

:- func constructor_arg(comments, tvarset, constructor_arg) = xml.

constructor_arg(C, TVarset, ctor_arg(MaybeFieldName, Type, Context)) = Xml :-
    XmlType = elem("arg_type", [], [mer_type(TVarset, Type)]),
    XmlContext = prog_context(Context),
    (
        MaybeFieldName = yes(FieldName),
        Id = attr("id", sym_name_to_id("field", FieldName)),
        XmlMaybeFieldName = [elem("field", [Id], [name(FieldName)])]
    ;
        MaybeFieldName = no,
        XmlMaybeFieldName = []
    ),

    Xml0 = elem("ctor_arg", [], [XmlType, XmlContext | XmlMaybeFieldName]),
    Xml = maybe_add_comment(C, Context, Xml0).

:- func mer_type(tvarset, mer_type) = xml.

mer_type(TVarset, type_variable(TVar, _)) = type_param(TVarset, TVar).
mer_type(TVarset, defined_type(TypeName, Args, _)) = Xml :-
    Ref = attr("ref", sym_name_and_arity_to_id("type", TypeName, length(Args))),
    XmlName = name(TypeName),
    XmlArgs = xml_list("type_args", mer_type(TVarset), Args),
    Xml = elem("type", [Ref], [XmlName, XmlArgs]).
mer_type(_, builtin_type(builtin_type_int)) = elem("int", [], []).
mer_type(_, builtin_type(builtin_type_float)) = elem("float", [], []).
mer_type(_, builtin_type(builtin_type_string)) = elem("string", [], []).
mer_type(_, builtin_type(builtin_type_character)) = elem("character", [], []).
mer_type(TVarset, higher_order_type(Types, MaybeResult, _, _)) = Xml :-
    XmlTypes = xml_list("higher_order_type_args", mer_type(TVarset), Types),
    ( MaybeResult = yes(ResultType),
        XmlReturn = elem("return_type", [], [mer_type(TVarset, ResultType)]),
        XmlChildren = [XmlTypes, XmlReturn]
    ; MaybeResult = no,
        XmlChildren = [XmlTypes]
    ),
    Xml = elem("higher_order_type", [], XmlChildren).
mer_type(TVarset, tuple_type(Types, _)) = Xml :-
    XmlArgs = xml_list("tuple_types", mer_type(TVarset), Types),
    Xml = elem("tuple", [], [XmlArgs]).
mer_type(_, apply_n_type(_, _, _)) = nyi("apply_n_type").
mer_type(_, kinded_type(_, _)) = nyi("kinded_type").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred pred_documentation(comments::in, pred_id::in, pred_info::in,
    list(xml)::in, list(xml)::out) is det.

pred_documentation(C, _PredId, PredInfo, !Xml) :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    pred_info_get_origin(PredInfo, Origin),
    pred_info_get_markers(PredInfo, Markers),

    (
        status_defined_in_this_module(ImportStatus) = yes,
        Origin = origin_user(_),
        not check_marker(Markers, marker_class_method)
    ->
        Xml = predicate_documentation(C, PredInfo),
        !:Xml = [Xml | !.Xml]
    ;
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
    pred_info_get_import_status(PredInfo, ImportStatus),

    Types = get_orig_arg_types(PredInfo),
    pred_info_get_class_context(PredInfo, Constraints),
    pred_info_context(PredInfo, Context),
    (
        IsPredOrFunc = predicate,
        Tag = "predicate"
    ;
        IsPredOrFunc = function,
        Tag = "function"
    ),
    Id = sym_name_and_arity_to_id(Tag, PredName, Arity),

    XmlName = name(PredName),
    XmlContext = prog_context(Context),
    XmlTypes = xml_list("pred_types", mer_type(TVarset), Types),
    XmlExistVars = xml_list("pred_exist_vars", type_param(TVarset), Exists),
    XmlConstraints = prog_constraints(TVarset, Constraints),
    XmlVisibility = visibility(ImportStatus),

    pred_info_get_procedures(PredInfo, ProcTable),
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
    ( list.drop(list.length(L0) - N, L0, L) ->
        L
    ;
        func_error("keep_last_n")
    ).

:- func prog_constraints(tvarset, prog_constraints) = xml.

prog_constraints(TVarset, constraints(Univs, Exists)) = Xml :-
    XmlUnivs = xml_list("pred_universal", prog_constraint(TVarset), Univs),
    XmlExists = xml_list("pred_exist", prog_constraint(TVarset), Exists),
    
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

    XmlModes = xml_list("modes", mer_mode(IVarSet), Modes),
    XmlDet = determinism(Determinism),
    Xml = elem("pred_mode", [], [XmlModes, XmlDet]),

    !:Xml = [Xml | !.Xml].
    
:- func mer_mode(inst_varset, mer_mode) = xml.

mer_mode(IVarset, A -> B) = Xml :-
    XmlFrom = xml_list("from", mer_inst(IVarset), [A]),
    XmlTo = xml_list("to", mer_inst(IVarset), [B]),
    Xml = elem("inst_to_inst", [], [XmlFrom, XmlTo]).
mer_mode(IVarset, user_defined_mode(Name, Args)) = Xml :-
    Ref = attr("ref", sym_name_and_arity_to_id("mode", Name, length(Args))),
    XmlArgs = xml_list("mode_args", mer_inst(IVarset), Args),
    Xml = elem("user_defined_mode" , [Ref], [name(Name), XmlArgs]).

:- func mer_inst(inst_varset, mer_inst) = xml.

mer_inst(_, any(U)) = elem("any", [], [uniqueness(U)]).
mer_inst(_, free) = elem("free", [], []).
mer_inst(_, free(_)) = elem("free", [], []).
mer_inst(IVarset, bound(U, BoundInsts)) = Xml :-
    XmlUniq = uniqueness(U),
    XmlInsts = xml_list("bound_insts", bound_inst(IVarset), BoundInsts),
    Xml = elem("bound", [], [XmlUniq, XmlInsts]).
mer_inst(_, ground(U, _)) = elem("ground", [], [uniqueness(U)]).
mer_inst(_, not_reached) = elem("not_reached", [] , []).
mer_inst(IVarset, inst_var(IVar)) = Xml :-
    IVarName = varset.lookup_name(IVarset, IVar),
    Xml = tagged_string("inst_var", IVarName).
mer_inst(IVarSet, constrained_inst_vars(_, Inst)) = mer_inst(IVarSet, Inst).
mer_inst(IVarset, defined_inst(Name)) = Xml :-
    XmlName = inst_name(IVarset, Name),
    Xml = elem("defined_inst", [], [XmlName]).
mer_inst(IVarset, abstract_inst(SymName, Insts)) =
    mer_inst(IVarset, defined_inst(user_inst(SymName, Insts))).

:- func inst_name(inst_varset, inst_name) = xml.

inst_name(IVarset, user_inst(Name, Insts)) = Xml :-
    Ref = attr("ref", sym_name_and_arity_to_id("inst", Name, length(Insts))),
    XmlName = name(Name),
    XmlInsts = xml_list("inst_args", mer_inst(IVarset), Insts),
    Xml = elem("user_inst", [Ref], [XmlName, XmlInsts]).
inst_name(_, merge_inst(_, _)) = nyi("merge_inst").
inst_name(_, unify_inst(_, _, _, _)) = nyi("unify_inst").
inst_name(_, ground_inst(_, _, _, _)) = nyi("ground_inst").
inst_name(_, any_inst(_, _, _, _)) = nyi("any_inst").
inst_name(_, shared_inst(_)) = nyi("shared_inst").
inst_name(_, mostly_uniq_inst(_)) = nyi("mostly_uniq_inst").
inst_name(_, typed_ground(_, _)) = nyi("typed_ground").
inst_name(_, typed_inst(_, _)) = nyi("typed_inst").

:- func uniqueness(uniqueness) = xml.

uniqueness(U) = tagged_string("uniqueness", string(U)).

:- func bound_inst(inst_varset, bound_inst) = xml.

bound_inst(IVarset, bound_functor(ConsId, Insts)) = Xml :-
    XmlCons = cons_id(ConsId),
    XmlInsts = xml_list("insts", mer_inst(IVarset), Insts),
    Xml = elem("bound_functor", [], [XmlCons, XmlInsts]).

:- func cons_id(cons_id) = xml.

cons_id(cons(Name, Arity)) = elem("cons", [], [name(Name), arity(Arity)]).
cons_id(int_const(I)) = tagged_int("int", I).
cons_id(string_const(S)) = tagged_string("string", S).
cons_id(float_const(F)) = tagged_float("float", F).
cons_id(pred_const(_, _)) = nyi("pred_const").
cons_id(type_ctor_info_const(_, _, _)) = nyi("type_ctor_info_const").
cons_id(base_typeclass_info_const(_,_,_,_)) = nyi("base_typeclass_info_const").
cons_id(type_info_cell_constructor(_)) = nyi("type_info_cell_constructor").
cons_id(typeclass_info_cell_constructor) =
    nyi("typeclass_info_cell_constructor").
cons_id(tabling_info_const(_)) = nyi("tabling_info_const").
cons_id(deep_profiling_proc_layout(_)) = nyi("deep_profiling_proc_layout").
cons_id(table_io_decl(_)) = nyi("table_io_decl").

:- func arity(int) = xml.

arity(Arity) = tagged_int("arity", Arity).


:- func determinism(determinism) = xml.

determinism(detism_det) = tagged_string("determinism", "det").
determinism(detism_semi) = tagged_string("determinism", "semi").
determinism(detism_multi) = tagged_string("determinism", "multi").
determinism(detism_non) = tagged_string("determinism", "non").
determinism(detism_cc_non) = tagged_string("determinism", "cc_non").
determinism(detism_cc_multi) = tagged_string("determinism", "cc_multi").
determinism(detism_erroneous) = tagged_string("determinism", "erroneous").
determinism(detism_failure) = tagged_string("determinism", "failure").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred class_documentation(comments::in, pred_table::in,
    class_id::in, hlds_class_defn::in,
    list(xml)::in, list(xml)::out) is det.

class_documentation(C, PredTable, class_id(Name, Arity), ClassDefn, !Xml) :-
    ImportStatus = ClassDefn ^ class_status,
    ( status_defined_in_this_module(ImportStatus) = yes ->
        Id = sym_name_and_arity_to_id("class", Name, Arity),

        Context = ClassDefn ^ class_context,
        TVarset = ClassDefn ^ class_tvarset,
        Vars = ClassDefn ^ class_vars,

        XmlName = name(Name),
        XmlClassVars = xml_list("class_vars", type_param(TVarset), Vars),
        XmlSupers = xml_list("superclasses",
                prog_constraint(TVarset), ClassDefn ^ class_supers),
        XmlFundeps = xml_list("fundeps",
                fundep(TVarset, Vars), ClassDefn ^ class_fundeps),
        XmlMethods = class_methods(C,
                PredTable, ClassDefn ^ class_hlds_interface),
        XmlVisibility = visibility(ImportStatus),
        XmlContext = prog_context(Context),

        Xml0 = elem("typeclass", [attr("id", Id)],
                [XmlName, XmlClassVars, XmlSupers,
                XmlFundeps, XmlMethods, XmlVisibility, XmlContext]),

        Xml = maybe_add_comment(C, Context, Xml0),

        !:Xml = [Xml | !.Xml]
    ;
        true
    ).

:- func fundep(tvarset, list(tvar), hlds_class_fundep) = xml.

fundep(TVarset, Vars, fundep(Domain, Range)) = Xml :-
    XmlDomain = fundep_2("domain", TVarset, Vars, Domain),
    XmlRange = fundep_2("range", TVarset, Vars, Range),
    Xml = elem("fundep", [], [XmlDomain, XmlRange]).
    

:- func fundep_2(string, tvarset, list(tvar), set(hlds_class_argpos)) = xml.

fundep_2(Tag, TVarset, Vars, Set) =
    xml_list(Tag, type_param(TVarset), restrict_list_elements(Set, Vars)).

:- func class_methods(comments, pred_table, hlds_class_interface) = xml.

class_methods(C, PredTable, Methods) = Xml :-
    AllPredIds = list.map(func(hlds_class_proc(PredId, _)) = PredId, Methods),
    PredIds = list.sort_and_remove_dups(AllPredIds),
    PredInfos = list.map(func(Id) = map.lookup(PredTable, Id), PredIds),
    Xml = xml_list("methods", predicate_documentation(C), PredInfos).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func name(sym_name) = xml.

name(unqualified(Name)) = tagged_string("unqualified", Name).
name(qualified(Module, Name)) =
    elem("qualified", [], [
        tagged_string("module", sym_name_to_string(Module)),
        tagged_string("name", Name)]).

%-----------------------------------------------------------------------------%

:- func prog_context(prog_context) = xml.

prog_context(context(FileName, LineNumber)) =
    elem("context", [], [
        tagged_string("filename", FileName),
        tagged_int("line", LineNumber)]).

%-----------------------------------------------------------------------------%

:- func prog_constraint(tvarset, prog_constraint) = xml.

prog_constraint(TVarset, constraint(ClassName, Types)) = Xml :-
    Id = sym_name_and_arity_to_id("constraint", ClassName, list.length(Types)),
    XmlName = name(ClassName),
    XmlTypes = xml_list("constraint_types", mer_type(TVarset), Types),
    Xml = elem("constraint", [attr("ref", Id)], [XmlName, XmlTypes]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func visibility(import_status) = xml.

visibility(Status) = tagged_string("visibility", Visibility) :-
    ( status_defined_in_impl_section(Status) = yes ->
        ( Status = status_abstract_exported ->
            Visibility = "abstract"
        ;
            Visibility = "implementation"
        )
    ;
        Visibility = "interface"
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    %
    % sym_name_to_id(P, S) converts the sym_name, S, into
    % a string with prefix, P, prefixed to the generated name.
    %
:- func sym_name_to_id(string, sym_name) = string.

sym_name_to_id(Prefix, Name) = prefixed_sym_name(Prefix, Name).

    %
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

:- func tagged_string(string, string) = xml.

tagged_string(E, S) = elem(E, [], [data(S)]).

:- func tagged_int(string, int) = xml.

tagged_int(E, I) = elem(E, [], [data(int_to_string(I))]).

:- func tagged_float(string, float) = xml.

tagged_float(E, F) = elem(E, [], [data(float_to_string(F))]).

%-----------------------------------------------------------------------------%

:- func xml_list(string, func(T) = xml, list(T)) = xml.

xml_list(Tag, F, L) = elem(Tag, [], list.map(F, L)).

%-----------------------------------------------------------------------------%

:- func nyi(string) = xml.

nyi(Tag) = tagged_string(Tag, "Not yet implemented!").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "xml_documentation.m".
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
