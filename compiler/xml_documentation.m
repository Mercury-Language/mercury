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

:- interface.
:- import_module term_to_xml.
        % my latest typeclass
:- typeclass tc(T) <= xmlable(T) where [
    pred p(T, line_type),
    mode p(in, out) is det,
    mode p(in, in) is semidet,
    func f(T) = string % Simple function
].
:- implementation.

%-----------------------------------------------------------------------------%

xml_documentation(ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(ModuleName, ".xml", no, FileName, !IO),

    lookup_module_source_file(ModuleName, SrcFileName, !IO),
    io.open_input(SrcFileName, SrcResult, !IO),
    (
        SrcResult = ok(SrcStream),
        build_comments(SrcStream, comments(map.init), Comments, !IO),

        io.open_output(FileName, OpenResult, !IO),
        (
            OpenResult = ok(Stream),
            ModuleInfoXmlDoc = module_info_xml_doc(Comments, ModuleInfo),
            write_xml_doc(Stream, ModuleInfoXmlDoc, !IO)
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
    --->     module_info_xml_doc(comments, module_info).

:- instance xmlable(module_info_xml_doc) where [
    (to_xml(module_info_xml_doc(Comments, ModuleInfo)) = Xml :-
        module_info_get_type_table(ModuleInfo, TypeTable),
        map.foldl(type_documentation(Comments), TypeTable, [], TypeXmls),
        TypeXml = elem("types", [], TypeXmls),

        module_info_preds(ModuleInfo, PredTable),
        map.foldl(pred_documentation(Comments), PredTable, [], PredXmls),
        PredXml = elem("preds", [], PredXmls),

        module_info_get_class_table(ModuleInfo, ClassTable),
        map.foldl(class_documentation(Comments), ClassTable, [], ClassXmls),
        ClassXml = elem("typeclasses", [], ClassXmls),

        Xml = elem("module", [], [TypeXml, PredXml, ClassXml])
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

        Tag = type_xml_tag(TypeBody),
        Id = attr("id", sym_name_and_arity_to_id("type", TypeName, TypeArity)),
        Children = [XmlName, XmlTypeParams, prog_context(Context) |
                type_body(C, TVarset, TypeBody)],
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
mer_type(_, higher_order_type(_, _, _, _)) = nyi("higher_order_type").
mer_type(_, tuple_type(_, _)) = nyi("tuple_type").
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
        pred_info_get_typevarset(PredInfo, TVarset),
        pred_info_get_exist_quant_tvars(PredInfo, Exists),

        IsPredOrFunc = pred_info_is_pred_or_func(PredInfo),
        Module = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        PredName = qualified(Module, Name), 

        pred_info_get_arg_types(PredInfo, Types),
        pred_info_get_class_context(PredInfo, Constraints),
        pred_info_context(PredInfo, Context),

        Xml = predicate_documentation(C, TVarset, Exists,
                        IsPredOrFunc, PredName, Types, Constraints, Context),

        !:Xml = [Xml | !.Xml]
    ;
        true
    ).

:- func predicate_documentation(comments,
            tvarset, existq_tvars, pred_or_func, sym_name,
            list(mer_type), prog_constraints, prog_context) = xml.

predicate_documentation(C, TVarset, Exists,
        IsPredOrFunc, PredName, Types, Constraints, Context) = Xml :-
    Arity0 = list.length(Types),
    (
        IsPredOrFunc = predicate,
        Tag = "predicate",
        Arity = Arity0
    ;
        IsPredOrFunc = function,
        Tag = "function",
        Arity = Arity0 - 1
    ),
    Id = sym_name_and_arity_to_id(Tag, PredName, Arity),

    XmlName = name(PredName),
    XmlContext = prog_context(Context),
    XmlTypes = xml_list("pred_types", mer_type(TVarset), Types),
    XmlExistVars = xml_list("pred_exist_vars", type_param(TVarset), Exists),
    XmlConstraints = prog_constraints(TVarset, Constraints),

    Xml0 = elem(Tag, [attr("id", Id)],
            [XmlName, XmlTypes, XmlContext, XmlExistVars, XmlConstraints]),

    Xml = maybe_add_comment(C, Context, Xml0).

:- func prog_constraints(tvarset, prog_constraints) = xml.

prog_constraints(TVarset, constraints(Univs, Exists)) = Xml :-
    XmlUnivs = xml_list("pred_universal", prog_constraint(TVarset), Univs),
    XmlExists = xml_list("pred_exist", prog_constraint(TVarset), Exists),
    
    Xml = elem("pred_constraints", [], [XmlUnivs, XmlExists]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred class_documentation(comments::in, class_id::in, hlds_class_defn::in,
    list(xml)::in, list(xml)::out) is det.

class_documentation(C, class_id(Name, Arity), ClassDefn, !Xml) :-
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
        XmlMethods = class_methods(C, ClassDefn ^ class_interface),
        XmlContext = prog_context(Context),

        Xml0 = elem("typeclass", [attr("id", Id)],
                [XmlName, XmlClassVars, XmlSupers,
                XmlFundeps, XmlMethods, XmlContext]),

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

:- func class_methods(comments, class_interface) = xml.

class_methods(_, class_interface_abstract) = elem("methods", [], []).
class_methods(C, class_interface_concrete(Methods)) =
    xml_list("methods", class_method(C), Methods).

:- func class_method(comments, class_method) = xml.

class_method(C, method_pred_or_func(TVarset, _, Exist, PredOrFunc,
        Name, TypeAndModes, _, _, _, _, _, Constraints, Context)) =
    predicate_documentation(C, TVarset, Exist, PredOrFunc,
            Name, list.map(type_only, TypeAndModes), Constraints, Context).
class_method(_, method_pred_or_func_mode(_, _, _, _, _, _, _, _)) =
    nyi("method_pred_or_func_mode").

:- func type_only(type_and_mode) = mer_type.

type_only(type_only(T)) = T.
type_only(type_and_mode(T, _)) = T.


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
