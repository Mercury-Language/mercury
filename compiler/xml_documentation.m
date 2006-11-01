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
:- import_module parse_tree.source_file_map.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
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
        unexpected("xml_documentation.m", io.error_message(E))
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
    % Get the XML representation of the comment associated
    % with the given prog_context.
    %
:- func comment(comments, prog_context) = xml.

comment(Comments, Context) =
    elem("comment", [], [cdata(get_comment(Comments, Context))]).
    
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
        Xml = elem("module", [], [TypeXml])
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

        XmlComment = comment(C, Context),

        Tag = type_xml_tag(TypeBody),
        Id = attr("id", sym_name_and_arity_to_id("type", TypeName, TypeArity)),
        Children = [XmlComment, prog_context(Context) |
                type_body(C, TVarset, TypeBody)],
        Xml = elem(Tag, [Id], Children),

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

:- func type_body(comments, tvarset, hlds_type_body) = list(xml).

type_body(C, TVarset, hlds_du_type(Ctors, _, _, _, _, _)) = Xml :-
    Xml = [xml_list("constructors", constructor(C, TVarset), Ctors)].

    % XXX TODO
type_body(_, _, hlds_eqv_type(_)) = [nyi("hlds_eqv_type")].
type_body(_, _, hlds_foreign_type(_)) = [nyi("hlds_foreign_type")].
type_body(_, _, hlds_solver_type(_, _)) = [nyi("hlds_solver_type")].
type_body(_, _, hlds_abstract_type(_)) = [nyi("hlds_abstract_type")].


:- func constructor(comments, tvarset, constructor) = xml.

constructor(C, TVarset,
        ctor(_Exist, _Constraints, Name, Args, Context)) = Xml :-
    Id = attr("id", sym_name_and_arity_to_id("cons", Name, length(Args))),
    XmlName = name(Name),
    XmlComment = comment(C, Context),
    XmlContext = prog_context(Context),
    XmlArgs = [xml_list("args", constructor_arg(C, TVarset), Args)],
    Xml = elem("constructor", [Id],
            [XmlName, XmlComment, XmlContext | XmlArgs]).

:- func constructor_arg(comments, tvarset, constructor_arg) = xml.

constructor_arg(C, TVarset, ctor_arg(MaybeFieldName, Type, Context)) = Xml :-
    XmlType = elem("type", [], [mer_type(TVarset, Type)]),
    Comment = comment(C, Context),
    XmlContext = prog_context(Context),
    (
        MaybeFieldName = yes(FieldName),
        Id = attr("Id", sym_name_to_id("field", FieldName)),
        XmlMaybeFieldName = [elem("field", [Id], [name(FieldName)])]
    ;
        MaybeFieldName = no,
        XmlMaybeFieldName = []
    ),

    Xml = elem("arg", [], [XmlType, Comment, XmlContext | XmlMaybeFieldName]).

:- func mer_type(tvarset, mer_type) = xml.

mer_type(TVarset, type_variable(TVar, _)) = Xml :-
    TVarName = varset.lookup_name(TVarset, TVar),
    Xml = tagged_string("type_variable", TVarName).
mer_type(TVarset, defined_type(TypeName, Args, _)) = Xml :-
    Ref = attr("ref", sym_name_and_arity_to_id("type", TypeName, length(Args))),
    XmlName = name(TypeName),
    XmlArgs = xml_list("args", mer_type(TVarset), Args),
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
    prefixed_sym_name(Prefix, Name) ++ "/" ++ int_to_string(Arity).

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
