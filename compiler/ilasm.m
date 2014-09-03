%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ilasm.m.
% Main author: trd.
%
% Generate IL for the ilasm assembler.
%
% IL assembler syntax is documented in the Microsoft .NET Framework SDK.
% See ilds.m for links to the documentation.
%
% This code is a little messy.  Some of the code here is a hangover from
% earlier versions of the assembler grammar.
%
% To do:
%   [ ] Implement missing instructions.
%   [ ] Add any missing functionality from the assembler grammar
%       (events, properties, etc).
%   [ ] Fix up all the XXXs.
%   [ ] Replace all reference to io.write with predicates that do not depend
%       on the compiler's internal data representations.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.ilasm.
:- interface.

:- import_module libs.globals.
:- import_module ml_backend.ilds.

:- import_module bool.
:- import_module integer.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module term.

%-----------------------------------------------------------------------------%

:- pred ilasm_output(globals::in, list(il_decl)::in, io::di, io::uo) is det.

:- type int64 ---> int64(integer).
:- type int32 ---> int32(int).
:- type int16 ---> int16(int).
:- type int8 ---> int8(int).
:- type byte == int8.
:- type float64 ---> float64(float).
:- type float32 ---> float32(float).

    % A top level declaration in IL assembler.
    %
:- type il_decl
            % .class declaration
    --->    ildecl_class(
                list(classattr),        % Attributes for the class.
                ilds.id,                % Name of the class.
                extends,                % What is the parent class?
                implements,             % What interfaces are implemented?
                list(class_member)      % Methods and fields.
            )

            % .namespace declaration
    ;       ildecl_namespace(
                namespace_qual_name,    % Namespace name.
                list(il_decl)           % Contents.
            )

            % .method  (a global function)
            % There are lots of restrictions on global functions so
            % don't get too excited about using them for anything.
            % In particular, you can't reference a namespace
            % qualified global function from outside the module.
    ;       ildecl_method(
                methodhead,
                method_defn
            )

            % .data  (module local data)
    ;       ildecl_data(
                bool,                   % Is data in thread local storage?
                maybe(ilds.id),         % id to name this data.
                data_body               % Body of data.
            )

            % .file
            % Declares a file associated with the current assembly.
    ;       ildecl_file(ilds.id)

            % .module extern
            % Declares a module name.
    ;       ildecl_extern_module(ilds.id)

            % .assembly extern
            % Declares an assembly name, and possibly its strong
            % name/version number.
    ;       ildecl_extern_assembly(ilds.id, list(assembly_decl))

            % .assembly
            % Defines an assembly.
    ;       ildecl_assembly(ilds.id)

            % .custom
            % A custom attribute.
    ;       ildecl_custom(custom_decl)

    ;       ildecl_comment_term(term)

            % Print almost anything using pprint.to_doc
            % (see library/pprint.m for limitations).
    ;       some [T] ildecl_comment_thing(T)
    ;       ildecl_comment(string).

:- type assembly_decl
    --->    version(int, int, int, int)     % Version number.
    ;       hash(list(int8))                % Hash.
    ;       public_key_token(list(int8))    % Public key token.
    ;       custom(custom_decl).            % A custom attribute.

    % A method definition is just a list of body decls.
    %
:- type method_defn == list(method_body_decl).

:- type methodhead
    --->    methodhead(
                list(methattr),     % Method attributes.
                member_name,        % Method name.
                signature,          % Method signature.
                list(implattr)      % Implementation attributes.
            ).

:- type class_member
            % .method (a class method)
    --->    member_method(
                methodhead,     % Name, signature, attributes.
                method_defn     % Definition of method.
            )

            % .field (a class field)
    ;       member_field(
                list(fieldattr),    % Attributes.
                il_type,            % Field type.
                ilds.id,            % Field name.
                maybe(int32),       % Offset for explicit layout.
                field_initializer   % Initializer.
            )

            % .property (a class property)
    ;       member_property(
                il_type,            % Property type.
                ilds.id,            % Property name.
                maybe(methodhead),  % Get property.
                maybe(methodhead)   % Set property.
            )

            % .class (a nested class)
    ;       member_nested_class(
                list(classattr),    % Attributes for the class.
                ilds.id,           % Name of the class.
                extends,            % What is the parent class?
                implements,         % What interfaces are implemented?
                list(class_member)  % Methods and fields.
            )

    ;       member_custom(custom_decl)     % custom attribute

    ;       member_comment_term(term)
    ;       member_comment(string)

            % print almost anything using pprint.to_doc
            % (see library/pprint.m for limitations).
    ;       some [T] member_comment_thing(T).

:- type field_initializer
    --->    none                % No initializer.
    ;       at(ilds.id)         % Initialize with .data at given location.
    ;       equals(field_init). % Initialize with constant.

    % Note that for some reason the syntax for field_init is almost,
    % but not quite the same as data items.
    %
:- type field_init
    --->    data_item(data_item)        % Most data_items are valid.
            % XXX unicode is not yet implemented, don't use
            % wchar_ptr unless you intend to implement it
    ;       wchar_ptr(string)           % A string to convert to unicode.
    ;       binary_float32(int32)       % Binary rep. of float.
    ;       binary_float64(int64).      % Binary rep. of double.

    % A parent class to extend.
    %
:- type extends
    --->    extends(ilds.class_name)
    ;       extends_nothing.

    % A list of interfaces that we implement.
    %
:- type implements
    --->    implements(list(ilds.class_name)).

    % Declarations that can form the body of a method.
    %
:- type method_body_decl
    --->    emitbyte(int32)
            % raw byte output (danger! danger!)
            % "emits an int32 to the code section of the method" according
            % to the IL Assembly Language Programmers' Reference.
            % This probably means it can output IL bytecodes.

    ;       maxstack(int32)
            % "Defines the maximum size of the stack, specified by the int32"
            % But does it measure in bits, nibbles, bytes, words or
            % something else?

    ;       entrypoint          % Is this "main"?
    ;       zeroinit            % Initialize locals to zero.
    ;       custom(custom_decl) % Custom attribute.
    ;       instrs(list(instr)) % Instructions.
    ;       label(string).      % A label.

    % Attributes that a class can have.
    % See SDK documentation for what they all mean.
    %
:- type classattr
    --->    abstract
    ;       ansi
    ;       auto
    ;       autochar
    ;       beforefieldinit
    ;       explicit
    ;       interface
    ;       nestedassembly
    ;       nestedfamandassem
    ;       nestedfamily
    ;       nestedfamorassem
    ;       nestedprivate
    ;       nestedpublic
    ;       private
    ;       public
    ;       rtspecialname
    ;       sealed
    ;       sequential
    ;       serializable
    ;       specialname
    ;       unicode.

    % Attributes that a method can have.
    % See SDK documentation for what they all mean.
    %
:- type methattr
    --->    abstract
    ;       assembly
    ;       famandassem
    ;       family
    ;       famorassem
    ;       final
    ;       hidebysig
    ;       newslot
    ;       private
    ;       privatescope
    ;       public
    ;       rtspecialname
    ;       specialname
    ;       static
    ;       synchronized
    ;       virtual
    ;       pinvokeimpl.

    % Attributes that a field can have.
    % See SDK documentation for what they all mean.
    %
:- type fieldattr
    --->    assembly
    ;       famandassem
    ;       family
    ;       famorassem
    ;       initonly
    ;       literal
    ;       notserialized
    ;       pinvokeimpl
    ;       private
    ;       privatescope
    ;       public
    ;       static
    ;       volatile.

    % Attributes that a method implementation can have.
    % See SDK documentation for what they all mean.
    %
:- type implattr
    --->    il
    ;       implemented
    ;       internalcall
    ;       managed
    ;       native
    ;       ole
    ;       optil
    ;       runtime
    ;       unmanaged.

    % The body of a .data declaration
    %
:- type data_body
    --->    itemlist(list(data_item))
    ;       item(data_item).

    % Various constants that can be used in .data declarations.
    %
:- type data_item
    --->    float32(float32)
    ;       float64(float64)
    ;       int64(int64)
    ;       int32(int32)
    ;       int16(int16)
    ;       int8(int8)
    ;       char_ptr(string)
    ;       '&'(ilds.id)
    ;       bytearray(list(byte)).  % Output as two digit hex, e.g. 01 F7 0A.

:- type custom_decl
    --->    custom_decl(
                custom_type,
                maybe(custom_type),
                qstring_or_bytes
            ).

:- type qstring_or_bytes
    --->    qstring(string)
    ;       bytes(list(int8))
    ;       no_initalizer.

:- type custom_type
    --->    type(il_type)
    ;       methodref(ilds.methodref).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.c_util. % for output_float_literal
:- import_module libs.options.

:- import_module char.
:- import_module int.
:- import_module pair.
:- import_module pprint.
:- import_module require.
:- import_module string.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Some versions of the IL assembler enforce a rule that if you output
    %   .assembly foo { }
    % you are not allowed to use the assembly reference in the rest of
    % the file, e.g.
    %   [foo]blah.bletch
    % Instead you have to output just
    %   blah.bletch
    %
    % So we need to duplicate this checking in the output phase and
    % make sure we don't output [foo].
    %
    % It's a good idea to do this anyway, as there is apparently a
    % performance hit if you use assembly references to a symbol that is
    % in the local assembly.

:- type ilasm_info
    --->    ilasm_info(
                current_assembly :: ilds.id
            ).

:- pred ilasm_write_list(list(T)::in, string::in,
    pred(T, ilasm_info, ilasm_info, io, io)
        ::in(pred(in, in, out, di, uo) is det),
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

ilasm_write_list([], _Separator, _OutputPred, !Info, !IO).
ilasm_write_list([E | Es], Separator, OutputPred, !Info, !IO) :-
    OutputPred(E, !Info, !IO),
    (
        Es = []
    ;
        Es = [_ | _],
        io.write_string(Separator, !IO)
    ),
    ilasm_write_list(Es, Separator, OutputPred, !Info, !IO).

ilasm_output(Globals, Blocks, !IO) :-
    OutInfo = init_ilasm_out_info(Globals),
    Info0 = ilasm_info(""),
    ilasm_output(OutInfo, Blocks, Info0, _Info, !IO).

:- pred ilasm_output(ilasm_out_info::in, list(il_decl)::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

ilasm_output(OutInfo, Blocks, !Info, !IO) :-
    ilasm_write_list(Blocks, "\n\n", output_decl(OutInfo), !Info, !IO),
    io.write_string("\n\n", !IO).

:- pred output_decl(ilasm_out_info::in, il_decl::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_decl(OutInfo, Decl, !Info, !IO) :-
    (
        Decl = ildecl_custom(CustomDecl),
        output_custom_decl(OutInfo, CustomDecl, !Info, !IO)
    ;
        Decl = ildecl_class(Attrs, Id, Extends, Implements, Contents),
        io.write_string(".class ", !IO),
        io.write_list(Attrs, " ", output_classattr, !IO),
        (
            Attrs = [_ | _],
            io.write_string(" ", !IO)
        ;
            Attrs = []
        ),
        output_id(Id, !IO),
        (
            Extends = extends(ExtendsModule),
            io.write_string(" extends ", !IO),
            output_class_name(OutInfo, ExtendsModule, !Info, !IO)
        ;
            Extends = extends_nothing
        ),
        Implements = implements(ImplementsList),
        (
            ImplementsList = [_ | _],
            io.write_string(" implements ", !IO),
            ilasm_write_list(ImplementsList, ", ", output_class_name(OutInfo),
                !Info, !IO)
        ;
            ImplementsList = []
        ),
        io.write_string(" {\n", !IO),
        ilasm_write_list(Contents, "\n", output_class_member(OutInfo),
            !Info, !IO),
        io.write_string("\n}", !IO)
    ;
        Decl = ildecl_namespace(DottedName, Contents),
        (
            DottedName = [_ | _],
            io.write_string(".namespace ", !IO),
            output_dotted_name(DottedName, !IO),
            io.write_string(" {\n", !IO),
            ilasm_output(OutInfo, Contents, !Info, !IO),
            io.write_string("}\n", !IO)
        ;
            DottedName = [],
            ilasm_output(OutInfo, Contents, !Info, !IO)
        )
    ;
        Decl = ildecl_method(MethodHead, MethodDecls),
        io.write_string(".method ", !IO),
        output_methodhead(OutInfo, MethodHead, !Info, !IO),
        io.write_string("\n{\n", !IO),
        ilasm_write_list(MethodDecls, "\n", output_method_body_decl(OutInfo),
            !Info, !IO),
        io.write_string("}\n", !IO)
    ;
        Decl = ildecl_data(TLS, MaybeId, Body),
        io.write_string(".data ", !IO),
        (
            TLS = yes,
            io.write_string("tls ", !IO)
        ;
            TLS = no
        ),
        (
            MaybeId = yes(Id),
            output_id(Id, !IO),
            io.write_string(" = ", !IO)
        ;
            MaybeId = no
        ),
        output_data_body(Body, !IO)
    ;
        Decl = ildecl_comment_term(CommentTerm),
        AutoComments = OutInfo ^ ilaoi_auto_comments,
        (
            AutoComments = yes,
            io.write_string("// ", !IO),
            varset.init(VarSet),
            term_io.write_term(VarSet, CommentTerm, !IO),
            io.nl(!IO)
        ;
            AutoComments = no
        )
    ;
        Decl = ildecl_comment_thing(Thing),
        AutoComments = OutInfo ^ ilaoi_auto_comments,
        (
            AutoComments = yes,
            Doc = label("// ", to_doc(Thing)),
            write(70, Doc, !IO),
            io.nl(!IO)
        ;
            AutoComments = no
        )
    ;
        Decl = ildecl_comment(CommentStr),
        AutoComments = OutInfo ^ ilaoi_auto_comments,
        (
            AutoComments = yes,
            output_comment_string(CommentStr, !IO)
        ;
            AutoComments = no
        )
    ;
        Decl = ildecl_extern_assembly(AsmName, AssemblyDecls),
        io.write_string(".assembly extern ", !IO),
        output_id(AsmName, !IO),
        io.write_string("{\n", !IO),
        list.foldl2(
            (pred(A::in, I0::in, I::out, IO0::di, IO::uo) is det :-
                output_assembly_decl(OutInfo, A, I0, I, IO0, IO1),
                io.write_string("\n\t", IO1, IO)
            ), AssemblyDecls, !Info, !IO),
        io.write_string("\n}\n", !IO)
    ;
        Decl = ildecl_assembly(AsmName),
        io.write_string(".assembly ", !IO),
        output_id(AsmName, !IO),
        !Info ^ current_assembly := AsmName,
        io.write_string(" { }", !IO)
    ;
        Decl = ildecl_file(FileName),
        io.write_string(".file ", !IO),
        output_id(FileName, !IO)
    ;
        Decl = ildecl_extern_module(ModName),
        io.write_string(".module extern ", !IO),
        output_id(ModName, !IO)
    ).

:- pred output_class_member(ilasm_out_info::in, class_member::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_class_member(OutInfo, ClassMember, !Info, !IO) :-
    (
        ClassMember = member_method(MethodHead, MethodDecls),
        MethodDecl = ildecl_method(MethodHead, MethodDecls),
        ( MethodHead = methodhead(_, cctor, _, _) ->
            % Don't do debug output on class constructors, since
            % they are automatically generated and take forever to run.
            NoDebugOutInfo = OutInfo ^ ilaoi_debug_il_asm := no,
            output_decl(NoDebugOutInfo, MethodDecl, !Info, !IO)
        ;
            output_decl(OutInfo, MethodDecl, !Info, !IO)
        )
    ;
        ClassMember = member_custom(CustomDecl),
        output_custom_decl(OutInfo, CustomDecl, !Info, !IO)
    ;
        ClassMember = member_field(FieldAttrs, Type, IlId, MaybeOffset,
            Initializer),
        io.write_string(".field ", !IO),
        (
            MaybeOffset = yes(Offset),
            output_int32(Offset, !IO),
            io.write_string(" ", !IO)
        ;
            MaybeOffset = no
        ),
        io.write_list(FieldAttrs, " ", io.write, !IO),
        io.write_string("\n\t", !IO),
        output_type(OutInfo, Type, !Info, !IO),
        io.write_string("\n\t", !IO),
        output_id(IlId, !IO),
        output_field_initializer(Initializer, !IO)
    ;
        ClassMember = member_property(Type, Name, MaybeGet, MaybeSet),
        io.write_string(".property instance ", !IO),
        output_type(OutInfo, Type, !Info, !IO),
        io.write_string(" ", !IO),
        output_id(Name, !IO),
        io.write_string("() {", !IO),
        (
            MaybeGet = yes(methodhead(_, GetMethodName, GetSignature, _)),
            io.nl(!IO),
            io.write_string("\t.get instance ", !IO),
            output_name_signature_and_call_conv(OutInfo, GetSignature,
                yes(GetMethodName), "\t\t", !Info, !IO)
        ;
            MaybeGet = no
        ),
        (
            MaybeSet = yes(methodhead(_, SetMethodName, SetSignature, _)),
            io.nl(!IO),
            io.write_string("\t.set instance ", !IO),
            output_name_signature_and_call_conv(OutInfo, SetSignature,
                yes(SetMethodName), "\t\t", !Info, !IO)
        ;
            MaybeSet = no
        ),
        io.write_string("\n}\n", !IO)
    ;
        ClassMember = member_nested_class(Attrs, Id, Extends, Implements,
            Contents),
        ClassDecl = ildecl_class(Attrs, Id, Extends, Implements, Contents),
        output_decl(OutInfo, ClassDecl, !Info, !IO)
    ;
        ClassMember = member_comment(CommentStr),
        AutoComments = OutInfo ^ ilaoi_auto_comments,
        (
            AutoComments = yes,
            output_comment_string(CommentStr, !IO)
        ;
            AutoComments = no
        )
    ;
        ClassMember = member_comment_term(CommentTerm),
        AutoComments = OutInfo ^ ilaoi_auto_comments,
        (
            AutoComments = yes,
            io.write_string("// ", !IO),
            varset.init(VarSet),
            term_io.write_term(VarSet, CommentTerm, !IO),
            io.nl(!IO)
        ;
            AutoComments = no
        )
    ;
        ClassMember = member_comment_thing(Thing),
        AutoComments = OutInfo ^ ilaoi_auto_comments,
        (
            AutoComments = yes,
            Doc = label("// ", to_doc(Thing)),
            write(70, Doc, !IO),
            io.nl(!IO)
        ;
            AutoComments = no
        )
    ).

:- pred output_methodhead(ilasm_out_info::in, methodhead::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_methodhead(OutInfo, MethodHead, !Info, !IO) :-
    MethodHead = methodhead(Attrs, MethodName, Signature, ImplAttrs),
    io.write_list(Attrs, " ", io.write, !IO),
    (
        Attrs = [_ | _],
        io.write_string(" ", !IO)
    ;
        Attrs = []
    ),
    output_name_signature_and_call_conv(OutInfo, Signature, yes(MethodName),
        "\t", !Info, !IO),
    io.write_list(ImplAttrs, " ", io.write, !IO).

:- pred output_method_body_decl(ilasm_out_info::in, method_body_decl::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_method_body_decl(OutInfo, MethodBodyDecl, !Info, !IO) :-
    (
        MethodBodyDecl = emitbyte(Int32),
        io.write_string(".emitbyte ", !IO),
        output_int32(Int32, !IO)
    ;
        MethodBodyDecl = custom(CustomDecl),
            output_custom_decl(OutInfo, CustomDecl, !Info, !IO)
        ;
            MethodBodyDecl = maxstack(Int32),
            io.write_string(".maxstack ", !IO),
            output_int32(Int32, !IO)
        ;
            MethodBodyDecl = entrypoint,
            io.write_string(".entrypoint ", !IO)
        ;
            MethodBodyDecl = zeroinit,
            io.write_string(".zeroinit ", !IO)
        ;
            MethodBodyDecl = instrs(Instrs),
            output_instructions(OutInfo, Instrs, !Info, !IO)
        ;
            MethodBodyDecl = label(Label),
            output_label(Label, !IO),
            io.write_string(":", !IO)
        ).

    :- pred output_label(label::in, io::di, io::uo) is det.

    output_label(Label, !IO) :-
        io.write_string(Label, !IO).

    :- pred output_class_name(ilasm_out_info::in, ilds.class_name::in,
        ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

    output_class_name(OutInfo, ClassName, !Info, !IO) :-
        output_structured_name(OutInfo, !.Info, ClassName, !IO).

    :- pred output_call_conv(call_conv::in, io::di, io::uo) is det.

    output_call_conv(call_conv(IsInstance, IlCallConv), !IO) :-
        (
            IsInstance = yes,
            io.write_string("instance ", !IO)
        ;
            IsInstance = no,
            io.write(IlCallConv, !IO),
            io.write_string(" ", !IO)
        ).

:- pred output_name_signature_and_call_conv(ilasm_out_info::in, signature::in,
    maybe(member_name)::in, string::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_name_signature_and_call_conv(OutInfo, Signature, MaybeMethodName,
        Indent, !Info, !IO) :-
    Signature = signature(CallConv, ReturnType, ArgTypes),
    output_call_conv(CallConv, !IO),
    io.write_string("\n", !IO),
    io.write_string(Indent, !IO),
    output_ret_type(OutInfo, ReturnType, !Info, !IO),
    (
        MaybeMethodName = yes(MethodName),
        io.write_string("\n", !IO),
        io.write_string(Indent, !IO),
        output_member_name(MethodName, !IO)
    ;
        MaybeMethodName = no,
        io.write_string(" ", !IO)
    ),
    (
        ArgTypes = [],
        io.write_string("()", !IO)
    ;
        ArgTypes = [_ | _],
        io.write_string("(\n\t\t", !IO),
        ilasm_write_list(ArgTypes, ",\n\t\t", output_method_param(OutInfo),
            !Info, !IO),
        io.write_string("\n\t)", !IO)
    ).

:- pred output_member_name(member_name::in, io::di, io::uo) is det.

output_member_name(MethodName, !IO) :-
    (
        MethodName = ctor,
        io.write_string(".ctor", !IO)
    ;
        MethodName = cctor,
        io.write_string(".cctor", !IO)
    ;
        MethodName = id(IlId),
        output_id(IlId, !IO)
    ).

:- pred output_ret_type(ilasm_out_info::in, ret_type::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_ret_type(OutInfo, RetType, !Info, !IO) :-
    (
        RetType = void,
        io.write_string("void", !IO)
    ;
        RetType = simple_type(Type),
        output_simple_type(OutInfo, Type, !Info, !IO)
    ).

:- pred output_local(ilasm_out_info::in, pair(ilds.id, il_type)::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_local(OutInfo, Id - Type, !Info, !IO) :-
    output_type(OutInfo, Type, !Info, !IO),
    io.write_string(" ", !IO),
    output_id(Id, !IO).

:- pred output_method_param(ilasm_out_info::in, il_method_param::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_method_param(OutInfo, MethodParam, !Info, !IO) :-
    MethodParam = il_method_param(Type, MaybeId),
    output_type(OutInfo, Type, !Info, !IO),
    (
        MaybeId = no
    ;
        MaybeId = yes(Id),
        io.write_string(" ", !IO),
        output_id(Id, !IO)
    ).

:- pred output_type(ilasm_out_info::in, il_type::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_type(OutInfo, IlType, !Info, !IO) :-
    IlType = il_type(Modifiers, SimpleType),
    io.write_list(Modifiers, " ", output_modifier, !IO),
    output_simple_type(OutInfo, SimpleType, !Info, !IO).

:- pred output_simple_type(ilasm_out_info::in, simple_type::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_simple_type(OutInfo, SimpleType, !Info, !IO) :-
    (
        SimpleType = int8,
        io.write_string("int8", !IO)
    ;
        SimpleType = int16,
        io.write_string("int16", !IO)
    ;
        SimpleType = int32,
        io.write_string("int32", !IO)
    ;
        SimpleType = int64,
        io.write_string("int64", !IO)
    ;
        SimpleType = uint8,
        io.write_string("unsigned int8", !IO)
    ;
        SimpleType = uint16,
        io.write_string("unsigned int16", !IO)
    ;
        SimpleType = uint32,
        io.write_string("unsigned int32", !IO)
    ;
        SimpleType = uint64,
        io.write_string("unsigned int64", !IO)
    ;
        SimpleType = native_int,
        io.write_string("native int", !IO)
    ;
        SimpleType = native_uint,
        io.write_string("native unsigned int", !IO)
    ;
        SimpleType = float32,
        io.write_string("float32", !IO)
    ;
        SimpleType = float64,
        io.write_string("float64", !IO)
    ;
        SimpleType = native_float,
        io.write_string("native float", !IO)
    ;
        SimpleType = bool,
        io.write_string("bool", !IO)
    ;
        SimpleType = char,
        io.write_string("char", !IO)
    ;
        SimpleType = object,
        io.write_string("object", !IO)
    ;
        SimpleType = string,
        io.write_string("string", !IO)
    ;
        SimpleType = refany,
        io.write_string("refany", !IO)
    ;
        SimpleType = class(ClassName),
        ( name_to_simple_type(ClassName, ClassType) ->
            (
                ClassType = reference(ClassSimpleType),
                output_simple_type(OutInfo, ClassSimpleType, !Info, !IO)
            ;
                ClassType = value(_),
                % If it is a value type then we are refering
                % to the boxed version of the value type.
                io.write_string("class ", !IO),
                output_structured_name(OutInfo, !.Info, ClassName, !IO)
            )
        ;
            io.write_string("class ", !IO),
            output_structured_name(OutInfo, !.Info, ClassName, !IO)
        )
    ;
        SimpleType = valuetype(ValueName),
        ( name_to_simple_type(ValueName, ValueType) ->
            (
                ValueType = value(ValueSimpleType),
                output_simple_type(OutInfo, ValueSimpleType, !Info, !IO)
            ;
                ValueType = reference(_),
                unexpected($module, $pred, "builtin reference type")
            )
        ;
            io.write_string("valuetype ", !IO),
            output_structured_name(OutInfo, !.Info, ValueName, !IO)
        )
    ;
        SimpleType = interface(Name),
        io.write_string("interface ", !IO),
        output_structured_name(OutInfo, !.Info, Name, !IO)
    ;
        SimpleType = '[]'(Type, Bounds),
        output_type(OutInfo, Type, !Info, !IO),
        output_bounds(Bounds, !IO)
    ;
        SimpleType = '*'(Type),
        output_type(OutInfo, Type, !Info, !IO),
        io.write_string("*", !IO)
    ;
        SimpleType = '&'(Type),
        output_type(OutInfo, Type, !Info, !IO),
        io.write_string("&", !IO)
    ).

:- type ref_or_value
    --->    reference(simple_type)
    ;       value(simple_type).

    % If possible converts a class name to a simple type and an
    % indicator of whether or not that simple type is a reference or
    % value class.
    %
:- pred name_to_simple_type(class_name::in, ref_or_value::out) is semidet.

name_to_simple_type(Name, Type) :-
    % Parition II section 'Built-in Types' (Section 7.2) states
    % that all builtin types *must* be referenced by their
    % special encoding in signatures.
    % See Parition I 'Built-In Types' % (Section 8.2.2) for the
    % list of all builtin types.
    Name = structured_name(AssemblyName, QualifiedName, _),
    AssemblyName = assembly("mscorlib"),
    QualifiedName = ["System", TypeName],
    (
        TypeName = "Boolean",
        Type = value(bool)
    ;
        TypeName = "Char",
        Type = value(char)
    ;
        TypeName = "Object",
        Type = reference(object)
    ;
        TypeName = "String",
        Type = reference(string)
    ;
        TypeName = "Single",
        Type = value(float32)
    ;
        TypeName = "Double",
        Type = value(float64)
    ;
        TypeName = "SByte",
        Type = value(int8)
    ;
        TypeName = "Int16",
        Type = value(int16)
    ;
        TypeName = "Int32",
        Type = value(int32)
    ;
        TypeName = "Int64",
        Type = value(int64)
    ;
        TypeName = "IntPtr",
        Type = value(native_int)
    ;
        TypeName = "UIntPtr",
        Type = value(native_uint)
    ;
        TypeName = "TypedReference",
        Type = value(refany)
    ;
        TypeName = "Byte",
        Type = value(uint8)
    ;
        TypeName = "UInt16",
        Type = value(uint16)
    ;
        TypeName = "UInt32",
        Type = value(uint32)
    ;
        TypeName = "UInt64",
        Type = value(uint64)
    ).

    % The names are all different if it is an opcode.
    % There's probably a very implementation dependent reason for this.
    %
:- pred output_simple_type_opcode(simple_type::in, io::di, io::uo) is det.

output_simple_type_opcode(int8, !IO) :-
    io.write_string("i1", !IO).
output_simple_type_opcode(int16, !IO) :-
    io.write_string("i2", !IO).
output_simple_type_opcode(int32, !IO) :-
    io.write_string("i4", !IO).
output_simple_type_opcode(int64, !IO) :-
    io.write_string("i8", !IO).
output_simple_type_opcode(uint8, !IO) :-
    io.write_string("u1", !IO).
output_simple_type_opcode(uint16, !IO) :-
    io.write_string("u2", !IO).
output_simple_type_opcode(uint32, !IO) :-
    io.write_string("u4", !IO).
output_simple_type_opcode(uint64, !IO) :-
    io.write_string("u8", !IO).
output_simple_type_opcode(native_int, !IO) :-
    io.write_string("i", !IO).
output_simple_type_opcode(native_uint, !IO) :-
    io.write_string("u", !IO).
output_simple_type_opcode(float32, !IO) :-
    io.write_string("r4", !IO).
output_simple_type_opcode(float64, !IO) :-
    io.write_string("r8", !IO).
output_simple_type_opcode(native_float, !IO) :-
    unexpected($module, $pred, "unable to create opcode for native_float").
output_simple_type_opcode(bool, !IO) :-
    % XXX should i4 be used for bool?
    io.write_string("i4", !IO).
output_simple_type_opcode(char, !IO) :-
    io.write_string("i2", !IO).
output_simple_type_opcode(object, !IO) :-
    % All reference types use "ref" as their opcode.
    % XXX is "ref" here correct for value classes?
    io.write_string("ref", !IO).
output_simple_type_opcode(string, !IO) :-
    io.write_string("ref", !IO).
output_simple_type_opcode(refany, !IO) :-
    io.write_string("ref", !IO).
output_simple_type_opcode(class(_Name), !IO) :-
    io.write_string("ref", !IO).
output_simple_type_opcode(valuetype(_Name), !IO) :-
    io.write_string("ref", !IO).
output_simple_type_opcode(interface(_Name), !IO) :-
    io.write_string("ref", !IO).
output_simple_type_opcode('[]'(_Type, _Bounds), !IO) :-
    io.write_string("ref", !IO).
output_simple_type_opcode('*'(_Type), !IO) :-
    io.write_string("ref", !IO).
output_simple_type_opcode('&'(_Type), !IO) :-
    io.write_string("ref", !IO).

:- pred output_bounds(bounds::in, io::di, io::uo) is det.

output_bounds(Bounds, !IO) :-
    io.write_string("[", !IO),
    io.write_list(Bounds, ", ", output_bound, !IO),
    io.write_string("]", !IO).

:- pred output_bound(bound::in, io::di, io::uo) is det.

output_bound(upper(X), !IO) :-
    io.write_int(X, !IO).
output_bound(lower(X), !IO) :-
    io.write_int(X, !IO),
    io.write_string("...", !IO).
output_bound(between(X, Y), !IO) :-
    io.write_int(X, !IO),
    io.write_string("...", !IO),
    io.write_int(Y, !IO).

:- pred output_modifier(ilds.type_modifier::in, io::di, io::uo) is det.

output_modifier(const, !IO) :-
    io.write_string("const", !IO).
output_modifier(volatile, !IO) :-
    io.write_string("volatile", !IO).
output_modifier(readonly, !IO) :-
    io.write_string("readonly", !IO).

:- pred output_instructions(ilasm_out_info::in, list(instr)::in,
    ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_instructions(OutInfo, Instructions, !Info, !IO) :-
    DebugIlAsm = OutInfo ^ ilaoi_debug_il_asm,
    (
        DebugIlAsm = yes,
        list.foldl2(output_debug_instruction(OutInfo), Instructions,
            !Info, !IO)
    ;
        DebugIlAsm = no,
        list.foldl2(output_instruction(OutInfo), Instructions, !Info, !IO)
    ).

    % We write each instruction before we execute it.
    % This is a nice way of debugging IL as it executes, although as
    % the IL debugger improves we might not need this any more.
    %
:- pred output_debug_instruction(ilasm_out_info::in, instr::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_debug_instruction(OutInfo, Instr, !Info, !IO) :-
    % We can't handle tailcalls easily -- you need to put it out as
    %       trace the tail instruction
    %       trace the call instruction
    %       output the tail instruction
    %       output the call instruction
    % For the moment we'll just ignore tailcalls.

    ( Instr = tailcall ->
        true
    ; Instr = context(_, _) ->
        % Contexts are messy, let's ignore them for now.
        true
    ; Instr = start_block(bt_catch(ClassName), Id) ->
        output_instr(OutInfo, start_block(bt_catch(ClassName), Id),
            !Info, !IO),
        io.write_string("\n", !IO),
        io.write_string("\t", !IO),
        output_trace_instr(OutInfo, Instr, !Info, !IO),
        io.write_string("\n", !IO)
    ; Instr = start_block(bt_scope(Locals), Id) ->
        string.format("{\t// #%d", [i(Id)], S),
        io.write_string(S, !IO),
        io.nl(!IO),
        output_trace(S, !IO),
        (
            Locals = []
        ;
            Locals = [_ | _],
            % output the .locals decl
            io.write_string("\t.locals (\n\t\t", !IO),
            ilasm_write_list(Locals, ",\n\t\t", output_local(OutInfo),
                !Info, !IO),
            io.write_string("\n\t)", !IO),
            io.write_string("\n", !IO),

                % trace the .locals decl
            io.write_string("\t\tldstr """, !IO),
            io.write_string(".locals (\\n\\t\\t", !IO),
            ilasm_write_list(Locals, ",\\n\\t\\t", output_local(OutInfo),
                !Info, !IO),
            io.write_string(")", !IO),
            io.write_string("\\n""", !IO),
            io.write_string("\n", !IO),
            io.write_string("\t\tcall void " ++
                "['mscorlib']System.Console::" ++
                "Write(class ['mscorlib']System.String)\n",
                !IO)
        )
    ;
        output_trace_instr(OutInfo, Instr, !Info, !IO),
        io.write_string("\t", !IO),
        output_instr(OutInfo, Instr, !Info, !IO),
        io.write_string("\n", !IO)
    ).

:- pred output_trace_instr(ilasm_out_info::in, instr::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_trace_instr(OutInfo, Instr, !Info, !IO) :-
    io.write_string("\t\tldstr """, !IO),
        % We have to quote loadstrings.
    ( Instr = ldstr(LoadString) ->
        io.write_string("ldstr \\""", !IO),
        output_escaped_string(LoadString, '\"', !IO),
        io.write_string("\\""", !IO)
            % XXX there could be issues with
            % comments containing embedded newlines
    ; Instr = comment(Comment) ->
        io.write_string("comment: ", !IO),
        io.write_string(Comment, !IO)
    ;
        output_instr(OutInfo, Instr, !Info, !IO)
    ),
    io.write_string("\\n", !IO),
    io.write_string("""\n", !IO),
    io.write_string("\t\tcall void ['mscorlib']System.Console::" ++
        "Write(class ['mscorlib']System.String)\n", !IO).

:- pred output_trace(string::in, io::di, io::uo) is det.

output_trace(S, !IO) :-
    io.write_string("\t\tldstr """, !IO),
    io.write_string(S, !IO),
    io.write_string("\\n""\n", !IO),
    io.write_string("\t\tcall void " ++
        "['mscorlib']System.Console::Write(class System.String)\n",
        !IO).

:- pred output_instruction(ilasm_out_info::in, instr::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_instruction(OutInfo, Instr, !Info, !IO) :-
    (
        Instr = comment(_),
        OutInfo ^ ilaoi_auto_comments = no
    ->
        true
    ;
        io.write_string("\t", !IO),
        output_instr(OutInfo, Instr, !Info, !IO),
        io.write_string("\n", !IO)
    ).

:- pred output_instr(ilasm_out_info::in, instr::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_instr(OutInfo, Instr, !Info, !IO) :-
    (
        Instr = il_asm_code(Code, _MaxStack),
        io.write_string(Code, !IO)
    ;
        Instr = comment(Comment),
        output_comment_string(Comment, !IO)
    ;
        Instr = label(Label),
        output_label(Label, !IO),
        io.write_string(":", !IO)
    ;
        Instr = start_block(BlockType, Id),
        (
            BlockType = bt_scope(Locals),
            io.write_string("{", !IO),
            io.write_string("\t// #", !IO),
            io.write_int(Id, !IO),
            (
                Locals = []
            ;
                Locals = [_ | _],
                io.write_string("\n\t.locals (\n\t\t", !IO),
                ilasm_write_list(Locals, ",\n\t\t", output_local(OutInfo),
                    !Info, !IO),
                io.write_string("\n\t)\n", !IO)
            )
        ;
            BlockType = bt_try,
            io.write_string(".try {", !IO),
            io.write_string("\t// #", !IO),
            io.write_int(Id, !IO)
        ;
            BlockType = bt_catch(ClassName),
            io.write_string("catch ", !IO),
            output_class_name(OutInfo, ClassName, !Info, !IO),
            io.write_string(" {", !IO),
            io.write_string("\t// #", !IO),
            io.write_int(Id, !IO)
        )
    ;
        Instr = end_block(BlockType, Id),
        (
            BlockType = bt_scope(_),
            io.write_string("}", !IO),
            io.write_string("\t// #", !IO),
            io.write_int(Id, !IO)
        ;
            BlockType = bt_catch(_),
            io.write_string("}", !IO),
            io.write_string("\t// #", !IO),
            io.write_int(Id, !IO),
            io.write_string(" (catch block)", !IO)
        ;
            BlockType = bt_try,
            io.write_string("}", !IO),
            io.write_string("\t// #", !IO),
            io.write_int(Id, !IO),
            io.write_string(" (try block)", !IO)
        )
    ;
        Instr = context(File, Line),
        LineNumbers = OutInfo ^ ilaoi_line_numbers,
        (
            LineNumbers = yes,
            io.write_string("\n\t.line ", !IO),
            io.write_int(Line, !IO),
            io.write_string(" '", !IO),
            io.write_string(File, !IO),
            io.write_string("'", !IO)
        ;
            LineNumbers = no
        )
    ;
        Instr = call(MethodRef),
        io.write_string("call\t", !IO),
        output_methodref(OutInfo, MethodRef, !Info, !IO)
    ;
        Instr = callvirt(MethodRef),
        io.write_string("callvirt\t", !IO),
        output_methodref(OutInfo, MethodRef, !Info, !IO)
    ;
        Instr = calli(Signature),
        io.write_string("calli\t", !IO),
        output_name_signature_and_call_conv(OutInfo, Signature, no, "\t\t",
            !Info, !IO)
    ;
        Instr = ret,
        io.write_string("ret", !IO)
    ;
        Instr = bitwise_and,
        io.write_string("and", !IO)
    ;
        Instr = arglist,
        io.write_string("arglist", !IO)
    ;
        Instr = break,
        io.write_string("break", !IO)
    ;
        Instr = ceq,
        io.write_string("ceq", !IO)
    ;
        Instr = ckfinite,
        io.write_string("ckfinite", !IO)
    ;
        Instr = cpblk,
        io.write_string("cpblk", !IO)
    ;
        Instr = dup,
        io.write_string("dup", !IO)
    ;
        Instr = endfilter,
        io.write_string("endfilter", !IO)
    ;
        Instr = endfinally,
        io.write_string("endfinally", !IO)
    ;
        Instr = initblk,
        io.write_string("initblk", !IO)
    ;
        Instr = ldnull,
        io.write_string("ldnull", !IO)
    ;
        Instr = localloc,
        io.write_string("localloc", !IO)
    ;
        Instr = neg,
        io.write_string("neg", !IO)
    ;
        Instr = nop,
        io.write_string("nop", !IO)
    ;
        Instr = bitwise_not,
        io.write_string("not", !IO)
    ;
        Instr = bitwise_or,
        io.write_string("or", !IO)
    ;
        Instr = pop,
        io.write_string("pop", !IO)
    ;
        Instr = shl,
        io.write_string("shl", !IO)
    ;
        Instr = tailcall,
        io.write_string("tail.", !IO)
    ;
        Instr = volatile,
        io.write_string("volatile", !IO)
    ;
        Instr = bitwise_xor,
        io.write_string("xor", !IO)
    ;
        Instr = ldlen,
        io.write_string("ldlen", !IO)
    ;
        Instr = throw,
        io.write_string("throw", !IO)
    ;
        % There are short forms of various instructions.
        % The assembler can't generate them for you.
        Instr = ldarg(index(Index)),
        ( Index < 4 ->
            io.write_string("ldarg.", !IO),
            io.write_int(Index, !IO)
        ; Index < 256 ->
            io.write_string("ldarg.s\t", !IO),
            output_index(Index, !IO)
        ;
            io.write_string("ldarg\t", !IO),
            output_index(Index, !IO)
        )
    ;
        Instr = ldarg(name(Id)),
        io.write_string("ldarg\t", !IO),
        output_id(Id, !IO)
    ;
        Instr = ldc(Type, Const),
        % Lots of short forms for loading integer.
        % XXX Should probably put the magic numbers in functions.
        ( ( Type = int32 ; Type = bool ), Const = i(IntConst)  ->
            ( IntConst < 8, IntConst >= 0 ->
                io.write_string("ldc.i4.", !IO),
                io.write_int(IntConst, !IO)
            ; IntConst = -1 ->
                io.write_string("ldc.i4.m1", !IO)
            ; IntConst < 128, IntConst > -128 ->
                io.write_string("ldc.i4.s\t", !IO),
                io.write_int(IntConst, !IO)
            ;
                io.write_string("ldc.i4\t", !IO),
                io.write_int(IntConst, !IO)
            )
        ; Type = int64, Const = i(IntConst) ->
            io.write_string("ldc.i8\t", !IO),
            io.write_int(IntConst, !IO)
        ; Type = float32, Const = f(FloatConst) ->
            io.write_string("ldc.r4\t", !IO),
            c_util.output_float_literal(FloatConst, !IO)
        ; Type = float64, Const = f(FloatConst) ->
            io.write_string("ldc.r8\t", !IO),
            c_util.output_float_literal(FloatConst, !IO)
        ;
            unexpected($module, $pred,
                "Inconsistent arguments in ldc instruction")
        )
    ;
        Instr = ldstr(String),
        io.write_string("ldstr\t", !IO),
        output_string_constant(String, !IO)
    ;
        Instr = add(Overflow, Signed),
        io.write_string("add", !IO),
        output_overflow(Overflow, !IO),
        output_signed(Signed, !IO)
    ;
        Instr = beq(Target),
        io.write_string("beq ", !IO),
        output_target(Target, !IO)
    ;
        Instr = bge(Signed, Target),
        io.write_string("bge", !IO),
        output_signed(Signed, !IO),
        io.write_string("\t", !IO),
        output_target(Target, !IO)
    ;
        Instr = bgt(Signed, Target),
        io.write_string("bgt", !IO),
        output_signed(Signed, !IO),
        io.write_string("\t", !IO),
        output_target(Target, !IO)
    ;
        Instr = ble(Signed, Target),
        io.write_string("ble", !IO),
        output_signed(Signed, !IO),
        io.write_string("\t", !IO),
        output_target(Target, !IO)
    ;
        Instr = blt(Signed, Target),
        io.write_string("blt", !IO),
        output_signed(Signed, !IO),
        io.write_string("\t", !IO),
        output_target(Target, !IO)
    ;
        Instr = bne(Signed, Target),
        io.write_string("bne", !IO),
        output_signed(Signed, !IO),
        io.write_string("\t", !IO),
        output_target(Target, !IO)
    ;
        Instr = br(Target),
        io.write_string("br\t", !IO),
        output_target(Target, !IO)
    ;
        Instr = brfalse(Target),
        io.write_string("brfalse\t", !IO),
        output_target(Target, !IO)
    ;
        Instr = brtrue(Target),
        io.write_string("brtrue\t", !IO),
        output_target(Target, !IO)
    ;
        Instr = cgt(Signed),
        io.write_string("cgt", !IO),
        output_signed(Signed, !IO)
    ;
        Instr = clt(Signed),
        io.write_string("clt", !IO),
        output_signed(Signed, !IO)
    ;
        Instr = conv(SimpleType),
        io.write_string("conv.", !IO),
        output_simple_type_opcode(SimpleType, !IO)
    ;
        Instr = div(Signed),
        io.write_string("div", !IO),
        output_signed(Signed, !IO)
    ;
        Instr = jmp(MethodRef),
        io.write_string("jmp\t", !IO),
        output_methodref(OutInfo, MethodRef, !Info, !IO)
    ;
        % XXX can use short encoding for indexes
        Instr = ldarga(Variable),
        io.write_string("ldarga\t", !IO),
        (
            Variable = index(Index),
            output_index(Index, !IO)
        ;
            Variable = name(Name),
            output_id(Name, !IO)
        )
    ;
        Instr = ldftn(MethodRef),
        io.write_string("ldftn\t", !IO),
        output_methodref(OutInfo, MethodRef, !Info, !IO)
    ;
        Instr = ldind(SimpleType),
        io.write_string("ldind.", !IO),
        output_simple_type_opcode(SimpleType, !IO)
    ;
        % XXX can use short encoding for indexes
        Instr = ldloc(Variable),
        io.write_string("ldloc\t", !IO),
        (
            Variable = index(Index),
            output_index(Index, !IO)
        ;
            Variable = name(Name),
            output_id(Name, !IO)
        )
    ;
        % XXX can use short encoding for indexes
        Instr = ldloca(Variable),
        io.write_string("ldloca\t", !IO),
        (
            Variable = index(Index),
            output_index(Index, !IO)
        ;
            Variable = name(Name),
            output_id(Name, !IO)
        )
    ;
        Instr = leave(Target),
        io.write_string("leave\t", !IO),
        output_target(Target, !IO)
    ;
        Instr = mul(Overflow, Signed),
        io.write_string("mul", !IO),
        output_overflow(Overflow, !IO),
        output_signed(Signed, !IO)
    ;
        Instr = rem(Signed),
        io.write_string("rem", !IO),
        output_signed(Signed, !IO)
    ;
        Instr = shr(Signed),
        io.write_string("shr", !IO),
        output_signed(Signed, !IO)
    ;
        % XXX can use short encoding for indexes
        Instr = starg(Variable),
        io.write_string("starg\t", !IO),
        (
            Variable = index(Index),
            output_index(Index, !IO)
        ;
            Variable = name(Name),
            output_id(Name, !IO)
        )
    ;
        % XXX can use short encoding for indexes
        Instr = stind(SimpleType),
        io.write_string("stind.", !IO),
        output_simple_type_opcode(SimpleType, !IO)
    ;
        Instr = stloc(Variable),
        io.write_string("stloc\t", !IO),
        (
            Variable = index(Index),
            output_index(Index, !IO)
        ;
            Variable = name(Name),
            output_id(Name, !IO)
        )
    ;
        Instr = sub(OverFlow, Signed),
        io.write_string("sub", !IO),
        output_overflow(OverFlow, !IO),
        output_signed(Signed, !IO)
    ;
        Instr = switch(Targets),
        io.write_string("switch (", !IO),
        io.write_list(Targets, ", ", output_target, !IO),
        io.write_string(")", !IO)
    ;
        Instr = unaligned(_),
        io.write_string("unaligned.", !IO)
    ;
        Instr = box(Type),
        io.write_string("box\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = castclass(Type),
        (
            Type = il_type(_, '[]'(ElementType, _)),
            ElementType = il_type(_, class(Name)),
            Name = structured_name(assembly("mscorlib"),
                ["System", "Type"], _)
        ->
            % XXX There is bug where castclass to System.Type[]
            % sometimes erroneously fails, so we comment out these
            % castclass's.
            io.write_string("// ", !IO)
        ;
            true
        ),
        io.write_string("castclass\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = cpobj(Type),
        io.write_string("cpobj\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = initobj(Type),
        io.write_string("initobj\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = isinst(Type),
        io.write_string("isinst\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = ldelem(SimpleType),
        io.write_string("ldelem.", !IO),
        output_simple_type_opcode(SimpleType, !IO)
    ;
        Instr = ldelema(Type),
        io.write_string("ldelema\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = ldfld(FieldRef),
        io.write_string("ldfld\t", !IO),
        output_fieldref(OutInfo, FieldRef, !Info, !IO)
    ;
        Instr = ldflda(FieldRef),
        io.write_string("ldflda\t", !IO),
        output_fieldref(OutInfo, FieldRef, !Info, !IO)
    ;
        Instr = ldobj(Type),
        io.write_string("ldobj\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = ldsfld(FieldRef),
        io.write_string("ldsfld\t", !IO),
        output_fieldref(OutInfo, FieldRef, !Info, !IO)
    ;
        Instr = ldsflda(FieldRef),
        io.write_string("ldsflda\t", !IO),
        output_fieldref(OutInfo, FieldRef, !Info, !IO)
    ;
        % XXX should be implemented
        Instr = ldtoken(_),
        sorry($module, $pred, "ldtoken not implemented")
    ;
        Instr = ldvirtftn(MethodRef),
        io.write_string("ldvirtftn\t", !IO),
        output_methodref(OutInfo, MethodRef, !Info, !IO)
    ;
        Instr = mkrefany(Type),
        io.write_string("mkrefany\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = newarr(Type),
        io.write_string("newarr\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = newobj(MethodRef),
        io.write_string("newobj\t", !IO),
        output_methodref(OutInfo, MethodRef, !Info, !IO)
    ;
        Instr = refanytype,
        io.write_string("refanytype", !IO)
    ;
        Instr = refanyval(Type),
        io.write_string("refanyval\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = rethrow,
        io.write_string("rethrow", !IO)
    ;
        Instr = stelem(SimpleType),
        io.write_string("stelem.", !IO),
        output_simple_type_opcode(SimpleType, !IO)
    ;
        Instr = stfld(FieldRef),
        io.write_string("stfld\t", !IO),
        output_fieldref(OutInfo, FieldRef, !Info, !IO)
    ;
        Instr = stobj(Type),
        io.write_string("stobj\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = sizeof(Type),
        io.write_string("sizeof\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        Instr = stsfld(FieldRef),
        io.write_string("stsfld\t", !IO),
        output_fieldref(OutInfo, FieldRef, !Info, !IO)
    ;
        Instr = unbox(Type),
        io.write_string("unbox\t", !IO),
        output_type(OutInfo, Type, !Info, !IO)
    ).

    % XXX might use this later.
:- func max_efficient_encoding_short = int.

max_efficient_encoding_short = 256.

:- pred output_overflow(overflow::in, io::di, io::uo) is det.

output_overflow(OverFlow, !IO) :-
    (
        OverFlow = checkoverflow,
        io.write_string(".ovf", !IO)
    ;
        OverFlow = nocheckoverflow
    ).

:- pred output_signed(signed::in, io::di, io::uo) is det.

output_signed(Signed, !IO) :-
    (
        Signed = signed
    ;
        Signed = unsigned,
        io.write_string(".un", !IO)
    ).

:- pred output_target(target::in, io::di, io::uo) is det.

output_target(offset_target(Target), !IO) :-
    io.write_int(Target, !IO).
output_target(label_target(Label), !IO) :-
    output_label(Label, !IO).

:- pred output_fieldref(ilasm_out_info::in, fieldref::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_fieldref(OutInfo, fieldref(Type, ClassMemberName), !Info, !IO) :-
    output_type(OutInfo, Type, !Info, !IO),
    io.write_string("\n\t\t", !IO),
    output_class_member_name(OutInfo, !.Info, ClassMemberName, !IO).

:- pred output_methodref(ilasm_out_info::in, methodref::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_methodref(OutInfo, MethodRef, !Info, !IO) :-
    (
        MethodRef = methoddef(call_conv(IsInstance, _), ReturnType,
            ClassMemberName, ArgTypes),
        (
            IsInstance = yes,
            io.write_string("instance ", !IO)
        ;
            IsInstance = no
        ),
        output_ret_type(OutInfo, ReturnType, !Info, !IO),
        io.write_string("\n\t\t", !IO),
        output_class_member_name(OutInfo, !.Info, ClassMemberName, !IO),
        (
            ArgTypes = [],
            io.write_string("()\n", !IO)
        ;
            ArgTypes = [_ | _],
            io.write_string("(\n\t\t\t", !IO),
            ilasm_write_list(ArgTypes, ",\n\t\t\t", output_type(OutInfo),
                !Info, !IO),
            io.write_string("\n\t\t)", !IO)
        )
    ;
        MethodRef = local_method(call_conv(IsInstance, _), ReturnType,
            MethodName, ArgTypes),
        (
            IsInstance = yes,
            io.write_string("instance ", !IO)
        ;
            IsInstance = no
        ),
        output_ret_type(OutInfo, ReturnType, !Info, !IO),
        io.write_string("\n\t\t", !IO),
        output_member_name(MethodName, !IO),
        (
            ArgTypes = [],
            io.write_string("()\n", !IO)
        ;
            ArgTypes = [_ | _],
            io.write_string("(\n\t\t\t", !IO),
            ilasm_write_list(ArgTypes, ",\n\t\t\t", output_type(OutInfo),
                !Info, !IO),
            io.write_string("\n\t\t)", !IO)
        )
    ).

:- pred output_classattr(classattr::in, io::di, io::uo) is det.

output_classattr(abstract, !IO) :-
    io.write_string("abstract", !IO).
output_classattr(ansi, !IO) :-
    io.write_string("ansi", !IO).
output_classattr(auto, !IO) :-
    io.write_string("auto", !IO).
output_classattr(autochar, !IO) :-
    io.write_string("autochar", !IO).
output_classattr(beforefieldinit, !IO) :-
    io.write_string("beforefieldinit", !IO).
output_classattr(explicit, !IO) :-
    io.write_string("explicit", !IO).
output_classattr(interface, !IO) :-
    io.write_string("interface", !IO).
output_classattr(nestedassembly, !IO) :-
    io.write_string("nested assembly", !IO).
output_classattr(nestedfamandassem, !IO) :-
    io.write_string("nested famandassem", !IO).
output_classattr(nestedfamily, !IO) :-
    io.write_string("nested family", !IO).
output_classattr(nestedfamorassem, !IO) :-
    io.write_string("nested famorassem", !IO).
output_classattr(nestedprivate, !IO) :-
    io.write_string("nested private", !IO).
output_classattr(nestedpublic, !IO) :-
    io.write_string("nested public", !IO).
output_classattr(private, !IO) :-
    io.write_string("private", !IO).
output_classattr(public, !IO) :-
    io.write_string("public", !IO).
output_classattr(rtspecialname, !IO) :-
    io.write_string("rtspecialname", !IO).
output_classattr(sealed, !IO) :-
    io.write_string("sealed", !IO).
output_classattr(sequential, !IO) :-
    io.write_string("sequential", !IO).
output_classattr(serializable, !IO) :-
    io.write_string("serializable", !IO).
output_classattr(specialname, !IO) :-
    io.write_string("specialname", !IO).
output_classattr(unicode, !IO) :-
    io.write_string("unicode", !IO).

:- pred output_assembly_decl(ilasm_out_info::in, assembly_decl::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_assembly_decl(OutInfo, AssemblyDecl, !Info, !IO) :-
    (
        AssemblyDecl = version(A, B, C, D),
        io.format(".ver %d:%d:%d:%d", [i(A), i(B), i(C), i(D)], !IO)
    ;
        AssemblyDecl = public_key_token(Token),
        io.write_string(".publickeytoken = ( ", !IO),
        io.write_list(Token, " ", output_hexbyte, !IO),
        io.write_string(" ) ", !IO)
    ;
        AssemblyDecl = hash(Hash),
        io.write_string(".hash = ( ", !IO),
        io.write_list(Hash, " ", output_hexbyte, !IO),
        io.write_string(" ) ", !IO)
    ;
        AssemblyDecl = custom(CustomDecl),
        output_custom_decl(OutInfo, CustomDecl, !Info, !IO)
    ).

:- pred output_custom_decl(ilasm_out_info::in, custom_decl::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_custom_decl(OutInfo, CustomDecl, !Info, !IO) :-
    CustomDecl = custom_decl(Type, MaybeOwner, StringOrBytes),
    io.write_string(".custom ", !IO),
    (
        MaybeOwner = yes(Owner),
        io.write_string(" (", !IO),
        output_custom_type(OutInfo, Owner, !Info, !IO),
        io.write_string(") ", !IO)
    ;
        MaybeOwner = no
    ),
    output_custom_type(OutInfo, Type, !Info, !IO),
    (
        StringOrBytes = bytes(Bytes),
        io.write_string(" = (", !IO),
        io.write_list(Bytes, " ", output_hexbyte, !IO),
        io.write_string(")", !IO)
    ;
        ( StringOrBytes = qstring(_)
        ; StringOrBytes = no_initalizer
        ),
        sorry($module, $pred, "unexpected custom_decl")
    ),
    io.write_string("\n", !IO).

:- pred output_custom_type(ilasm_out_info::in, custom_type::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_custom_type(OutInfo, CustomType, !Info, !IO) :-
    (
        CustomType = type(Type),
        output_type(OutInfo, Type, !Info, !IO)
    ;
        CustomType = methodref(MethodRef),
        output_methodref(OutInfo, MethodRef, !Info, !IO)
    ).

:- pred output_index(index::in, io::di, io::uo) is det.

output_index(Index, !IO) :-
    io.write_int(Index, !IO).

:- pred output_string_constant(string::in, io::di, io::uo) is det.

output_string_constant(String, !IO) :-
    io.write_string("""", !IO),
    output_escaped_string(String, '\"', !IO),
    io.write_string("""", !IO).

:- pred output_class_member_name(ilasm_out_info::in, ilasm_info::in,
    class_member_name::in, io::di, io::uo) is det.

output_class_member_name(OutInfo, Info, ClassMemberName, !IO) :-
    ClassMemberName = class_member_name(StructuredName, MemberName),
    output_structured_name(OutInfo, Info, StructuredName, !IO),
    io.write_string("::", !IO),
    output_member_name(MemberName, !IO).

:- pred output_structured_name(ilasm_out_info::in, ilasm_info::in,
    structured_name::in, io::di, io::uo) is det.

output_structured_name(OutInfo, Info, StructuredName, !IO) :-
    StructuredName = structured_name(Asm, DottedName, NestedClasses),
    SeparateAssemblies = OutInfo ^ ilaoi_separate_assemblies,
    (
        Asm = assembly(Assembly),
        maybe_output_quoted_assembly_name(Assembly, Info, !IO)
    ;
        Asm = module(Module, Assembly),
        (
            SeparateAssemblies = yes,
            maybe_output_quoted_assembly_name(Module, Info, !IO)
        ;
            SeparateAssemblies = no,
            (
                Info ^ current_assembly \= "",
                string.prefix(Module, Info ^ current_assembly)
            ->
                quote_id(Module ++ ".dll", QuotedModuleName),
                io.format("[.module %s]", [s(QuotedModuleName)], !IO)
            ;
                maybe_output_quoted_assembly_name(Assembly, Info, !IO)
            )
        )
    ),
    output_dotted_name(DottedName, !IO),
    output_nested_class_quals(NestedClasses, !IO).

:- pred maybe_output_quoted_assembly_name(ilds.id::in, ilasm_info::in,
    io::di, io::uo) is det.

maybe_output_quoted_assembly_name(Assembly, Info, !IO) :-
    (
        Assembly \= "",
        Assembly \= Info ^ current_assembly
    ->
        quote_id(Assembly, QuotedAssemblyName),
        io.format("[%s]", [s(QuotedAssemblyName)], !IO)
    ;
        true
    ).

:- pred output_dotted_name(namespace_qual_name::in, io::di, io::uo) is det.

output_dotted_name(Name, !IO) :-
    io.write_list(Name, ".", output_id, !IO).

:- pred output_nested_class_quals(nested_class_name::in,
    io::di, io::uo) is det.

output_nested_class_quals(Name, !IO) :-
    list.foldl(
        (pred(Id::in, IO0::di, IO::uo) is det :-
            io.write_char('/', IO0, IO1),
            output_id(Id, IO1, IO)
        ),
        Name, !IO).

:- pred output_id(ilds.id::in, io::di, io::uo) is det.

output_id(Id, !IO) :-
    quote_id(Id, QuotedId),
    io.write_string(QuotedId, !IO).

:- pred output_field_initializer(field_initializer::in, io::di, io::uo) is det.

output_field_initializer(none, !IO).
output_field_initializer(at(Id), !IO) :-
    io.write_string(" at ", !IO),
    output_id(Id, !IO).
output_field_initializer(equals(FieldInit), !IO) :-
    io.write_string(" = ", !IO),
    output_field_init(FieldInit, !IO).

:- pred output_field_init(field_init::in, io::di, io::uo) is det.

output_field_init(binary_float64(Int64), !IO) :-
    io.write_string("float64(", !IO),
    output_int64(Int64, !IO),
    io.write_string(")", !IO).
output_field_init(binary_float32(Int32), !IO) :-
    io.write_string("float32(", !IO),
    output_int32(Int32, !IO),
    io.write_string(")", !IO).
output_field_init(wchar_ptr(String), !IO) :-
    io.write_string("wchar *(", !IO),
    io.write(String, !IO),
    io.write_string(")", !IO).
        % XXX should check for invalid data_items
output_field_init(data_item(DataItem), !IO) :-
    ( DataItem = char_ptr(String) ->
        io.write(String, !IO)
    ;
        output_data_item(DataItem, !IO)
    ).

:- pred output_data_body(data_body::in, io::di, io::uo) is det.

output_data_body(itemlist(DataItemList), !IO) :-
    io.write_string("{", !IO),
    io.write_list(DataItemList, ", ", output_data_item, !IO),
    io.write_string("}", !IO).
output_data_body(item(DataItem), !IO) :-
    output_data_item(DataItem, !IO).

:- pred output_data_item(data_item::in, io::di, io::uo) is det.

output_data_item(float64(Float), !IO) :-
    io.write_string("float64(", !IO),
    output_float64(Float, !IO),
    io.write_string(")", !IO).
output_data_item(float32(Float32), !IO) :-
    io.write_string("float32(", !IO),
    output_float32(Float32, !IO),
    io.write_string(")", !IO).
output_data_item(int64(Int64), !IO) :-
    io.write_string("int64(", !IO),
    output_int64(Int64, !IO),
    io.write_string(")", !IO).
output_data_item(int32(Int32), !IO) :-
    io.write_string("int32(", !IO),
    output_int32(Int32, !IO),
    io.write_string(")", !IO).
output_data_item(int16(Int16), !IO) :-
    io.write_string("int16(", !IO),
    output_int16(Int16, !IO),
    io.write_string(")", !IO).
output_data_item(int8(Int8), !IO) :-
    io.write_string("int8(", !IO),
    output_int8(Int8, !IO),
    io.write_string(")", !IO).
output_data_item(char_ptr(String), !IO) :-
    io.write_string("char *(", !IO),
    io.write(String, !IO),
    io.write_string(")", !IO).
output_data_item('&'(Id), !IO) :-
    io.write_string("&(", !IO),
    output_id(Id, !IO),
    io.write_string(")", !IO).
output_data_item(bytearray(Bytes), !IO) :-
    io.write_string("bytearray(", !IO),
    io.write_list(Bytes, " ", output_hexbyte, !IO),
    io.write_string(")", !IO).

:- pred output_float64(float64::in, io::di, io::uo) is det.

output_float64(float64(Float), !IO) :-
    io.write_float(Float, !IO).

:- pred output_float32(float32::in, io::di, io::uo) is det.

output_float32(float32(Float), !IO) :-
    io.write_float(Float, !IO).

:- pred output_int64(int64::in, io::di, io::uo) is det.

output_int64(int64(Integer), !IO) :-
    io.write_string(integer.to_string(Integer), !IO).

:- pred output_int32(int32::in, io::di, io::uo) is det.

output_int32(int32(Int), !IO) :-
    io.write_int(Int, !IO).

:- pred output_int16(int16::in, io::di, io::uo) is det.

output_int16(int16(Int), !IO) :-
    io.write_int(Int, !IO).

:- pred output_int8(int8::in, io::di, io::uo) is det.

output_int8(int8(Int), !IO) :-
    io.write_int(Int, !IO).

:- pred output_byte(byte::in, io::di, io::uo) is det.

output_byte(Byte, !IO) :-
    output_int8(Byte, !IO).

:- pred output_hexbyte(byte::in, io::di, io::uo) is det.

output_hexbyte(int8(Int), !IO) :-
    string.int_to_base_string(Int, 16, Tmp),
    io.write_string(Tmp, !IO).

:- pred output_comment_string(string::in, io::di, io::uo) is det.

output_comment_string(Comment, !IO) :-
    io.write_string("// ", !IO),
    CommentDoc = separated(text, line,
        string.words_separator((pred('\n'::in) is semidet :- true), Comment)),
    Doc = label("\t// ", CommentDoc),
    write(70, Doc, !IO).

    % We need to quote all the IDs we output to avoid bumping into
    % keywords that assembler uses (there are a lot of them, and
    % there is no list available).
:- pred quote_id(ilds.id::in, string::out) is det.

quote_id(Id, QuotedId) :-
    escape_string(Id, '\'', EscapedId),
    string.append_list(["'", EscapedId, "'"], QuotedId).

:- pred output_escaped_string(string::in, char::in, io::di, io::uo) is det.

output_escaped_string(String, EscapeChar, !IO) :-
    escape_string(String, EscapeChar, EscapedString),
    io.write_string(EscapedString, !IO).

    % Replace all Rep0 with backslash quoted Rep0 in Str0,
    % giving the escaped string Str.
    % We also escape embedded newlines and other characters.
    % We already do some name mangling during code generation that
    % means we avoid most weird characters here.
    %
:- pred escape_string(string::in, char::in, string::out) is det.

escape_string(Str0, ReplaceChar, Str) :-
    string.to_char_list(Str0, CharList0),
    list.foldl(
        (pred(Char::in, E0::in, E::out) is det :-
            ( escape_special_char(Char, QuoteChar) ->
                E = [QuoteChar, '\\' | E0]
            ; Char = ReplaceChar ->
                E = [ReplaceChar, '\\' | E0]
            ;
                E = [Char | E0]
            )
        ), CharList0, [], CharList),
    string.from_rev_char_list(CharList, Str).

    % Characters that should be escaped in strings, and the
    % character to escape with.
    %
:- pred escape_special_char(char::in, char::out) is semidet.

escape_special_char('\\', '\\').
escape_special_char('\n', 'n').
escape_special_char('\t', 't').
escape_special_char('\b', 'b').

%-----------------------------------------------------------------------------%

:- type ilasm_out_info
    --->    ilasm_out_info(
                ilaoi_auto_comments         :: bool,
                ilaoi_line_numbers          :: bool,
                ilaoi_debug_il_asm          :: bool,
                ilaoi_separate_assemblies   :: bool
            ).

:- func init_ilasm_out_info(globals) = ilasm_out_info.

init_ilasm_out_info(Globals) = Info :-
    globals.lookup_bool_option(Globals, auto_comments, AutoComments),
    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    globals.lookup_bool_option(Globals, debug_il_asm, DebugIlAsm),
    globals.lookup_bool_option(Globals, separate_assemblies,
        SeparateAssemblies),
    Info = ilasm_out_info(AutoComments, LineNumbers, DebugIlAsm,
        SeparateAssemblies).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.ilasm.
%-----------------------------------------------------------------------------%
