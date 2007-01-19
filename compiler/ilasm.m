%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2007 The University of Melbourne.
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

:- import_module ml_backend.ilds.

:- import_module bool.
:- import_module integer.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module term.

%-----------------------------------------------------------------------------%

:- pred ilasm.output(list(decl)::in, io::di, io::uo) is det.

:- type int64 ---> int64(integer).
:- type int32 ---> int32(int).
:- type int16 ---> int16(int).
:- type int8 ---> int8(int).
:- type byte == int8.
:- type float64 ---> float64(float).
:- type float32 ---> float32(float).

    % A top level declaration in IL assembler.
    %
:- type decl
            % .class declaration
    --->    class(
                list(classattr),    % Attributes for the class.
                ilds.id,            % Name of the class.
                extends,            % What is the parent class?
                implements,         % What interfaces are implemented?
                list(class_member)  % Methods and fields.
            )

            % .namespace declaration
    ;       namespace(
                namespace_qual_name,    % Namespace name.
                list(decl)              % Contents.
            )

            % .method  (a global function)
            % There are lots of restrictions on global functions so
            % don't get too excited about using them for anything.
            % In particular, you can't reference a namespace
            % qualified global function from outside the module.
    ;       method(
                methodhead,
                method_defn
            )

            % .data  (module local data)
    ;       data(
                bool,            % Is data in thread local storage?
                maybe(ilds.id),  % id to name this data.
                data_body        % Body of data.
            )

            % .file
            % Declares a file associated with the current assembly.
    ;       file(ilds.id)

            % .module extern
            % Declares a module name.
    ;       extern_module(ilds.id)

            % .assembly extern
            % Declares an assembly name, and possibly its strong
            % name/version number.
    ;       extern_assembly(ilds.id, list(assembly_decl))

            % .assembly
            % Defines an assembly.
    ;       assembly(ilds.id)

            % .custom
            % A custom attribute.
    ;       custom(custom_decl)

    %
    % Comments
    %

    ;       comment_term(term)

            % Print almost anything using pprint.to_doc
            % (see library/pprint.m for limitations).
    ;       some [T] comment_thing(T)
    ;       comment(string).

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
    --->    method(
                methodhead,     % Name, signature, attributes.
                method_defn     % Definition of method.
            )

            % .field (a class field)
    ;       field(
                list(fieldattr),    % Attributes.
                il_type,            % Field type.
                ilds.id,            % Field name.
                maybe(int32),       % Offset for explicit layout.
                field_initializer   % Initializer.
            )

            % .property (a class property)
    ;       property(
                il_type,            % Property type.
                ilds.id,            % Property name.
                maybe(methodhead),  % Get property.
                maybe(methodhead)   % Set property.
            )

            % .class (a nested class)
    ;       nested_class(
                list(classattr),    % Attributes for the class.
                ilds.id,           % Name of the class.
                extends,            % What is the parent class?
                implements,         % What interfaces are implemented?
                list(class_member)  % Methods and fields.
            )

    ;       custom(custom_decl)     % custom attribute

    %
    % Comments
    %

    ;       comment_term(term)
    ;       comment(string)

            % print almost anything using pprint.to_doc
            % (see library/pprint.m for limitations).
    ;       some [T] comment_thing(T).

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
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.

:- import_module char.
:- import_module getopt_io.
:- import_module int.
:- import_module pair.
:- import_module pprint.
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

:- pred ilasm.write_list(list(T)::in, string::in,
    pred(T, ilasm_info, ilasm_info, io, io)
        ::in(pred(in, in, out, di, uo) is det),
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

ilasm.write_list([], _Separator, _OutputPred, !Info, !IO).
ilasm.write_list([E | Es], Separator, OutputPred, !Info, !IO) :-
    OutputPred(E, !Info, !IO),
    (
        Es = []
    ;
        Es = [_ | _],
        io.write_string(Separator, !IO)
    ),
    ilasm.write_list(Es, Separator, OutputPred, !Info, !IO).

ilasm.output(Blocks, !IO) :-
    Info0 = ilasm_info(""),
    ilasm.output(Blocks, Info0, _Info, !IO).

:- pred ilasm.output(list(decl)::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

ilasm.output(Blocks, !Info, !IO) :-
    ilasm.write_list(Blocks, "\n\n", output_decl, !Info, !IO),
    io.write_string("\n\n", !IO).

:- pred output_decl(decl::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_decl(custom(CustomDecl), !Info, !IO) :-
    output_custom_decl(CustomDecl, !Info, !IO).
output_decl(class(Attrs, Id, Extends, Implements, Contents), !Info, !IO) :-
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
        output_class_name(ExtendsModule, !Info, !IO)
    ;
        Extends = extends_nothing
    ),
    Implements = implements(ImplementsList),
    (
        ImplementsList = [_ | _],
        io.write_string(" implements ", !IO),
        ilasm.write_list(ImplementsList, ", ", output_class_name, !Info, !IO)
    ;
        ImplementsList = []
    ),
    io.write_string(" {\n", !IO),
    ilasm.write_list(Contents, "\n", output_class_member, !Info, !IO),
    io.write_string("\n}", !IO).
output_decl(namespace(DottedName, Contents), !Info, !IO) :-
    (
        DottedName = [_ | _],
        io.write_string(".namespace ", !IO),
        output_dotted_name(DottedName, !IO),
        io.write_string(" {\n", !IO),
        output(Contents, !Info, !IO),
        io.write_string("}\n", !IO)
    ;
        DottedName = [],
        output(Contents, !Info, !IO)
    ).
output_decl(method(MethodHead, MethodDecls), !Info, !IO) :-
    io.write_string(".method ", !IO),
    output_methodhead(MethodHead, !Info, !IO),
    io.write_string("\n{\n", !IO),
    ilasm.write_list(MethodDecls, "\n", output_method_body_decl, !Info, !IO),
    io.write_string("}\n", !IO).
output_decl(data(TLS, MaybeId, Body), !Info, !IO) :-
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
    output_data_body(Body, !IO).
output_decl(comment_term(CommentTerm), !Info, !IO) :-
    globals.io_lookup_bool_option(auto_comments, PrintComments, !IO),
    (
        PrintComments = yes,
        io.write_string("// ", !IO),
        varset.init(VarSet),
        term_io.write_term(VarSet, CommentTerm, !IO),
        io.nl(!IO)
    ;
        PrintComments = no
    ).
output_decl(comment_thing(Thing), !Info, !IO) :-
    globals.io_lookup_bool_option(auto_comments, PrintComments, !IO),
    (
        PrintComments = yes,
        Doc = label("// ", to_doc(Thing)),
        write(70, Doc, !IO),
        io.nl(!IO)
    ;
        PrintComments = no
    ).
output_decl(comment(CommentStr), !Info, !IO) :-
    globals.io_lookup_bool_option(auto_comments, PrintComments, !IO),
    (
        PrintComments = yes,
        output_comment_string(CommentStr, !IO)
    ;
        PrintComments = no
    ).
output_decl(extern_assembly(AsmName, AssemblyDecls), !Info, !IO) :-
    io.write_string(".assembly extern ", !IO),
    output_id(AsmName, !IO),
    io.write_string("{\n", !IO),
    list.foldl2((pred(A::in, I0::in, I::out, IO0::di, IO::uo) is det :-
            output_assembly_decl(A, I0, I, IO0, IO1),
            io.write_string("\n\t", IO1, IO)
        ), AssemblyDecls, !Info, !IO),
    io.write_string("\n}\n", !IO).
output_decl(assembly(AsmName), !Info, !IO) :-
    io.write_string(".assembly ", !IO),
    output_id(AsmName, !IO),
    !:Info = !.Info ^ current_assembly := AsmName,
    io.write_string(" { }", !IO).
output_decl(file(FileName), !Info, !IO) :-
    io.write_string(".file ", !IO),
    output_id(FileName, !IO).
output_decl(extern_module(ModName), !Info, !IO) :-
    io.write_string(".module extern ", !IO),
    output_id(ModName, !IO).

:- pred output_class_member(class_member::in, ilasm_info::in,
    ilasm_info::out, io::di, io::uo) is det.

output_class_member(method(MethodHead, MethodDecls), !Info, !IO) :-
        % Don't do debug output on class constructors, since
        % they are automatically generated and take forever to
        % run.
    globals.io_lookup_option(debug_il_asm, DebugIlAsm, !IO),
    ( MethodHead = methodhead(_, cctor, _, _) ->
        globals.io_set_option(debug_il_asm, bool(no), !IO),
        output_decl(method(MethodHead, MethodDecls), !Info, !IO),
        globals.io_set_option(debug_il_asm, DebugIlAsm, !IO)
    ;
        output_decl(method(MethodHead, MethodDecls), !Info, !IO)
    ).

output_class_member(custom(CustomDecl), !Info, !IO) :-
    output_custom_decl(CustomDecl, !Info, !IO).

output_class_member(field(FieldAttrs, Type, IlId, MaybeOffset, Initializer),
        !Info, !IO) :-
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
    output_type(Type, !Info, !IO),
    io.write_string("\n\t", !IO),
    output_id(IlId, !IO),
    output_field_initializer(Initializer, !IO).
output_class_member(property(Type, Name, MaybeGet, MaybeSet), !Info, !IO) :-
    io.write_string(".property instance ", !IO),
    output_type(Type, !Info, !IO),
    io.write_string(" ", !IO),
    output_id(Name, !IO),
    io.write_string("() {", !IO),
    (
        MaybeGet = yes(methodhead(_, GetMethodName, GetSignature, _)),
        io.nl(!IO),
        io.write_string("\t.get instance ", !IO),
        output_name_signature_and_call_conv(GetSignature,
            yes(GetMethodName), "\t\t", !Info, !IO)
    ;
        MaybeGet = no
    ),
    (
        MaybeSet = yes(methodhead(_, SetMethodName, SetSignature, _)),
        io.nl(!IO),
        io.write_string("\t.set instance ", !IO),
        output_name_signature_and_call_conv(SetSignature,
            yes(SetMethodName), "\t\t", !Info, !IO)
    ;
        MaybeSet = no
    ),
    io.write_string("\n}\n", !IO).
output_class_member(nested_class(Attrs, Id, Extends, Implements, Contents),
        !Info, !IO) :-
    output_decl(class(Attrs, Id, Extends, Implements, Contents), !Info, !IO).
output_class_member(comment(CommentStr), !Info, !IO) :-
    globals.io_lookup_bool_option(auto_comments, PrintComments, !IO),
    (
        PrintComments = yes,
        output_comment_string(CommentStr, !IO)
    ;
        PrintComments = no
    ).
output_class_member(comment_term(CommentTerm), !Info, !IO) :-
    globals.io_lookup_bool_option(auto_comments, PrintComments, !IO),
    (
        PrintComments = yes,
        io.write_string("// ", !IO),
        varset.init(VarSet),
        term_io.write_term(VarSet, CommentTerm, !IO),
        io.nl(!IO)
    ;
        PrintComments = no
    ).
output_class_member(comment_thing(Thing), !Info, !IO) :-
    globals.io_lookup_bool_option(auto_comments, PrintComments, !IO),
    (
        PrintComments = yes,
        Doc = label("// ", to_doc(Thing)),
        write(70, Doc, !IO),
        io.nl(!IO)
    ;
        PrintComments = no
    ).

:- pred output_methodhead(methodhead::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_methodhead(methodhead(Attrs, MethodName, Signature, ImplAttrs),
        !Info, !IO) :-
    io.write_list(Attrs, " ", io.write, !IO),
    (
        Attrs = [_ | _],
        io.write_string(" ", !IO)
    ;
        Attrs = []
    ),
    output_name_signature_and_call_conv(Signature, yes(MethodName), "\t",
        !Info, !IO),
    io.write_list(ImplAttrs, " ", io.write, !IO).

:- pred output_method_body_decl(method_body_decl::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_method_body_decl(emitbyte(Int32), !Info, !IO) :-
    io.write_string(".emitbyte ", !IO),
    output_int32(Int32, !IO).
output_method_body_decl(custom(CustomDecl), !Info, !IO) :-
    output_custom_decl(CustomDecl, !Info, !IO).
output_method_body_decl(maxstack(Int32), !Info, !IO) :-
    io.write_string(".maxstack ", !IO),
    output_int32(Int32, !IO).
output_method_body_decl(entrypoint, !Info, !IO) :-
    io.write_string(".entrypoint ", !IO).
output_method_body_decl(zeroinit, !Info, !IO) :-
    io.write_string(".zeroinit ", !IO).
output_method_body_decl(instrs(Instrs), !Info, !IO) :-
    output_instructions(Instrs, !Info, !IO).
output_method_body_decl(label(Label), !Info, !IO) :-
    output_label(Label, !IO),
    io.write_string(":", !IO).

:- pred output_label(label::in, io::di, io::uo) is det.

output_label(Label, !IO) :-
    io.write_string(Label, !IO).

:- pred output_class_name(ilds.class_name::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_class_name(ClassName, !Info, !IO) :-
    output_structured_name(ClassName, !.Info, !IO).

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

:- pred output_name_signature_and_call_conv(signature::in,
    maybe(member_name)::in, string::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_name_signature_and_call_conv(signature(CallConv, ReturnType, ArgTypes),
        MaybeMethodName, Indent, !Info, !IO) :-
    output_call_conv(CallConv, !IO),
    io.write_string("\n", !IO),
    io.write_string(Indent, !IO),
    output_ret_type(ReturnType, !Info, !IO),
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
        ilasm.write_list(ArgTypes, ",\n\t\t", output_param, !Info, !IO),
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

:- pred output_ret_type(ret_type::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_ret_type(void, !Info, !IO) :-
    io.write_string("void", !IO).
output_ret_type(simple_type(Type), !Info, !IO) :-
    output_simple_type(Type, !Info, !IO).

:- pred output_local(pair(ilds.id, il_type)::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_local(Id - Type, !Info, !IO) :-
    output_type(Type, !Info, !IO),
    io.write_string(" ", !IO),
    output_id(Id, !IO).

:- pred output_param(pair(il_type, maybe(ilds.id))::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_param(Type - no, !Info, !IO) :-
    output_type(Type, !Info, !IO).
output_param(Type - yes(Id), !Info, !IO) :-
    output_type(Type, !Info, !IO),
    io.write_string(" ", !IO),
    output_id(Id, !IO).

:- pred output_type(il_type::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_type(il_type(Modifiers, SimpleType), !Info, !IO) :-
    io.write_list(Modifiers, " ", output_modifier, !IO),
    output_simple_type(SimpleType, !Info, !IO).

:- pred output_simple_type(simple_type::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_simple_type(int8, !Info, !IO) :-
    io.write_string("int8", !IO).
output_simple_type(int16, !Info, !IO) :-
    io.write_string("int16", !IO).
output_simple_type(int32, !Info, !IO) :-
    io.write_string("int32", !IO).
output_simple_type(int64, !Info, !IO) :-
    io.write_string("int64", !IO).
output_simple_type(uint8, !Info, !IO) :-
    io.write_string("unsigned int8", !IO).
output_simple_type(uint16, !Info, !IO) :-
    io.write_string("unsigned int16", !IO).
output_simple_type(uint32, !Info, !IO) :-
    io.write_string("unsigned int32", !IO).
output_simple_type(uint64, !Info, !IO) :-
    io.write_string("unsigned int64", !IO).
output_simple_type(native_int, !Info, !IO) :-
    io.write_string("native int", !IO).
output_simple_type(native_uint, !Info, !IO) :-
    io.write_string("native unsigned int", !IO).
output_simple_type(float32, !Info, !IO) :-
    io.write_string("float32", !IO).
output_simple_type(float64, !Info, !IO) :-
    io.write_string("float64", !IO).
output_simple_type(native_float, !Info, !IO) :-
    io.write_string("native float", !IO).
output_simple_type(bool, !Info, !IO) :-
    io.write_string("bool", !IO).
output_simple_type(char, !Info, !IO) :-
    io.write_string("char", !IO).
output_simple_type(object, !Info, !IO) :-
    io.write_string("object", !IO).
output_simple_type(string, !Info, !IO) :-
    io.write_string("string", !IO).
output_simple_type(refany, !Info, !IO) :-
    io.write_string("refany", !IO).
output_simple_type(class(Name), !Info, !IO) :-
    ( name_to_simple_type(Name, Type) ->
        ( Type = reference(SimpleType) ->
            output_simple_type(SimpleType, !Info, !IO)
        ;
            % If it is a value type then we are refering
            % to the boxed version of the value type.
            io.write_string("class ", !IO),
            output_structured_name(Name, !.Info, !IO)
        )
    ;
        io.write_string("class ", !IO),
        output_structured_name(Name, !.Info, !IO)
    ).
output_simple_type(valuetype(Name), !Info, !IO) :-
    ( name_to_simple_type(Name, Type) ->
        ( Type = value(SimpleType) ->
            output_simple_type(SimpleType, !Info, !IO)
        ;
            unexpected(this_file, "builtin reference type")
        )
    ;
        io.write_string("valuetype ", !IO),
        output_structured_name(Name, !.Info, !IO)
    ).
output_simple_type(interface(Name), !Info, !IO) :-
    io.write_string("interface ", !IO),
    output_structured_name(Name, !.Info, !IO).
output_simple_type('[]'(Type, Bounds), !Info, !IO) :-
    output_type(Type, !Info, !IO),
    output_bounds(Bounds, !IO).
output_simple_type('*'(Type), !Info, !IO) :-
    output_type(Type, !Info, !IO),
    io.write_string("*", !IO).
output_simple_type('&'(Type), !Info, !IO) :-
    output_type(Type, !Info, !IO),
    io.write_string("&", !IO).

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

output_simple_type_opcode(int8) --> io.write_string("i1").
output_simple_type_opcode(int16) --> io.write_string("i2").
output_simple_type_opcode(int32) --> io.write_string("i4").
output_simple_type_opcode(int64) --> io.write_string("i8").
output_simple_type_opcode(uint8) --> io.write_string("u1").
output_simple_type_opcode(uint16) --> io.write_string("u2").
output_simple_type_opcode(uint32) --> io.write_string("u4").
output_simple_type_opcode(uint64) --> io.write_string("u8").
output_simple_type_opcode(native_int) --> io.write_string("i").
output_simple_type_opcode(native_uint) --> io.write_string("u").
output_simple_type_opcode(float32) --> io.write_string("r4").
output_simple_type_opcode(float64) --> io.write_string("r8").
output_simple_type_opcode(native_float) -->
    { unexpected(this_file, "unable to create opcode for native_float") }.
    % XXX should i4 be used for bool?
output_simple_type_opcode(bool) --> io.write_string("i4").
output_simple_type_opcode(char) --> io.write_string("i2").

    % All reference types use "ref" as their opcode.
    % XXX is "ref" here correct for value classes?
output_simple_type_opcode(object) --> io.write_string("ref").
output_simple_type_opcode(string) --> io.write_string("ref").
output_simple_type_opcode(refany) --> io.write_string("ref").
output_simple_type_opcode(class(_Name)) --> io.write_string("ref").
output_simple_type_opcode(valuetype(_Name)) --> io.write_string("ref").
output_simple_type_opcode(interface(_Name)) --> io.write_string("ref").
output_simple_type_opcode('[]'(_Type, _Bounds)) --> io.write_string("ref").
output_simple_type_opcode('*'(_Type)) --> io.write_string("ref").
output_simple_type_opcode('&'(_Type)) --> io.write_string("ref").

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

output_modifier(const)    --> io.write_string("const").
output_modifier(volatile) --> io.write_string("volatile").
output_modifier(readonly) --> io.write_string("readonly").

:- pred output_instructions(list(instr)::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_instructions(Instructions, !Info, !IO) :-
    globals.io_lookup_bool_option(auto_comments, PrintComments, !IO),
    globals.io_lookup_bool_option(debug_il_asm, DebugIlAsm, !IO),
    (
        DebugIlAsm = yes,
        list.foldl2(output_debug_instruction, Instructions, !Info, !IO)
    ;
        DebugIlAsm = no,
        list.foldl2(output_instruction(PrintComments), Instructions,
            !Info, !IO)
    ).

    % We write each instruction before we execute it.
    % This is a nice way of debugging IL as it executes, although as
    % the IL debugger improves we might not need this any more.
    %
:- pred output_debug_instruction(instr::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_debug_instruction(Instr, !Info, !IO) :-
    %
    % We can't handle tailcalls easily -- you need to put
    % it out as
    %       trace the tail instruction
    %       trace the call instruction
    %       output the tail instruction
    %       output the call instruction
    % For the moment we'll just ignore tailcalls.
    %
    ( Instr = tailcall ->
        true
    ; Instr = context(_, _) ->
        % Contexts are messy, let's ignore them for now.
        true
    ; Instr = start_block(catch(ClassName), Id) ->
        output_instr(start_block(catch(ClassName), Id), !Info, !IO),
        io.write_string("\n", !IO),
        io.write_string("\t", !IO),
        output_trace_instr(Instr, !Info, !IO),
        io.write_string("\n", !IO)
    ; Instr = start_block(scope(Locals), Id) ->
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
            ilasm.write_list(Locals, ",\n\t\t", output_local,
                !Info, !IO),
            io.write_string("\n\t)", !IO),
            io.write_string("\n", !IO),

                % trace the .locals decl
            io.write_string("\t\tldstr """, !IO),
            io.write_string(".locals (\\n\\t\\t", !IO),
            ilasm.write_list(Locals, ",\\n\\t\\t", output_local, !Info, !IO),
            io.write_string(")", !IO),
            io.write_string("\\n""", !IO),
            io.write_string("\n", !IO),
            io.write_string("\t\tcall void " ++
                "['mscorlib']System.Console::" ++
                "Write(class ['mscorlib']System.String)\n",
                !IO)
        )
    ;
        output_trace_instr(Instr, !Info, !IO),
        io.write_string("\t", !IO),
        output_instr(Instr, !Info, !IO),
        io.write_string("\n", !IO)
    ).

:- pred output_trace_instr(instr::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_trace_instr(Instr, !Info, !IO) :-
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
        output_instr(Instr, !Info, !IO)
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

:- pred output_instruction(bool::in, instr::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_instruction(PrintComments, Instr, !Info, !IO) :-
    (
        Instr = comment(_),
        PrintComments = no
    ->
        true
    ;
        io.write_string("\t", !IO),
        output_instr(Instr, !Info, !IO),
        io.write_string("\n", !IO)
    ).

:- pred output_instr(instr::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_instr(il_asm_code(Code, _MaxStack), !Info, !IO) :-
    io.write_string(Code, !IO).
output_instr(comment(Comment), !Info, !IO) :-
    output_comment_string(Comment, !IO).
output_instr(label(Label), !Info, !IO) :-
    output_label(Label, !IO),
    io.write_string(":", !IO).
output_instr(start_block(scope(Locals), Id), !Info, !IO) :-
    io.write_string("{", !IO),
    io.write_string("\t// #", !IO),
    io.write_int(Id, !IO),
    (
        Locals = []
    ;
        Locals = [_ | _],
        io.write_string("\n\t.locals (\n\t\t", !IO),
        ilasm.write_list(Locals, ",\n\t\t", output_local, !Info, !IO),
        io.write_string("\n\t)\n", !IO)
    ).
output_instr(start_block(try, Id), !Info, !IO) :-
    io.write_string(".try {", !IO),
    io.write_string("\t// #", !IO),
    io.write_int(Id, !IO).
output_instr(start_block(catch(ClassName), Id), !Info, !IO) :-
    io.write_string("catch ", !IO),
    output_class_name(ClassName, !Info, !IO),
    io.write_string(" {", !IO),
    io.write_string("\t// #", !IO),
    io.write_int(Id, !IO).
output_instr(end_block(scope(_), Id), !Info, !IO) :-
    io.write_string("}", !IO),
    io.write_string("\t// #", !IO),
    io.write_int(Id, !IO).
output_instr(end_block(catch(_), Id), !Info, !IO) :-
    io.write_string("}", !IO),
    io.write_string("\t// #", !IO),
    io.write_int(Id, !IO),
    io.write_string(" (catch block)", !IO).
output_instr(end_block(try, Id), !Info, !IO) :-
    io.write_string("}", !IO),
    io.write_string("\t// #", !IO),
    io.write_int(Id, !IO),
    io.write_string(" (try block)", !IO).
output_instr(context(File, Line), !Info, !IO) :-
    globals.io_lookup_bool_option(line_numbers, LineNumbers, !IO),
    (
        LineNumbers = yes,
        io.write_string("\n\t.line ", !IO),
        io.write_int(Line, !IO),
        io.write_string(" '", !IO),
        io.write_string(File, !IO),
        io.write_string("'", !IO)
    ;
        LineNumbers = no
    ).
output_instr(call(MethodRef), !Info, !IO) :-
    io.write_string("call\t", !IO),
    output_methodref(MethodRef, !Info, !IO).
output_instr(callvirt(MethodRef), !Info, !IO) :-
    io.write_string("callvirt\t", !IO),
    output_methodref(MethodRef, !Info, !IO).
output_instr(calli(Signature), !Info, !IO) :-
    io.write_string("calli\t", !IO),
    output_name_signature_and_call_conv(Signature, no, "\t\t", !Info, !IO).
output_instr(ret, !Info, !IO) :-
    io.write_string("ret", !IO).
output_instr(bitwise_and, !Info, !IO) :-
    io.write_string("and", !IO).
output_instr(arglist, !Info, !IO) :-
    io.write_string("arglist", !IO).
output_instr(break, !Info, !IO) :-
    io.write_string("break", !IO).
output_instr(ceq, !Info, !IO) :-
    io.write_string("ceq", !IO).
output_instr(ckfinite, !Info, !IO) :-
    io.write_string("ckfinite", !IO).
output_instr(cpblk, !Info, !IO) :-
    io.write_string("cpblk", !IO).
output_instr(dup, !Info, !IO) :-
    io.write_string("dup", !IO).
output_instr(endfilter, !Info, !IO) :-
    io.write_string("endfilter", !IO).
output_instr(endfinally, !Info, !IO) :-
    io.write_string("endfinally", !IO).
output_instr(initblk, !Info, !IO) :-
    io.write_string("initblk", !IO).
output_instr(ldnull, !Info, !IO) :-
    io.write_string("ldnull", !IO).
output_instr(localloc, !Info, !IO) :-
    io.write_string("localloc", !IO).
output_instr(neg, !Info, !IO) :-
    io.write_string("neg", !IO).
output_instr(nop, !Info, !IO) :-
    io.write_string("nop", !IO).
output_instr(bitwise_not, !Info, !IO) :-
    io.write_string("not", !IO).
output_instr(bitwise_or, !Info, !IO) :-
    io.write_string("or", !IO).
output_instr(pop, !Info, !IO) :-
    io.write_string("pop", !IO).
output_instr(shl, !Info, !IO) :-
    io.write_string("shl", !IO).
output_instr(tailcall, !Info, !IO) :-
    io.write_string("tail.", !IO).
output_instr(volatile, !Info, !IO) :-
    io.write_string("volatile", !IO).
output_instr(bitwise_xor, !Info, !IO) :-
    io.write_string("xor", !IO).
output_instr(ldlen, !Info, !IO) :-
    io.write_string("ldlen", !IO).
output_instr(throw, !Info, !IO) :-
    io.write_string("throw", !IO).
    % There are short forms of various instructions.
    % The assembler can't generate them for you.
output_instr(ldarg(index(Index)), !Info, !IO) :-
    ( Index < 4 ->
        io.write_string("ldarg.", !IO),
        io.write_int(Index, !IO)
    ; Index < 256 ->
        io.write_string("ldarg.s\t", !IO),
        output_index(Index, !IO)
    ;
        io.write_string("ldarg\t", !IO),
        output_index(Index, !IO)
    ).
output_instr(ldarg(name(Id)), !Info, !IO) :-
    io.write_string("ldarg\t", !IO),
    output_id(Id, !IO).

    % Lots of short forms for loading integer.
    % XXX Should probably put the magic numbers in functions.
output_instr(ldc(Type, Const), !Info, !IO) :-
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
        unexpected(this_file,
            "Inconsistent arguments in ldc instruction")
    ).

output_instr(ldstr(String), !Info, !IO) :-
    io.write_string("ldstr\t", !IO),
    output_string_constant(String, !IO).

output_instr(add(Overflow, Signed), !Info, !IO) :-
    io.write_string("add", !IO),
    output_overflow(Overflow, !IO),
    output_signed(Signed, !IO).

output_instr(beq(Target), !Info, !IO) :-
    io.write_string("beq ", !IO),
    output_target(Target, !IO).

output_instr(bge(Signed, Target), !Info, !IO) :-
    io.write_string("bge", !IO),
    output_signed(Signed, !IO),
    io.write_string("\t", !IO),
    output_target(Target, !IO).

output_instr(bgt(Signed, Target), !Info, !IO) :-
    io.write_string("bgt", !IO),
    output_signed(Signed, !IO),
    io.write_string("\t", !IO),
    output_target(Target, !IO).

output_instr(ble(Signed, Target), !Info, !IO) :-
    io.write_string("ble", !IO),
    output_signed(Signed, !IO),
    io.write_string("\t", !IO),
    output_target(Target, !IO).

output_instr(blt(Signed, Target), !Info, !IO) :-
    io.write_string("blt", !IO),
    output_signed(Signed, !IO),
    io.write_string("\t", !IO),
    output_target(Target, !IO).

output_instr(bne(Signed, Target), !Info, !IO) :-
    io.write_string("bne", !IO),
    output_signed(Signed, !IO),
    io.write_string("\t", !IO),
    output_target(Target, !IO).

output_instr(br(Target), !Info, !IO) :-
    io.write_string("br\t", !IO),
    output_target(Target, !IO).

output_instr(brfalse(Target), !Info, !IO) :-
    io.write_string("brfalse\t", !IO),
    output_target(Target, !IO).

output_instr(brtrue(Target), !Info, !IO) :-
    io.write_string("brtrue\t", !IO),
    output_target(Target, !IO).

output_instr(cgt(Signed), !Info, !IO) :-
    io.write_string("cgt", !IO),
    output_signed(Signed, !IO).

output_instr(clt(Signed), !Info, !IO) :-
    io.write_string("clt", !IO),
    output_signed(Signed, !IO).

output_instr(conv(SimpleType), !Info, !IO) :-
    io.write_string("conv.", !IO),
    output_simple_type_opcode(SimpleType, !IO).

output_instr(div(Signed), !Info, !IO) :-
    io.write_string("div", !IO),
    output_signed(Signed, !IO).

output_instr(jmp(MethodRef), !Info, !IO) :-
    io.write_string("jmp\t", !IO),
    output_methodref(MethodRef, !Info, !IO).

    % XXX can use short encoding for indexes
output_instr(ldarga(Variable), !Info, !IO) :-
    io.write_string("ldarga\t", !IO),
    (
        Variable = index(Index),
        output_index(Index, !IO)
    ;
        Variable = name(Name),
        output_id(Name, !IO)
    ).

output_instr(ldftn(MethodRef), !Info, !IO) :-
    io.write_string("ldftn\t", !IO),
    output_methodref(MethodRef, !Info, !IO).

output_instr(ldind(SimpleType), !Info, !IO) :-
    io.write_string("ldind.", !IO),
    output_simple_type_opcode(SimpleType, !IO).

    % XXX can use short encoding for indexes
output_instr(ldloc(Variable), !Info, !IO) :-
    io.write_string("ldloc\t", !IO),
    (
        Variable = index(Index),
        output_index(Index, !IO)
    ;
        Variable = name(Name),
        output_id(Name, !IO)
    ).

    % XXX can use short encoding for indexes
output_instr(ldloca(Variable), !Info, !IO) :-
    io.write_string("ldloca\t", !IO),
    (
        Variable = index(Index),
        output_index(Index, !IO)
    ;
        Variable = name(Name),
        output_id(Name, !IO)
    ).

output_instr(leave(Target), !Info, !IO) :-
    io.write_string("leave\t", !IO),
    output_target(Target, !IO).

output_instr(mul(Overflow, Signed), !Info, !IO) :-
    io.write_string("mul", !IO),
    output_overflow(Overflow, !IO),
    output_signed(Signed, !IO).

output_instr(rem(Signed), !Info, !IO) :-
    io.write_string("rem", !IO),
    output_signed(Signed, !IO).

output_instr(shr(Signed), !Info, !IO) :-
    io.write_string("shr", !IO),
    output_signed(Signed, !IO).

    % XXX can use short encoding for indexes
output_instr(starg(Variable), !Info, !IO) :-
    io.write_string("starg\t", !IO),
    (
        Variable = index(Index),
        output_index(Index, !IO)
    ;
        Variable = name(Name),
        output_id(Name, !IO)
    ).

    % XXX can use short encoding for indexes
output_instr(stind(SimpleType), !Info, !IO) :-
    io.write_string("stind.", !IO),
    output_simple_type_opcode(SimpleType, !IO).

output_instr(stloc(Variable), !Info, !IO) :-
    io.write_string("stloc\t", !IO),
    (
        Variable = index(Index),
        output_index(Index, !IO)
    ;
        Variable = name(Name),
        output_id(Name, !IO)
    ).

output_instr(sub(OverFlow, Signed), !Info, !IO) :-
    io.write_string("sub", !IO),
    output_overflow(OverFlow, !IO),
    output_signed(Signed, !IO).

output_instr(switch(Targets), !Info, !IO) :-
    io.write_string("switch (", !IO),
    io.write_list(Targets, ", ", output_target, !IO),
    io.write_string(")", !IO).

output_instr(unaligned(_), !Info, !IO) :-
    io.write_string("unaligned.", !IO).

output_instr(box(Type), !Info, !IO) :-
    io.write_string("box\t", !IO),
    output_type(Type, !Info, !IO).

output_instr(castclass(Type), !Info, !IO) :-
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
    output_type(Type, !Info, !IO).

output_instr(cpobj(Type), !Info, !IO) :-
    io.write_string("cpobj\t", !IO),
    output_type(Type, !Info, !IO).

output_instr(initobj(Type), !Info, !IO) :-
    io.write_string("initobj\t", !IO),
    output_type(Type, !Info, !IO).

output_instr(isinst(Type), !Info, !IO) :-
    io.write_string("isinst\t", !IO),
    output_type(Type, !Info, !IO).

output_instr(ldelem(SimpleType), !Info, !IO) :-
    io.write_string("ldelem.", !IO),
    output_simple_type_opcode(SimpleType, !IO).

output_instr(ldelema(Type), !Info, !IO) :-
    io.write_string("ldelema\t", !IO),
    output_type(Type, !Info, !IO).

output_instr(ldfld(FieldRef), !Info, !IO) :-
    io.write_string("ldfld\t", !IO),
    output_fieldref(FieldRef, !Info, !IO).

output_instr(ldflda(FieldRef), !Info, !IO) :-
    io.write_string("ldflda\t", !IO),
    output_fieldref(FieldRef, !Info, !IO).

output_instr(ldobj(Type), !Info, !IO) :-
    io.write_string("ldobj\t", !IO),
    output_type(Type, !Info, !IO).

output_instr(ldsfld(FieldRef), !Info, !IO) :-
    io.write_string("ldsfld\t", !IO),
    output_fieldref(FieldRef, !Info, !IO).

output_instr(ldsflda(FieldRef), !Info, !IO) :-
    io.write_string("ldsflda\t", !IO),
    output_fieldref(FieldRef, !Info, !IO).

    % XXX should be implemented
output_instr(ldtoken(_), !Info, !IO) :-
    sorry(this_file, "output not implemented").

output_instr(ldvirtftn(MethodRef), !Info, !IO) :-
    io.write_string("ldvirtftn\t", !IO),
    output_methodref(MethodRef, !Info, !IO).

output_instr(mkrefany(Type), !Info, !IO) :-
    io.write_string("mkrefany\t", !IO),
    output_type(Type, !Info, !IO).

output_instr(newarr(Type), !Info, !IO) :-
    io.write_string("newarr\t", !IO),
    output_type(Type, !Info, !IO).

output_instr(newobj(MethodRef), !Info, !IO) :-
    io.write_string("newobj\t", !IO),
    output_methodref(MethodRef, !Info, !IO).

output_instr(refanytype, !Info, !IO) :-
    io.write_string("refanytype", !IO).

output_instr(refanyval(Type), !Info, !IO) :-
    io.write_string("refanyval\t", !IO),
    output_type(Type, !Info, !IO).

output_instr(rethrow, !Info, !IO) :-
    io.write_string("rethrow", !IO).

output_instr(stelem(SimpleType), !Info, !IO) :-
    io.write_string("stelem.", !IO),
    output_simple_type_opcode(SimpleType, !IO).

output_instr(stfld(FieldRef), !Info, !IO) :-
    io.write_string("stfld\t", !IO),
    output_fieldref(FieldRef, !Info, !IO).

output_instr(stobj(Type), !Info, !IO) :-
    io.write_string("stobj\t", !IO),
    output_type(Type, !Info, !IO).

output_instr(sizeof(Type), !Info, !IO) :-
    io.write_string("sizeof\t", !IO),
    output_type(Type, !Info, !IO).

output_instr(stsfld(FieldRef), !Info, !IO) :-
    io.write_string("stsfld\t", !IO),
    output_fieldref(FieldRef, !Info, !IO).

output_instr(unbox(Type), !Info, !IO) :-
    io.write_string("unbox\t", !IO),
    output_type(Type, !Info, !IO).

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

:- pred output_fieldref(fieldref::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_fieldref(fieldref(Type, ClassMemberName), !Info, !IO) :-
    output_type(Type, !Info, !IO),
    io.write_string("\n\t\t", !IO),
    output_class_member_name(ClassMemberName, !.Info, !IO).

:- pred output_methodref(methodref::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_methodref(methoddef(call_conv(IsInstance, _), ReturnType,
        ClassMemberName, ArgTypes), !Info, !IO) :-
    (
        IsInstance = yes,
        io.write_string("instance ", !IO)
    ;
        IsInstance = no
    ),
    output_ret_type(ReturnType, !Info, !IO),
    io.write_string("\n\t\t", !IO),
    output_class_member_name(ClassMemberName, !.Info, !IO),
    (
        ArgTypes = [],
        io.write_string("()\n", !IO)
    ;
        ArgTypes = [_ | _],
        io.write_string("(\n\t\t\t", !IO),
        ilasm.write_list(ArgTypes, ",\n\t\t\t", output_type, !Info, !IO),
        io.write_string("\n\t\t)", !IO)
    ).
output_methodref(local_method(call_conv(IsInstance, _), ReturnType,
        MethodName, ArgTypes), !Info, !IO) :-
    (
        IsInstance = yes,
        io.write_string("instance ", !IO)
    ;
        IsInstance = no
    ),
    output_ret_type(ReturnType, !Info, !IO),
    io.write_string("\n\t\t", !IO),
    output_member_name(MethodName, !IO),
    (
        ArgTypes = [],
        io.write_string("()\n", !IO)
    ;
        ArgTypes = [_ | _],
        io.write_string("(\n\t\t\t", !IO),
        ilasm.write_list(ArgTypes, ",\n\t\t\t", output_type, !Info, !IO),
        io.write_string("\n\t\t)", !IO)
    ).

:- pred output_classattr(classattr::in, io::di, io::uo) is det.

output_classattr(abstract) --> io.write_string("abstract").
output_classattr(ansi) --> io.write_string("ansi").
output_classattr(auto) --> io.write_string("auto").
output_classattr(autochar) --> io.write_string("autochar").
output_classattr(beforefieldinit) --> io.write_string("beforefieldinit").
output_classattr(explicit) --> io.write_string("explicit").
output_classattr(interface) --> io.write_string("interface").
output_classattr(nestedassembly) --> io.write_string("nested assembly").
output_classattr(nestedfamandassem) --> io.write_string("nested famandassem").
output_classattr(nestedfamily) --> io.write_string("nested family").
output_classattr(nestedfamorassem) --> io.write_string("nested famorassem").
output_classattr(nestedprivate) --> io.write_string("nested private").
output_classattr(nestedpublic) --> io.write_string("nested public").
output_classattr(private) --> io.write_string("private").
output_classattr(public) --> io.write_string("public").
output_classattr(rtspecialname) --> io.write_string("rtspecialname").
output_classattr(sealed) --> io.write_string("sealed").
output_classattr(sequential) --> io.write_string("sequential").
output_classattr(serializable) --> io.write_string("serializable").
output_classattr(specialname) --> io.write_string("specialname").
output_classattr(unicode) --> io.write_string("unicode").

:- pred output_assembly_decl(assembly_decl::in,
    ilasm_info::in, ilasm_info::out, io::di, io::uo) is det.

output_assembly_decl(version(A, B, C, D), !Info, !IO) :-
    io.format(".ver %d:%d:%d:%d", [i(A), i(B), i(C), i(D)], !IO).
output_assembly_decl(public_key_token(Token), !Info, !IO) :-
    io.write_string(".publickeytoken = ( ", !IO),
    io.write_list(Token, " ", output_hexbyte, !IO),
    io.write_string(" ) ", !IO).
output_assembly_decl(hash(Hash), !Info, !IO) :-
    io.write_string(".hash = ( ", !IO),
    io.write_list(Hash, " ", output_hexbyte, !IO),
    io.write_string(" ) ", !IO).
output_assembly_decl(custom(CustomDecl), !Info, !IO) :-
    output_custom_decl(CustomDecl, !Info, !IO).

:- pred output_custom_decl(custom_decl::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_custom_decl(custom_decl(Type, MaybeOwner, StringOrBytes), !Info, !IO) :-
    io.write_string(".custom ", !IO),
    (
        MaybeOwner = yes(Owner),
        io.write_string(" (", !IO),
        output_custom_type(Owner, !Info, !IO),
        io.write_string(") ", !IO)
    ;
        MaybeOwner = no
    ),
    output_custom_type(Type, !Info, !IO),
    ( StringOrBytes = bytes(Bytes) ->
        io.write_string(" = (", !IO),
        io.write_list(Bytes, " ", output_hexbyte, !IO),
        io.write_string(")", !IO)
    ;
        sorry(this_file, "custom_decl of this sort")
    ),
    io.write_string("\n", !IO).

:- pred output_custom_type(custom_type::in, ilasm_info::in, ilasm_info::out,
    io::di, io::uo) is det.

output_custom_type(type(Type), !Info, !IO) :-
    output_type(Type, !Info, !IO).
output_custom_type(methodref(MethodRef), !Info, !IO) :-
    output_methodref(MethodRef, !Info, !IO).

:- pred output_index(index::in, io::di, io::uo) is det.

output_index(Index, !IO) :-
    io.write_int(Index, !IO).

:- pred output_string_constant(string::in, io::di, io::uo) is det.

output_string_constant(String, !IO) :-
    io.write_string("""", !IO),
    output_escaped_string(String, '\"', !IO),
    io.write_string("""", !IO).

:- pred output_class_member_name(class_member_name::in, ilasm_info::in,
    io::di, io::uo) is det.

output_class_member_name(class_member_name(StructuredName, MemberName), Info,
        !IO) :-
    output_structured_name(StructuredName, Info, !IO),
    io.write_string("::", !IO),
    output_member_name(MemberName, !IO).

:- pred output_structured_name(structured_name::in, ilasm_info::in,
    io::di, io::uo) is det.

output_structured_name(structured_name(Asm, DottedName, NestedClasses), Info,
        !IO) :-
    globals.io_lookup_bool_option(separate_assemblies, SeparateAssemblies,
        !IO),
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

:- func this_file = string.

this_file = "ilasm.m".

%-----------------------------------------------------------------------------%
:- end_module ilasm.
%-----------------------------------------------------------------------------%
