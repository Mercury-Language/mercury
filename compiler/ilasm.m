%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% ilasm - Generate IL for the ilasm assembler
% Main author: trd.
%
%
% IL assembler syntax is documented in the Microsoft .NET Framework SDK.
% See ilds.m for links to the documentation.
%
% This code is a little messy.  Some of the code here is a hangover from
% earlier versions of the assembler grammar.
%
% To do:
%	[ ] Implement missing instructions.
%	[ ] Add any missing functionality from the assembler grammar
%	    (events, properties, etc).
%	[ ] Fix up all the XXXs.

:- module ilasm.

:- interface.

:- import_module io, list, term, std_util, bool, integer.
:- import_module ilds.

:- pred ilasm__output(
	list(decl)::in, io__state::di, io__state::uo) is det.

:- type int64 ---> int64(integer).
:- type int32 ---> int32(int).
:- type int16 ---> int16(int).
:- type int8  ---> int8(int).
:- type byte  == int8.
:- type float64 ---> float64(float).
:- type float32 ---> float32(float).

% A top level declaration in IL assembler.
:- type decl
		% .class declaration
	--->	class(
			list(classattr),	% attributes for the class
			ilds__id,		% name of the class
			extends,		% what is the parent class
			implements, 		% what interfaces are 
						% implemented
			list(class_member)		% methods and fields
		)
		% .namespace declaration
	;	namespace(
			namespace_qual_name,	% namespace name
			list(decl)		% contents
		)
		% .method  (a global function)
		% there are lots of restriction on global functions so
		% don't get too excited about using them for anything.
		% In particular, you can't reference a namespace
		% qualified global function from outside the module.
	;	method(
			methodhead,
			method_defn
		)
		% .data  (module local data)
	;	data(
			bool, 		 % is data in thread local storage?
			maybe(ilds__id), % id to name this data
			data_body 	 % body of data
		) 

		% .file
		% Declares a file associated with the current assembly
	;	file(ilds__id)

		% .module extern
		% declares a module name.
	;	extern_module(ilds__id)

		% .assembly extern
		% declares an assembly name, and possibly its strong
		% name/version number.
	;	extern_assembly(ilds__id, list(assembly_decl))

		% .assembly
		% defines an assembly
	;	assembly(ilds__id)

		% .custom
		% a custom attribute
	;	custom(custom_decl)

		% comments
	;	comment_term(term)
			% print almost anything using pprint__to_doc
			% (see library/pprint.m for limitations).
	;	some [T] comment_thing(T)
	;	comment(string).

:- type assembly_decl 
	--->	version(int, int, int, int)	% version number
	;	hash(list(int8))		% hash 
	;	public_key_token(list(int8))	% public key token
	;	custom(custom_decl).		% a custom attribute


	% a method definition is just a list of body decls.
:- type method_defn == list(method_body_decl).

:- type methodhead 
	---> methodhead(
			list(methattr),		% method attributes
			member_name,		% method name
			signature,		% method signature
			list(implattr)		% implementation attributes
	).

:- type class_member
		% .method (a class method)
	--->	method(
			methodhead,		% name, signature, attributes
			method_defn		% definition of method
		)	
		% .field (a class field)
	;	field(
			list(fieldattr),	% attributes
			ilds__type,		% field type
			ilds__id,		% field name
			maybe(int32),		% offset for explicit layout
			field_initializer	% initializer
		)
		% .property (a class property)
	;	property(
			ilds__type,		% property type
			ilds__id,		% property name
			maybe(methodhead),	% get property
			maybe(methodhead)	% set property
		)
		% .class (a nested class)
	;	nested_class(
			list(classattr),	% attributes for the class
			ilds__id,		% name of the class
			extends,		% what is the parent class
			implements, 		% what interfaces are 
						% implemented
			list(class_member)	% methods and fields
		)
	;	custom(custom_decl)		% custom attribute
		% comments
	;	comment_term(term)
	;	comment(string)
			% print almost anything using pprint__to_doc
			% (see library/pprint.m for limitations).
	;	some [T] comment_thing(T).

:- type field_initializer
	--->	none		% no initializer
	;	at(ilds__id)	% initialize with .data at given location
	;	equals(field_init).	% initialize with constant

	% note that for some reason the syntax for field_init is almost,
	% but not quite the same as data items. 
:- type field_init
	--->	data_item(data_item)		% most data_items are valid
			% XXX unicode is not yet implemented, don't use
			% wchar_ptr unless you intend to implement it
	;	wchar_ptr(string)		% a string to convert to unicode
	;	binary_float32(int32)		% binary rep. of float
	;	binary_float64(int64).		% binary rep. of double

	% a parent class to extend
:- type extends 
	--->	extends(ilds__class_name)
	;	extends_nothing.

	% a list of interfaces that we implement
:- type implements
	--->	implements(list(ilds__class_name)).

	% declarations that can form the body of a method.
:- type method_body_decl
	--->	emitbyte(int32)		% raw byte output (danger! danger!)
		        % "emits an int32 to the code section of the method"
			% according to the IL Assembly Language
			% Programmers' Reference.
			% This probably means it can output IL
			% bytecodes.
	;	maxstack(int32)		% maximum stack size 
		        % "Defines the maximum size of the stack,
			% specified by the int32"
			% But does it measure in bits, nibbles, bytes,
			% words or something else?
	;	entrypoint		% is this "main"?
	;	zeroinit		% initialize locals to zero.
	;	custom(custom_decl)	% custom attribute
	;	instrs(list(instr))	% instructions
	;	label(string).		% a label

	% attributes that a class can have.
	% see SDK documentation for what they all mean.
:-  type classattr
	--->	abstract		; ansi
	;	auto			; autochar
	;	beforefieldinit		; explicit
	;	interface		; nestedassembly
	;	nestedfamandassem	; nestedfamily
	;	nestedfamorassem	; nestedprivate
	;	nestedpublic		; private
	;	public 			; rtspecialname
	;	sealed			; sequential
	;	serializable		; specialname
	;	unicode.

	% attributes that a method can have.	
	% see SDK documentation for what they all mean.
:- type methattr
	--->  abstract      ;  assembly     ;   famandassem  ;    family
	;     famorassem    ;  final        ;   hidebysig    ;    newslot
	;     private       ;  privatescope ;   public
	;     rtspecialname ;  specialname  ;   static
	;     synchronized  ;  virtual      ;   pinvokeimpl.

	% attributes that a field can have.	
	% see SDK documentation for what they all mean.
:- type fieldattr
	--->  assembly      ;  famandassem  ;  family        ;  famorassem
	;     initonly      ;  literal      ;  notserialized ;  pinvokeimpl
	;     private       ;  privatescope ;  public        ;  static
	;     volatile.

	% attributes that a method implementation can have.	
	% see SDK documentation for what they all mean.
:- type implattr
	--->  il            ;  implemented  ;  internalcall  ;  managed
	;     native        ;  ole          ;  optil         ;  runtime
        ;     unmanaged.

	% the body of a .data declaration
:- type data_body 
	--->	itemlist(list(data_item))
	;	item(data_item).

	% various constants that can be used in .data declarations
:- type data_item 
	---> 	float32(float32)
	;	float64(float64)
	;	int64(int64)
	;	int32(int32)
	;	int16(int16)
	;	int8(int8)
	;	char_ptr(string)
	;	'&'(ilds__id)
	;	bytearray(list(byte)).	% output as two digit hex, e.g.
					% 01 F7 0A

:- type custom_decl ---> 
	custom_decl(
		custom_type, 
		maybe(custom_type),
		qstring_or_bytes).

:- type qstring_or_bytes
	--->	qstring(string)
	;	bytes(list(int8))
	; 	no_initalizer.

:- type custom_type
	--->	type(ilds__type)
	;	methodref(ilds__methodref).


:- implementation.

:- import_module char, string, pprint, getopt.
:- import_module require, int, term_io, varset, bool.
:- import_module globals, options, error_util.


	% Some versions of the IL assembler enforce a rule that if you output 
	% 	.assembly foo { } 
	% you are not allowed to use the assembly reference in the rest of 
	% the file, e.g.
	% 	[foo]blah.bletch
	% Instead you have to output just
	% 	blah.bletch
	%
	% So we need to duplicate this checking in the output phase and
	% make sure we don't output [foo].
	%
	% It's a good idea to do this anyway, as there is apparently a
	% performance hit if you use assembly references to a symbol that is
	% in the local assembly.

:- type ilasm_info ---> 
		ilasm_info(
			current_assembly :: ilds__id
		).

:- pred ilasm__write_list(list(T), string, 
	pred(T, ilasm_info, ilasm_info, io__state, io__state),
	ilasm_info, ilasm_info, io__state, io__state).
:- mode ilasm__write_list(in, in, pred(in, in, out, di, uo) is det,
	in, out, di, uo) is det.

ilasm__write_list([], _Separator, _OutputPred, Info, Info) --> [].
ilasm__write_list([E | Es], Separator, OutputPred, Info0, Info) --> 
	call(OutputPred, E, Info0, Info1),
	(
		{ Es = [] }
	;
		{ Es = [_|_] },
		io__write_string(Separator)
	),
	ilasm__write_list(Es, Separator, OutputPred, Info1, Info).
		

ilasm__output(Blocks) --> 
	{ Info0 = ilasm_info("") },
	ilasm__output(Blocks, Info0, _Info).

:- pred ilasm__output(list(decl)::in, ilasm_info::in, ilasm_info::out,
	io__state::di, io__state::uo) is det.

ilasm__output(Blocks, Info0, Info) --> 
	ilasm__write_list(Blocks, "\n\n", output_decl, Info0, Info),
	io__write_string("\n\n").


:- pred ilasm__output_decl(decl::in, ilasm_info::in, ilasm_info::out,
		io__state::di, io__state::uo) is det.

ilasm__output_decl(custom(CustomDecl), Info0, Info) --> 
	output_custom_decl(CustomDecl, Info0, Info).
ilasm__output_decl(class(Attrs, Id, Extends, Implements, Contents),
		Info0, Info) --> 
	io__write_string(".class "),
	io__write_list(Attrs, " ", output_classattr),
	( { Attrs \= [] } ->
		io__write_string(" ")
	;
		[]
	),
	output_id(Id),
	(
		{ Extends = extends(ExtendsModule) },
		io__write_string(" extends "),
		output_class_name(ExtendsModule, Info0, Info1)
	;
		{ Info1 = Info0 },
		{ Extends = extends_nothing }
	),
	{ Implements = implements(ImplementsList) },
	(
		{ ImplementsList = [_|_] }
	->
		io__write_string(" implements "),
		ilasm__write_list(ImplementsList, ", ", output_class_name,
			Info1, Info2)
	;
		{ Info2 = Info1 }
	),
	io__write_string(" {\n"),
	ilasm__write_list(Contents, "\n", output_class_member, Info2, Info),
	io__write_string("\n}").
ilasm__output_decl(namespace(DottedName, Contents), Info0, Info) --> 
	( { DottedName \= [] } ->
		io__write_string(".namespace "),
		output_dotted_name(DottedName),
		io__write_string(" {\n"),
		output(Contents, Info0, Info),
		io__write_string("}\n")
	;
		output(Contents, Info0, Info)
	).
ilasm__output_decl(method(MethodHead, MethodDecls), Info0, Info) --> 
	io__write_string(".method "),
	output_methodhead(MethodHead, Info0, Info1),
	io__write_string("\n{\n"),
	ilasm__write_list(MethodDecls, "\n", output_method_body_decl,
		Info1, Info),
	io__write_string("}\n").
ilasm__output_decl(data(TLS, MaybeId, Body), Info, Info) --> 
	io__write_string(".data "),
	( { TLS = yes } ->
		io__write_string("tls ")
	;
		[]
	),
	( { MaybeId = yes(Id) } ->
		output_id(Id),
		io__write_string(" = ")
	;
		[]
	),
	output_data_body(Body).

ilasm__output_decl(comment_term(CommentTerm), Info, Info) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		io__write_string("// "),
		{ varset__init(VarSet) },
		term_io__write_term(VarSet, CommentTerm),
		io__write_string("\n")
	;
		[]
	).

ilasm__output_decl(comment_thing(Thing), Info, Info) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		{ Doc = label("// ", to_doc(Thing)) },
		write(70, Doc),
		io__nl
	;
		[]
	).

ilasm__output_decl(comment(CommentStr), Info, Info) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		output_comment_string(CommentStr)
	;
		[]
	).

ilasm__output_decl(extern_assembly(AsmName, AssemblyDecls), Info0, Info) --> 
	io__write_string(".assembly extern "),
	output_id(AsmName),
	io__write_string("{\n"),
	list__foldl2((pred(A::in, I0::in, I::out, di, uo) is det -->
			output_assembly_decl(A, I0, I),
			io__write_string("\n\t")
		), AssemblyDecls, Info0, Info),
	io__write_string("\n}\n").


ilasm__output_decl(assembly(AsmName), Info0, Info) --> 
	io__write_string(".assembly "),
	output_id(AsmName),
	{ Info = Info0 ^ current_assembly := AsmName },
	io__write_string(" { }").

ilasm__output_decl(file(FileName), Info, Info) --> 
	io__write_string(".file "),
	output_id(FileName).

ilasm__output_decl(extern_module(ModName), Info, Info) --> 
	io__write_string(".module extern "),
	output_id(ModName).


:- pred ilasm__output_class_member(class_member::in, ilasm_info::in,
	ilasm_info::out, io__state::di, io__state::uo) is det.

ilasm__output_class_member(method(MethodHead, MethodDecls), Info0, Info) -->
		% Don't do debug output on class constructors, since
		% they are automatically generated and take forever to
		% run.
	globals__io_lookup_option(debug_il_asm, DebugIlAsm),
	( { MethodHead = methodhead(_, cctor, _, _) } ->
		globals__io_set_option(debug_il_asm, bool(no)),
		ilasm__output_decl(method(MethodHead, MethodDecls),
			Info0, Info),
		globals__io_set_option(debug_il_asm, DebugIlAsm)
	;
		ilasm__output_decl(method(MethodHead, MethodDecls), 
			Info0, Info)
	).

ilasm__output_class_member(custom(CustomDecl), Info0, Info) -->
	output_custom_decl(CustomDecl, Info0, Info).

ilasm__output_class_member(
		field(FieldAttrs, Type, IlId, MaybeOffset, Initializer),
		Info0, Info) -->
	io__write_string(".field "),
	( { MaybeOffset = yes(Offset) } ->
		output_int32(Offset),
		io__write_string(" ")
	;
		[]
	),
	io__write_list(FieldAttrs, " ", io__write),
	io__write_string("\n\t"),
	output_type(Type, Info0, Info),
	io__write_string("\n\t"),
	output_id(IlId),
	output_field_initializer(Initializer).

ilasm__output_class_member(
		property(Type, Name, MaybeGet, MaybeSet), Info0, Info) -->
	io__write_string(".property instance "),
	output_type(Type, Info0, Info1),
	io__write_string(" "),
	output_id(Name),
	io__write_string("() {"),
	( { MaybeGet = yes(methodhead(_, GetMethodName, GetSignature, _)) },
		io__nl,
		io__write_string("\t.get instance "),
		output_name_signature_and_call_conv(GetSignature,
				yes(GetMethodName), "\t\t", Info1, Info2)
	; { MaybeGet = no },
		{ Info2 = Info1 }
	),
	( { MaybeSet = yes(methodhead(_, SetMethodName, SetSignature, _)) },
		io__nl,
		io__write_string("\t.set instance "),
		output_name_signature_and_call_conv(SetSignature,
				yes(SetMethodName), "\t\t", Info2, Info)
	; { MaybeSet = no },
		{ Info = Info2 }
	),
	io__write_string("\n}\n").

ilasm__output_class_member(nested_class(Attrs, Id, Extends, Implements,
		Contents), Info0, Info) --> 
	ilasm__output_decl(class(Attrs, Id, Extends, Implements, Contents),
		Info0, Info).

ilasm__output_class_member(comment(CommentStr), Info, Info) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		output_comment_string(CommentStr)
	;
		[]
	).

ilasm__output_class_member(comment_term(CommentTerm), Info, Info) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		io__write_string("// "),
		{ varset__init(VarSet) },
		term_io__write_term(VarSet, CommentTerm),
		io__nl
	;
		[]
	).

ilasm__output_class_member(comment_thing(Thing), Info, Info) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		{ Doc = label("// ", to_doc(Thing)) },
		write(70, Doc),
		io__nl
	;
		[]
	).

:- pred ilasm__output_methodhead(methodhead::in,
	ilasm_info::in, ilasm_info::out,
	io__state::di, io__state::uo) is det.
ilasm__output_methodhead(methodhead(Attrs, MethodName, Signature,
		ImplAttrs), Info0, Info) -->
	io__write_list(Attrs, " ", io__write),
	( { Attrs \= [] } ->
		io__write_string(" ")
	;
		[]
	),
	output_name_signature_and_call_conv(Signature, yes(MethodName), "\t",
		Info0, Info),
	io__write_list(ImplAttrs, " ", io__write).

:- pred ilasm__output_method_body_decl(method_body_decl::in,
	ilasm_info::in, ilasm_info::out,
	io__state::di, io__state::uo) is det.
ilasm__output_method_body_decl(emitbyte(Int32), I, I) -->
	io__write_string(".emitbyte "),
	output_int32(Int32).

ilasm__output_method_body_decl(custom(CustomDecl), Info0, Info) -->
	output_custom_decl(CustomDecl, Info0, Info).

ilasm__output_method_body_decl(maxstack(Int32), I, I) -->
	io__write_string(".maxstack "),
	output_int32(Int32).

ilasm__output_method_body_decl(entrypoint, I, I) -->
	io__write_string(".entrypoint ").

ilasm__output_method_body_decl(zeroinit, I, I) -->
	io__write_string(".zeroinit ").

ilasm__output_method_body_decl(instrs(Instrs), Info0, Info) -->
	output_instructions(Instrs, Info0, Info).

ilasm__output_method_body_decl(label(Label), I, I) -->
	output_label(Label),
	io__write_string(":").

:- pred output_label(label::in, io__state::di, io__state::uo) is det.
output_label(Label) -->
	io__write_string(Label).

:- pred output_class_name(ilds__class_name::in,
		ilasm_info::in, ilasm_info::out,
		io__state::di, io__state::uo) is det.
output_class_name(ClassName, Info0, Info) -->
	output_structured_name(ClassName, Info0, Info).

:- pred output_call_conv(call_conv::in, io__state::di, io__state::uo) is det.
output_call_conv(call_conv(IsInstance, IlCallConv)) -->
	(
		{ IsInstance = yes }
	->
		io__write_string("instance ")
	;
		io__write(IlCallConv),
		io__write_string(" ")
	).

:- pred output_name_signature_and_call_conv(signature::in,
	maybe(member_name)::in, string::in, ilasm_info::in, ilasm_info::out,
	io__state::di, io__state::uo) is det.
output_name_signature_and_call_conv(signature(CallConv, ReturnType,
		 ArgTypes), MaybeMethodName, Indent, Info0, Info) -->
	output_call_conv(CallConv),
	io__write_string("\n"),
	io__write_string(Indent),
	output_ret_type(ReturnType, Info0, Info1),
	( { MaybeMethodName = yes(MethodName) } ->
		io__write_string("\n"),
		io__write_string(Indent),
		output_member_name(MethodName)
	;
		io__write_string(" ")
	),
	( { ArgTypes = [] } ->
		io__write_string("()"),
		{ Info = Info0 }
	;
		io__write_string("(\n\t\t"),
		ilasm__write_list(ArgTypes, ",\n\t\t", output_param,
			Info1, Info),
		io__write_string("\n\t)")
	).

:- pred output_member_name(member_name::in, io__state::di,
	 io__state::uo) is det.
output_member_name(MethodName) -->
	( { MethodName = ctor },
		io__write_string(".ctor")
	; { MethodName = cctor },
		io__write_string(".cctor")
	; { MethodName = id(IlId) },
		output_id(IlId)
	).

:- pred output_ret_type(ret_type::in,
	ilasm_info::in, ilasm_info::out,
	io__state::di, io__state::uo) is det.
output_ret_type(void, I, I) --> io__write_string("void").
output_ret_type(simple_type(Type), Info0, Info) -->
	output_simple_type(Type, Info0, Info).

:- pred output_local(pair(ilds__id, ilds__type)::in, 
		ilasm_info::in, ilasm_info::out,
		io__state::di, io__state::uo) is det.
output_local(Id - Type, Info0, Info) -->
	output_type(Type, Info0, Info),
	io__write_string(" "),
	output_id(Id).

:- pred output_param(pair(ilds__type, maybe(ilds__id))::in, 
		ilasm_info::in, ilasm_info::out,	
		io__state::di, io__state::uo) is det.
output_param(Type - no, Info0, Info) -->
	output_type(Type, Info0, Info).
output_param(Type - yes(Id), Info0, Info) -->
	output_type(Type, Info0, Info),
	io__write_string(" "),
	output_id(Id).

:- pred output_type(ilds__type::in, ilasm_info::in, ilasm_info::out,
		io__state::di, io__state::uo) is det.

output_type(ilds__type(Modifiers, SimpleType), Info0, Info) -->
	io__write_list(Modifiers, " ", output_modifier),
	output_simple_type(SimpleType, Info0, Info).

:- pred output_simple_type(simple_type::in,
	ilasm_info::in, ilasm_info::out, io__state::di, io__state::uo) is det.

output_simple_type(int8, I, I) --> io__write_string("int8").
output_simple_type(int16, I, I) --> io__write_string("int16").
output_simple_type(int32, I, I) --> io__write_string("int32").
output_simple_type(int64, I, I) --> io__write_string("int64").
output_simple_type(uint8, I, I) --> io__write_string("uint8").
output_simple_type(uint16, I, I) --> io__write_string("uint16").
output_simple_type(uint32, I, I) --> io__write_string("uint32").
output_simple_type(uint64, I, I) --> io__write_string("uint64").
output_simple_type(native_int, I, I) --> io__write_string("native int").
output_simple_type(native_uint, I, I) --> io__write_string("native unsigned int").
output_simple_type(float32, I, I) --> io__write_string("float32").
output_simple_type(float64, I, I) --> io__write_string("float64").
output_simple_type(native_float, I, I) --> io__write_string("native float").
output_simple_type(bool, I, I) --> io__write_string("bool").
output_simple_type(char, I, I) --> io__write_string("char").
output_simple_type(refany, I, I) --> io__write_string("refany").
output_simple_type(class(Name), Info0, Info) --> 
	io__write_string("class "),
	output_structured_name(Name, Info0, Info).
output_simple_type(value_class(Name), Info0, Info) --> 
	io__write_string("valuetype "),
	output_structured_name(Name, Info0, Info).
output_simple_type(interface(Name), Info0, Info) --> 
	io__write_string("interface "),
	output_structured_name(Name, Info0, Info).
output_simple_type('[]'(Type, Bounds), Info0, Info) --> 
	output_type(Type, Info0, Info),
	output_bounds(Bounds).
output_simple_type('*'(Type), Info0, Info) --> 
	output_type(Type, Info0, Info),
	io__write_string("*").
output_simple_type('&'(Type), Info0, Info) --> 
	output_type(Type, Info0, Info),
	io__write_string("&").

	% The names are all different if it is an opcode.
	% There's probably a very implementation dependent reason for
	% this.
:- pred output_simple_type_opcode(simple_type::in, io__state::di,
		io__state::uo) is det.
output_simple_type_opcode(int8) --> io__write_string("i1").
output_simple_type_opcode(int16) --> io__write_string("i2").
output_simple_type_opcode(int32) --> io__write_string("i4").
output_simple_type_opcode(int64) --> io__write_string("i8").
output_simple_type_opcode(uint8) --> io__write_string("u1").
output_simple_type_opcode(uint16) --> io__write_string("u2").
output_simple_type_opcode(uint32) --> io__write_string("u4").
output_simple_type_opcode(uint64) --> io__write_string("u8").
output_simple_type_opcode(native_int) --> io__write_string("i").
output_simple_type_opcode(native_uint) --> io__write_string("u").
output_simple_type_opcode(float32) --> io__write_string("r4").
output_simple_type_opcode(float64) --> io__write_string("r8").
output_simple_type_opcode(native_float) --> 
	{ error("unable to create opcode for native_float") }.
		% XXX should i4 be used for bool? 
output_simple_type_opcode(bool) --> io__write_string("i4").
output_simple_type_opcode(char) --> io__write_string("i2").

	% all reference types use "ref" as their opcode.
	% XXX is "ref" here correct for value classes?
output_simple_type_opcode(refany) --> io__write_string("ref").
output_simple_type_opcode(class(_Name)) --> io__write_string("ref").
output_simple_type_opcode(value_class(_Name)) --> io__write_string("ref").
output_simple_type_opcode(interface(_Name)) --> io__write_string("ref").
output_simple_type_opcode('[]'(_Type, _Bounds)) --> io__write_string("ref").
output_simple_type_opcode('*'(_Type)) --> io__write_string("ref").
output_simple_type_opcode('&'(_Type)) --> io__write_string("ref").

:- pred output_bounds(bounds::in, io__state::di, io__state::uo) is det.
output_bounds(Bounds) -->
	io__write_string("["),
	io__write_list(Bounds, ", ", output_bound),
	io__write_string("]").

:- pred output_bound(bound::in, io__state::di, io__state::uo) is det.
output_bound(upper(X)) --> 
	io__write_int(X).
output_bound(lower(X)) --> 
	io__write_int(X),
	io__write_string("...").
output_bound(between(X, Y)) --> 
	io__write_int(X),
	io__write_string("..."),
	io__write_int(Y).



:- pred output_modifier(ilds__type_modifier::in, io__state::di,
		io__state::uo) is det.
output_modifier(const)    --> io__write_string("const").
output_modifier(volatile) --> io__write_string("volatile").
output_modifier(readonly) --> io__write_string("readonly").

:- pred output_instructions(
	list(instr)::in, ilasm_info::in, ilasm_info::out,
	io__state::di, io__state::uo) is det.

output_instructions(Instructions, Info0, Info) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	globals__io_lookup_bool_option(debug_il_asm, DebugIlAsm),
	( 
		{ DebugIlAsm = yes },
		list__foldl2(output_debug_instruction, Instructions, Info0,
			Info)
	;
		{ DebugIlAsm = no },
		list__foldl2(output_instruction(PrintComments), Instructions,
			Info0, Info)
	).


	% We write each instruction before we execute it.
	% This is a nice way of debugging IL as it executes, although as
	% the IL debugger improves we might not need this any more.
:- pred output_debug_instruction(instr::in,
	ilasm_info::in, ilasm_info::out,
	io__state::di, io__state::uo) is det.

output_debug_instruction(Instr, Info0, Info) --> 

		% We can't handle tailcalls easily -- you need to put
		% it out as
		% 		trace the tail instruction
		% 		trace the call instruction
		%		output the tail instruction
		% 		output the call instruction
		% For the moment we'll just ignore tailcalls.
	( { Instr = tailcall } ->
		{ Info = Info0 }

		% Contexts are messy, let's ignore them for now.
	; { Instr = context(_, _) } ->
		{ Info = Info0 }
		
	; { Instr = start_block(catch(ClassName), Id) } ->
		output_instr(start_block(catch(ClassName), Id), Info0, Info1),
		io__write_string("\n"),
		io__write_string("\t"),
		output_trace_instr(Instr, Info1, Info),
		io__write_string("\n")

	; { Instr = start_block(scope(Locals), Id) } ->
		{ string__format("{\t// #%d", [i(Id)], S) },
		io__write_string(S),
		io__nl,
		output_trace(S),

		( { Locals = [] } ->
			{ Info = Info0 }
		;
			% output the .locals decl
			io__write_string("\t.locals (\n\t\t"),
			ilasm__write_list(Locals, ",\n\t\t", output_local,
				Info0, Info1),
			io__write_string("\n\t)"),
			io__write_string("\n"),

				% trace the .locals decl
			io__write_string("\t\tldstr """),
			io__write_string(".locals (\\n\\t\\t"),
			ilasm__write_list(Locals, ",\\n\\t\\t", output_local,
				Info1, Info),
			io__write_string(")"),
			io__write_string("\\n"""),
			io__write_string("\n"),
			io__write_string("\t\tcall void ['mscorlib']System.Console::Write(class ['mscorlib']System.String)\n")
		)

	;
		output_trace_instr(Instr, Info0, Info1),

		io__write_string("\t"),
		output_instr(Instr, Info1, Info),
		io__write_string("\n")
	).

:- pred output_trace_instr(instr::in, ilasm_info::in, ilasm_info::out,
	io__state::di, io__state::uo) is det.
output_trace_instr(Instr, Info0, Info) -->
	io__write_string("\t\tldstr """),
		% We have to quote loadstrings.
	( { Instr = ldstr(LoadString) } ->
		{ Info = Info0 },
		io__write_string("ldstr \\"""),
		output_escaped_string(LoadString, '\"'),
		io__write_string("\\""")
			% XXX there could be issues with
			% comments containing embedded newlines
	; { Instr = comment(Comment) } ->
		{ Info = Info0 },
		io__write_string("comment: "),
		io__write_string(Comment)
	; 
		output_instr(Instr, Info0, Info)
	),
	io__write_string("\\n"),
	io__write_string("""\n"),
	io__write_string("\t\tcall void ['mscorlib']System.Console::Write(class ['mscorlib']System.String)\n").


:- pred output_trace(string, io__state, io__state).
:- mode output_trace(in, di, uo) is det.
output_trace(S) -->
	io__write_string("\t\tldstr """),
	io__write_string(S),
	io__write_string("\\n""\n"),
	io__write_string("\t\tcall void ['mscorlib']System.Console::Write(class System.String)\n").

:- pred output_instruction(bool::in, instr::in,
	ilasm_info::in, ilasm_info::out, io__state::di, io__state::uo) is det.

output_instruction(PrintComments, Instr, Info0, Info) --> 
	( { Instr = comment(_), PrintComments = no } ->
		{ Info = Info0 }
	;
		io__write_string("\t"),
		output_instr(Instr, Info0, Info),
		io__write_string("\n")
	).

:- pred output_instr(instr::in, ilasm_info::in, ilasm_info::out,
	io__state::di, io__state::uo) is det.

output_instr(il_asm_code(Code, _MaxStack), I, I) --> 
	io__write_string(Code).

output_instr(comment(Comment), I, I) --> 
	output_comment_string(Comment).

output_instr(label(Label), I, I) --> 
	output_label(Label),
	io__write_string(":").

output_instr(start_block(scope(Locals), Id), Info0, Info) -->
	io__write_string("{"),
	io__write_string("\t// #"),
	io__write_int(Id),
	( { Locals = [] } ->
		{ Info = Info0 }
	;
		io__write_string("\n\t.locals (\n\t\t"),
		ilasm__write_list(Locals, ",\n\t\t", output_local, Info0, Info),
		io__write_string("\n\t)\n")
	).

output_instr(start_block(try, Id), I, I) -->
	io__write_string(".try {"),
	io__write_string("\t// #"),
	io__write_int(Id).

output_instr(start_block(catch(ClassName), Id), Info0, Info) -->
	io__write_string("catch "),
	output_class_name(ClassName, Info0, Info),
	io__write_string(" {"),
	io__write_string("\t// #"),
	io__write_int(Id).

output_instr(end_block(scope(_), Id), I, I) -->
	io__write_string("}"),
	io__write_string("\t// #"),
	io__write_int(Id).

output_instr(end_block(catch(_), Id), I, I) -->
	io__write_string("}"),
	io__write_string("\t// #"),
	io__write_int(Id),
	io__write_string(" (catch block)").

output_instr(end_block(try, Id), I, I) -->
	io__write_string("}"),
	io__write_string("\t// #"),
	io__write_int(Id),
	io__write_string(" (try block)").

output_instr(context(File, Line), I, I) -->
	io__write_string("\n\t.line "),
	io__write_int(Line),
	io__write_string(" '"),
	io__write_string(File),
	io__write_string("'").

output_instr(call(MethodRef), Info0, Info) --> 
	io__write_string("call\t"),
	output_methodref(MethodRef, Info0, Info).

output_instr(callvirt(MethodRef), Info0, Info) --> 
	io__write_string("callvirt\t"),
	output_methodref(MethodRef, Info0, Info).

output_instr(calli(Signature), Info0, Info) -->
	io__write_string("calli\t"),
	output_name_signature_and_call_conv(Signature, no, "\t\t", Info0, Info).

output_instr(ret, I, I) --> 
	io__write_string("ret").
output_instr((and), I, I) --> 
	io__write_string("and").
output_instr(arglist, I, I) --> 
	io__write_string("arglist").
output_instr(break, I, I) --> 
	io__write_string("break").
output_instr(ceq, I, I) --> 
	io__write_string("ceq").
output_instr(ckfinite, I, I) --> 
	io__write_string("ckfinite").
output_instr(cpblk, I, I) --> 
	io__write_string("cpblk").
output_instr(dup, I, I) -->
	io__write_string("dup").
output_instr(endfilter, I, I) --> 
	io__write_string("endfilter").
output_instr(endfinally, I, I) --> 
	io__write_string("endfinally").
output_instr(initblk, I, I) --> 
	io__write_string("initblk").
output_instr(ldnull, I, I) --> 
	io__write_string("ldnull").
output_instr(localloc, I, I) --> 
	io__write_string("localloc").
output_instr(neg, I, I) --> 
	io__write_string("neg").
output_instr(nop, I, I) --> 
	io__write_string("nop").
output_instr((not), I, I) --> 
	io__write_string("not").
output_instr((or), I, I) --> 
	io__write_string("or").
output_instr(pop, I, I) --> 
	io__write_string("pop").
output_instr(shl, I, I) --> 
	io__write_string("shl").
output_instr(tailcall, I, I) --> 
	io__write_string("tail.").
output_instr(volatile, I, I) --> 
	io__write_string("volatile").
output_instr(xor, I, I) --> 
	io__write_string("xor").
output_instr(ldlen, I, I) --> 
	io__write_string("ldlen").
output_instr(throw, I, I) --> 
	io__write_string("throw").

	% There are short forms of various instructions.
	% The assembler can't generate them for you.
output_instr(ldarg(index(Index)), I, I) --> 
	( { Index < 4 } ->
		io__write_string("ldarg."),
		io__write_int(Index)
	; { Index < 256 } ->
		io__write_string("ldarg.s\t"),
		output_index(Index)
	; 
		io__write_string("ldarg\t"),
		output_index(Index)
	).
output_instr(ldarg(name(Id)), I, I) --> 
	io__write_string("ldarg\t"),
	output_id(Id).

	% Lots of short forms for loading integer.
	% XXX Should probably put the magic numbers in functions.
output_instr(ldc(Type, Const), I, I) -->
	( { Type = int32, Const = i(IntConst) }  ->
		( { IntConst < 8, IntConst >= 0 } ->
			io__write_string("ldc.i4."),
			io__write_int(IntConst)
		; { IntConst = -1 } ->
			io__write_string("ldc.i4.m1")
		; { IntConst < 128, IntConst > -128 } ->
			io__write_string("ldc.i4.s\t"),
			io__write_int(IntConst)
		;
		 	io__write_string("ldc.i4\t"),
			io__write_int(IntConst)
		)
	; { Type = int64, Const = i(IntConst) } ->
		io__write_string("ldc.i8\t"),
		io__write_int(IntConst)
	; { Type = float32, Const = f(FloatConst) } ->
		io__write_string("ldc.r4\t"),
		io__write_float(FloatConst)
	; { Type = float64, Const = f(FloatConst) } ->
		io__write_string("ldc.r8\t"),
		io__write_float(FloatConst)
	;
	 	{ error("Inconsistent arguments in ldc instruction") }
	).

output_instr(ldstr(String), I, I) --> 
	io__write_string("ldstr\t"),
	output_string_constant(String).
		

	% XXX might use this later.
:- func max_efficient_encoding_short = int.
max_efficient_encoding_short = 256.

output_instr(add(Overflow, Signed), I, I) --> 
	io__write_string("add"),
	output_overflow(Overflow),
	output_signed(Signed).
	
output_instr(beq(Target), I, I) -->
	io__write_string("beq "),
	output_target(Target).

output_instr(bge(Signed, Target), I, I) -->
	io__write_string("bge"),
	output_signed(Signed),
	io__write_string("\t"),
	output_target(Target).

output_instr(bgt(Signed, Target), I, I) --> 
	io__write_string("bgt"),
	output_signed(Signed),
	io__write_string("\t"),
	output_target(Target).

output_instr(ble(Signed, Target), I, I) -->
	io__write_string("ble"),
	output_signed(Signed),
	io__write_string("\t"),
	output_target(Target).

output_instr(blt(Signed, Target), I, I) -->
	io__write_string("blt"),
	output_signed(Signed),
	io__write_string("\t"),
	output_target(Target).

output_instr(bne(Signed, Target), I, I) -->
	io__write_string("bne"),
	output_signed(Signed),
	io__write_string("\t"),
	output_target(Target).

output_instr(br(Target), I, I) -->
	io__write_string("br\t"),
	output_target(Target).

output_instr(brfalse(Target), I, I) --> 
	io__write_string("brfalse\t"),
	output_target(Target).

output_instr(brtrue(Target), I, I) --> 
	io__write_string("brtrue\t"),
	output_target(Target).

output_instr(cgt(Signed), I, I) -->
	io__write_string("cgt"),
	output_signed(Signed).

output_instr(clt(Signed), I, I) -->
	io__write_string("clt"),
	output_signed(Signed).

output_instr(conv(SimpleType), I, I) --> 
	io__write_string("conv."),
	output_simple_type_opcode(SimpleType).

output_instr(div(Signed), I, I) --> 
	io__write_string("div"),
	output_signed(Signed).

output_instr(jmp(MethodRef), Info0, Info) --> 
	io__write_string("jmp\t"),
	output_methodref(MethodRef, Info0, Info).

	% XXX can use short encoding for indexes
output_instr(ldarga(Variable), I, I) -->
	io__write_string("ldarga\t"),
	( { Variable = index(Index) }, output_index(Index)
	; { Variable = name(Name) }, output_id(Name)
	).
	
output_instr(ldftn(MethodRef), Info0, Info) --> 
	io__write_string("ldftn\t"),
	output_methodref(MethodRef, Info0, Info).

output_instr(ldind(SimpleType), I, I) --> 
	io__write_string("ldind."),
	output_simple_type_opcode(SimpleType).

	% XXX can use short encoding for indexes
output_instr(ldloc(Variable), I, I) --> 
	io__write_string("ldloc\t"),
	( { Variable = index(Index) }, output_index(Index)
	; { Variable = name(Name) }, output_id(Name)
	).

	% XXX can use short encoding for indexes
output_instr(ldloca(Variable), I, I) -->
	io__write_string("ldloca\t"),
	( { Variable = index(Index) }, output_index(Index)
	; { Variable = name(Name) }, output_id(Name)
	).

output_instr(leave(Target), I, I) --> 
	io__write_string("leave\t"),
	output_target(Target).
	
output_instr(mul(Overflow, Signed), I, I) --> 
	io__write_string("mul"),
	output_overflow(Overflow),
	output_signed(Signed).

output_instr(rem(Signed), I, I) --> 
	io__write_string("rem"),
	output_signed(Signed).

output_instr(shr(Signed), I, I) --> 
	io__write_string("shr"),
	output_signed(Signed).

	% XXX can use short encoding for indexes
output_instr(starg(Variable), I, I) -->
	io__write_string("starg\t"),
	( { Variable = index(Index) }, output_index(Index)
	; { Variable = name(Name) }, output_id(Name)
	).

	% XXX can use short encoding for indexes
output_instr(stind(SimpleType), I, I) --> 
	io__write_string("stind."),
	output_simple_type_opcode(SimpleType).
	
output_instr(stloc(Variable), I, I) --> 
	io__write_string("stloc\t"),
	( { Variable = index(Index) }, output_index(Index)
	; { Variable = name(Name) }, output_id(Name)
	).

output_instr(sub(OverFlow, Signed), I, I) --> 
	io__write_string("sub"),
	output_overflow(OverFlow),
	output_signed(Signed).
	
output_instr(switch(Targets), I, I) --> 
	io__write_string("switch ("),
	io__write_list(Targets, ", ", output_target),
	io__write_string(")").

output_instr(unaligned(_), I, I) --> 
	io__write_string("unaligned.").

output_instr(box(Type), Info0, Info) --> 
	io__write_string("box\t"),
	output_type(Type, Info0, Info).

output_instr(castclass(Type), Info0, Info) -->
	io__write_string("castclass\t"),
	output_type(Type, Info0, Info).

output_instr(cpobj(Type), Info0, Info) --> 
	io__write_string("cpobj\t"),
	output_type(Type, Info0, Info).

output_instr(initobj(Type), Info0, Info) --> 
	io__write_string("initobj\t"),
	output_type(Type, Info0, Info).
	
output_instr(isinst(Type), Info0, Info) --> 
	io__write_string("isinst\t"),
	output_type(Type, Info0, Info).

output_instr(ldelem(SimpleType), I, I) --> 
	io__write_string("ldelem."),
	output_simple_type_opcode(SimpleType).

output_instr(ldelema(Type), Info0, Info) --> 
	io__write_string("ldelema\t"),
	output_type(Type, Info0, Info).

output_instr(ldfld(FieldRef), Info0, Info) -->
	io__write_string("ldfld\t"),
	output_fieldref(FieldRef, Info0, Info).

output_instr(ldflda(FieldRef), Info0, Info) -->
	io__write_string("ldflda\t"),
	output_fieldref(FieldRef, Info0, Info).
	
output_instr(ldobj(Type), Info0, Info) -->
	io__write_string("ldobj\t"),
	output_type(Type, Info0, Info).

output_instr(ldsfld(FieldRef), Info0, Info) --> 
	io__write_string("ldsfld\t"),
	output_fieldref(FieldRef, Info0, Info).

output_instr(ldsflda(FieldRef), Info0, Info) --> 
	io__write_string("ldsflda\t"),
	output_fieldref(FieldRef, Info0, Info).

	% XXX should be implemented
output_instr(ldtoken(_), I, I) --> { error("output not implemented") }.

output_instr(ldvirtftn(MethodRef), Info0, Info) -->
	io__write_string("ldvirtftn\t"),
	output_methodref(MethodRef, Info0, Info).
	
output_instr(mkrefany(Type), Info0, Info) -->
	io__write_string("mkrefany\t"),
	output_type(Type, Info0, Info).

output_instr(newarr(Type), Info0, Info) --> 
	io__write_string("newarr\t"),
	output_type(Type, Info0, Info).

output_instr(newobj(MethodRef), Info0, Info) --> 
	io__write_string("newobj\t"),
	output_methodref(MethodRef, Info0, Info).

output_instr(refanytype, I, I) --> 
	io__write_string("refanytype").

output_instr(refanyval(Type), Info0, Info) --> 
	io__write_string("refanyval\t"),
	output_type(Type, Info0, Info).

output_instr(rethrow, I, I) --> 
	io__write_string("rethrow").

output_instr(stelem(SimpleType), I, I) --> 
	io__write_string("stelem."),
	output_simple_type_opcode(SimpleType).

output_instr(stfld(FieldRef), Info0, Info) --> 
	io__write_string("stfld\t"),
	output_fieldref(FieldRef, Info0, Info).

output_instr(stobj(Type), Info0, Info) -->
	io__write_string("stobj\t"),
	output_type(Type, Info0, Info).
	
output_instr(sizeof(Type), Info0, Info) -->
	io__write_string("sizeof\t"),
	output_type(Type, Info0, Info).

output_instr(stsfld(FieldRef), Info0, Info) --> 
	io__write_string("stsfld\t"),
	output_fieldref(FieldRef, Info0, Info).

output_instr(unbox(Type), Info0, Info) -->
	io__write_string("unbox\t"),
	output_type(Type, Info0, Info).

:- pred output_overflow(overflow::in, io__state::di,
		io__state::uo) is det.
output_overflow(OverFlow) -->
	(
		{ OverFlow = checkoverflow },
		io__write_string(".ovf")
	;
		{ OverFlow = nocheckoverflow }
	).

:- pred output_signed(signed::in, io__state::di, io__state::uo) is det.
output_signed(Signed) -->
	(
		{ Signed = signed }
	;
		{ Signed = unsigned },
		io__write_string(".un")
	).

:- pred output_target(target::in, io__state::di, io__state::uo) is det.
output_target(offset_target(Target)) -->
	io__write_int(Target).
output_target(label_target(Label)) -->
	output_label(Label).


:- pred output_fieldref(fieldref::in,
	ilasm_info::in, ilasm_info::out,
	io__state::di, io__state::uo) is det.
output_fieldref(fieldref(Type, ClassMemberName), Info0, Info) -->
	output_type(Type, Info0, Info1),
	io__write_string("\n\t\t"),
	output_class_member_name(ClassMemberName, Info1, Info).

:- pred output_methodref(methodref::in, ilasm_info::in, ilasm_info::out,
		io__state::di, io__state::uo) is det.
output_methodref(methoddef(call_conv(IsInstance, _), ReturnType, 
		ClassMemberName, ArgTypes), Info0, Info) -->
	( { IsInstance = yes } ->
		io__write_string("instance ")
	;
		[]
	),
	output_ret_type(ReturnType, Info0, Info1),
	io__write_string("\n\t\t"),
	output_class_member_name(ClassMemberName, Info1, Info2),
	( { ArgTypes = [] } ->
		io__write_string("()\n"),
		{ Info = Info0 }
	;
		io__write_string("(\n\t\t\t"),
		ilasm__write_list(ArgTypes, ",\n\t\t\t", output_type,
			Info2, Info),
		io__write_string("\n\t\t)")
	).
output_methodref(local_method(call_conv(IsInstance, _), ReturnType, 
		MethodName, ArgTypes), Info0, Info) -->
	( { IsInstance = yes } ->
		io__write_string("instance ")
	;
		[]
	),
	output_ret_type(ReturnType, Info0, Info1),
	io__write_string("\n\t\t"),
	output_member_name(MethodName),
	( { ArgTypes = [] } ->
		io__write_string("()\n"),
		{ Info = Info0 }
	;
		io__write_string("(\n\t\t\t"),
		ilasm__write_list(ArgTypes, ",\n\t\t\t", output_type,
			Info1, Info),
		io__write_string("\n\t\t)")
	).

:- pred output_classattr(classattr::in, io__state::di, io__state::uo) is det.

output_classattr(abstract) --> io__write_string("abstract").
output_classattr(ansi) --> io__write_string("ansi").
output_classattr(auto) --> io__write_string("auto").
output_classattr(autochar) --> io__write_string("autochar").
output_classattr(beforefieldinit) --> io__write_string("beforefieldinit").
output_classattr(explicit) --> io__write_string("explicit").
output_classattr(interface) --> io__write_string("interface").
output_classattr(nestedassembly) --> io__write_string("nested assembly").
output_classattr(nestedfamandassem) --> io__write_string("nested famandassem").
output_classattr(nestedfamily) --> io__write_string("nested family").
output_classattr(nestedfamorassem) --> io__write_string("nested famorassem").
output_classattr(nestedprivate) --> io__write_string("nested private").
output_classattr(nestedpublic) --> io__write_string("nested public").
output_classattr(private) --> io__write_string("private").
output_classattr(public) --> io__write_string("public").
output_classattr(rtspecialname) --> io__write_string("rtspecialname").
output_classattr(sealed) --> io__write_string("sealed").
output_classattr(sequential) --> io__write_string("sequential").
output_classattr(serializable) --> io__write_string("serializable").
output_classattr(specialname) --> io__write_string("specialname").
output_classattr(unicode) --> io__write_string("unicode").

:- pred ilasm__output_assembly_decl(assembly_decl::in, ilasm_info::in,
		ilasm_info::out, io__state::di, io__state::uo) is det.

ilasm__output_assembly_decl(version(A, B, C, D), I, I) -->
	io__format(".ver %d:%d:%d:%d", [i(A), i(B), i(C), i(D)]).
ilasm__output_assembly_decl(public_key_token(Token), I, I) -->
	io__write_string(".publickeytoken = ( "),
	io__write_list(Token, " ", output_hexbyte),
	io__write_string(" ) ").
ilasm__output_assembly_decl(hash(Hash), I, I) -->
	io__write_string(".hash = ( "),
	io__write_list(Hash, " ", output_hexbyte),
	io__write_string(" ) ").
ilasm__output_assembly_decl(custom(CustomDecl), Info0, Info) -->
	output_custom_decl(CustomDecl, Info0, Info).

:- pred output_custom_decl(custom_decl::in, ilasm_info::in, ilasm_info::out,
		io__state::di, io__state::uo) is det.
output_custom_decl(custom_decl(Type, MaybeOwner, StringOrBytes), 
		Info0, Info) -->
	io__write_string(".custom "),

	( { MaybeOwner = yes(Owner) } ->
		io__write_string(" ("),
		output_custom_type(Owner, Info0, Info1),
		io__write_string(") ")
	;
		{ Info1 = Info0 }
	),
	output_custom_type(Type, Info1, Info),
	( { StringOrBytes = bytes(Bytes) } ->
		io__write_string(" = ("),
		io__write_list(Bytes, " ", output_hexbyte),
		io__write_string(")")
	;
		{ sorry(this_file, "custom_decl of this sort") }
	),
	io__write_string("\n").

:- pred output_custom_type(custom_type::in, ilasm_info::in, ilasm_info::out,
		io__state::di, io__state::uo) is det.
output_custom_type(type(Type), Info0, Info) -->
	output_type(Type, Info0, Info).
output_custom_type(methodref(MethodRef), Info0, Info) -->
	output_methodref(MethodRef, Info0, Info).

:- pred output_index(index::in, io__state::di, io__state::uo) is det.
output_index(Index) -->
	io__write_int(Index).

:- pred output_string_constant(string::in, io__state::di, io__state::uo)
	is det.
output_string_constant(String) -->
	io__write_string(""""),
	output_escaped_string(String, '\"'),
	io__write_string("""").

:- pred output_class_member_name(class_member_name::in,
	ilasm_info::in, ilasm_info::out, io__state::di, io__state::uo) is det.
output_class_member_name(class_member_name(StructuredName, MemberName),
		Info0, Info) -->
	output_structured_name(StructuredName, Info0, Info),
	io__write_string("::"),
	output_member_name(MemberName).

:- pred output_structured_name(structured_name::in, ilasm_info::in,
	ilasm_info::out, io__state::di, io__state::uo) is det.
output_structured_name(structured_name(Asm, DottedName, NestedClasses),
		Info, Info) -->
	globals__io_lookup_bool_option(separate_assemblies, SeparateAssemblies),
	( { Asm = assembly(Assembly) },
		maybe_output_quoted_assembly_name(Assembly, Info)
	; { Asm = module(Module, Assembly) },
		( { SeparateAssemblies = yes },
			maybe_output_quoted_assembly_name(Module, Info)
		; { SeparateAssemblies = no },
			(
				{ Info ^ current_assembly \= "" },
				{ string__prefix(Module,
						Info ^ current_assembly) }
			->
				{ quote_id(Module ++ ".dll",
						QuotedModuleName) },
				io__format("[.module %s]",
						[s(QuotedModuleName)])
			;
				maybe_output_quoted_assembly_name(Assembly,
						Info)
			)
		)
	),
	output_dotted_name(DottedName),
	output_nested_class_quals(NestedClasses).


:- pred maybe_output_quoted_assembly_name(ilds__id::in, ilasm_info::in,
		io__state::di, io__state::uo) is det.

maybe_output_quoted_assembly_name(Assembly, Info) -->
	( { Assembly \= "", Assembly \= Info ^ current_assembly } ->
		{ quote_id(Assembly, QuotedAssemblyName) },
		io__format("[%s]", [s(QuotedAssemblyName)])
	;
		[]
	).

:- pred output_dotted_name(namespace_qual_name::in,
	io__state::di, io__state::uo) is det.
output_dotted_name(Name) -->
	io__write_list(Name, ".", output_id).

:- pred output_nested_class_quals(nested_class_name::in,
	io__state::di, io__state::uo) is det.
output_nested_class_quals(Name) -->
	list__foldl(
		(pred(Id::in, di, uo) is det -->
			io__write_char('/'), output_id(Id)),
		Name).

:- pred output_id(ilds__id::in, io__state::di, io__state::uo) is det.
output_id(Id) -->
	{ quote_id(Id, QuotedId) },
	io__write_string(QuotedId).

:- pred output_field_initializer(field_initializer::in, io__state::di,
	io__state::uo) is det.
output_field_initializer(none) --> [].
output_field_initializer(at(Id)) -->
	io__write_string(" at "),
	output_id(Id).
output_field_initializer(equals(FieldInit)) -->
	io__write_string(" = "),
	output_field_init(FieldInit).

:- pred output_field_init(field_init::in, io__state::di, io__state::uo) is det.
output_field_init(binary_float64(Int64)) -->
	io__write_string("float64("),
	output_int64(Int64),
	io__write_string(")").
output_field_init(binary_float32(Int32)) -->
	io__write_string("float32("),
	output_int32(Int32),
	io__write_string(")").
output_field_init(wchar_ptr(String)) -->
	io__write_string("wchar *("),
	io__write(String), 
	io__write_string(")").
		% XXX should check for invalid data_items
output_field_init(data_item(DataItem)) -->
	( { DataItem = char_ptr(String) } ->
		io__write(String)
	;
		output_data_item(DataItem)
	).


:- pred output_data_body(data_body::in, io__state::di, io__state::uo) is det.
output_data_body(itemlist(DataItemList)) -->
	io__write_string("{"),
	io__write_list(DataItemList, ", ", output_data_item),
	io__write_string("}").
output_data_body(item(DataItem)) -->
	output_data_item(DataItem).

:- pred output_data_item(data_item::in, io__state::di, io__state::uo) is det.
output_data_item(float64(Float)) -->
	io__write_string("float64("),
	output_float64(Float),
	io__write_string(")").
output_data_item(float32(Float32)) -->
	io__write_string("float32("),
	output_float32(Float32),
	io__write_string(")").
output_data_item(int64(Int64)) -->
	io__write_string("int64("),
	output_int64(Int64),
	io__write_string(")").
output_data_item(int32(Int32)) -->
	io__write_string("int32("),
	output_int32(Int32),
	io__write_string(")").
output_data_item(int16(Int16)) -->
	io__write_string("int16("),
	output_int16(Int16),
	io__write_string(")").
output_data_item(int8(Int8)) -->
	io__write_string("int8("),
	output_int8(Int8),
	io__write_string(")").
output_data_item(char_ptr(String)) -->
	io__write_string("char *("),
	io__write(String), 
	io__write_string(")").
output_data_item('&'(Id)) -->
	io__write_string("&("),
	output_id(Id),
	io__write_string(")").
output_data_item(bytearray(Bytes)) -->
	io__write_string("bytearray("),
	io__write_list(Bytes, " ", output_hexbyte),
	io__write_string(")").

:- pred output_float64(float64::in, io__state::di, io__state::uo) is det.
output_float64(float64(Float)) -->
	io__write_float(Float).

:- pred output_float32(float32::in, io__state::di, io__state::uo) is det.
output_float32(float32(Float)) -->
	io__write_float(Float).

:- pred output_int64(int64::in, io__state::di, io__state::uo) is det.
output_int64(int64(Integer)) -->
	io__write_string(integer__to_string(Integer)).

:- pred output_int32(int32::in, io__state::di, io__state::uo) is det.
output_int32(int32(Int)) -->
	io__write_int(Int).

:- pred output_int16(int16::in, io__state::di, io__state::uo) is det.
output_int16(int16(Int)) -->
	io__write_int(Int).

:- pred output_int8(int8::in, io__state::di, io__state::uo) is det.
output_int8(int8(Int)) -->
	io__write_int(Int).

:- pred output_byte(byte::in, io__state::di, io__state::uo) is det.
output_byte(Byte) --> output_int8(Byte).

:- pred output_hexbyte(byte::in, io__state::di, io__state::uo) is det.
output_hexbyte(int8(Int)) -->
	{ string__int_to_base_string(Int, 16, Tmp) },
	io__write_string(Tmp).

:- pred output_comment_string(string::in, io__state::di, io__state::uo) is det.
output_comment_string(Comment) -->
	io__write_string("// "),
	{ CommentDoc = separated(text, line, 
		string__words((pred('\n'::in) is semidet :- true), Comment)) },
	{ Doc = label("\t// ", CommentDoc) },
	write(70, Doc).

	% We need to quote all the IDs we output to avoid bumping into
	% keywords that assembler uses (there are a lot of them, and
	% there is no list available).
:- pred quote_id(ilds__id::in, string::out) is det.
quote_id(Id, QuotedId) :-
	escape_string(Id, '\'', EscapedId),
	string__append_list(["'", EscapedId, "'"], QuotedId).

:- pred output_escaped_string(string::in, char::in,
		io__state::di, io__state::uo) is det.
output_escaped_string(String, EscapeChar) -->
	{ escape_string(String, EscapeChar, EscapedString) },
	io__write_string(EscapedString).

	% Replace all Rep0 with backslash quoted Rep0 in Str0,
	% giving the escaped string Str.
	% We also escape embedded newlines and other characters.
	% We already do some name mangling during code generation that
	% means we avoid most weird characters here.
:- pred escape_string(string::in, char::in, string::out) is det.
escape_string(Str0, ReplaceChar, Str) :-
	string__to_char_list(Str0, CharList0),
	list__foldl(
		(pred(Char::in, E0::in, E::out) is det :-
			( escape_special_char(Char, QuoteChar) ->
				E = [QuoteChar, '\\' | E0]
			; Char = ReplaceChar ->
				E = [ReplaceChar, '\\' | E0]
			;
				E = [Char | E0]
			)
		), CharList0, [], CharList),
	string__from_rev_char_list(CharList, Str).


	% Characters that should be escaped in strings, and the
	% character to escape with.
:- pred escape_special_char(char::in, char::out) is semidet.
escape_special_char('\\', '\\').
escape_special_char('\n', 'n').
escape_special_char('\t', 't').
escape_special_char('\b', 'b').


:- func this_file = string.
this_file = "ilasm.m".

:- end_module ilasm.
