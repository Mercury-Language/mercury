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
			list(classdecl)		% methods and fields
		)
		% .namespace declaration
	;	namespace(
			structured_name,	% namespace name
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


		% .assembly extern
		% declares an assembly name, and possibly its strong
		% name/version number.
	;	extern_assembly(ilds__id, list(assembly_decl))

		% .assembly
		% defines an assembly
	;	assembly(ilds__id)

		% comments
	;	comment_term(term)
			% print almost anything using pprint__to_doc
			% (see library/pprint.m for limitations).
	;	some [T] comment_thing(T)
	;	comment(string).

:- type assembly_decl 
	--->	version(int, int, int, int)	% version number
	;	hash(list(int8))		% hash 
	;	public_key_token(list(int8)).	% public key token

	% a method definition is just a list of body decls.
:- type method_defn == list(method_body_decl).

:- type methodhead 
	---> methodhead(
			list(methattr),		% method attributes
			member_name,		% method name
			signature,		% method signature
			list(implattr)		% implementation attributes
	).

:- type classdecl
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
	--->	extends(classname)
	;	extends_nothing.

	% a list of interfaces that we implement
:- type implements
	--->	implements(list(classname)).

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
	;	instrs(list(instr))	% instructions
	;	label(string).		% a label

	% attributes that a class can have.
	% see SDK documentation for what they all mean.
:- type classattr
	--->	abstract    ;  ansi       ;  auto          ;  autochar
	;	contextful  ;  enum       ;  explicit      ;  import
	;	interface   ;  lazyinit   ;  marshalbyref  ;  public
	;	sealed      ;  sequential ;  unicode       ;  value
	;	wrapper.

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


	% classnames are just structured names.
:- type classname == structured_name.

:- implementation.

:- import_module char, string, pprint, getopt.
:- import_module require, int, term_io, varset, bool.
:- import_module globals, options.

ilasm__output(Blocks) --> 
	io__write_list(Blocks, "\n\n", output_decl),
	io__write_string("\n\n").


:- pred ilasm__output_decl(decl::in, io__state::di,
	io__state::uo) is det.

ilasm__output_decl(class(Attrs, Id, Extends, Implements, Contents)) --> 
	io__write_string(".class "),
	io__write_list(Attrs, " ", io__write),
	( { Attrs \= [] } ->
		io__write_string(" ")
	;
		[]
	),
	output_id(Id),
	(
		{ Extends = extends(ExtendsModule) },
		io__write_string(" extends "),
		output_classname(ExtendsModule)
	;
		{ Extends = extends_nothing }
	),
	{ Implements = implements(ImplementsList) },
	(
		{ ImplementsList = [_|_] }
	->
		io__write_string(" implements "),
		io__write_list(ImplementsList, ", ", output_classname)
	;
		[]
	),
	io__write_string(" {\n"),
	io__write_list(Contents, "\n", output_classdecl),
	io__write_string("\n}").
ilasm__output_decl(namespace(DottedName, Contents)) --> 
	io__write_string(".namespace "),
	output_dotted_name(DottedName),
	io__write_string(" {\n"),
	output(Contents),
	io__write_string("}\n").
ilasm__output_decl(method(MethodHead, MethodDecls)) --> 
	io__write_string(".method "),
	output_methodhead(MethodHead),
	io__write_string(" {\n"),
	io__write_list(MethodDecls, "\n", output_method_body_decl),
	io__write_string("}\n").
ilasm__output_decl(data(TLS, MaybeId, Body)) --> 
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

ilasm__output_decl(comment_term(CommentTerm)) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		io__write_string("// "),
		{ varset__init(VarSet) },
		term_io__write_term(VarSet, CommentTerm),
		io__write_string("\n")
	;
		[]
	).

ilasm__output_decl(comment_thing(Thing)) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		{ Doc = label("// ", to_doc(Thing)) },
		write(70, Doc),
		io__nl
	;
		[]
	).

ilasm__output_decl(comment(CommentStr)) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		output_comment_string(CommentStr)
	;
		[]
	).

ilasm__output_decl(extern_assembly(AsmName, AssemblyDecls)) --> 
	io__write_string(".assembly extern "),
	output_id(AsmName),
	io__write_string("{\n"),
	io__write_list(AssemblyDecls, "\n\t", output_assembly_decl),
	io__write_string("\n}\n").


ilasm__output_decl(assembly(AsmName)) --> 
	io__write_string(".assembly "),
	output_id(AsmName),
	io__write_string(" { }").

:- pred ilasm__output_classdecl(classdecl::in, io__state::di,
	io__state::uo) is det.

ilasm__output_classdecl(method(MethodHead, MethodDecls)) -->
		% Don't do debug output on class constructors, since
		% they are automatically generated and take forever to
		% run.
	globals__io_lookup_option(debug_il_asm, DebugIlAsm),
	( { MethodHead = methodhead(_, cctor, _, _) } ->
		globals__io_set_option(debug_il_asm, bool(no)),
		ilasm__output_decl(method(MethodHead, MethodDecls)),
		globals__io_set_option(debug_il_asm, DebugIlAsm)
	;
		ilasm__output_decl(method(MethodHead, MethodDecls))
	).

ilasm__output_classdecl(
		field(FieldAttrs, Type, IlId, MaybeOffset, Initializer)) -->
	io__write_string(".field "),
	( { MaybeOffset = yes(Offset) } ->
		output_int32(Offset),
		io__write_string(" ")
	;
		[]
	),
	io__write_list(FieldAttrs, " ", io__write),
	( { FieldAttrs \= [] } ->
		io__write_string(" ")
	;
		[]
	),
	output_type(Type),
	io__write_string(" "),
	output_id(IlId),
	output_field_initializer(Initializer).

ilasm__output_classdecl(comment(CommentStr)) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		output_comment_string(CommentStr)
	;
		[]
	).

ilasm__output_classdecl(comment_term(CommentTerm)) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		io__write_string("// "),
		{ varset__init(VarSet) },
		term_io__write_term(VarSet, CommentTerm),
		io__nl
	;
		[]
	).

ilasm__output_classdecl(comment_thing(Thing)) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	( { PrintComments = yes } ->
		{ Doc = label("// ", to_doc(Thing)) },
		write(70, Doc),
		io__nl
	;
		[]
	).

:- pred ilasm__output_methodhead(methodhead::in, io__state::di,
	io__state::uo) is det.
ilasm__output_methodhead(methodhead(Attrs, MethodName, Signature,
		ImplAttrs)) -->
	io__write_list(Attrs, " ", io__write),
	( { Attrs \= [] } ->
		io__write_string(" ")
	;
		[]
	),
	output_name_signature_and_call_conv(Signature, yes(MethodName)),
	io__write_list(ImplAttrs, " ", io__write).

:- pred ilasm__output_method_body_decl(method_body_decl::in, io__state::di,
	io__state::uo) is det.
ilasm__output_method_body_decl(emitbyte(Int32)) -->
	io__write_string(".emitbyte "),
	output_int32(Int32).

ilasm__output_method_body_decl(maxstack(Int32)) -->
	io__write_string(".maxstack "),
	output_int32(Int32).

ilasm__output_method_body_decl(entrypoint) -->
	io__write_string(".entrypoint ").

ilasm__output_method_body_decl(zeroinit) -->
	io__write_string(".zeroinit ").

ilasm__output_method_body_decl(instrs(Instrs)) -->
	output_instructions(Instrs).

ilasm__output_method_body_decl(label(Label)) -->
	output_label(Label),
	io__write_string(":").

:- pred output_label(label::in, io__state::di, io__state::uo) is det.
output_label(Label) -->
	io__write_string(Label).

:- pred output_classname(classname::in, io__state::di, io__state::uo) is det.
output_classname(ClassName) -->
	output_structured_name(ClassName).

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
	maybe(member_name)::in, io__state::di, io__state::uo) is det.
output_name_signature_and_call_conv(signature(CallConv, ReturnType,
		 ArgTypes), MaybeMethodName) -->
	output_call_conv(CallConv),
	io__write_string(" "),
	output_ret_type(ReturnType),
	io__write_string(" "),
	( { MaybeMethodName = yes(MethodName) } ->
		output_member_name(MethodName)
	;
		[]
	),
	io__write_string("("),
	io__write_list(ArgTypes, ", ", output_param),
	io__write_string(")").

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
	io__state::di, io__state::uo) is det.
output_ret_type(void) --> io__write_string("void").
output_ret_type(simple_type(Type)) --> output_simple_type(Type).

:- pred output_local(pair(ilds__id, ilds__type)::in, 
		io__state::di, io__state::uo) is det.
output_local(Id - Type) -->
	output_type(Type),
	io__write_string(" "),
	output_id(Id).

:- pred output_param(pair(ilds__type, maybe(ilds__id))::in, 
		io__state::di, io__state::uo) is det.
output_param(Type - no) -->
	output_type(Type).
output_param(Type - yes(Id)) -->
	output_type(Type),
	io__write_string(" "),
	output_id(Id).

:- pred output_type(ilds__type::in, io__state::di, io__state::uo) is det.
output_type(ilds__type(Modifiers, SimpleType)) -->
	io__write_list(Modifiers, " ", output_modifier),
	output_simple_type(SimpleType).

:- pred output_simple_type(simple_type::in, io__state::di,
		io__state::uo) is det.
output_simple_type(int8) --> io__write_string("int8").
output_simple_type(int16) --> io__write_string("int16").
output_simple_type(int32) --> io__write_string("int32").
output_simple_type(int64) --> io__write_string("int64").
output_simple_type(uint8) --> io__write_string("uint8").
output_simple_type(uint16) --> io__write_string("uint16").
output_simple_type(uint32) --> io__write_string("uint32").
output_simple_type(uint64) --> io__write_string("uint64").
output_simple_type(native_int) --> io__write_string("nativeint").
output_simple_type(native_uint) --> io__write_string("nativeuint").
output_simple_type(float32) --> io__write_string("float32").
output_simple_type(float64) --> io__write_string("float64").
output_simple_type(native_float) --> io__write_string("native_float").
output_simple_type(bool) --> io__write_string("bool").
output_simple_type(char) --> io__write_string("char").
output_simple_type(refany) --> io__write_string("refany").
output_simple_type(class(Name)) --> 
	io__write_string("class "),
	output_structured_name(Name).
output_simple_type(value_class(Name)) --> 
	io__write_string("value_class "),
	output_structured_name(Name).
output_simple_type(interface(Name)) --> 
	io__write_string("interface "),
	output_structured_name(Name).
output_simple_type('[]'(Type, Bounds)) --> 
	output_type(Type),
	output_bounds(Bounds).
output_simple_type('*'(Type)) --> 
	output_type(Type),
	io__write_string("*").
output_simple_type('&'(Type)) --> 
	output_type(Type),
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
		% XXX should i4 be used for bool and char? 
output_simple_type_opcode(bool) --> io__write_string("i4").
output_simple_type_opcode(char) --> io__write_string("i4").

	% all reference types use "ref" as their opcode.
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
	list(instr)::in, io__state::di, io__state::uo) is det.

output_instructions(Instructions) --> 
	globals__io_lookup_bool_option(auto_comments, PrintComments),
	globals__io_lookup_bool_option(debug_il_asm, DebugIlAsm),
	( 
		{ DebugIlAsm = yes },
		list__foldl(output_debug_instruction, Instructions)
	;
		{ DebugIlAsm = no },
		list__foldl(output_instruction(PrintComments), Instructions)
	).


	% We write each instruction before we execute it.
	% This is a nice way of debugging IL as it executes, although as
	% the IL debugger improves we might not need this any more.
:- pred output_debug_instruction(instr::in, io__state::di,
	io__state::uo) is det.

output_debug_instruction(Instr) --> 

		% We can't handle tailcalls easily -- you need to put
		% it out as
		% 		trace the tail instruction
		% 		trace the call instruction
		%		output the tail instruction
		% 		output the call instruction
		% For the moment we'll just ignore tailcalls.
	( { Instr = tailcall } ->
		[]
	; { Instr = start_block(scope(Locals), Id) } ->
		{ string__format("{\t// #%d", [i(Id)], S) },
		io__write_string(S),
		io__nl,
		output_trace(S),

			% output the .locals decl
		io__write_string(".locals ("),
		io__write_list(Locals, ", ", output_local),
		io__write_string(")"),
		io__write_string("\n"),

			% trace the .locals decl
		io__write_string("\t\tldstr """),
		io__write_string(".locals ("),
		io__write_list(Locals, ", ", output_local),
		io__write_string(")"),
		io__write_string("\\n"""),
		io__write_string("\n"),
		io__write_string("\t\tcall void System.Console::Write(class System.String)\n")

	;
		io__write_string("\t\tldstr """),
			% We have to quote loadstrings.
		( { Instr = ldstr(LoadString) } ->
			io__write_string("ldstr \\"""),
			output_escaped_string(LoadString, '\"'),
			io__write_string("\\""")
				% XXX there could be issues with
				% comments containing embedded newlines
		; { Instr = comment(Comment) } ->
			io__write_string("comment: "),
			io__write_string(Comment)
		; 
			output_instr(Instr)
		),
		io__write_string("\\n"),
		io__write_string("""\n"),
		io__write_string("\t\tcall void System.Console::Write(class System.String)\n"),

		io__write_string("\t"),
		output_instr(Instr),
		io__write_string("\n")
	).

:- pred output_trace(string, io__state, io__state).
:- mode output_trace(in, di, uo) is det.
output_trace(S) -->
	io__write_string("\t\tldstr """),
	io__write_string(S),
	io__write_string("\\n""\n"),
	io__write_string("\t\tcall void System.Console::Write(class System.String)\n").

:- pred output_instruction(bool::in, instr::in, io__state::di,
	io__state::uo) is det.

output_instruction(PrintComments, Instr) --> 
	( { Instr = comment(_), PrintComments = no } ->
		[]
	;
		io__write_string("\t"),
		output_instr(Instr),
		io__write_string("\n")
	).

:- pred output_instr(instr::in, io__state::di,
	io__state::uo) is det.

output_instr(comment(Comment)) --> 
	output_comment_string(Comment).

output_instr(label(Label)) --> 
	output_label(Label),
	io__write_string(":").

output_instr(start_block(scope(Locals), Id)) -->
	io__write_string("{"),
	io__write_string("\t// #"),
	io__write_int(Id),
	io__write_string("\n\t.locals ("),
	io__write_list(Locals, ", ", output_local),
	io__write_string(")\n").

output_instr(start_block(try, Id)) -->
	io__write_string(".try {"),
	io__write_string("\t// #"),
	io__write_int(Id).

output_instr(start_block(catch(ClassName), Id)) -->
	io__write_string("catch "),
	output_classname(ClassName),
	io__write_string(" {"),
	io__write_string("\t// #"),
	io__write_int(Id).

output_instr(end_block(scope(_), Id)) -->
	io__write_string("}"),
	io__write_string("\t// #"),
	io__write_int(Id).

output_instr(end_block(catch(_), Id)) -->
	io__write_string("}"),
	io__write_string("\t// #"),
	io__write_int(Id),
	io__write_string(" (catch block)").

output_instr(end_block(try, Id)) -->
	io__write_string("}"),
	io__write_string("\t// #"),
	io__write_int(Id),
	io__write_string(" (try block)").

output_instr(context(File, Line)) -->
	io__write_string(".line "),
	io__write_int(Line),
	io__write_string(" '"),
	io__write_string(File),
	io__write_string("'\n").

output_instr(call(MethodRef)) --> 
	io__write_string("call\t"),
	output_methodref(MethodRef).

output_instr(callvirt(MethodRef)) --> 
	io__write_string("callvirt\t"),
	output_methodref(MethodRef).

output_instr(calli(Signature)) -->
	io__write_string("calli\t"),
	output_name_signature_and_call_conv(Signature, no).

output_instr(ret) --> 
	io__write_string("ret").
output_instr((and)) --> 
	io__write_string("and").
output_instr(ann_catch) --> 
	io__write_string("ann_catch").
output_instr(ann_def) --> 
	io__write_string("ann_def").
output_instr(ann_lab) --> 
	io__write_string("ann_lab").
output_instr(arglist) --> 
	io__write_string("arglist").
output_instr(break) --> 
	io__write_string("break").
output_instr(ceq) --> 
	io__write_string("ceq").
output_instr(ckfinite) --> 
	io__write_string("ckfinite").
output_instr(cpblk) --> 
	io__write_string("cpblk").
output_instr(dup) -->
	io__write_string("dup").
output_instr(endcatch) -->
	io__write_string("endcatch").
output_instr(endfilter) --> 
	io__write_string("endfilter").
output_instr(endfinally) --> 
	io__write_string("endfinally").
output_instr(initblk) --> 
	io__write_string("initblk").
output_instr(jmpi) --> 
	io__write_string("jmpi").
output_instr(ldnull) --> 
	io__write_string("ldnull").
output_instr(localloc) --> 
	io__write_string("localloc").
output_instr(neg) --> 
	io__write_string("neg").
output_instr(nop) --> 
	io__write_string("nop").
output_instr((not)) --> 
	io__write_string("not").
output_instr((or)) --> 
	io__write_string("or").
output_instr(pop) --> 
	io__write_string("pop").
output_instr(shl) --> 
	io__write_string("shl").
output_instr(tailcall) --> 
	io__write_string("tail.").
output_instr(volatile) --> 
	io__write_string("volatile").
output_instr(xor) --> 
	io__write_string("xor").
output_instr(entercrit) --> 
	io__write_string("entercrit").
output_instr(exitcrit) -->
	io__write_string("exitcrit").
output_instr(ldlen) --> 
	io__write_string("ldlen").
output_instr(throw) --> 
	io__write_string("throw").
output_instr(ann_hoisted_call) -->
	io__write_string("ann_hoisted_call").

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

	% There are short forms of various instructions.
	% The assembler can't generate them for you.
output_instr(ldarg(index(Index))) --> 
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
output_instr(ldarg(name(Id))) --> 
	io__write_string("ldarg\t"),
	output_id(Id).

	% Lots of short forms for loading integer.
	% XXX Should probably put the magic numbers in functions.
output_instr(ldc(Type, Const)) -->
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

output_instr(ldstr(String)) --> 
	io__write_string("ldstr\t"),
	output_string_constant(String).
		

	% XXX might use this later.
:- func max_efficient_encoding_short = int.
max_efficient_encoding_short = 256.

output_instr(add(Overflow, Signed)) --> 
	io__write_string("add"),
	output_overflow(Overflow),
	output_signed(Signed).
	
output_instr(beq(Target)) -->
	io__write_string("beq "),
	output_target(Target).

output_instr(bge(Signed, Target)) -->
	io__write_string("bge"),
	output_signed(Signed),
	io__write_string("\t"),
	output_target(Target).

output_instr(bgt(Signed, Target)) --> 
	io__write_string("bgt"),
	output_signed(Signed),
	io__write_string("\t"),
	output_target(Target).

output_instr(ble(Signed, Target)) -->
	io__write_string("ble"),
	output_signed(Signed),
	io__write_string("\t"),
	output_target(Target).

output_instr(blt(Signed, Target)) -->
	io__write_string("blt"),
	output_signed(Signed),
	io__write_string("\t"),
	output_target(Target).

output_instr(bne(Signed, Target)) -->
	io__write_string("bne"),
	output_signed(Signed),
	io__write_string("\t"),
	output_target(Target).

output_instr(br(Target)) -->
	io__write_string("br\t"),
	output_target(Target).

output_instr(brfalse(Target)) --> 
	io__write_string("brfalse\t"),
	output_target(Target).

output_instr(brtrue(Target)) --> 
	io__write_string("brtrue\t"),
	output_target(Target).

output_instr(cgt(Signed)) -->
	io__write_string("cgt"),
	output_signed(Signed).

output_instr(clt(Signed)) -->
	io__write_string("clt"),
	output_signed(Signed).

output_instr(conv(SimpleType)) --> 
	io__write_string("conv."),
	output_simple_type_opcode(SimpleType).

output_instr(div(Signed)) --> 
	io__write_string("div"),
	output_signed(Signed).

output_instr(jmp(MethodRef)) --> 
	io__write_string("jmp\t"),
	output_methodref(MethodRef).

	% XXX can use short encoding for indexes
output_instr(ldarga(Variable)) -->
	io__write_string("ldarga\t"),
	( { Variable = index(Index) }, output_index(Index)
	; { Variable = name(Name) }, output_id(Name)
	).
	
output_instr(ldftn(MethodRef)) --> 
	io__write_string("ldftn\t"),
	output_methodref(MethodRef).

output_instr(ldind(SimpleType)) --> 
	io__write_string("ldind."),
	output_simple_type_opcode(SimpleType).

	% XXX can use short encoding for indexes
output_instr(ldloc(Variable)) --> 
	io__write_string("ldloc\t"),
	( { Variable = index(Index) }, output_index(Index)
	; { Variable = name(Name) }, output_id(Name)
	).

	% XXX can use short encoding for indexes
output_instr(ldloca(Variable)) -->
	io__write_string("ldloca\t"),
	( { Variable = index(Index) }, output_index(Index)
	; { Variable = name(Name) }, output_id(Name)
	).

output_instr(leave(Target)) --> 
	io__write_string("leave\t"),
	output_target(Target).
	
output_instr(mul(Overflow, Signed)) --> 
	io__write_string("mul"),
	output_overflow(Overflow),
	output_signed(Signed).

output_instr(rem(Signed)) --> 
	io__write_string("rem"),
	output_signed(Signed).

output_instr(shr(Signed)) --> 
	io__write_string("shr"),
	output_signed(Signed).

	% XXX can use short encoding for indexes
output_instr(starg(Variable)) -->
	io__write_string("starg\t"),
	( { Variable = index(Index) }, output_index(Index)
	; { Variable = name(Name) }, output_id(Name)
	).

	% XXX can use short encoding for indexes
output_instr(stind(SimpleType)) --> 
	io__write_string("stind."),
	output_simple_type_opcode(SimpleType).
	
output_instr(stloc(Variable)) --> 
	io__write_string("stloc\t"),
	( { Variable = index(Index) }, output_index(Index)
	; { Variable = name(Name) }, output_id(Name)
	).

output_instr(sub(OverFlow, Signed)) --> 
	io__write_string("sub"),
	output_overflow(OverFlow),
	output_signed(Signed).
	
output_instr(switch(Targets)) --> 
	io__write_string("switch ("),
	io__write_list(Targets, ", ", output_target),
	io__write_string(")").

output_instr(unaligned(_)) --> 
	io__write_string("unaligned.").

output_instr(box(Type)) --> 
	io__write_string("box\t"),
	output_type(Type).

output_instr(castclass(Type)) -->
	io__write_string("castclass\t"),
	output_type(Type).

output_instr(cpobj(Type)) --> 
	io__write_string("cpobj\t"),
	output_type(Type).

output_instr(initobj(Type)) --> 
	io__write_string("initobj\t"),
	output_type(Type).
	
output_instr(isinst(Type)) --> 
	io__write_string("isinst\t"),
	output_type(Type).

output_instr(ldelem(SimpleType)) --> 
	io__write_string("ldelem."),
	output_simple_type_opcode(SimpleType).

output_instr(ldelema(Type)) --> 
	io__write_string("ldelema\t"),
	output_type(Type).

output_instr(ldfld(FieldRef)) -->
	io__write_string("ldfld\t"),
	output_fieldref(FieldRef).

output_instr(ldflda(FieldRef)) -->
	io__write_string("ldflda\t"),
	output_fieldref(FieldRef).
	
output_instr(ldobj(Type)) -->
	io__write_string("ldobj\t"),
	output_type(Type).
	
output_instr(ldrefany(Index)) -->
	io__write_string("ldrefany\t"),
	output_index(Index).

output_instr(ldsfld(FieldRef)) --> 
	io__write_string("ldsfld\t"),
	output_fieldref(FieldRef).

output_instr(ldsflda(FieldRef)) --> 
	io__write_string("ldsflda\t"),
	output_fieldref(FieldRef).

	% XXX should be implemented
output_instr(ldtoken(_)) --> { error("output not implemented") }.

output_instr(ldvirtftn(MethodRef)) -->
	io__write_string("ldvirtftn\t"),
	output_methodref(MethodRef).
	
output_instr(mkrefany(Type)) -->
	io__write_string("mkrefany\t"),
	output_type(Type).

output_instr(newarr(Type)) --> 
	io__write_string("newarr\t"),
	output_type(Type).

output_instr(newobj(MethodRef)) --> 
	io__write_string("newobj\t"),
	output_methodref(MethodRef).

output_instr(stelem(SimpleType)) --> 
	io__write_string("stelem."),
	output_simple_type_opcode(SimpleType).

output_instr(stfld(FieldRef)) --> 
	io__write_string("stfld\t"),
	output_fieldref(FieldRef).

output_instr(stsfld(FieldRef)) --> 
	io__write_string("stsfld\t"),
	output_fieldref(FieldRef).

output_instr(typerefany(Index)) -->
	io__write_string("typerefany\t"),
	output_index(Index).

output_instr(unbox(Type)) -->
	io__write_string("unbox\t"),
	output_type(Type).

	% This is stuff for "Opt-IL", which was (is?) some sort of 
	% optimization annotated IL.  I have no idea whether it is used
	% at all.
output_instr(ann_call(_)) --> { error("output not implemented") }.
output_instr(ann_data(_)) --> { error("output not implemented") }.
output_instr(ann_dead(_)) --> { error("output not implemented") }.
output_instr(ann_hoisted(_)) --> { error("output not implemented") }.
output_instr(ann_live(_)) --> { error("output not implemented") }.
output_instr(ann_phi(_)) --> { error("output not implemented") }.
output_instr(ann_ref(_)) --> { error("output not implemented") }.

:- pred output_fieldref(fieldref::in,
	io__state::di, io__state::uo) is det.
output_fieldref(fieldref(Type, ClassMemberName)) -->
	output_type(Type),
	io__write_string(" "),
	output_class_member_name(ClassMemberName).

:- pred output_methodref(methodref::in,
	io__state::di, io__state::uo) is det.
output_methodref(methodref(call_conv(IsInstance, _), ReturnType, 
		StructuredName, ArgTypes)) -->
	( { IsInstance = yes } ->
		io__write_string("instance ")
	;
		[]
	),
	output_ret_type(ReturnType),
	io__write_string(" "),
	output_structured_name(StructuredName),
	io__write_string("("),
	io__write_list(ArgTypes, ", ", output_type),
	io__write_string(")").
output_methodref(methoddef(call_conv(IsInstance, _), ReturnType, 
		ClassMemberName, ArgTypes)) -->
	( { IsInstance = yes } ->
		io__write_string("instance ")
	;
		[]
	),
	output_ret_type(ReturnType),
	io__write_string(" "),
	output_class_member_name(ClassMemberName),
	io__write_string("("),
	io__write_list(ArgTypes, ", ", output_type),
	io__write_string(")").
output_methodref(local_method(call_conv(IsInstance, _), ReturnType, 
		MethodName, ArgTypes)) -->
	( { IsInstance = yes } ->
		io__write_string("instance ")
	;
		[]
	),
	output_ret_type(ReturnType),
	io__write_string(" "),
	output_member_name(MethodName),
	io__write_string("("),
	io__write_list(ArgTypes, ", ", output_type),
	io__write_string(")").

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
	io__state::di, io__state::uo) is det.
output_class_member_name(class_member_name(StructuredName, MemberName)) -->
	( { StructuredName = [_ | _] } ->
		output_structured_name(StructuredName),
		io__write_string("::")
	;
		[]
	),
	output_member_name(MemberName).


	% For any "other" modules, we we reference the
	% assembly "[modulename]".  All mercury standard library modules
	% are already prefixed with "mercury" so we will reference [mercury].
	% "mscorlib" is not to be treated like this.
	% Temporarily, mercury_base_typeclass_info is not treated like this.
	% (until we get type class instances working properly, we may
	% try to generate them all in the one namespace, although so far
	% this isn't working very well because although namespaces are
	% merged at runtime, you still need to give an assembly
	% reference for which assembly the thing you want to reference
	% is located).
:- pred output_structured_name(structured_name::in,
	io__state::di, io__state::uo) is det.
output_structured_name(Name) -->
	( { Name = [ModuleName | Rest] } ->
		( { ModuleName = "mscorlib" } ->
			{ DottedName = Rest }
		; { ModuleName = "mercury_base_typeclass_info" } ->
			{ DottedName = [ModuleName | Rest] }
		;
			{ quote_id(ModuleName, QuotedModuleName) },
			io__format("[%s]", [s(QuotedModuleName)]),
			{ DottedName = [ModuleName | Rest] }
		),
		output_dotted_name(DottedName)
	;
		[]
	).

:- pred output_dotted_name(structured_name::in,
	io__state::di, io__state::uo) is det.
output_dotted_name(Name) -->
	io__write_list(Name, ".", output_id).

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

:- pred ilasm__output_assembly_decl(assembly_decl::in, 
	io__state::di, io__state::uo) is det.

ilasm__output_assembly_decl(version(A, B, C, D)) -->
	io__format(".ver %d:%d:%d:%d", [i(A), i(B), i(C), i(D)]).
ilasm__output_assembly_decl(public_key_token(Token)) -->
	io__write_string(".publickeytoken = ( "),
	io__write_list(Token, " ", output_hexbyte),
	io__write_string(" ) ").
ilasm__output_assembly_decl(hash(Hash)) -->
	io__write_string(".hash = ( "),
	io__write_list(Hash, " ", output_hexbyte),
	io__write_string(" ) ").



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



:- end_module ilasm.
