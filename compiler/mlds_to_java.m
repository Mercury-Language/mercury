%----------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% mlds_to_java - Convert MLDS to Java code.
% Main author: juliensf 
%
% DONE:
%	det and semidet predicates
%	multiple output arguments
%       boxing and unboxing
%       conjunctions
%       disjunctions
%	if-then-else's
%       enumerations
%	discriminated unions
% TODO: 
%	multidet and nondet predicates
%       RTTI
%	handle foreign code written in Java
%       higher order functions
%       generate names of classes etc. correctly 
% 	generate du constructors instead of directly assigning fields
%	generate optimized tailcalls
%       handle foreign code written in C 
%	handle static ground terms
%
% NOTES: 
%       To avoid namespace conflicts all Java names must be fully qualified.
%	e.g. The classname `String' must be qualified as `java.lang.String'
%	     to avoid conflicting with `mercury.String'.
%-----------------------------------------------------------------------------%

:- module mlds_to_java.
:- interface.

:- import_module mlds.
:- import_module io.

:- pred mlds_to_java__output_mlds(mlds, io__state, io__state).
:- mode mlds_to_java__output_mlds(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_util.
:- import_module java_util. 
:- import_module c_util.	% XXX needed for c_util__output_quoted_string
				% c_util_output_quoted_multi_string
:- import_module llds_out.	% XXX needed for llds_out__name_mangle,
				% llds_out__sym_name_mangle,
				% llds_out__make_base_typeclass_info_name,
:- import_module rtti.		% for rtti__addr_to_string.
:- import_module rtti_to_mlds.	% for mlds_rtti_type_name.
:- import_module hlds_pred.	% for pred_proc_id.
:- import_module modules.       % for mercury_std_library_name.
:- import_module ml_code_util.	% for ml_gen_mlds_var_decl, which is used by
				% the code that handles derived classes
:- import_module ml_type_gen.	% for ml_gen_type_name
:- import_module export.	% for export__type_to_type_string
:- import_module globals, options, passes_aux.
:- import_module builtin_ops.
:- import_module prog_data, prog_out, type_util, error_util.

:- import_module bool, int, string, library, list.
:- import_module assoc_list, term, std_util, require.

%-----------------------------------------------------------------------------%

mlds_to_java__output_mlds(MLDS) -->
	{ ModuleName = mlds__get_module_name(MLDS) },
	module_name_to_file_name(ModuleName, ".java", yes, JavaSourceFile),
	{ Indent = 0 },
	output_to_file(JavaSourceFile, output_java_src_file(Indent, MLDS)).

%-----------------------------------------------------------------------------%
%
% Utility predicates for various purposes. 
%

	% Succeeds iff the given qualified name is part of the standard
	% library (as listed in compiler/modules.m).   
	%
:- pred qualified_name_is_stdlib(mercury_module_name).
:- mode qualified_name_is_stdlib(in) is semidet.

qualified_name_is_stdlib(unqualified(_)) :- fail.
qualified_name_is_stdlib(qualified(Module, Name)) :-
	 ( 
	 	mercury_std_library_module(Name), 
		Module = unqualified("mercury") 
	 ;
	 	qualified_name_is_stdlib(Module)
	 ).

	% Succeeds iff this definition is a function definition which
	% defines the `unify' or `compare' special predicate.
	%
:- pred defn_is_unify_or_compare(mlds__defn).
:- mode defn_is_unify_or_compare(in) is semidet.

defn_is_unify_or_compare(Defn) :-
	Defn  = mlds__defn(Name, _Context, _Flags, _Body),
	Name  = function(Label, _ProcID, _MaybeSeqNum, _PredID),
	Label = special_pred(PredName, _, _, _),
	(
		PredName = "__Compare__"
	;
		PredName = "__Unify__"
	).

	% Succeeds iff this definition is a data definition which
	% defines RTTI.
	%
:- pred defn_is_rtti_data(mlds__defn).
:- mode defn_is_rtti_data(in) is semidet.

defn_is_rtti_data(Defn) :-
	Defn = mlds__defn(_Name, _Context, _Flags, Body),
	Body = mlds__data(Type, _),
	Type = mlds__rtti_type(_).

	% Succeeds iff this type is a enumeration.
	%
:- pred type_is_enum(mlds__type).
:- mode type_is_enum(in) is semidet.

type_is_enum(Type) :-
	Type = mercury_type(_, Builtin),
	Builtin = enum_type.

	%  Succeeds iff this type is something that 
	%  the Java backend will represent as an object 
	%  i.e. something created using the new operator.
	%
:- pred type_is_object(mlds__type).
:- mode type_is_object(in) is semidet.

type_is_object(Type) :-
	Type = mercury_type(_, Builtin),
	( Builtin = enum_type 
	; Builtin = polymorphic_type
	; Builtin = user_type 
	).

	% Succeeds iff the Rval represents an integer constant.  
	%
:- pred rval_is_int_const(mlds__rval).
:- mode rval_is_int_const(in) is semidet.

rval_is_int_const(Rval) :-
	Rval = const(int_const(_)).

	% Succeeds iff the Rval represents an enumeration
	% object in the Java backend.  We need to check both Rval's
	% that are variables and Rval's that are casts.  
	% We need to know this in order to append the field name 
	% to the object so we can access the value of the enumeration object.
	%
:- pred rval_is_enum_object(mlds__rval).
:- mode rval_is_enum_object(in) is semidet.

rval_is_enum_object(Rval) :-
		Rval = lval(Lval),
		Lval = var(_, VarType),
		type_is_enum(VarType).

	% Succeeds iff a given string matches the unqualified
	% interface name of a interface in Mercury's Java runtime system.
	%
:- pred interface_is_special(string).
:- mode interface_is_special(in) is semidet.

interface_is_special("Unify").
interface_is_special("Compare").
interface_is_special("ProcAddr").

%-----------------------------------------------------------------------------%
%
% Code to mangle names, enforce Java code conventions regarding class names
% etc.
% XXX None of this stuff works as it should. The idea is that class
% names should start with an uppercase letter, while method names and 
% package specifiers should start with a lowercase letter.  
% The current implementation of the MLDS makes this rather harder to achieve 
% than it might initially seem.  The current position is that coding 
% conventions are only enforced on library modules.
% This is needed as Java compilers don't take too well to compiling
% classes named `char',`int', `float' etc.
% XXX It might be nice if the name mangling code was taken out of which
% ever LLDS module it's hiding in and put in a seperate one.
%

	% XXX This won't work if we start using the Java
	% coding conventions for all names.  At the moment
	% it only affects library modules.  
	%
:- pred enforce_java_names(string, string).
:- mode enforce_java_names(in, out) is det.

enforce_java_names(Name, JavaName) :-
	%
	% If the Name contains one or more dots (`.'), then
	% capitalize the first letter after the last dot.
	%
	reverse_string(Name, RevName),
	( string__sub_string_search(RevName, ".", Pos) ->
		string__split(RevName, Pos, Head0, Tail0),
		reverse_string(Tail0, Tail),
		reverse_string(Head0, Head1),
		string__capitalize_first(Head1, Head),
		string__append(Tail, Head, JavaName)
	;
		JavaName = Name
	).

:- pred reverse_string(string, string).
:- mode reverse_string(in, out) is det.

reverse_string(String0, String) :-
	string__to_char_list(String0, String1),
	string__from_rev_char_list(String1, String).
	
:- pred mangle_mlds_sym_name_for_java(sym_name, string).
:- mode mangle_mlds_sym_name_for_java(in, out) is det.

mangle_mlds_sym_name_for_java(unqualified(Name), MangledName) :-
	llds_out__name_mangle(Name, MangledName).
mangle_mlds_sym_name_for_java(qualified(ModuleName, PlainName), MangledName) :-
	mangle_mlds_sym_name_for_java(ModuleName, MangledModuleName),
	llds_out__name_mangle(PlainName, MangledPlainName),
	java_qualify_mangled_name(MangledModuleName, MangledPlainName, 
		MangledName).

:- pred java_qualify_mangled_name(string, string, string).
:- mode java_qualify_mangled_name(in, in, out) is det.

java_qualify_mangled_name(Module0, Name0, Name) :-
	string__append_list([Module0, ".", Name0], Name).

%----------------------------------------------------------------------------
%
% Code to output imports.
% 

:- pred output_imports(mlds__imports, io__state, io__state).
:- mode output_imports(in, di, uo) is det.

output_imports(Imports) -->
	list__foldl(output_import, Imports),
	%
	% We should always import the mercury.runtime classes.
	%
	io__write_string("import mercury.runtime.*;\n\n").

:- pred output_import(mlds__import, io__state, io__state).
:- mode output_import(in, di, uo) is det.

output_import(Import) -->
	{ SymName = mlds_module_name_to_sym_name(Import) },
	{ prog_out__sym_name_to_string(SymName, ".", File) }, 
	( { qualified_name_is_stdlib(SymName) } ->
		{ enforce_java_names(File, ClassFile) }
	;
		{ ClassFile = File }
	),
	io__write_strings(["import ", ClassFile, ";\n"]).

%--------------------------------------------------------------------
%
% Code to generate the `.java' file.
% 

:- pred output_java_src_file(indent, mlds, io__state, io__state).
:- mode output_java_src_file(in, in, di, uo) is det.

output_java_src_file(Indent, MLDS) -->
	%
	% Run further transformations on the MLDS. 
	%	
	{ MLDS = mlds(ModuleName, _ForeignCode, Imports, Defns0) },
	{ MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName) }, 
	{ Defns1 = Defns0 },
	% XXX The code to transform special predicates isn't working yet.
	%{ transform_special_predicates(ModuleName, Defns0, Defns1) },
	%
	% Output transformed MLDS as Java souce.  
	%
	output_src_start(Indent, ModuleName, Imports, Defns1), 
	{ list__filter(defn_is_rtti_data, Defns1, _RttiDefns, NonRttiDefns) },
	% XXX Need to output RTTI data at this point.
	output_defns(Indent + 1, MLDS_ModuleName, NonRttiDefns),
	output_src_end(Indent, ModuleName).
	% XXX Need to handle non-Java foreign code at this point.
	
%------------------------------------------------------------------------------%
%
% MLDS->MLDS Transformations 
%

	% For each Unify and Compare predicate, create a class that 
	% implements either the Unify or Compare interface respectively.  
	% The `call' function that is implemented in the class then 
	% calls Unify and Compare.
	%
:- pred transform_special_predicates(mercury_module_name, mlds__defns, 
		mlds__defns).
:- mode transform_special_predicates(in, in, out) is det.

transform_special_predicates(ModuleName, Defns0, Defns) :- 
	list__filter(defn_is_unify_or_compare, Defns0, SpecialPredDefns), 
	wrap_predicates(ModuleName, SpecialPredDefns, WrappedDefns),
	list__append(WrappedDefns, Defns0, Defns). 

:- pred wrap_predicates(mercury_module_name, mlds__defns, mlds__defns).
:- mode wrap_predicates(in, in, out) is det.

wrap_predicates(ModuleName, Defns0, Defns) :-
	list__map(wrap_predicate(ModuleName), Defns0, Defns).
	
	% 
	% Given the definition of a function, generate a class
	% definition that implements an interface that contains
	% a call method. The implementation of the `call' method, then calls 
	% the original function.  The results of the original function are
	% then returned by the `call' method.  How the results are returned 
	% depends on which interface is implemented.
	% The interfaces are:
	%	`mercury.runtime.Unify' -- for the `unify' special pred. 
	% 	`mercury.runtime.Compare' -- for the `compare' special pred.
	%	`mercury.runtime.ProcAddr' -- for any predicate.
	%
	% XXX This is not complete.
	%
:- pred wrap_predicate(mercury_module_name, mlds__defn, mlds__defn).
:- mode wrap_predicate(in, in, out) is det.

wrap_predicate(ModuleName, Defn, ClassDefn) :-
	Defn = mlds__defn(Name, _, _, _),
	( 
		Name = function(Label, _, _, _),
   	 	( 
			Label = special_pred(PredName, _, _, _) 
		;
			Label = pred(_, _, PredName, _, _, _)
		)
	->
		
		( PredName = "__Unify__" 
		
		-> 
			InterfaceName = "Unify"
		; 	PredName = "__Compare__" 
		->
			InterfaceName = "Compare"
		;
			InterfaceName = "ProcAddr"
		),
		InterfaceModuleName = mercury_module_name_to_mlds(
			qualified(unqualified("mercury"), "runtime")), 
		Interface = qual(InterfaceModuleName, InterfaceName),
		%
		% Create the new class
		%
		generate_wrapper_class(ModuleName, Interface, Defn, ClassDefn)
	;
		unexpected(this_file,
			"wrap_predicate: definition was not a predicate/function")
	).

:- pred generate_wrapper_class(mercury_module_name, mlds__class,
	mlds__defn, mlds__defn).
:- mode generate_wrapper_class(in, in, in, out) is det.

generate_wrapper_class(ModuleName, Interface, MethodDefn, ClassDefn) :-
	MethodDefn = mlds__defn(Name, Context, _DeclFlags, _DefnBody),
	( 	
		Name = function(Label, _ProcID, _MaybeSeqNum, _PredID),
		Label = special_pred(PredName0, _, Type, Arity)
	->
	   
		%
		% Create class components. 
		%
		ClassImports  = [],
		ClassExtends = [],
		InterfaceDefn = mlds__class_type(Interface, 0, mlds__interface),
		ClassImplements = [InterfaceDefn],
		%
		% Create a method that calls the original predicate.
		%
		generate_wrapper_method(ModuleName, MethodDefn, NewMethodDefn),
		%
		% Put it all together
		%
		string__append(PredName0, Type, PredName),
		ClassMembers  = [NewMethodDefn],
		ClassCtors    = [],
		ClassName     = type(PredName, Arity),
		ClassContext  = Context,
		ClassFlags    = ml_gen_type_decl_flags,
		ClassBodyDefn = mlds__class_defn(mlds__class, ClassImports, 
			ClassExtends, ClassImplements,
			ClassCtors, ClassMembers),
		ClassBody     = mlds__class(ClassBodyDefn)
	;

		error("mlds_to_java: generate_wrapper_class")
	),
	ClassDefn = mlds__defn(ClassName, ClassContext, ClassFlags, ClassBody).
	

:- pred generate_wrapper_method(mercury_module_name, mlds__defn, mlds__defn).
:- mode generate_wrapper_method(in, in, out) is det.

generate_wrapper_method(ModuleName, Defn0, Defn) :-
	Defn0 = mlds__defn(Name0, Context, _Flags0, Body0),
	(
		Name0 = function(_Label0, ProcID, MaybeSeqNum, PredID),
		Body0 = mlds__function(MaybeID, Params0, 
			MaybeStatements0, Attributes),
		MaybeStatements0 = defined_here(Statements0),
		Statements0 = mlds__statement(
			block(BlockDefns0, _BlockList0), _) 
	->
		%
		% Create new method name
		%
		Label = special_pred("call", no, "", 0),
		Name = function(Label, ProcID, MaybeSeqNum, PredID),		
		%
		% Create new argument.
		% There is only one as "call" takes an array of Object.
		%
	        Arg = data(var(var_name("args", no))) - 
				mlds__array_type(mlds__generic_type),
		Args = [Arg],
		%
		% Create new declarations for old arguments and assign
		% the new arguments to them in the initializers.
		%
		Params0 = mlds__func_params(Args0, RetTypes),
		generate_wrapper_decls(ModuleName, Context, Args0, 0, 
			BlockDefns1), 
		list__append(BlockDefns1, BlockDefns0, BlockDefns),
		%
		% Create call to original predicate
		% XXX Not yet implemented. We need to insert a call
		% to the original predicate and then return 
		% what it returns
		%
		Block = block(BlockDefns, []),
		Statements = mlds__statement(Block, Context), 
		%
		% Put it all together.
		%
		Params = mlds__func_params(Args, RetTypes),
		Body   = mlds__function(MaybeID, Params,
			defined_here(Statements), Attributes),
		Flags  = ml_gen_special_member_decl_flags,	
		Defn   = mlds__defn(Name, Context, Flags, Body) 
	;
		error("mlds_to_java.m: cannot create new method")	
	).
	
	%
	% Transform a list of function arguments into a list of local
	% variable declarations of the same name and type.  Create
	% initializers that initialize each new local variable to the
	% correct element in the `args' array.
	%
:- pred generate_wrapper_decls(mercury_module_name, mlds__context, 
	mlds__arguments, int, mlds__defns).
:- mode generate_wrapper_decls(in, in, in, in, out) is det.

generate_wrapper_decls(_, _, [], _, []).
generate_wrapper_decls(ModuleName, Context, [Arg | Args], 
		Count, [Defn | Defns]) :-
	Arg = Name - Type,
	Flags = ml_gen_local_var_decl_flags,
	ArrayIndex = const(int_const(Count)),		
	NewVarName = qual(mercury_module_name_to_mlds(ModuleName), 
		var_name("args", no)),
	NewArgLval = var(NewVarName, mlds__array_type(mlds__generic_type)),
	%	
	% Package everything together. 
	%
	% XXX Don't we need a cast here? -fjh.
	%
	Initializer = binop(array_index(elem_type_generic),
		lval(NewArgLval), ArrayIndex),
	Body = mlds__data(Type, init_obj(Initializer)),	
	Defn = mlds__defn(Name, Context, Flags, Body),
	%	
	% Recursively call ourself to process the next argument.		
	%
	generate_wrapper_decls(ModuleName, Context, Args, Count + 1, Defns).
		
%------------------------------------------------------------------------------
%
% Code to output the start and end of a source file. 
% 

:- pred output_src_start(indent, mercury_module_name, mlds__imports, 
	mlds__defns, io__state, io__state).

:- mode output_src_start(in, in, in, in, di, uo) is det.

output_src_start(Indent, ModuleName, Imports, Defns) -->
	output_auto_gen_comment(ModuleName),
	indent_line(Indent),
	io__write_string("/* :- module "),
	prog_out__write_sym_name(ModuleName),
	io__write_string(". */\n\n"),
	output_package_info(ModuleName),	
	output_imports(Imports),
	io__write_string("public class "),
	prog_out__write_sym_name(ModuleName),
	io__write_string(" {\n"),
	maybe_write_main_driver(Indent + 1, ModuleName, Defns).

	% Output a `package' directive at the top of the Java source file,
	% if necessary.
	%
:- pred output_package_info(sym_name, io__state, io__state).
:- mode output_package_info(in, di, uo) is det.

output_package_info(unqualified(_)) --> [].
output_package_info(qualified(Module, _)) -->
	io__write_string("package "),
	{ prog_out__sym_name_to_string(Module, ".", Package) },
	io__write_string(Package),
	io__write_string(";\n").

	% Check if this module contains a `main' predicate and if it does insert
	% a `main' method in the resulting Java class that calls the 
	% `main' predicate. Save the command line arguments in the class 
	% variable `args' in the class `mercury.runtime.JavaInternal'.
	%
:- pred maybe_write_main_driver(indent, mercury_module_name,  
	mlds__defns, io__state, io__state). 
:- mode maybe_write_main_driver(in, in, in, di, uo) is det.

maybe_write_main_driver(Indent, ModuleName, Defns) -->
	(
		{ defns_contain_main(Defns) }
	->
		indent_line(Indent),
		io__write_string("public static void main"),
		io__write_string("(java.lang.String[] args)\n"),
		indent_line(Indent),
		io__write_string("{\n"), 
		indent_line(Indent + 1),	
		%		
		% Save the command line arguments in the class variable
		% `mercury.runtime.JavaInternal.args'.
		%
		io__write_string("mercury.runtime.JavaInternal.args = args;\n"),
		indent_line(Indent + 1),
		prog_out__write_sym_name(ModuleName),
		io__write_string(".main_2_p_0();\n"),
		indent_line(Indent + 1),
		io__write_string("return;\n"), 
		indent_line(Indent),
		io__write_string("}\n") 
	;
		[]
	),
	io__nl.

:- pred output_src_end(indent, mercury_module_name, io__state, io__state).
:- mode output_src_end(in, in, di, uo) is det.

output_src_end(Indent, ModuleName) -->
	io__write_string("}\n"),
	indent_line(Indent),
	io__write_string("// :- end_module "),
	prog_out__write_sym_name(ModuleName),
	io__write_string(".\n").

	% Output a Java comment saying that the file was automatically
	% generated and give details such as the compiler version.
	%
:- pred output_auto_gen_comment(module_name, io__state, io__state).
:- mode output_auto_gen_comment(in, di, uo) is det.

output_auto_gen_comment(ModuleName) --> 
	{ library__version(Version) },
	module_name_to_file_name(ModuleName, ".m", no, SourceFileName),
	io__write_string("//\n//\n// Automatically generated from "),
	io__write_string(SourceFileName),
	io__write_string(" by the Mercury Compiler,\n"),
	io__write_string("// version "),
	io__write_string(Version),io__nl,
	io__write_string("//\n"),
	io__write_string("//\n"),
	io__nl.
%-----------------------------------------------------------------------------%
%
% Code to output declarations and definitions.
%

:- pred output_defns(indent, mlds_module_name, mlds__defns,
		io__state, io__state).
:- mode output_defns(in, in, in, di, uo) is det.

output_defns(Indent, ModuleName, Defns) -->
	{ OutputDefn = output_defn(Indent, ModuleName) },
	list__foldl(OutputDefn, Defns).

:- pred output_defn(indent, mlds_module_name, mlds__defn,
		io__state, io__state).
:- mode output_defn(in, in, in, di, uo) is det.

output_defn(Indent, ModuleName, Defn) -->
	{ Defn = mlds__defn(Name, Context, Flags, DefnBody) },
	indent_line(Context, Indent),
	output_decl_flags(Flags, Name),
	output_defn_body(Indent, qual(ModuleName, Name), Context, DefnBody).

:- pred output_defn_body(indent, mlds__qualified_entity_name,
		mlds__context, mlds__entity_defn, io__state, io__state).
:- mode output_defn_body(in, in, in, in, di, uo) is det.

output_defn_body(_, Name, _, mlds__data(Type, Initializer)) -->
	output_data_defn(Name, Type, Initializer).
output_defn_body(Indent, Name, Context, 
		mlds__function(MaybePredProcId, Signature, MaybeBody,
			_Attributes)) -->
	output_maybe(MaybePredProcId, output_pred_proc_id),
	output_func(Indent, Name, Context, Signature, MaybeBody).
output_defn_body(Indent, Name, Context, mlds__class(ClassDefn)) -->
	output_class(Indent, Name, Context, ClassDefn).

%-----------------------------------------------------------------------------%
%
% Code to output classes.
%

:- pred output_class(indent, mlds__qualified_entity_name, mlds__context,
		mlds__class_defn, io__state, io__state).
:- mode output_class(in, in, in, in, di, uo) is det.

output_class(Indent, Name, _Context, ClassDefn) -->
	{ Name = qual(ModuleName, UnqualName) },
	( { UnqualName = type(_, _) } ->
		[]	
	;
		{ unexpected(this_file, "output_class") }
	),
	{ ClassDefn = class_defn(Kind, _Imports, BaseClasses, Implements,
		Ctors, AllMembers) },
	{ Ctors = [] ->
		true
	;
		sorry(this_file, "constructors")
	},
	( { Kind = mlds__interface } -> 
		io__write_string("interface ")
	;
		io__write_string("class ")
	),
	output_class_name(UnqualName),
	output_extends_list(BaseClasses),
	output_implements_list(Implements),
	io__write_string(" {\n"),
	output_class_body(Indent + 1, Kind, Name, AllMembers, ModuleName),
	indent_line(Indent),
	io__write_string("}\n\n").

	% Output superclass that this class extends.  Java does
	% not support multiple inheritance, so more than one superclass
	% is an error.
	%
:- pred output_extends_list(list(mlds__class_id), io__state, io__state).
:- mode output_extends_list(in, di, uo) is det.

output_extends_list([]) --> [].
output_extends_list([SuperClass]) -->
	io__write_string(" extends "),
	output_type(SuperClass).
output_extends_list([_, _ | _]) -->
	{ unexpected(this_file, 
		"output_extends_list: multiple inheritance not supported in Java") }.

	% Output list of interfaces that this class implements.
	%
:- pred output_implements_list(list(mlds__interface_id), 
		io__state, io__state).
:- mode output_implements_list(in, di, uo) is det.

output_implements_list(InterfaceList) --> 
	( { InterfaceList = [] }  ->
		[]
	;
		io__write_string(" implements "),
		io__write_list(InterfaceList, ",", output_interface) 
	).

:- pred output_interface(mlds__interface_id, io__state, io__state).
:- mode output_interface(in, di, uo) is det.

output_interface(Interface) -->
	 ( { Interface = class_type(qual(ModuleQualifier, Name), Arity, _) } ->
		{ SymName = mlds_module_name_to_sym_name(ModuleQualifier) },	
		{ mangle_mlds_sym_name_for_java(SymName, ModuleName) },
		io__format("%s.%s", [s(ModuleName), s(Name)]),
		%
		% Check if the interface is one of the ones in the runtime
		% system.  If it is we don't need to output the arity.
		%
		( { interface_is_special(Name) } ->
			[]
		;
			io__format("%d", [i(Arity)])
		)
	;
		{ unexpected(this_file, 
			"output_interface: interface was not a class.") }
	).
	

:- pred output_class_body(indent, mlds__class_kind, 
	mlds__qualified_entity_name, mlds__defns, mlds_module_name, io__state, 
	io__state).
:- mode output_class_body(in, in, in, in, in, di, uo) is det.

output_class_body(Indent, mlds__class, _Name, AllMembers, Module) -->
	output_defns(Indent, Module, AllMembers).	

output_class_body(_Indent, mlds__package, _Name, _AllMembers, _) -->
	{ error("mlds_to_java.m: cannot use package as a type.") }.

output_class_body(Indent, mlds__interface, _, AllMembers, Module) --> 
	output_defns(Indent, Module, AllMembers). 

output_class_body(_Indent, mlds__struct, _, _AllMembers, _) -->	
	{ unexpected(this_file, 
		"output_class_body: structs not supported in Java.") }.

output_class_body(Indent, mlds__enum, Name, AllMembers, _) -->
	{ list__filter(defn_is_const, AllMembers, EnumConsts) },
	{ Name = qual(ModuleName, UnqualName) },
	output_enum_constants(Indent + 1, ModuleName, EnumConsts),
	indent_line(Indent + 1),
	io__write_string("public int value;\n\n"),  
	output_enum_ctor(Indent + 1, UnqualName).

%-----------------------------------------------------------------------------%
%
% Additional code for generating enumerations
%
% Enumerations are a bit different from normal classes because although the 
% ml code generator generates them as classes, it treats them as integers.  
% Here we treat them as objects (instantiations of the classes) rather than 
% just as integers.  
%

:- pred defn_is_const(mlds__defn).
:- mode defn_is_const(in) is semidet.

defn_is_const(Defn) :-
	Defn = mlds__defn(_Name, _Context, Flags, _DefnBody),
	constness(Flags) = const.
	
	% Output a (Java) constructor for the class representing
	% the enumeration. 
	%
:- pred output_enum_ctor(indent, mlds__entity_name, io__state, io__state).
:- mode output_enum_ctor(in, in, di, uo) is det.

output_enum_ctor(Indent, UnqualName) -->
	indent_line(Indent),
	io__write_string("public "),
	output_name(UnqualName),
	io__write_string("(int val) {\n"),
	indent_line(Indent + 1),
	%	
	% The use of `value' is hardcoded into ml_type_gen.m.  Any
	% changes there should probably be reflected here.
	%
	io__write_string("this.value = val;\n"),
	indent_line(Indent + 1),
	io__write_string("return;\n"),
	indent_line(Indent),
	io__write_string("}\n").
		
:- pred output_enum_constants(indent, mlds_module_name,
		mlds__defns, io__state, io__state).
:- mode output_enum_constants(in, in, in, di, uo) is det.

output_enum_constants(Indent, EnumModuleName, EnumConsts) -->
	io__write_list(EnumConsts, "\n",
		output_enum_constant(Indent, EnumModuleName)),
	io__nl.

:- pred output_enum_constant(indent, mlds_module_name, mlds__defn,
		io__state, io__state).
:- mode output_enum_constant(in, in, in, di, uo) is det.

output_enum_constant(Indent, EnumModuleName, Defn) -->
	{ Defn = mlds__defn(Name, _Context, _Flags, DefnBody) },
	(
		{ DefnBody = data(Type, Initializer) }
	->
		indent_line(Indent),
		io__write_string("public static final int "),
		output_fully_qualified_name(qual(EnumModuleName, Name)),
		output_initializer(Type, Initializer),
		io__write_char(';')
	;
		{ unexpected(this_file, 
			"output_enum_constant: definition body was not data.") }
	).

%-----------------------------------------------------------------------------%
%
% Code to output data declarations/definitions
%

:- pred output_data_decl(mlds__qualified_entity_name, mlds__type,
			io__state, io__state).
:- mode output_data_decl(in, in, di, uo) is det.

output_data_decl(Name, Type) -->
	output_type(Type),
	io__write_char(' '),
	output_fully_qualified_name(Name).

:- pred output_data_defn(mlds__qualified_entity_name, mlds__type,
			mlds__initializer, io__state, io__state).
:- mode output_data_defn(in, in, in, di, uo) is det.

output_data_defn(Name, Type, Initializer) -->
	output_data_decl(Name, Type),
	output_initializer(Type, Initializer),
	io__write_string(";\n").

	% We need to provide initializers for local variables
	% to avoid problems with Java's rules for definite assignment.  
	% This mirrors the default Java initializers for class and 
	% instance variables.
	%
:- func get_java_type_initializer(mlds__type) = string.
:- mode get_java_type_initializer(in) = out is det.

get_java_type_initializer(mercury_type(_, int_type)) = "0".
get_java_type_initializer(mercury_type(_, char_type)) = "0".
get_java_type_initializer(mercury_type(_, float_type)) = "0".
get_java_type_initializer(mercury_type(_, str_type)) = "null".
get_java_type_initializer(mercury_type(_, pred_type)) = "null".
get_java_type_initializer(mercury_type(_, tuple_type)) = "null".
get_java_type_initializer(mercury_type(_, enum_type)) = "null".
get_java_type_initializer(mercury_type(_, polymorphic_type)) = "null".
get_java_type_initializer(mercury_type(_, user_type)) = "null".
get_java_type_initializer(mlds__mercury_array_type(_)) = "null".
get_java_type_initializer(mlds__cont_type(_)) = "null".
get_java_type_initializer(mlds__commit_type) = "null".
get_java_type_initializer(mlds__native_bool_type) = "false".
get_java_type_initializer(mlds__native_int_type) = "0".
get_java_type_initializer(mlds__native_float_type) = "0".
get_java_type_initializer(mlds__native_char_type) = "0".
get_java_type_initializer(mlds__class_type(_, _, _)) = "null".
get_java_type_initializer(mlds__array_type(_)) = "null".
get_java_type_initializer(mlds__ptr_type(_)) = "null".
get_java_type_initializer(mlds__func_type(_)) = "null".
get_java_type_initializer(mlds__generic_type) = "null".
get_java_type_initializer(mlds__generic_env_ptr_type) = "null".
get_java_type_initializer(mlds__pseudo_type_info_type) = "null".
get_java_type_initializer(mlds__rtti_type(_)) = "null".
get_java_type_initializer(mlds__unknown_type) = _ :-
	unexpected(this_file, 
		"get_type_initializer: variable has unknown_type"). 

:- pred output_maybe(maybe(T), pred(T, io__state, io__state),
		io__state, io__state).
:- mode output_maybe(in, pred(in, di, uo) is det, di, uo) is det.

output_maybe(MaybeValue, OutputAction) -->
	( { MaybeValue = yes(Value) } ->
		OutputAction(Value)
	;
		[]
	).

:- pred output_initializer(mlds__type, mlds__initializer,
		io__state, io__state).
:- mode output_initializer(in, in, di, uo) is det.

output_initializer(Type, Initializer) -->
	io__write_string(" = "),
	( { needs_initialization(Initializer) = yes } ->
		( { Initializer = init_obj(Rval) } ->
			( 
				{ type_is_object(Type) },
		   	  	{ rval_is_int_const(Rval) } 
			->
				%
				% If it is a enumeration object
				% create new object.
				%
				io__write_string("new "),
				output_type(Type),
				io__write_char('('),
				output_initializer_body(Initializer),
				io__write_char(')')
			;
				% If it is an non-enumeration
				% object, insert appropriate
				% cast.
				% XXX The logic of this is a bit
				% wrong.  Fixing it would eliminate
				% some of the unecessary casting
				% that happens
				%
				io__write_string("("),
				output_type(Type),
				io__write_string(") "),
				output_initializer_body(Initializer)
			)
		;
			output_initializer_body(Initializer)
		)
	;
		%
		% If we are not provided with an initializer we just, 
		% supply the default java values -- note: this is strictly
		% only necessary for local variables, but it's not going
		% to hurt anything else.  
		%
		io__write_string(get_java_type_initializer(Type))
	).

:- func needs_initialization(mlds__initializer) = bool.
:- mode needs_initialization(in) = out is det.

needs_initialization(no_initializer) = no.
needs_initialization(init_obj(_)) = yes.
needs_initialization(init_struct([])) = no.
needs_initialization(init_struct([_|_])) = yes.
needs_initialization(init_array(_)) = yes.

:- pred output_initializer_body(mlds__initializer, io__state, io__state).
:- mode output_initializer_body(in,di, uo) is det.

output_initializer_body(no_initializer) --> [].
output_initializer_body(init_obj(Rval)) -->
	output_rval_maybe_with_enum(Rval).
output_initializer_body(init_struct(FieldInits)) --> 
	io__write_list(FieldInits, ",\n\t\t", output_initializer_body).
output_initializer_body(init_array(ElementInits)) -->
	io__write_string("{\n\t\t"),
	io__write_list(ElementInits, ",\n\t\t", output_initializer_body),
	io__write_string("}").

%-----------------------------------------------------------------------------%
%
% Code to output function declarations/definitions
%

:- pred output_pred_proc_id(pred_proc_id, io__state, io__state).
:- mode output_pred_proc_id(in, di, uo) is det.

output_pred_proc_id(proc(PredId, ProcId)) -->
	globals__io_lookup_bool_option(auto_comments, AddComments),
	( { AddComments = yes } ->
		io__write_string("// pred_id: "),
		{ pred_id_to_int(PredId, PredIdNum) },
		io__write_int(PredIdNum),
		io__write_string(", proc_id: "),
		{ proc_id_to_int(ProcId, ProcIdNum) },
		io__write_int(ProcIdNum),
		io__nl	
	;
		[]
	).

:- pred output_func(indent, qualified_entity_name, mlds__context,
		func_params, function_body, io__state, io__state).
:- mode output_func(in, in, in, in, in, di, uo) is det.

output_func(Indent, Name, Context, Signature, MaybeBody) -->
	output_func_decl(Indent, Name, Context, Signature),
	(
		{ MaybeBody = external },
		io__write_string(";\n")
	;
		{ MaybeBody = defined_here(Body) },
		io__write_string("\n"),
		indent_line(Context, Indent),
		io__write_string("{\n"),
		{ FuncInfo = func_info(Name, Signature) },
		output_statement(Indent + 1, FuncInfo, Body),
		indent_line(Context, Indent),
		io__write_string("}\n")	% end the function
	).

:- pred output_func_decl(indent, qualified_entity_name, mlds__context,
		func_params, io__state, io__state).
:- mode output_func_decl(in, in, in, in, di, uo) is det.

output_func_decl(Indent, QualifiedName, Context, Signature) -->
	{ Signature = mlds__func_params(Parameters, RetTypes) },
	( { RetTypes = [] } ->
		io__write_string("void")
	; { RetTypes = [RetType] } ->
		output_type(RetType)
	;
		% for multiple outputs, we return an array of objects.
		io__write_string("java.lang.Object []")
	),
	io__write_char(' '),
	{ QualifiedName = qual(ModuleName, Name) },
	output_name(Name),	
	output_params(Indent, ModuleName, Context, Parameters).

:- pred output_params(indent, mlds_module_name, mlds__context,
		mlds__arguments, io__state, io__state).
:- mode output_params(in, in, in, in, di, uo) is det.

output_params(Indent, ModuleName, Context, Parameters) -->
	io__write_char('('),
	( { Parameters = [] } ->
		[]
	;
		io__nl,
		io__write_list(Parameters, ",\n",
			output_param(Indent + 1, ModuleName, Context))
	),
	io__write_char(')').

:- pred output_param(indent, mlds_module_name, mlds__context,
		pair(mlds__entity_name, mlds__type), io__state, io__state).
:- mode output_param(in, in, in, in, di, uo) is det.

output_param(Indent, ModuleName, Context, Name - Type) -->
	indent_line(Context, Indent),
	output_type(Type),
	io__write_char(' '),
	output_fully_qualified_name(qual(ModuleName, Name)).

%-----------------------------------------------------------------------------%
%
% Code to output names of various entities
% XXX Much of the code in this section will not work when we 
%     start enforcing names properly.

:- pred output_fully_qualified_name(mlds__qualified_entity_name,
		io__state, io__state).
:- mode output_fully_qualified_name(in, di, uo) is det.

output_fully_qualified_name(QualifiedName) -->
	{ QualifiedName = qual(_ModuleName, Name) },
	%
	% Don't module qualify data names, otherwise all 
	% variable declarations will be qualified with the
	% module name.
	%
	(
		{ Name = data(_) }
	->
		output_name(Name)
	;
		output_fully_qualified(QualifiedName, output_name)
	).

:- pred output_fully_qualified_proc_label(mlds__qualified_proc_label,
		io__state, io__state).
:- mode output_fully_qualified_proc_label(in, di, uo) is det.

output_fully_qualified_proc_label(QualifiedName) -->
		output_fully_qualified(QualifiedName, mlds_output_proc_label).

:- pred output_fully_qualified(mlds__fully_qualified_name(T),
		pred(T, io__state, io__state), io__state, io__state).
:- mode output_fully_qualified(in, pred(in, di, uo) is det, di, uo) is det.

output_fully_qualified(qual(ModuleName, Name), OutputFunc) -->
	{ SymName = mlds_module_name_to_sym_name(ModuleName) },
	{ mangle_mlds_sym_name_for_java(SymName, MangledModuleName) },
	( { qualified_name_is_stdlib(SymName) } ->
		{ enforce_java_names(MangledModuleName, JavaMangledName) }
	;
		{ MangledModuleName = JavaMangledName }
	),
	io__write_string(JavaMangledName),
	io__write_string("."),
	OutputFunc(Name).

:- pred output_module_name(mercury_module_name, io__state, io__state).
:- mode output_module_name(in, di, uo) is det.

output_module_name(ModuleName) -->
	{ llds_out__sym_name_mangle(ModuleName, MangledModuleName) },
	io__write_string(MangledModuleName).

:- pred output_class_name(mlds__entity_name, io__state, io__state).
:- mode output_class_name(in, di, uo) is det.

output_class_name(type(Name, Arity)) -->
	{ llds_out__name_mangle(Name, MangledName) },
	io__format("%s_%d", [s(MangledName), i(Arity)]).

output_class_name(data(_)) --> []. 
output_class_name(function(_, _, _, _)) --> [].
output_class_name(export(_)) --> [].

:- pred output_name(mlds__entity_name, io__state, io__state).
:- mode output_name(in, di, uo) is det.

output_name(type(Name, Arity)) -->
	{ llds_out__name_mangle(Name, MangledName) },
	io__format("%s_%d", [s(MangledName), i(Arity)]).
output_name(data(DataName)) -->
	output_data_name(DataName).
output_name(function(PredLabel, ProcId, MaybeSeqNum, _PredId)) -->
	output_pred_label(PredLabel),
	{ proc_id_to_int(ProcId, ModeNum) },
	io__format("_%d", [i(ModeNum)]),
	( { MaybeSeqNum = yes(SeqNum) } ->
		io__format("_%d", [i(SeqNum)])
	;
		[]
	).
output_name(export(Name)) -->
	io__write_string(Name).

:- pred output_pred_label(mlds__pred_label, io__state, io__state).
:- mode output_pred_label(in, di, uo) is det.

output_pred_label(pred(PredOrFunc, MaybeDefiningModule, Name, Arity,
		_CodeModel, _NonOutputFunc)) -->
	( { PredOrFunc = predicate, Suffix = "p" }
	; { PredOrFunc = function, Suffix = "f" }
	),
	{ llds_out__name_mangle(Name, MangledName) },
	io__format("%s_%d_%s", [s(MangledName), i(Arity), s(Suffix)]),
	( { MaybeDefiningModule = yes(DefiningModule) } ->
		io__write_string("_in__"),
		output_module_name(DefiningModule)
	;
		[]
	).

output_pred_label(special_pred(PredName, MaybeTypeModule,
		TypeName, TypeArity)) -->
	{ llds_out__name_mangle(PredName, MangledPredName) },
	{ llds_out__name_mangle(TypeName, MangledTypeName) },
	io__write_string(MangledPredName),
	io__write_string("__"),
	( { MaybeTypeModule = yes(TypeModule) } ->
		output_module_name(TypeModule),
		io__write_string("__")
	;
		[]
	),
	io__format("%s_%d", [s(MangledTypeName), i(TypeArity)]).

:- pred output_data_name(mlds__data_name, io__state, io__state).
:- mode output_data_name(in, di, uo) is det.

output_data_name(var(VarName)) -->
	output_mlds_var_name(VarName).

output_data_name(common(Num)) -->
	io__write_string("common_"),
	io__write_int(Num).

:- pred output_mlds_var_name(mlds__var_name, io__state, io__state).
:- mode output_mlds_var_name(in, di, uo) is det.
output_mlds_var_name(var_name(Name, no)) -->
	output_mangled_name(Name).
output_mlds_var_name(var_name(Name, yes(Num))) -->
	output_mangled_name(string__format("%s_%d", [s(Name), i(Num)])).

%==============================================================================%
% XXX Most of this code doesn't yet work/hasn't been implemented in the Java
% backend.
%
output_data_name(rtti(RttiTypeId, RttiName)) -->
	{ rtti__addr_to_string(RttiTypeId, RttiName, RttiAddrName) },
	io__write_string(RttiAddrName).
output_data_name(base_typeclass_info(ClassId, InstanceStr)) -->
        { llds_out__make_base_typeclass_info_name(ClassId, InstanceStr,
		Name) },
	io__write_string(Name).
output_data_name(module_layout) -->
	{ error("mlds_to_java.m: NYI: module_layout") }.
output_data_name(proc_layout(_ProcLabel)) -->
	{ error("mlds_to_java.m: NYI: proc_layout") }.
output_data_name(internal_layout(_ProcLabel, _FuncSeqNum)) -->
	{ error("mlds_to_java.m: NYI: internal_layout") }.
output_data_name(tabling_pointer(ProcLabel)) -->
	io__write_string("table_for_"),
	mlds_output_proc_label(ProcLabel).
%=============================================================================%
%-----------------------------------------------------------------------------%
%
% Code to output types
%

:- pred output_type(mlds__type, io__state, io__state).
:- mode output_type(in, di, uo) is det.

output_type(mercury_type(Type, TypeCategory)) -->
	output_mercury_type(Type, TypeCategory).

output_type(mercury_array_type(MLDSType)) -->
	output_type(MLDSType),
	io__write_string("[]").
output_type(mlds__native_int_type)   --> io__write_string("int").
output_type(mlds__native_float_type) --> io__write_string("double").
output_type(mlds__native_bool_type) --> io__write_string("boolean").
output_type(mlds__native_char_type)  --> io__write_string("char").
output_type(mlds__class_type(Name, Arity, ClassKind)) -->
	( { ClassKind = mlds__enum } ->
		output_fully_qualified(Name, output_mangled_name),
		io__format("_%d", [i(Arity)])
	;
		output_fully_qualified(Name, output_mangled_name),
		io__format("_%d", [i(Arity)])
	).
output_type(mlds__ptr_type(Type)) -->
	( { Type = mlds__class_type(Name, Arity, _Kind) } ->
		output_fully_qualified(Name, output_mangled_name),
		io__format("_%d", [i(Arity)])
	;
		output_type(Type)
	).
output_type(mlds__array_type(Type)) -->
	output_type(Type),
	io__write_string("[]").
output_type(mlds__func_type(_FuncParams)) -->
	% XXX Not yet implemented.
	{ unexpected(this_file, "output_type: cannot handle function types") }.
output_type(mlds__generic_type) -->
	io__write_string("java.lang.Object").	
output_type(mlds__generic_env_ptr_type) -->
	io__write_string("java.lang.Object").
output_type(mlds__pseudo_type_info_type) -->
	io__write_string("mercury.runtime.PseudoTypeInfo").
output_type(mlds__cont_type(_)) -->
	% XXX Not yet implemented.
	{ unexpected(this_file, 
		"output_type: nondet code not yet implemented") }.
output_type(mlds__commit_type) -->
	% XXX Not yet implemented.
	{ unexpected(this_file, "output_type: commits not yet implemented") }.
%
% XXX The RTTI data should actually be static but it isn't being
% generated as such.
%
output_type(mlds__rtti_type(RttiName)) -->
	io__write_string("static mercury.runtime."),
	io__write_string(mlds_rtti_type_name(RttiName)).
output_type(mlds__unknown_type) -->
	{ unexpected(this_file, "output_type: unknown type") }.

:- pred output_mercury_type(mercury_type, builtin_type,
		io__state, io__state).
:- mode output_mercury_type(in, in, di, uo) is det.

output_mercury_type(Type, TypeCategory) -->
	(
		{ TypeCategory = char_type }, 
		 io__write_string("char")
	;
		{ TypeCategory = int_type }, 
		 io__write_string("int")
	;
		{ TypeCategory = str_type }, 
		io__write_string("java.lang.String")
	;
		{ TypeCategory = float_type }, 
		io__write_string("double")
	;
		{ TypeCategory = polymorphic_type }, 
		io__write_string("java.lang.Object")
	;
		{ TypeCategory = tuple_type }, 
		io__write_string("java.lang.Object")
	;
		% XXX Not yet implemented.
		{ TypeCategory = pred_type },
		io__write_string("MR_ClosurePtr")
	;
		{ TypeCategory = enum_type },
		output_mercury_user_type(Type, TypeCategory)
	;
		{ TypeCategory = user_type },
		output_mercury_user_type(Type, TypeCategory)
	).

:- pred output_mercury_user_type(mercury_type, builtin_type, 
		io__state, io__state).
:- mode output_mercury_user_type(in, in, di, uo) is det.

output_mercury_user_type(Type, TypeCategory) -->
	( { type_to_type_id(Type, TypeId, _ArgsTypes) } ->
		{ ml_gen_type_name(TypeId, ClassName, ClassArity) },
		( { TypeCategory = enum_type } ->
			{ MLDS_Type = mlds__class_type(ClassName,
				ClassArity, mlds__enum) }
		;
			{ MLDS_Type = mlds__class_type(
				ClassName, ClassArity, mlds__class) }
		),
		output_type(MLDS_Type)
	;
		{ unexpected(this_file, 
			"output_mercury_user_type: not a user type") }
	).

%-----------------------------------------------------------------------------%
%
% Code to output declaration specifiers
%

:- pred output_decl_flags(mlds__decl_flags, 
		mlds__entity_name, io__state, io__state).
:- mode output_decl_flags(in, in, di, uo) is det.

output_decl_flags(Flags, _Name) -->
	output_access(access(Flags)),
	output_per_instance(per_instance(Flags)),
	output_virtuality(virtuality(Flags)),
	output_finality(finality(Flags)),
	output_constness(constness(Flags)),
	output_abstractness(abstractness(Flags)).

:- pred output_access(access, io__state, io__state).
:- mode output_access(in, di, uo) is det.

output_access(public)    --> io__write_string("public ").
output_access(private)   --> io__write_string("private ").
output_access(protected) --> io__write_string("protected ").
output_access(default)   --> maybe_output_comment("default").
output_access(local)     --> [].

:- pred output_per_instance(per_instance, io__state, io__state).
:- mode output_per_instance(in, di, uo) is det.

output_per_instance(per_instance) --> [].
output_per_instance(one_copy)     --> io__write_string("static ").

:- pred output_virtuality(virtuality, io__state, io__state).
:- mode output_virtuality(in, di, uo) is det.

output_virtuality(virtual)     --> maybe_output_comment("virtual").
output_virtuality(non_virtual) --> [].

:- pred output_finality(finality, io__state, io__state).
:- mode output_finality(in, di, uo) is det.

output_finality(final)       --> io__write_string("final "). 
output_finality(overridable) --> [].

:- pred output_constness(constness, io__state, io__state).
:- mode output_constness(in, di, uo) is det.

output_constness(const)      --> maybe_output_comment("const").
output_constness(modifiable) --> [].

:- pred output_abstractness(abstractness, io__state, io__state).
:- mode output_abstractness(in, di, uo) is det.

output_abstractness(abstract) --> io__write_string("abstract "). 
output_abstractness(concrete) --> [].

:- pred maybe_output_comment(string, io__state, io__state).
:- mode maybe_output_comment(in, di, uo) is det.

maybe_output_comment(Comment) -->
	globals__io_lookup_bool_option(auto_comments, AddComments),
	( { AddComments = yes } ->
		io__write_string("/* "),
		io__write_string(Comment),
		io__write_string(" */")
	;
		[]
	).
%-----------------------------------------------------------------------------%
%
% Code to output statements
%

:- type func_info
	--->	func_info(mlds__qualified_entity_name, mlds__func_params).

:- pred output_statements(indent, func_info, list(mlds__statement),
		io__state, io__state).
:- mode output_statements(in, in, in, di, uo) is det.

output_statements(Indent, FuncInfo, Statements) -->
	list__foldl(output_statement(Indent, FuncInfo), Statements).

:- pred output_statement(indent, func_info, mlds__statement,
		io__state, io__state).
:- mode output_statement(in, in, in, di, uo) is det.

output_statement(Indent, FuncInfo, mlds__statement(Statement, Context)) -->
	output_context(Context),
	output_stmt(Indent, FuncInfo, Statement, Context).

:- pred output_stmt(indent, func_info, mlds__stmt, mlds__context, 
		io__state, io__state).
:- mode output_stmt(in, in, in, in, di, uo) is det.

	%
	% sequence
	%
output_stmt(Indent, FuncInfo, block(Defns, Statements), Context) -->
	indent_line(Indent),
	io__write_string("{\n"),
	( { Defns \= [] } ->
		{ FuncInfo = func_info(FuncName, _) },
		{ FuncName = qual(ModuleName, _) },
		output_defns(Indent + 1, ModuleName, Defns),
		io__write_string("\n")
	;
		[]
	),
	output_statements(Indent + 1, FuncInfo, Statements),
	indent_line(Context, Indent),
	io__write_string("}\n").

	%
	% iteration
	%
output_stmt(Indent, FuncInfo, while(Cond, Statement, no), _) -->
	indent_line(Indent),
	io__write_string("while ("),
	output_rval(Cond),
	io__write_string(")\n"),
	output_statement(Indent + 1, FuncInfo, Statement).
output_stmt(Indent, FuncInfo, while(Cond, Statement, yes), Context) -->
	indent_line(Indent),
	io__write_string("do\n"),
	output_statement(Indent + 1, FuncInfo, Statement),
	indent_line(Context, Indent),
	io__write_string("while ("),
	output_rval(Cond),
	io__write_string(");\n").

	%
	% selection (if-then-else)
	%
output_stmt(Indent, FuncInfo, if_then_else(Cond, Then0, MaybeElse), Context) -->
	%
	% we need to take care to avoid problems caused by the
	% dangling else ambiguity
	%
	{
		%
		% For examples of the form
		%
		%	if (...)
		%		if (...)
		%			...
		%	else
		%		...
		%
		% we need braces around the inner `if', otherwise
		% they wouldn't parse they way we want them to:
		% Java would match the `else' with the inner `if'
		% rather than the outer `if'.
		%
		MaybeElse = yes(_),
		Then0 = statement(if_then_else(_, _, no), ThenContext)
	->
		Then = statement(block([], [Then0]), ThenContext)
	;
		Then = Then0
	},

	indent_line(Indent),
	io__write_string("if ("),
	output_rval(Cond),
	io__write_string(")\n"),
	output_statement(Indent + 1, FuncInfo, Then),
	( { MaybeElse = yes(Else) } ->
		indent_line(Context, Indent),
		io__write_string("else\n"),
		output_statement(Indent + 1, FuncInfo, Else)
	;
		[]
	).


	%
	% selection (switch)
	%
output_stmt(Indent, FuncInfo, switch(_Type, Val, _Range, Cases, Default),
		Context) -->
	indent_line(Context, Indent),
	io__write_string("switch ("),
	output_rval_maybe_with_enum(Val),
	io__write_string(") {\n"),
	list__foldl(output_switch_case(Indent + 1, FuncInfo, Context), Cases),
	output_switch_default(Indent + 1, FuncInfo, Context, Default),
	indent_line(Context, Indent),
	io__write_string("}\n").
	
	%
	% transfer of control
	% 
output_stmt(_Indent, _FuncInfo, label(_LabelName), _Context) --> 
	{ unexpected(this_file, 
		"output_stmt: labels not supported in Java.") }.
output_stmt(_Indent, _FuncInfo, goto(_LabelName), _Context) --> 
	{ unexpected(this_file,
		"output_stmt: gotos not supported in Java.") }.
output_stmt(_Indent, _FuncInfo, computed_goto(_Expr, _Labels), _Context) --> 
	{ unexpected(this_file, 
		"output_stmt: computed gotos not supported in Java.") }.
	
	%
	% function call/return
	%
output_stmt(Indent, CallerFuncInfo, Call, Context) -->
	{ Call = call(Signature, FuncRval, MaybeObject, CallArgs,
		Results, IsTailCall) },
	{ CallerFuncInfo = func_info(_Name, _Params) },
	indent_line(Indent),
	io__write_string("{\n"),
	indent_line(Context, Indent + 1),
	( { Results = [] } ->
		[]
	; { Results = [Lval] } ->
		output_lval(Lval),
		io__write_string(" = ")
	;
		% for multiple return values,
		% we generate the following code:
		%	{ java.lang.Object [] result = <func>(<args>);
		%	  <output1> = (<type1>) result[0];
		%	  <output2> = (<type2>) result[1];
		%	  ...
		%	}
		%
		io__write_string("java.lang.Object [] result = ")
	),
	( { MaybeObject = yes(Object) } ->
		output_bracketed_rval(Object),
		io__write_string(".")
	;
		[]
	),
	output_bracketed_rval(FuncRval),
	io__write_string("("),
	io__write_list(CallArgs, ", ", output_rval),
	io__write_string(");\n"),

	( { Results = [_, _ | _] } ->
		% Copy the results from the "result" array into the Result
		% lvals (unboxing them as we go).
		{ Signature = mlds__func_signature(_Arguments, RetTypes) },
		output_assign_results(Results, RetTypes, 0, Indent + 1, Context)
	;
		[]
	),
	( { IsTailCall = tail_call, Results = [] } ->
		indent_line(Context, Indent + 1),
		io__write_string("return;\n")
	;
		[]
	),
	indent_line(Indent),
	io__write_string("}\n").

output_stmt(Indent, FuncInfo, return(Results), _Context) -->
	indent_line(Indent),
	io__write_string("return"),
	( { Results = [] } ->
		[]
	; { Results = [Rval] } ->
		io__write_char(' '),
		% 
		% Don't output `dummy_var'.
		%
		( 
	   		{ Rval = mlds__lval(Lval) },
	   		{ Lval = var(VarName, _) },
	   		{ VarName = qual(_, UnqualName) },
	   		{ UnqualName = var_name("dummy_var", no) } 
		->
			[]
		;
			output_rval(Rval)
		)
	;
		{ FuncInfo = func_info(_Name, Params) },
		{ Params = mlds__func_params(_Args, ReturnTypes) },
		{ TypesAndResults = assoc_list__from_corresponding_lists(
			ReturnTypes, Results) },
		io__write_string(" new java.lang.Object[] { "),
		io__write_list(TypesAndResults, ",\n ",
			(pred((Type - Result)::in, di, uo) is det -->
				output_boxed_rval(Type, Result))),
		io__write_string("}")
	),
	io__write_string(";\n").
	
	%
	% commits
	% XXX These are yet to be implemented.
output_stmt(_Indent, _FuncInfo, do_commit(_Ref), _) -->
	{ sorry(this_file, "output_stmt: commits not yet implemented") }.
output_stmt(_Indent, _FuncInfo, try_commit(_Ref, _Stmt0, _Handler), _) -->
	{ sorry(this_file, "output_stmt: commits not implemented") }.
%-----------------------------------------------------------------------------%
%
% When returning multiple values,
% we generate the following code:
%	{ java.lang.Object [] result = <func>(<args>);
%	  <output1> = (<type1>) result[0];
%	  <output2> = (<type2>) result[1];
%	  ...
%	}
%

% This procedure generates the assignments to the outputs.
%
:- pred output_assign_results(list(mlds__lval), list(mlds__type), int,
		indent, mlds__context, io__state, io__state).
:- mode output_assign_results(in, in, in, in, in, di, uo) is det.

output_assign_results([], [], _, _, _) --> [].
output_assign_results([Lval|Lvals], [Type|Types], ResultIndex, 
		Indent, Context) -->
	indent_line(Context, Indent),
	output_lval(Lval),
	io__write_string(" = "),
	output_unboxed_result(Type, ResultIndex),
	io__write_string(";\n"),
	output_assign_results(Lvals, Types, ResultIndex + 1, Indent, Context).
output_assign_results([_|_], [], _, _, _) -->
	{ error("output_assign_results: list length mismatch") }.
output_assign_results([], [_|_], _, _, _) -->
	{ error("output_assign_results: list length mismatch") }.
				
:- pred output_unboxed_result(mlds__type, int, io__state, io__state).
:- mode output_unboxed_result(in, in, di, uo) is det.

output_unboxed_result(Type, ResultIndex) -->
	(
		{ java_builtin_type(Type, _JavaName, JavaBoxedName,
			UnboxMethod) }
	->
		io__write_string("(("),
		io__write_string(JavaBoxedName),
		io__write_string(") "),
		io__format("result[%d]).%s()", [i(ResultIndex), s(UnboxMethod)])
	;
		io__write_string("("),
		output_type(Type),
		io__write_string(") "),
		io__format("result[%d]", [i(ResultIndex)])
	).

%-----------------------------------------------------------------------------%
%
% Extra code for outputting switch statements
%

:- pred output_switch_case(indent, func_info, mlds__context,
		mlds__switch_case, io__state, io__state).
:- mode output_switch_case(in, in, in, in, di, uo) is det.

output_switch_case(Indent, FuncInfo, Context, Case) -->
	{ Case = (Conds - Statement) },
	list__foldl(output_case_cond(Indent, Context), Conds),
	output_statement(Indent + 1, FuncInfo, Statement),
	indent_line(Context, Indent + 1),
	io__write_string("break;\n").

:- pred output_case_cond(indent, mlds__context, mlds__case_match_cond, 
		io__state, io__state).
:- mode output_case_cond(in, in, in, di, uo) is det.

output_case_cond(Indent, Context, match_value(Val)) -->
	indent_line(Context, Indent),
	io__write_string("case "),
	output_rval(Val),
	io__write_string(":\n").
output_case_cond(_Indent, _Context, match_range(_, _)) -->
	{ unexpected(this_file, 
		"output_case_cond: cannot match ranges in Java cases") }.

:- pred output_switch_default(indent, func_info, mlds__context,
		mlds__switch_default, io__state, io__state).
:- mode output_switch_default(in, in, in, in, di, uo) is det.

output_switch_default(_Indent, _FuncInfo, _Context, default_do_nothing) --> [].
output_switch_default(Indent, FuncInfo, Context, default_case(Statement)) -->
	indent_line(Context, Indent),
	io__write_string("default:\n"),
	output_statement(Indent + 1, FuncInfo, Statement).
output_switch_default(Indent, _FuncInfo, Context, default_is_unreachable) -->
	indent_line(Context, Indent),
	io__write_string("default: /*NOTREACHED*/\n"), 
	indent_line(Context, Indent + 1),
	io__write_string("throw new mercury.runtime.UnreachableDefault();\n").

%-----------------------------------------------------------------------------%

	%
	% exception handling
	%

	/* XXX not yet implemented */


	%
	% atomic statements
	%
output_stmt(Indent, FuncInfo, atomic(AtomicStatement), Context) -->
	output_atomic_stmt(Indent, FuncInfo, AtomicStatement, Context).

:- pred output_atomic_stmt(indent, func_info,
		mlds__atomic_statement, mlds__context, io__state, io__state).
:- mode output_atomic_stmt(in, in, in, in, di, uo) is det.

	%
	% comments
	%
output_atomic_stmt(Indent, _FuncInfo, comment(Comment), _) -->
	% XXX we should escape any "*/"'s in the Comment.
	%     we should also split the comment into lines and indent
	%     each line appropriately.
	indent_line(Indent),
	io__write_string("/* "),
	io__write_string(Comment),
	io__write_string(" */\n").

	%
	% assignment
	%
output_atomic_stmt(Indent, _FuncInfo, assign(Lval, Rval), _) -->
	indent_line(Indent),
	output_lval(Lval),
	io__write_string(" = "),
	( 
		{ Lval = var(_, VarType) }, 
	    	{ type_is_object(VarType) } 
	    
	->
		% If the Lval is a an object.	
		
		( { rval_is_int_const(Rval) } ->
			io__write_string("new "),
			output_type(VarType),
			io__write_string("("),
			output_rval(Rval),
			io__write_string(")")
		;		
			output_rval(Rval)
		)
	;
		output_rval_maybe_with_enum(Rval)
	),	
	io__write_string(";\n").

	%
	% heap management
	%
output_atomic_stmt(_Indent, _FuncInfo, delete_object(_Lval), _) -->
	{ error("mlds_to_java.m: delete_object not supported in Java.") }.

output_atomic_stmt(Indent, _FuncInfo, NewObject, Context) -->
	{ NewObject = new_object(Target, _MaybeTag, Type, _MaybeSize,
		MaybeCtorName, Args, ArgTypes) },
	
	indent_line(Indent),
	io__write_string("{\n"),
	indent_line(Context, Indent + 1),
	output_lval(Target),
	io__write_string(" = new "),
	%
	% XXX We should actually generate (Java) contructors for each class.  
	% This would make the treatment of discriminated unions more consistent
	% with the way we treat enumerations.  At the moment we just assign the
	% values directly to the fields.
	%
	( { MaybeCtorName = yes(QualifiedCtorId) } ->
		output_type(Type),
		io__write_char('.'),
		{ QualifiedCtorId = qual(_ModuleName, CtorDefn) },
		{ CtorDefn = ctor_id(CtorName, CtorArity) },
		{ llds_out__name_mangle(CtorName, MangledCtorName) },
		io__format("%s_%d", [s(MangledCtorName), i(CtorArity)])
	;
		{ unexpected(this_file, 
			"output_atomic_stmt: object has no constructor") }
	),
	io__write_string("();\n"),
	output_init_args(Args, ArgTypes, CtorDefn, Context, 0, Target, 0,
		Indent + 1),
	indent_line(Context, Indent),
	io__write_string("}\n").

output_atomic_stmt(_Indent, _FuncInfo, mark_hp(_Lval), _) -->
	{ error("mlds_to_java.m: sorry, mark_hp not implemented") }.

output_atomic_stmt(_Indent, _FuncInfo, restore_hp(_Rval), _) -->
	{ error("mlds_to_java.m: sorry, restore_hp not implemented") }.
	
	%
	% trail management
	%
output_atomic_stmt(_Indent, _FuncInfo, trail_op(_TrailOp), _) -->
	{ error("mlds_to_java.m: sorry, trail_ops not implemented") }.

	%
	% foreign language interfacing
	%
output_atomic_stmt(_Indent, _FuncInfo, 
		inline_target_code(_TargetLang, _Components), _Context) -->
	{ error("mlds_to_java.m: sorry, foreign language interfacing not implemented") }.

output_atomic_stmt(_Indent, _FuncInfo, 
		outline_foreign_proc(_TargetLang, _Lvals, _Code), _Context) -->
	{ error("mlds_to_java.m: sorry, foreign language interfacing not implemented") }.

%------------------------------------------------------------------------------%

	% Output initial values of an object's fields.
	%
:- pred output_init_args(list(mlds__rval), list(mlds__type), mlds__ctor_id, 
		mlds__context, int, mlds__lval, mlds__tag, indent, 
		io__state, io__state).
:- mode output_init_args(in, in, in, in, in, in, in, in, di, uo) is det.

output_init_args([], [], _, _, _, _, _, _) --> [].
output_init_args([_|_], [], _, _, _, _, _, _) -->
	{ error("output_init_args: length mismatch") }.
output_init_args([], [_|_], _, _, _, _, _, _) -->
	{ error("output_init_args: length mismatch") }.
output_init_args([Arg|Args], [ArgType|ArgTypes], CtorId, Context,
		ArgNum, Target, Tag, Indent) -->
	indent_line(Context, Indent),
	( { ArgNum = 0 } ->
		
		%
		% If it's just the data tag, no casting is necessary since
		% it is a member of the base class anyway.  Note: the
		% argument number of the data_tag is always going to be
		% zero as the numbering of other fields starts at 1.
		%
		output_lval(Target),
		io__write_string(".data_tag = "),
		output_rval(Arg)
	;
		
		%
		% Otherwise do the approriate downcasting to the derived
		% class
		%
		( 
			{ Target = var(_, TargetType),
		    	CtorId = ctor_id(CtorName, CtorArity) } 
		->
			io__write_string("(("),
			output_type(TargetType),
			io__write_string("."),
			output_mangled_name(CtorName),	
			io__write_string("_"),
			io__write_int(CtorArity),
			io__write_string(") "),
			output_lval(Target),
			io__write_string(").F"),
			io__write_int(ArgNum),	
			io__write_string(" = "),
			
			% If the Target type is the same as the argument
			% type then we just need to output the rval.
			% Otherwise we will need to output a boxed rval.
			% XXX The Context information in the ArgTypes is
			% not being filled out correctly, which is why
			% TargetType = ArgType sometimes fails when it
			% shouldn't; hence the disjunction below.
			%
			( 
				( 
				    { TargetType = ArgType }
			  	; 
			    	    { TargetType = 
					mercury_type(_, TargetBuiltinType),
			      	      ArgType = mercury_type(_, ArgBuiltinType),
			      	      TargetBuiltinType = ArgBuiltinType }
			  	) 
			
			->
				output_rval(Arg)
			;	
				output_boxed_rval(ArgType, Arg)
			)
		;
		
		%
		% Otherwise don't do the downcasting.
		%
		
			output_lval(Target),
			io__write_string(".F"),
			io__write_int(ArgNum),
			io__write_string(" = "),
			output_rval(Arg)
		)
	),
	io__write_string(";\n"),
	output_init_args(Args, ArgTypes, CtorId, Context,
		ArgNum + 1, Target, Tag, Indent).

%-----------------------------------------------------------------------------%
%
% Code to output expressions
%

:- pred output_lval(mlds__lval, io__state, io__state).
:- mode output_lval(in, di, uo) is det.

output_lval(field(_MaybeTag, _Rval, offset(_), _FieldType, _ClassType)) -->
	{ unexpected(this_file, "output_lval: offset field") }.

output_lval(field(_MaybeTag, PtrRval, named_field(FieldName, CtorType),
		_FieldType, _PtrType)) -->
	( 
		{ FieldName = qual(_, UnqualFieldName) }, 
	 	{ llds_out__name_mangle(UnqualFieldName, MangledFieldName) },
	  	{ MangledFieldName = "data_tag" } 
	->
		%
		% If the field we are trying to access is just a `data_tag'
		% then it is a member of the base class. 
		%
		output_bracketed_rval(PtrRval),
		io__write_string(".")
	;
		%
		% Otherwise the field we are trying to access may be in 
		% a derived class.  Objects are manipulated as instances
		% of their base class, so we need to downcast to the derived
		% class to access some fields.
		%
		io__write_string("(("),
		output_type(CtorType),
		io__write_string(") "),
		output_bracketed_rval(PtrRval),  % the actual variable
		io__write_string(").")
	),
	{ FieldName = qual(_, UnqualFieldName) },
	output_mangled_name(UnqualFieldName).    % the field name

output_lval(mem_ref(Rval, _Type)) -->
	output_bracketed_rval(Rval).

output_lval(var(qual(_ModuleName, Name), _VarType)) -->
	output_mlds_var_name(Name).
		
:- pred output_mangled_name(string, io__state, io__state).
:- mode output_mangled_name(in, di, uo) is det.

output_mangled_name(Name) -->
	{ llds_out__name_mangle(Name, MangledName) },
	io__write_string(MangledName).

:- pred mlds_output_bracketed_lval(mlds__lval, io__state, io__state).
:- mode mlds_output_bracketed_lval(in, di, uo) is det.

mlds_output_bracketed_lval(Lval) -->
	(
		% if it's just a variable name, then we don't need parentheses
		{ Lval = var(_,_) }
	->
		output_lval(Lval)
	;
		io__write_char('('),
		output_lval(Lval),
		io__write_char(')')
	).

:- pred output_bracketed_rval(mlds__rval, io__state, io__state).
:- mode output_bracketed_rval(in, di, uo) is det.

output_bracketed_rval(Rval) -->
	(
		% if it's just a variable name, then we don't need parentheses
		{ Rval = lval(var(_,_))
		; Rval = const(code_addr_const(_))
		}
	->
		output_rval(Rval)
	;
		io__write_char('('),
		output_rval(Rval),
		io__write_char(')')
	).

:- pred output_rval(mlds__rval, io__state, io__state).
:- mode output_rval(in, di, uo) is det.

output_rval(lval(Lval)) -->
	output_lval(Lval).
		
output_rval(mkword(_, _)) -->
	{ unexpected(this_file, 
		"output_rval: tags not supported in Java") }.

output_rval(const(Const)) -->
	output_rval_const(Const).

output_rval(unop(Op, Rval)) -->
	output_unop(Op, Rval).

output_rval(binop(Op, Rval1, Rval2)) -->
	output_binop(Op, Rval1, Rval2).

output_rval(mem_addr(_Lval)) -->
	{ unexpected(this_file, "output_rval: mem_addr(_) not supported") }.

output_rval(self(_)) -->
	io__write_string("this").

:- pred output_unop(mlds__unary_op, mlds__rval, io__state, io__state).
:- mode output_unop(in, in, di, uo) is det.
	
output_unop(cast(Type), Exprn) -->
	output_cast_rval(Type, Exprn).
output_unop(box(Type), Exprn) -->
	output_boxed_rval(Type, Exprn).
output_unop(unbox(Type), Exprn) -->
	output_unboxed_rval(Type, Exprn).
output_unop(std_unop(Unop), Exprn) -->
	output_std_unop(Unop, Exprn).

:- pred output_cast_rval(mlds__type, mlds__rval, io__state, io__state).
:- mode output_cast_rval(in, in, di, uo) is det.

output_cast_rval(Type, Exprn) -->
	io__write_string("("),
	output_type(Type),
	io__write_string(") "),
	output_rval_maybe_with_enum(Exprn).

:- pred output_boxed_rval(mlds__type, mlds__rval, io__state, io__state).
:- mode output_boxed_rval(in, in, di, uo) is det.
	
output_boxed_rval(Type, Exprn) -->
	(
		{ java_builtin_type(Type, _JavaName, JavaBoxedName,
			_UnboxMethod) }
	->
		io__write_string("new "),
		io__write_string(JavaBoxedName),
		io__write_string("("),
		output_rval(Exprn),
		io__write_string(")")
	;
		io__write_string("((java.lang.Object) ("),
		output_rval(Exprn),
		io__write_string("))")
	).

:- pred output_unboxed_rval(mlds__type, mlds__rval, io__state, io__state).
:- mode output_unboxed_rval(in, in, di, uo) is det.

output_unboxed_rval(Type, Exprn) -->
	(
		{ java_builtin_type(Type, _JavaName, JavaBoxedName,
			UnboxMethod) }
	->
		io__write_string("(("),
		io__write_string(JavaBoxedName),
		io__write_string(") "), 
		output_bracketed_rval(Exprn),
		io__write_string(")."),
		io__write_string(UnboxMethod),
		io__write_string("()")
	;
		io__write_string("(("),
		output_type(Type),
		io__write_string(") "),
		output_rval(Exprn),
		io__write_string(")")
	).

	% java_builtin_type(MLDS_Type, JavaUnboxedType, JavaBoxedType,
	%	UnboxMethod):
	% For a given Mercury type, check if this corresponds to a
	% Java type which has both unboxed (builtin) and boxed (class)
	% versions, and if so, return their names, and the name of
	% the method to get the unboxed value from the boxed type.
	%
:- pred java_builtin_type(mlds__type, string, string, string).
:- mode java_builtin_type(in, out, out, out) is semidet.

java_builtin_type(Type, "int", "java.lang.Integer", "intValue") :-
	Type = mlds__native_int_type.
java_builtin_type(Type, "int", "java.lang.Integer", "intValue") :-
	Type = mlds__mercury_type(term__functor(term__atom("int"), [], _), _).
java_builtin_type(Type, "double", "java.lang.Double", "doubleValue") :-
	Type = mlds__native_float_type.
java_builtin_type(Type, "double", "java.lang.Double", "doubleValue") :-
	Type = mlds__mercury_type(term__functor(term__atom("float"),
		[], _), _).
java_builtin_type(Type, "char", "java.lang.Character", "charValue") :-
	Type = mlds__native_char_type.
java_builtin_type(Type, "char", "java.lang.Character", "charValue") :-
	Type = mlds__mercury_type(term__functor(term__atom("character"),
		[], _), _).
java_builtin_type(Type, "boolean", "java.lang.Boolean", "booleanValue") :-
	Type = mlds__native_bool_type.

:- pred output_std_unop(builtin_ops__unary_op, mlds__rval, 
		io__state, io__state).
:- mode output_std_unop(in, in, di, uo) is det.
	
output_std_unop(UnaryOp, Exprn) -->
	{ java_util__unary_prefix_op(UnaryOp, UnaryOpString) },
	io__write_string(UnaryOpString),
	io__write_string("("),
	output_rval(Exprn),
	io__write_string(")").

:- pred output_binop(binary_op, mlds__rval, mlds__rval,
			io__state, io__state).
:- mode output_binop(in, in, in, di, uo) is det.
	
output_binop(Op, X, Y) -->
	(
		{ Op = array_index(_Type) }
	->
		output_bracketed_rval(X),
		io__write_string("["),
		output_rval(Y),
		io__write_string("]")
	;
		{ java_util__string_compare_op(Op, OpStr) }
	->
		output_rval(X),
		io__write_string(".compareTo("),
		output_rval(Y),
		io__write_string(")"),
		io__write_string(" "),
		io__write_string(OpStr),
		io__write_string(" "),
		io__write_string("0")
	;
		( { java_util__float_compare_op(Op, OpStr1) } ->
			{ OpStr = OpStr1 }
		; { java_util__float_op(Op, OpStr2) } ->
			{ OpStr = OpStr2 }
		;
			{ fail }
		)
	->
		io__write_string("("),
		output_rval_maybe_with_enum(X),
		io__write_string(" "),
		io__write_string(OpStr),
		io__write_string(" "),
		output_rval_maybe_with_enum(Y),
		io__write_string(")")
	;
		io__write_string("("),
		output_rval_maybe_with_enum(X),
		io__write_string(" "),
		output_binary_op(Op),
		io__write_string(" "),
		output_rval_maybe_with_enum(Y),
		io__write_string(")")
	).

	% Output an Rval and if the Rval is an enumeration object
	% append the string ".value", so we can access its value
	% field.
	%
:- pred output_rval_maybe_with_enum(mlds__rval, io__state, io__state).
:- mode output_rval_maybe_with_enum(in, di, uo) is det.

output_rval_maybe_with_enum(Rval) -->
	output_rval(Rval),
	( { rval_is_enum_object(Rval) } ->
		io__write_string(".value")
	;
		[]
	).

:- pred output_binary_op(binary_op, io__state, io__state).
:- mode output_binary_op(in, di, uo) is det.

output_binary_op(Op) -->
	( { java_util__binary_infix_op(Op, OpStr) } ->
		io__write_string(OpStr)
	;
		{ error("output_binary_op: invalid binary operator") }
	).

:- pred output_rval_const(mlds__rval_const, io__state, io__state).
:- mode output_rval_const(in, di, uo) is det.

output_rval_const(true) -->
	io__write_string("true").	

output_rval_const(false) -->
	io__write_string("false").	

output_rval_const(int_const(N)) -->
	io__write_int(N).

output_rval_const(float_const(FloatVal)) -->
	io__write_float(FloatVal).

output_rval_const(string_const(String)) -->
	io__write_string(""""),
	c_util__output_quoted_string(String),
	io__write_string("""").

output_rval_const(multi_string_const(Length, String)) -->
	io__write_string(""""),
	c_util__output_quoted_multi_string(Length, String),
	io__write_string("""").

output_rval_const(code_addr_const(CodeAddr)) -->
	mlds_output_code_addr(CodeAddr).

output_rval_const(data_addr_const(DataAddr)) -->
	mlds_output_data_addr(DataAddr).

output_rval_const(null(_)) -->
       io__write_string("null").

%-----------------------------------------------------------------------------%

:- pred mlds_output_code_addr(mlds__code_addr, io__state, io__state).
:- mode mlds_output_code_addr(in, di, uo) is det.

mlds_output_code_addr(proc(Label, _Sig)) -->
	output_fully_qualified_proc_label(Label).
mlds_output_code_addr(internal(Label, SeqNum, _Sig)) -->
	output_fully_qualified_proc_label(Label),
	io__write_string("_"),
	io__write_int(SeqNum).

:- pred mlds_output_proc_label(mlds__proc_label, io__state, io__state).
:- mode mlds_output_proc_label(in, di, uo) is det.

mlds_output_proc_label(PredLabel - ProcId) -->
	output_pred_label(PredLabel),
	{ proc_id_to_int(ProcId, ModeNum) },
	io__format("_%d", [i(ModeNum)]).

:- pred mlds_output_data_addr(mlds__data_addr, io__state, io__state).
:- mode mlds_output_data_addr(in, di, uo) is det.

mlds_output_data_addr(data_addr(ModuleName, DataName)) -->
	output_module_name(mlds_module_name_to_sym_name(ModuleName)),
	io__write_string("."),
	output_data_name(DataName).

%-----------------------------------------------------------------------------%
%
% Miscellaneous stuff to handle indentation and generation of
% source context annotations.  (XXX This can probably be simplified
% since Java doesn't have an equivalent of #line directives.)
%

:- pred output_context(mlds__context, io__state, io__state).
:- mode output_context(in, di, uo) is det.

output_context(_Context) --> [].

:- pred indent_line(mlds__context, indent, io__state, io__state).
:- mode indent_line(in, in, di, uo) is det.

indent_line(Context, N) -->
	output_context(Context),
	indent_line(N).

% A value of type `indent' records the number of levels
% of indentation to indent the next piece of code.
% Currently we output two spaces for each level of indentation.
% XXX There is a small amount of code duplication with mlds_to_c.m here.
:- type indent == int.

:- pred indent_line(indent, io__state, io__state).
:- mode indent_line(in, di, uo) is det.

indent_line(N) -->
	( { N =< 0 } ->
		[]
	;
		io__write_string("  "),
		indent_line(N - 1)
	).

:- func this_file = string.
this_file = "mlds_to_java.m".

:- end_module mlds_to_java.

%-----------------------------------------------------------------------------%
