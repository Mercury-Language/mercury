%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% mlds_to_java - Convert MLDS to Java code.
% Main authors: juliensf, mjwybrow. 
%
% DONE:
%	det and semidet predicates
%	multiple output arguments
%	boxing and unboxing
%	conjunctions
%	disjunctions
%	if-then-else's
%	enumerations
%	discriminated unions
%	higher order functions
%	multidet and nondet predicates
%	test tests/benchmarks/*.m
%	generate optimized tailcalls
% TODO: 
%	General code cleanup
%	handle static ground terms
%	RTTI (requires static ground terms)
%	generate names of classes etc. correctly (mostly same as IL backend)
%
%	handle foreign code written in Java
%	handle foreign code written in C 
%
% NOTES: 
%       To avoid namespace conflicts all Java names must be fully qualified.
%	e.g. The classname `String' must be qualified as `java.lang.String'
%	     to avoid conflicting with `mercury.String'.
%
%	There is currently some code threaded through the output predicates
%	(usually a variable called `ExitMethods') which keeps track of, and
%	removes unreachable code. Ideally this would be done as an MLDS->MLDS
%	transformation, preferably in a seperate module. Unfortunately this
%	is not possible due to the fact that the back-end generates `break'
%	statements for cases in switches as they are output, meaning that we
%	can't remove them in a pass over the MLDS. 
%
%-----------------------------------------------------------------------------%

:- module ml_backend__mlds_to_java.
:- interface.

:- import_module ml_backend__mlds.
:- import_module io.

:- pred mlds_to_java__output_mlds(mlds, io__state, io__state).
:- mode mlds_to_java__output_mlds(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_backend__ml_util.
:- import_module ml_backend__java_util. 
:- import_module backend_libs__c_util.	% XXX needed for c_util__output_quoted_string
				% c_util_output_quoted_multi_string
:- import_module ll_backend__llds_out.	% XXX needed for llds_out__name_mangle,
				% llds_out__sym_name_mangle,
				% llds_out__make_base_typeclass_info_name,
:- import_module backend_libs__rtti.		% for rtti__addr_to_string.
:- import_module ml_backend__rtti_to_mlds.	% for mlds_rtti_type_name.
:- import_module hlds__hlds_pred.	% for pred_proc_id.
:- import_module parse_tree__modules.       % for mercury_std_library_name.
:- import_module ml_backend__ml_code_util.	% for ml_gen_local_var_decl_flags.
:- import_module ml_backend__ml_type_gen.	% for ml_gen_type_name
:- import_module backend_libs__export.	% for export__type_to_type_string
:- import_module libs__globals, libs__options, hlds__passes_aux.
:- import_module backend_libs__builtin_ops.
:- import_module parse_tree__prog_data, parse_tree__prog_out.
:- import_module check_hlds__type_util, hlds__error_util.

:- import_module bool, int, string, library, list, set.
:- import_module assoc_list, term, std_util, require.

%-----------------------------------------------------------------------------%

mlds_to_java__output_mlds(MLDS) -->
	{ ModuleName = mlds__get_module_name(MLDS) },
	{ JavaSafeModuleName = valid_module_name(ModuleName) },
	module_name_to_file_name(JavaSafeModuleName, ".java", yes, 
			JavaSourceFile),
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
	Body = mlds__data(Type, _, _),
	Type = mlds__rtti_type(_).

	% Succeeds iff this type is a enumeration.
	%
:- pred type_is_enum(mlds__type).
:- mode type_is_enum(in) is semidet.

type_is_enum(Type) :-
	Type = mercury_type(_, Builtin, _),
	Builtin = enum_type.

	%  Succeeds iff this type is something that 
	%  the Java backend will represent as an object 
	%  i.e. something created using the new operator.
	%
:- pred type_is_object(mlds__type).
:- mode type_is_object(in) is semidet.

type_is_object(Type) :-
	Type = mercury_type(_, Builtin, _),
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

interface_is_special("MethodPtr").


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
	
:- pred mangle_mlds_sym_name_for_java(sym_name, string, string).
:- mode mangle_mlds_sym_name_for_java(in, in, out) is det.

mangle_mlds_sym_name_for_java(unqualified(Name), _Qualifier, JavaSafeName) :-
	llds_out__name_mangle(Name, MangledName),
	JavaSafeName = valid_symbol_name(MangledName).
mangle_mlds_sym_name_for_java(qualified(ModuleName, PlainName), Qualifier,
		MangledName) :-
	mangle_mlds_sym_name_for_java(ModuleName, Qualifier,
			MangledModuleName),
	llds_out__name_mangle(PlainName, MangledPlainName),
	JavaSafePlainName = valid_symbol_name(MangledPlainName),
	java_qualify_mangled_name(MangledModuleName, JavaSafePlainName,
			Qualifier, MangledName).

:- pred java_qualify_mangled_name(string, string, string, string).
:- mode java_qualify_mangled_name(in, in, in, out) is det.

java_qualify_mangled_name(Module0, Name0, Qualifier, Name) :-
	string__append_list([Module0, Qualifier, Name0], Name).


%-----------------------------------------------------------------------------%
%
% Name mangling code to fix problem of mercury modules having the same name
% as reserved Java words such as `char' and `int'. 
% 

	% If the given name conficts with a reserved Java word we must add a 
	% prefix to it to avoid compilation errors.
:- func valid_symbol_name(string) = string.
:- mode valid_symbol_name(in) = out is det.

valid_symbol_name(SymName) = ValidSymName :-
	Prefix = "mr_",
	( java_util__is_keyword(SymName) ->
		% This is a reserved Java word, add the above prefix.
		ValidSymName = Prefix ++ SymName
	; string__append(Prefix, Suffix, SymName) ->
		% This name already contains the prefix we are adding to 
		% variables to avoid conficts, so add an additional '_'.
		ValidSymName = Prefix ++ "_" ++ Suffix
	; 
		% Normal name; do nothing.
		ValidSymName = SymName
	).


:- func valid_module_name(mercury_module_name) = mercury_module_name.
:- mode valid_module_name(in) = out is det.

valid_module_name(unqualified(String)) =  ValidModuleName :-
	ValidString = valid_symbol_name(String),
	ValidModuleName = unqualified(ValidString).
valid_module_name(qualified(ModuleSpecifier, String)) =  ValidModuleName :-
	ValidModuleSpecifier = valid_module_name(ModuleSpecifier),
	ValidString = valid_symbol_name(String),
	ValidModuleName = qualified(ValidModuleSpecifier, ValidString).


%-----------------------------------------------------------------------------%
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
	{ Import = mercury_import(ImportName)
	; Import = foreign_import(_),
		unexpected(this_file, "foreign import in java backend")
	},
	{ SymName = mlds_module_name_to_sym_name(ImportName) },
	{ JavaSafeSymName = valid_module_name(SymName) },
	{ prog_out__sym_name_to_string(JavaSafeSymName, ".", File) }, 
	% XXX Name mangling code should be put here when we start enforcing
	%     Java's naming conventions.
	{ ClassFile = File },
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
	%
	% Find and build list of all methods which would have their addresses
	% taken to be used as a function pointer.
	%
	{ find_pointer_addressed_methods(Defns0, [], CodeAddrs0) },
	{ CodeAddrs = list__sort_and_remove_dups(CodeAddrs0) },
	%
	% Create wrappers in MLDS for all pointer addressed methods.
	% 
	{ generate_code_addr_wrappers(Indent + 1, CodeAddrs, [], 
			WrapperDefns) },
	{ Defns = WrapperDefns ++ Defns0 }, 
	%
	% Output transformed MLDS as Java source.  
	%
	output_src_start(Indent, ModuleName, Imports, Defns), 
	{ list__filter(defn_is_rtti_data, Defns, _RttiDefns, NonRttiDefns) },
	% XXX Need to output RTTI data at this point.
	{ CtorData = none },  % Not a constructor.
	output_defns(Indent + 1, MLDS_ModuleName, CtorData, NonRttiDefns),
	output_src_end(Indent, ModuleName).
	% XXX Need to handle non-Java foreign code at this point.


%-----------------------------------------------------------------------------%
%
% Code to search MLDS for all uses of function pointers.
% 
	
	% Returns code-address information (function label and signature)
	% for each method/function which has its address taken in the MLDS.
	% 
:- pred find_pointer_addressed_methods(mlds__defns, list(mlds__code_addr),
		list(mlds__code_addr)).
:- mode find_pointer_addressed_methods(in, in, out) is det.

find_pointer_addressed_methods([]) --> [].
find_pointer_addressed_methods([Defn | Defns]) -->
	{ Defn  = mlds__defn(_Name, _Context, _Flags, Body) },
	method_ptrs_in_entity_defn(Body),
	find_pointer_addressed_methods(Defns).


:- pred method_ptrs_in_entity_defn(mlds__entity_defn,
		list(mlds__code_addr), list(mlds__code_addr)).
:- mode method_ptrs_in_entity_defn(in, in, out) is det.

method_ptrs_in_entity_defn(mlds__function(_MaybeID, _Params, Body,
		_Attributes)) -->
	(	
		{ Body = mlds__defined_here(Statement) },
		method_ptrs_in_statement(Statement)
	;	
		{ Body = mlds__external }
	).
method_ptrs_in_entity_defn(mlds__data(_Type, Initializer, _GC_TraceCode)) -->
	method_ptrs_in_initializer(Initializer).
method_ptrs_in_entity_defn(mlds__class(ClassDefn)) --> 
	{ ClassDefn = mlds__class_defn(_, _, _, _, Ctors, Members) },
	method_ptrs_in_defns(Ctors),
	method_ptrs_in_defns(Members).


:- pred method_ptrs_in_statements(mlds__statements, list(mlds__code_addr),
		list(mlds__code_addr)).
:- mode method_ptrs_in_statements(in, in, out) is det.

method_ptrs_in_statements([]) --> [].
method_ptrs_in_statements([Statement|Statements]) -->
	method_ptrs_in_statement(Statement),
	method_ptrs_in_statements(Statements).


:- pred method_ptrs_in_statement(mlds__statement, list(mlds__code_addr),
		list(mlds__code_addr)).
:- mode method_ptrs_in_statement(in, in, out) is det.

method_ptrs_in_statement(mlds__statement(Stmt, _Context)) -->
	method_ptrs_in_stmt(Stmt).


:- pred method_ptrs_in_stmt(mlds__stmt, list(mlds__code_addr),
		list(mlds__code_addr)).
:- mode method_ptrs_in_stmt(in, in, out) is det.

method_ptrs_in_stmt(mlds__block(Defns, Statements)) -->
	method_ptrs_in_defns(Defns),
	method_ptrs_in_statements(Statements).
method_ptrs_in_stmt(mlds__while(Rval, Statement, _Bool)) -->
	method_ptrs_in_rval(Rval),
	method_ptrs_in_statement(Statement).
method_ptrs_in_stmt(mlds__if_then_else(Rval, StatementThen,
		MaybeStatementElse)) -->
	method_ptrs_in_rval(Rval),
	method_ptrs_in_statement(StatementThen),
	( { MaybeStatementElse = yes(StatementElse) } ->
		method_ptrs_in_statement(StatementElse)
	; % MaybeStatementElse = no
		[]
	).
method_ptrs_in_stmt(mlds__switch(_Type, Rval, _Range, Cases, Default)) -->
	method_ptrs_in_rval(Rval),
	method_ptrs_in_switch_cases(Cases),
	method_ptrs_in_switch_default(Default).
method_ptrs_in_stmt(mlds__label(_Label)) --> [].
method_ptrs_in_stmt(mlds__goto(_Label)) --> [].
method_ptrs_in_stmt(mlds__computed_goto(Rval, _Labels)) -->
	method_ptrs_in_rval(Rval).
method_ptrs_in_stmt(mlds__try_commit(_Lval, StatementGoal,
		StatementHandler)) -->
	% We don't check "_Lval" here as we expect it to be a local variable
	% of type mlds__commit_type.
	method_ptrs_in_statement(StatementGoal),
	method_ptrs_in_statement(StatementHandler).
method_ptrs_in_stmt(mlds__do_commit(_Rval)) -->
	% We don't check "_Rval" here as we expect it to be a local variable
	% of type mlds__commit_type.
	[].
method_ptrs_in_stmt(mlds__return(Rvals)) -->
	method_ptrs_in_rvals(Rvals).
method_ptrs_in_stmt(mlds__call(_FuncSig, _Rval, _MaybeThis, Rvals, _ReturnVars, 
		_IsTailCall)) -->
	% We don't check "_Rval" - it may be a code address but is a 
	% standard call rather than a function pointer use.
	method_ptrs_in_rvals(Rvals).
method_ptrs_in_stmt(mlds__atomic(AtomicStatement)) --> 
	( { AtomicStatement = mlds__new_object(Lval, _MaybeTag, _Bool, _Type,
			_MemRval, _MaybeCtorName, Rvals, _Types) } ->
	 	% We don't need to check "_MemRval" since this just stores
		% the amount of memory needed for the new object.
		method_ptrs_in_lval(Lval),
		method_ptrs_in_rvals(Rvals)
	; { AtomicStatement = mlds__assign(Lval, Rval) } ->
		method_ptrs_in_lval(Lval),
		method_ptrs_in_rval(Rval)
	; 
		[]
	).


:- pred method_ptrs_in_switch_default(mlds__switch_default,
		list(mlds__code_addr), list(mlds__code_addr)).
:- mode method_ptrs_in_switch_default(in, in, out) is det.

method_ptrs_in_switch_default(mlds__default_is_unreachable) --> [].
method_ptrs_in_switch_default(mlds__default_do_nothing) --> [].
method_ptrs_in_switch_default(mlds__default_case(Statement)) -->
	method_ptrs_in_statement(Statement).


:- pred method_ptrs_in_switch_cases(mlds__switch_cases, 
		list(mlds__code_addr), list(mlds__code_addr)).
:- mode method_ptrs_in_switch_cases(in, in, out) is det.

method_ptrs_in_switch_cases([]) --> [].
method_ptrs_in_switch_cases([Case|Cases]) -->
	{ Case = _Conditions - Statement },
	method_ptrs_in_statement(Statement),
	method_ptrs_in_switch_cases(Cases).


:- pred method_ptrs_in_defns(mlds__defns, list(mlds__code_addr),
		list(mlds__code_addr)).
:- mode method_ptrs_in_defns(in, in, out) is det.

method_ptrs_in_defns([]) --> [].
method_ptrs_in_defns([Defn|Defns]) --> 
	method_ptrs_in_defn(Defn),
	method_ptrs_in_defns(Defns).


:- pred method_ptrs_in_defn(mlds__defn, list(mlds__code_addr),
		list(mlds__code_addr)).
:- mode method_ptrs_in_defn(in, in, out) is det.

method_ptrs_in_defn(mlds__defn(_Name, _Context, _Flags, Body)) -->
	method_ptrs_in_entity_defn(Body).


:- pred method_ptrs_in_initializer(mlds__initializer,
		list(mlds__code_addr), list(mlds__code_addr)).
:- mode method_ptrs_in_initializer(in, in, out) is det.

method_ptrs_in_initializer(mlds__no_initializer) --> [].
method_ptrs_in_initializer(mlds__init_struct(Initializers)) -->
	method_ptrs_in_initializers(Initializers).
method_ptrs_in_initializer(mlds__init_array(Initializers)) -->
	method_ptrs_in_initializers(Initializers).
method_ptrs_in_initializer(mlds__init_obj(Rval)) --> 
	method_ptrs_in_rval(Rval).


:- pred method_ptrs_in_initializers(list(mlds__initializer),
		list(mlds__code_addr), list(mlds__code_addr)).
:- mode method_ptrs_in_initializers(in, in, out) is det.

method_ptrs_in_initializers([]) --> [].
method_ptrs_in_initializers([Initializer | Initializers]) -->
	method_ptrs_in_initializer(Initializer),
	method_ptrs_in_initializers(Initializers).


:- pred method_ptrs_in_rvals(list(mlds__rval), list(mlds__code_addr),
		list(mlds__code_addr)).
:- mode method_ptrs_in_rvals(in, in, out) is det.

method_ptrs_in_rvals([]) --> [].
method_ptrs_in_rvals([Rval|Rvals]) -->
	method_ptrs_in_rval(Rval),
	method_ptrs_in_rvals(Rvals).


:- pred method_ptrs_in_rval(mlds__rval, list(mlds__code_addr),
		list(mlds__code_addr)).
:- mode method_ptrs_in_rval(in, in, out) is det.

method_ptrs_in_rval(mlds__lval(Lval)) -->
	method_ptrs_in_lval(Lval).
method_ptrs_in_rval(mlds__mkword(_Tag, Rval)) -->
	method_ptrs_in_rval(Rval).
method_ptrs_in_rval(mlds__const(RvalConst), CodeAddrs0, CodeAddrs) :-
	( RvalConst = mlds__code_addr_const(CodeAddr) ->
		CodeAddrs = CodeAddrs0 ++ [CodeAddr]
	;	
		CodeAddrs = CodeAddrs0
	).
method_ptrs_in_rval(mlds__unop(_UnaryOp, Rval)) -->
	method_ptrs_in_rval(Rval).
method_ptrs_in_rval(mlds__binop(_BinaryOp, Rval1, Rval2)) -->
	method_ptrs_in_rval(Rval1),
	method_ptrs_in_rval(Rval2).
method_ptrs_in_rval(mlds__mem_addr(_Address)) --> [].
method_ptrs_in_rval(mlds__self(_Type)) --> [].


:- pred method_ptrs_in_lval(mlds__lval, list(mlds__code_addr),
                list(mlds__code_addr)).
:- mode method_ptrs_in_lval(in, in, out) is det.

	% Here, "_Rval" is the address of a variable so we don't check it.
method_ptrs_in_lval(mlds__mem_ref(_Rval, _Type)) --> [].
	% Here, "_Rval" is a pointer to a cell on the heap, and doesn't need
	% to be considered.
method_ptrs_in_lval(mlds__field(_MaybeTag, _Rval, _FieldId, _FieldType,
		_PtrType)) --> [].
method_ptrs_in_lval(mlds__var(_Variable, _Type)) --> [].


%-----------------------------------------------------------------------------%
%
% Code to output wrapper classes for the implementation of function pointers
% in Java.
%
% As there is no way to take the address of a method in Java, we must create a
% wrapper for that method which implements a common interface. We are then able
% to pass that class around as a java.lang.Object.
%
% XXX This implementation will not handle taking the address of instance
%     methods. This is not currently a problem as they will never be generated
%     by the MLDS back-end.
%
% XXX This implementation will not corectly handle the case which occurs where
%     there are two or more overloaded MLDS functions (that we take the
%     address of) with the same name and arity but different argument types,
%     both in the same module. This is due to the fact that the names of the
%     generated wrapper classes are based purely on the method name.
%


	% Generates the MLDS to output the required wrapper classes
	% 
:- pred generate_code_addr_wrappers(indent, list(mlds__code_addr),
		mlds__defns, mlds__defns).
:- mode generate_code_addr_wrappers(in, in, in, out) is det.

generate_code_addr_wrappers(_, []) --> [].
generate_code_addr_wrappers(Indent, [CodeAddr|CodeAddrs], Defns0, Defns) :-
	%
	% XXX We should fill in the Context properly. This would probably
	%     involve also returning context information for each "code_addr"
	%     returned by the "method_ptrs_*" predicates above. 
	%
	Context = mlds__make_context(term__context_init),
	InterfaceModuleName = mercury_module_name_to_mlds(
			qualified(unqualified("mercury"), "runtime")),
	Interface = qual(InterfaceModuleName, "MethodPtr"),
	generate_addr_wrapper_class(Interface, Context, CodeAddr, ClassDefn),
	Defns1 = [ClassDefn|Defns0],
	generate_code_addr_wrappers(Indent, CodeAddrs, Defns1, Defns).
	

	% Generate the MLDS wrapper class for a given code_addr.
:- pred generate_addr_wrapper_class(mlds__class,
		mlds__context, mlds__code_addr, mlds__defn).
:- mode generate_addr_wrapper_class(in, in, in, out) is det.

generate_addr_wrapper_class(Interface, Context, CodeAddr, ClassDefn) :-
	( 
		CodeAddr = mlds__proc(ProcLabel, _FuncSig),
		MaybeSeqNum = no
	; 
		CodeAddr = mlds__internal(ProcLabel, SeqNum, _FuncSig),
		MaybeSeqNum = yes(SeqNum)
	),
	ProcLabel = mlds__qual(ModuleQualifier, PredLabel - ProcID),
	PredName = make_pred_name_string(PredLabel, ProcID, MaybeSeqNum),
	%
	% Create class components. 
	%
	ClassImports = [],
	ClassExtends = [],
	InterfaceDefn = mlds__class_type(Interface, 0, mlds__interface),
	ClassImplements = [InterfaceDefn],
	%
	% Create a method that calls the original predicate.
	%
	generate_call_method(CodeAddr, MethodDefn),
	%
	% Create a name for this wrapper class based on the fully qualified
	% method (predicate) name.
	%
	ModuleNameStr = mlds_module_name_to_string(ModuleQualifier),	
	ClassEntityName = "AddrOf__" ++ ModuleNameStr ++ "__" ++ PredName,
	llds_out__name_mangle(ClassEntityName, MangledClassEntityName),
	%
	% Put it all together.
	%
	ClassMembers  = [MethodDefn],
	ClassCtors    = [],
	ClassName     = type(MangledClassEntityName, 0),
	ClassContext  = Context,
	ClassFlags    = ml_gen_type_decl_flags,
	ClassBodyDefn = mlds__class_defn(mlds__class, ClassImports,
			ClassExtends, ClassImplements, ClassCtors,
			ClassMembers),
	ClassBody     = mlds__class(ClassBodyDefn),
	ClassDefn = mlds__defn(ClassName, ClassContext, ClassFlags, ClassBody).


	% Generates a call methods which calls the original method we have
	% created the wrapper for.
	% 
:- pred generate_call_method(mlds__code_addr, mlds__defn).
:- mode generate_call_method(in, out) is det.

generate_call_method(CodeAddr, MethodDefn) :-
	( 
		CodeAddr = mlds__proc(ProcLabel, OrigFuncSignature)
	; 
		CodeAddr = mlds__internal(ProcLabel, _SeqNum,
				OrigFuncSignature)
	),
	OrigFuncSignature = mlds__func_signature(OrigArgTypes, OrigRetTypes),
	% XXX We should fill in the Context properly.
	Context = mlds__make_context(term__context_init),
	ProcLabel = mlds__qual(ModuleName, _EntityName),
	hlds_pred__initial_pred_id(PredID),
	initial_proc_id(ProcID),
	%
	% Create new method name
	%
	Label = special_pred("call", no, "", 0),
	MethodName = function(Label, ProcID, no, PredID),
	%
	% Create method argument and return type.
	% It will have the argument type java.lang.Object[]
	% It will have the return type java.lang.Object
	%
	MethodArgVariable = var_name("args", no),
	MethodArgType = argument(data(var(MethodArgVariable)), 
			mlds__array_type(mlds__generic_type), no),
	MethodRetType = mlds__generic_type,
	MethodArgs = [MethodArgType],
	MethodRets = [MethodRetType],
	%
	% Create a temporary variable to store the result of the call to the
	% original method.
	%
	ReturnVarName = var_name("return_value", no),
	ReturnVar = mlds__qual(ModuleName, ReturnVarName),
	%
	% Create a declaration for this variable.
	%
	( OrigRetTypes = [] ->
		ReturnVarType = mlds__generic_type
	; OrigRetTypes = [CallRetType] ->
		ReturnVarType = CallRetType
	;
		ReturnVarType = mlds__array_type(mlds__generic_type)
	),
	ReturnLval = mlds__var(ReturnVar, ReturnVarType),
	ReturnEntityName = mlds__data(mlds__var(ReturnVarName)),

        ReturnDecFlags = ml_gen_local_var_decl_flags,
	GCTraceCode = no,  % The Java back-end does its own garbage collection.
	ReturnEntityDefn = mlds__data(ReturnVarType, no_initializer,
			GCTraceCode), 
	ReturnVarDefn = mlds__defn(ReturnEntityName, Context, ReturnDecFlags,
			ReturnEntityDefn),
	MethodDefns = [ReturnVarDefn],
	%
	% Create the call to the original method:
	% 
	CallArgLabel = mlds__qual(ModuleName,  MethodArgVariable),
	generate_call_method_args(OrigArgTypes, CallArgLabel, 0, [], CallArgs),
	CallRval = mlds__const(mlds__code_addr_const(CodeAddr)),
        %
	% If the original method has a return type of void, then we obviously
	% cannot assign its return value to "return_value". Thus, in this 
	% case the value returned by the call method will just be the value
	% which "return_value" was initialised to.
	% 
	(
		OrigRetTypes = []
	->
		CallRetLvals = []
	;
		CallRetLvals = [ReturnLval]
	),
	Call = mlds__call(OrigFuncSignature, CallRval, no, CallArgs,
			CallRetLvals, ordinary_call),
	CallStatement = mlds__statement(Call, Context),
	%
	% Create a return statement that returns the result of the call to the
	% original method, boxed as a java.lang.Object.
	% 
	ReturnRval = unop(box(ReturnVarType), lval(ReturnLval)),
	Return = mlds__return([ReturnRval]),
	ReturnStatement = mlds__statement(Return, Context),
	
	Block = block(MethodDefns, [CallStatement, ReturnStatement]),
	Statements = mlds__statement(Block, Context), 
	%
	% Put it all together.
	%
	MethodParams = mlds__func_params(MethodArgs, MethodRets),
	MethodMaybeID = no, 
	MethodAttribs = [],
	MethodBody   = mlds__function(MethodMaybeID, MethodParams,
			defined_here(Statements), MethodAttribs),
	MethodFlags  = ml_gen_special_member_decl_flags,	
	MethodDefn   = mlds__defn(MethodName, Context, MethodFlags, MethodBody).


:- pred generate_call_method_args(list(mlds__type), mlds__var, int, 
		list(mlds__rval), list(mlds__rval)).
:- mode generate_call_method_args(in, in, in, in, out) is det.

generate_call_method_args([], _, _, Args, Args).
generate_call_method_args([Type|Types], Variable, Counter, Args0, Args) :-
	ArrayRval = mlds__lval(mlds__var(Variable, mlds__native_int_type)),
	IndexRval = mlds__const(int_const(Counter)),
	Rval = binop(array_index(elem_type_generic), ArrayRval, IndexRval),
	UnBoxedRval = unop(unbox(Type), Rval),
	Args1 = Args0 ++ [UnBoxedRval],
	generate_call_method_args(Types, Variable, Counter + 1, Args1, Args).


:- func mlds_module_name_to_string(mlds__mlds_module_name) = string.
:- mode mlds_module_name_to_string(in) = out is det.

mlds_module_name_to_string(MldsModuleName) = ModuleNameStr :-
	ModuleName = mlds_module_name_to_sym_name(MldsModuleName),
	ModuleNameStr = symbol_name_to_string(ModuleName, "").


:- func symbol_name_to_string(sym_name, string) = string.
:- mode symbol_name_to_string(in, in) = out is det.

symbol_name_to_string(unqualified(SymName), SymNameStr0) = SymNameStr :-
	SymNameStr = SymNameStr0 ++ SymName.
symbol_name_to_string(qualified(Qualifier, SymName), 
		SymNameStr0) = SymNameStr :-
	SymNameStr1 = symbol_name_to_string(Qualifier, SymNameStr0),
	SymNameStr = SymNameStr1 ++ "__" ++ SymName.


:- func make_pred_name_string(mlds__pred_label, proc_id,
		maybe(mlds__func_sequence_num)) = string.
:- mode make_pred_name_string(in, in, in) = out is det.

make_pred_name_string(PredLabel, ProcId, MaybeSeqNum) = NameStr :-
	PredLabelStr = pred_label_string(PredLabel),
	proc_id_to_int(ProcId, ModeNum),
	NameStr0 = PredLabelStr ++ "_" ++ string__int_to_string(ModeNum),
	( MaybeSeqNum = yes(SeqNum) ->
		NameStr = NameStr0 ++ "_" ++ string__int_to_string(SeqNum)
	;
		NameStr = NameStr0
	).


:- func pred_label_string(mlds__pred_label) = string.
:- mode pred_label_string(in) = out is det.

pred_label_string(pred(PredOrFunc, MaybeDefiningModule, Name, PredArity,
		_CodeModel, _NonOutputFunc)) = PredLabelStr :-
	( PredOrFunc = predicate, Suffix = "p", OrigArity = PredArity
	; PredOrFunc = function, Suffix = "f", OrigArity = PredArity - 1
	),
	llds_out__name_mangle(Name, MangledName),
	PredLabelStr0 = MangledName ++ "_" 
			++ string__int_to_string(OrigArity) ++ "_"
			++ Suffix,
	( MaybeDefiningModule = yes(DefiningModule) ->
		llds_out__sym_name_mangle(DefiningModule, MangledModuleName),
		PredLabelStr = PredLabelStr0 ++ "_in__" ++ MangledModuleName
	;
		PredLabelStr = PredLabelStr0
	).
pred_label_string(special_pred(PredName, MaybeTypeModule,
		TypeName, TypeArity)) = PredLabelStr :-
	llds_out__name_mangle(PredName, MangledPredName),
	llds_out__name_mangle(TypeName, MangledTypeName),
	PredLabelStr0 = MangledPredName ++ "__", 
	( MaybeTypeModule = yes(TypeModule) ->
		llds_out__sym_name_mangle(TypeModule, MangledModuleName),
		PredLabelStr1 = PredLabelStr0 ++ "__" ++ MangledModuleName
	;
		PredLabelStr1 = PredLabelStr0
	),
	PredLabelStr = PredLabelStr1 ++ MangledTypeName ++ "_" ++
			string__int_to_string(TypeArity).


	
%------------------------------------------------------------------------------
%
% Code to output the start and end of a source file. 
% 

:- pred output_src_start(indent, mercury_module_name, mlds__imports, 
	mlds__defns, io__state, io__state).

:- mode output_src_start(in, in, in, in, di, uo) is det.

output_src_start(Indent, ModuleName, Imports, Defns) -->
	{ JavaSafeModuleName = valid_module_name(ModuleName) },
	output_auto_gen_comment(ModuleName),
	indent_line(Indent),
	io__write_string("/* :- module "),
	prog_out__write_sym_name(JavaSafeModuleName),
	io__write_string(". */\n\n"),
	output_package_info(JavaSafeModuleName),	
	output_imports(Imports), 
	io__write_string("public class "),
	prog_out__write_sym_name(JavaSafeModuleName),
	io__write_string(" {\n"),
	maybe_write_main_driver(Indent + 1, JavaSafeModuleName, Defns).

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
	{ JavaSafeModuleName = valid_module_name(ModuleName) },
	io__write_string("}\n"),
	indent_line(Indent),
	io__write_string("// :- end_module "),
	prog_out__write_sym_name(JavaSafeModuleName),
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

	% Discriminated union which allows us to pass down the class name if 
	% a definition is a constructor; this is needed since the class name
	% is not available for a constructor in the mlds.
:- type ctor_data 
	--->	none				% not a constructor
	;	cname(mlds__entity_name)	% constructor class name
	.

:- pred output_defns(indent, mlds_module_name, ctor_data, mlds__defns,
		io__state, io__state).
:- mode output_defns(in, in, in, in, di, uo) is det.

output_defns(Indent, ModuleName, CtorData, Defns) -->
	{ OutputDefn = output_defn(Indent, ModuleName, CtorData) },
	list__foldl(OutputDefn, Defns).

:- pred output_defn(indent, mlds_module_name, ctor_data, mlds__defn,
		io__state, io__state).
:- mode output_defn(in, in, in, in, di, uo) is det.

output_defn(Indent, ModuleName, CtorData, Defn) -->
	{ Defn = mlds__defn(Name, Context, Flags, DefnBody) },
	indent_line(Context, Indent),
	output_decl_flags(Flags, Name),
	output_defn_body(Indent, qual(ModuleName, Name), CtorData, Context,
			DefnBody).

:- pred output_defn_body(indent, mlds__qualified_entity_name, ctor_data,
		mlds__context, mlds__entity_defn, io__state, io__state).
:- mode output_defn_body(in, in, in, in, in, di, uo) is det.

output_defn_body(_, Name, _, _, mlds__data(Type, Initializer, _GCTraceCode)) -->
	output_data_defn(Name, Type, Initializer).
output_defn_body(Indent, Name, CtorData, Context, 
		mlds__function(MaybePredProcId, Signature, MaybeBody,
		_Attributes)) -->
	output_maybe(MaybePredProcId, output_pred_proc_id),
	output_func(Indent, Name, CtorData, Context, Signature, MaybeBody).
output_defn_body(Indent, Name, _, Context, mlds__class(ClassDefn))
		-->
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
	( { Kind = mlds__interface } -> 
		io__write_string("interface ")
	;
		io__write_string("class ")
	),
	output_class_name(UnqualName), io__nl,
	output_extends_list(Indent + 1, BaseClasses),
	output_implements_list(Indent + 1, Implements),
	indent_line(Indent),
	io__write_string("{\n"),
	output_class_body(Indent + 1, Kind, Name, AllMembers, ModuleName),
	io__nl,
	output_defns(Indent + 1, ModuleName, cname(UnqualName),	Ctors),
	indent_line(Indent),
	io__write_string("}\n\n").

	% Output superclass that this class extends.  Java does
	% not support multiple inheritance, so more than one superclass
	% is an error.
	%
:- pred output_extends_list(indent, list(mlds__class_id), io__state, io__state).
:- mode output_extends_list(in, in, di, uo) is det.

output_extends_list(_, []) --> [].
output_extends_list(Indent, [SuperClass]) -->
	indent_line(Indent),
	io__write_string("extends "),
	output_type(SuperClass),
	io__nl.
output_extends_list(_, [_, _ | _]) -->
	{ unexpected(this_file, 
		"output_extends_list: multiple inheritance not supported in Java") }.

	% Output list of interfaces that this class implements.
	%
:- pred output_implements_list(indent, list(mlds__interface_id), 
		io__state, io__state).
:- mode output_implements_list(in, in, di, uo) is det.

output_implements_list(Indent, InterfaceList) --> 
	( { InterfaceList = [] }  ->
		[]
	;
		indent_line(Indent),
		io__write_string("implements "),
		io__write_list(InterfaceList, ",", output_interface) ,
		io__nl
	).

:- pred output_interface(mlds__interface_id, io__state, io__state).
:- mode output_interface(in, di, uo) is det.

output_interface(Interface) -->
	 ( { Interface = class_type(qual(ModuleQualifier, Name), Arity, _) } ->
		{ SymName = mlds_module_name_to_sym_name(ModuleQualifier) },	
		{ mangle_mlds_sym_name_for_java(SymName, ".", ModuleName) },
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
		mlds__qualified_entity_name, mlds__defns, mlds_module_name,
		io__state, io__state).
:- mode output_class_body(in, in, in, in, in, di, uo) is det.

output_class_body(Indent, mlds__class, _Name, AllMembers, Module)
		-->
	{ CtorData = none },  	% Not a constructor.
	output_defns(Indent, Module, CtorData, AllMembers).	

output_class_body(_Indent, mlds__package, _Name, _AllMembers, _) -->
	{ error("mlds_to_java.m: cannot use package as a type.") }.

output_class_body(Indent, mlds__interface, _, AllMembers, Module)
		--> 
	{ CtorData = none },  % Not a constructor.
	output_defns(Indent, Module, CtorData, AllMembers). 

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
		{ DefnBody = data(Type, Initializer, _GC_TraceCode) }
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

get_java_type_initializer(mercury_type(_, int_type, _)) = "0".
get_java_type_initializer(mercury_type(_, char_type, _)) = "0".
get_java_type_initializer(mercury_type(_, float_type, _)) = "0".
get_java_type_initializer(mercury_type(_, str_type, _)) = "null".
get_java_type_initializer(mercury_type(_, pred_type, _)) = "null".
get_java_type_initializer(mercury_type(_, tuple_type, _)) = "null".
get_java_type_initializer(mercury_type(_, enum_type, _)) = "null".
get_java_type_initializer(mercury_type(_, polymorphic_type, _)) = "null".
get_java_type_initializer(mercury_type(_, user_type, _)) = "null".
get_java_type_initializer(mlds__mercury_array_type(_)) = "null".
get_java_type_initializer(mlds__cont_type(_)) = "null".
get_java_type_initializer(mlds__commit_type) = "null".
get_java_type_initializer(mlds__native_bool_type) = "false".
get_java_type_initializer(mlds__native_int_type) = "0".
get_java_type_initializer(mlds__native_float_type) = "0".
get_java_type_initializer(mlds__native_char_type) = "0".
get_java_type_initializer(mlds__foreign_type(_)) = _ :-
	unexpected(this_file, 
		"get_type_initializer: variable has foreign_type"). 
get_java_type_initializer(mlds__class_type(_, _, _)) = "null".
get_java_type_initializer(mlds__array_type(_)) = "null".
get_java_type_initializer(mlds__ptr_type(_)) = "null".
get_java_type_initializer(mlds__func_type(_)) = "null".
get_java_type_initializer(mlds__generic_type) = "null".
get_java_type_initializer(mlds__generic_env_ptr_type) = "null".
get_java_type_initializer(mlds__type_info_type) = "null".
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


:- pred output_func(indent, qualified_entity_name, ctor_data, mlds__context,
		func_params, function_body, io__state, io__state).
:- mode output_func(in, in, in, in, in, in, di, uo) is det.

output_func(Indent, Name, CtorData, Context, Signature, MaybeBody)
		-->
	output_func_decl(Indent, Name, CtorData, Context, Signature),
	(
		{ MaybeBody = external },
		io__write_string(";\n")
	;
		{ MaybeBody = defined_here(Body) },
		io__write_string("\n"),
		indent_line(Context, Indent),
		io__write_string("{\n"),
		{ FuncInfo = func_info(Name, Signature) },
		output_statement(Indent + 1, FuncInfo, Body, _ExitMethods),
		indent_line(Context, Indent),
		io__write_string("}\n")	% end the function
	).

:- pred output_func_decl(indent, qualified_entity_name, ctor_data,
		mlds__context, func_params, io__state, io__state).
:- mode output_func_decl(in, in, in, in, in, di, uo) is det.

output_func_decl(Indent, QualifiedName, cname(CtorName), Context,
		Signature) -->
	{ Signature = mlds__func_params(Parameters, _RetTypes) },
	{ QualifiedName = qual(ModuleName, _Name) },
	output_name(CtorName),
	output_params(Indent, ModuleName, Context, Parameters).

output_func_decl(Indent, QualifiedName, none, Context, Signature) -->
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
		mlds__argument, io__state, io__state).
:- mode output_param(in, in, in, in, di, uo) is det.

output_param(Indent, ModuleName, Context, Arg) -->
	{ Arg = mlds__argument(Name, Type, _GC_TraceCode) },
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
		output_fully_qualified(QualifiedName, output_name, ".")
	).

:- pred output_fully_qualified_proc_label(mlds__qualified_proc_label, string,
		io__state, io__state).
:- mode output_fully_qualified_proc_label(in, in, di, uo) is det.

output_fully_qualified_proc_label(QualifiedName, Qualifier) -->
		output_fully_qualified(QualifiedName, mlds_output_proc_label,
				Qualifier).

:- pred output_fully_qualified(mlds__fully_qualified_name(T),
		pred(T, io__state, io__state), string, io__state, io__state).
:- mode output_fully_qualified(in, pred(in, di, uo) is det, in, di, uo) is det.

output_fully_qualified(qual(ModuleName, Name), OutputFunc, Qualifier) -->
	{ SymName = mlds_module_name_to_sym_name(ModuleName) },
	{ mangle_mlds_sym_name_for_java(SymName, Qualifier, 
			MangledModuleName) },
	% XXX Name mangling code should be put here when we start enforcing
	%     Java's naming convention.
	{ MangledModuleName = JavaMangledName},
	io__write_string(JavaMangledName),
	io__write_string(Qualifier),
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

output_pred_label(pred(PredOrFunc, MaybeDefiningModule, Name, PredArity,
		_CodeModel, _NonOutputFunc)) -->
	( { PredOrFunc = predicate, Suffix = "p", OrigArity = PredArity }
	; { PredOrFunc = function, Suffix = "f", OrigArity = PredArity - 1 }
	),
	{ llds_out__name_mangle(Name, MangledName) },
	io__format("%s_%d_%s", [s(MangledName), i(OrigArity), s(Suffix)]),
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
output_data_name(rtti(RttiTypeCtor, RttiName)) -->
	{ rtti__addr_to_string(RttiTypeCtor, RttiName, RttiAddrName) },
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

output_type(mercury_type(Type, TypeCategory, _)) -->
	output_mercury_type(Type, TypeCategory).

output_type(mercury_array_type(MLDSType)) -->
	output_type(MLDSType),
	io__write_string("[]").
output_type(mlds__native_int_type)   --> io__write_string("int").
output_type(mlds__native_float_type) --> io__write_string("double").
output_type(mlds__native_bool_type) --> io__write_string("boolean").
output_type(mlds__native_char_type)  --> io__write_string("char").
output_type(mlds__foreign_type(_))  -->
	{ unexpected(this_file, "output_type: foreign_type NYI.") }.
output_type(mlds__class_type(Name, Arity, ClassKind)) -->
	( { ClassKind = mlds__enum } ->
		output_fully_qualified(Name, output_mangled_name, "."),
		io__format("_%d", [i(Arity)])
	;
		output_fully_qualified(Name, output_mangled_name, "."),
		io__format("_%d", [i(Arity)])
	).
output_type(mlds__ptr_type(Type)) -->
	( { Type = mlds__class_type(Name, Arity, _Kind) } ->
		output_fully_qualified(Name, output_mangled_name, "."),
		io__format("_%d", [i(Arity)])
	;
		output_type(Type)
	).
output_type(mlds__array_type(Type)) -->
	output_type(Type),
	io__write_string("[]").
output_type(mlds__func_type(_FuncParams)) -->
	io__write_string("mercury.runtime.MethodPtr").
output_type(mlds__generic_type) -->
	io__write_string("java.lang.Object").	
output_type(mlds__generic_env_ptr_type) -->
	io__write_string("java.lang.Object").
output_type(mlds__type_info_type) -->
	io__write_string("mercury.runtime.TypeInfo").
output_type(mlds__pseudo_type_info_type) -->
	io__write_string("mercury.runtime.PseudoTypeInfo").
output_type(mlds__cont_type(_)) -->
	% XXX Should this actually be a class that extends MethodPtr? 
	io__write_string("mercury.runtime.MethodPtr").
output_type(mlds__commit_type) -->
	io__write_string("mercury.runtime.Commit").

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
		{ TypeCategory = pred_type },
		io__write_string("java.lang.Object[]")
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
	( { type_to_ctor_and_args(Type, TypeCtor, _ArgsTypes) } ->
		{ ml_gen_type_name(TypeCtor, ClassName, ClassArity) },
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

	% These types are used by many of the output_stmt style predicates to
	% return information about the statement's control flow,
	% i.e. about the different ways in which the statement can exit.
	% In general we only output the current statement if the previous 
	% statement could complete normally (fall through).
	% We keep a set of exit methods since some statements (like an
	% if-then-else) could potentially break, and also fall through. 
:- type exit_methods == set__set(exit_method).

:- type exit_method
	--->	can_break
	;	can_continue
	;	can_return
	;	can_throw
	;	can_fall_through	% Where the instruction can complete
					% normally and execution can continue
					% with the following statement.
	.


:- type func_info
	--->	func_info(mlds__qualified_entity_name, mlds__func_params).

:- pred output_statements(indent, func_info, list(mlds__statement),
		exit_methods, io__state, io__state).
:- mode output_statements(in, in, in, out, di, uo) is det.

output_statements(_, _, [], set__make_singleton_set(can_fall_through)) --> [].
output_statements(Indent, FuncInfo, [Statement|Statements], ExitMethods) -->
	output_statement(Indent, FuncInfo, Statement, StmtExitMethods),
	( { set__member(can_fall_through, StmtExitMethods) } ->
		output_statements(Indent, FuncInfo, Statements,
				StmtsExitMethods),
		{ ExitMethods0 = StmtExitMethods `set__union`
				StmtsExitMethods },
		( { set__member(can_fall_through, StmtsExitMethods) } ->
			{ ExitMethods = ExitMethods0 }
		;
			% If the last statement could not complete normally
			% the current block can no longer complete normally.
			{ ExitMethods = ExitMethods0 `set__delete`
					can_fall_through }
		)
	;
		% Don't output any more statements from the current list since
		% the preceeding statement cannot complete.
		{ ExitMethods = StmtExitMethods }
	).

:- pred output_statement(indent, func_info, mlds__statement, exit_methods,
		io__state, io__state).
:- mode output_statement(in, in, in, out, di, uo) is det.

output_statement(Indent, FuncInfo, mlds__statement(Statement, Context),
		ExitMethods) -->
	output_context(Context),
	output_stmt(Indent, FuncInfo, Statement, Context, ExitMethods).

:- pred output_stmt(indent, func_info, mlds__stmt, mlds__context, exit_methods,
		io__state, io__state).
:- mode output_stmt(in, in, in, in, out, di, uo) is det.

	%
	% sequence
	%
output_stmt(Indent, FuncInfo, block(Defns, Statements), Context,
		ExitMethods) -->
	indent_line(Indent),
	io__write_string("{\n"),
	( { Defns \= [] } ->
		{ FuncInfo = func_info(FuncName, _) },
		{ FuncName = qual(ModuleName, _) },
		{ CtorData = none },  % Not a constructor.
		output_defns(Indent + 1, ModuleName, CtorData, Defns),
		io__write_string("\n")
	;
		[]
	),
	output_statements(Indent + 1, FuncInfo, Statements, ExitMethods),
	indent_line(Context, Indent),
	io__write_string("}\n").

	%
	% iteration
	%
output_stmt(Indent, FuncInfo, while(Cond, Statement, no), _, ExitMethods) -->
	indent_line(Indent),
	io__write_string("while ("),
	output_rval(Cond),
	io__write_string(")\n"),
	% The contained statement is reachable iff the while statement is 
	% reachable and the condition expression is not a constant expression
	% whose value is false.
	( { Cond = const(false) } ->
		indent_line(Indent),
		io__write_string("{  /* Unreachable code */  }\n"),
		{ ExitMethods = set__make_singleton_set(can_fall_through) }
	;	
		output_statement(Indent + 1, FuncInfo, Statement,
				StmtExitMethods),
		{ ExitMethods = while_exit_methods(Cond, StmtExitMethods) }
	).
output_stmt(Indent, FuncInfo, while(Cond, Statement, yes), Context, 
		ExitMethods) -->
	indent_line(Indent),
	io__write_string("do\n"),
	output_statement(Indent + 1, FuncInfo, Statement, StmtExitMethods),
	indent_line(Context, Indent),
	io__write_string("while ("),
	output_rval(Cond),
	io__write_string(");\n"),
	{ ExitMethods = while_exit_methods(Cond, StmtExitMethods) }.


	% Returns a set of exit_methods that describes whether the while 
	% statement can complete normally.
:- func while_exit_methods(mlds__rval, exit_methods) = exit_methods.
:- mode while_exit_methods(in, in) = out is det.

while_exit_methods(Cond, BlockExitMethods) = ExitMethods :-
	% A while statement cannot complete normally if its condition
	% expression is a constant expression with value true, and it
	% doesn't contain a reachable break statement that exits the
	% while statement.
	(
		% XXX This is not a sufficient way of testing for a Java 
		%     "constant expression", though determining these
		%     accurately is a little difficult to do here.
		Cond = mlds__const(mlds__true),
		not set__member(can_break, BlockExitMethods)
	->
		% Cannot complete normally
		ExitMethods0 = BlockExitMethods `set__delete` can_fall_through
	;
		ExitMethods0 = BlockExitMethods `set__insert` can_fall_through
	),
	ExitMethods = (ExitMethods0 `set__delete` can_continue)
				    `set__delete` can_break.


	%
	% selection (if-then-else)
	%
output_stmt(Indent, FuncInfo, if_then_else(Cond, Then0, MaybeElse),
		Context, ExitMethods) -->
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
	output_statement(Indent + 1, FuncInfo, Then, ThenExitMethods),
	( { MaybeElse = yes(Else) } ->
		indent_line(Context, Indent),
		io__write_string("else\n"),
		output_statement(Indent + 1, FuncInfo, Else, ElseExitMethods),
		% An if-then-else statement can complete normally iff the 
		% then-statement can complete normally or the else-statement
		% can complete normally.
		{ ExitMethods = ThenExitMethods `set__union` ElseExitMethods }
	;
		% An if-then statement can complete normally iff it is 
		% reachable.
		{ ExitMethods = ThenExitMethods `set__union` 
				set__make_singleton_set(can_fall_through) }
	).
	
	

	%
	% selection (switch)
	%
output_stmt(Indent, FuncInfo, switch(_Type, Val, _Range, Cases, Default),
		Context, ExitMethods) -->
	indent_line(Context, Indent),
	io__write_string("switch ("),
	output_rval_maybe_with_enum(Val),
	io__write_string(") {\n"),
	output_switch_cases(Indent + 1, FuncInfo, Context, Cases, Default,
			ExitMethods),
	indent_line(Context, Indent),
	io__write_string("}\n").


	%
	% transfer of control
	% 
output_stmt(_Indent, _FuncInfo, label(_LabelName), _Context, _ExitMethods) --> 
	{ unexpected(this_file, 
		"output_stmt: labels not supported in Java.") }.
output_stmt(_Indent, _FuncInfo, goto(label(_LabelName)), _Context,
		_ExitMethods) --> 
	{ unexpected(this_file,
		"output_stmt: gotos not supported in Java.") }.
output_stmt(Indent, _FuncInfo, goto(break), _Context, ExitMethods) --> 
	indent_line(Indent),
	io__write_string("break;\n"),
	{ ExitMethods = set__make_singleton_set(can_break) }.
output_stmt(Indent, _FuncInfo, goto(continue), _Context, ExitMethods) --> 
	indent_line(Indent),
	io__write_string("continue;\n"),
	{ ExitMethods = set__make_singleton_set(can_continue) }.
output_stmt(_Indent, _FuncInfo, computed_goto(_Expr, _Labels), _Context,
		_ExitMethods) --> 
	{ unexpected(this_file, 
		"output_stmt: computed gotos not supported in Java.") }.
	
	%
	% function call/return
	%
output_stmt(Indent, CallerFuncInfo, Call, Context, ExitMethods) -->
	{ Call = call(Signature, FuncRval, MaybeObject, CallArgs,
		Results, _IsTailCall) },
	{ CallerFuncInfo = func_info(_Name, _Params) },
	{ Signature = mlds__func_signature(ArgTypes, RetTypes) },
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
	( { FuncRval = const(code_addr_const(_)) } -> 
		% This is a standard method call.
		( { MaybeObject = yes(Object) } ->
			output_bracketed_rval(Object),
			io__write_string(".")
		;
			[]
		),
		% This is a standard function call:
		% 
		output_call_rval(FuncRval),
		io__write_string("("),
		io__write_list(CallArgs, ", ", output_rval),
		io__write_string(")")
	;
		% This is a call using a method pointer.
		%
		% Here we do downcasting, as a call will always return
		% something of type java.lang.Object
		%
		% XXX This is a hack, I can't see any way to do this
		%     downcasting nicely, as it needs to effectively be
		%     wrapped around the method call itself, so it acts
		%     before this predicate's solution to multiple return
		%     values, see above.
		% 
		( { RetTypes = [] } ->
			[]
		; { RetTypes = [RetType] } ->
			( 
				{ java_builtin_type(RetType, _JavaName,
						JavaBoxedName, 	_UnboxMethod) }
			->
				io__write_string("(("),
				io__write_string(JavaBoxedName),
				io__write_string(") ") 
			;
				io__write_string("(("),
				output_type(RetType),
				io__write_string(") ")
			)
		;
			io__write_string("((java.lang.Object[]) ")
		),	
		( { MaybeObject = yes(Object) } ->
			output_bracketed_rval(Object),
			io__write_string(".")
		;
			[]
		),
		output_bracketed_rval(FuncRval),
		io__write_string(".call___0_0("),
		%
		% We need to pass the arguments as a single array of 
		% java.lang.Object.
		%
		output_args_as_array(CallArgs, ArgTypes),
		%
		% Closes brackets, and calls unbox methods for downcasting.
		%
		% XXX This is a hack, see the above comment.
		% 
		io__write_string(")"),
		( { RetTypes = [] } ->
			[]
		; { RetTypes = [RetType2] } ->
			( 
				{ java_builtin_type(RetType2, _, _,
						UnboxMethod) }
			->
				io__write_string(")."),
				io__write_string(UnboxMethod),
				io__write_string("()")
			;
				io__write_string(")")
			)
		;
			io__write_string(")")
		)	
	),
	io__write_string(";\n"),

	( { Results = [_, _ | _] } ->
		% Copy the results from the "result" array into the Result
		% lvals (unboxing them as we go).
		output_assign_results(Results, RetTypes, 0, Indent + 1, Context)
	;
		[]
	),
	% XXX Is this needed? If present, it causes compiler errors for a
	%     couple of files in the benchmarks directory.  -mjwybrow
	%
	% ( { IsTailCall = tail_call, Results = [] } ->
	%	indent_line(Context, Indent + 1),
	%	io__write_string("return;\n")
	% ;
	%	[]
	% ),
	%
	indent_line(Indent),
	io__write_string("}\n"),
	{ ExitMethods = set__make_singleton_set(can_fall_through) }.


:- pred output_args_as_array(list(mlds__rval), list(mlds__type),
		io__state, io__state).
:- mode output_args_as_array(in, in, di, uo) is det.

output_args_as_array(CallArgs, CallArgTypes) -->
	io__write_string("new java.lang.Object[] { "),
	output_boxed_args(CallArgs, CallArgTypes),
	io__write_string("} ").


:- pred output_boxed_args(list(mlds__rval), list(mlds__type),
		io__state, io__state).
:- mode output_boxed_args(in, in, di, uo) is det.

output_boxed_args([], []) --> [].
output_boxed_args([_|_], []) -->
	{ error("output_boxed_args: length mismatch") }.
output_boxed_args([], [_|_]) -->
	{ error("output_boxed_args: length mismatch") }.
output_boxed_args([CallArg|CallArgs], [CallArgType|CallArgTypes]) -->
	output_boxed_rval(CallArgType, CallArg),
	( { CallArgs = [] } ->
		[]
	;
		io__write_string(", "),
		output_boxed_args(CallArgs, CallArgTypes)
	).


output_stmt(Indent, FuncInfo, return(Results0), _Context, ExitMethods) -->
	%
	% XXX It's not right to just remove the dummy variables like this,
	%     but currently they do not seem to be included in the ReturnTypes
	%     of func_params by the MLDS, so the easiest thing to do here is
	%     just remove them.
	%     
	%     When this is resolved, the right way to handle it would be to
	%     check for `dummy_var' in the `var' clause for output_lval, and
	%     output a reference to a static variable `dummy_var' defined in
	%     a fixed class (e.g. some class in the mercury/java directory,
	%     or mercury.private_builtin).
	% 
	{ Results = remove_dummy_vars(Results0) },
	( { Results = [] } ->
		indent_line(Indent),
		io__write_string("return;\n")
	; { Results = [Rval] } ->
		indent_line(Indent),
		io__write_string("return "),
		output_rval(Rval),
		io__write_string(";\n")
	;
		{ FuncInfo = func_info(_Name, Params) },
		{ Params = mlds__func_params(_Args, ReturnTypes) },
		{ TypesAndResults = assoc_list__from_corresponding_lists(
			ReturnTypes, Results) },
		io__write_string("return new java.lang.Object[] { "),
		io__write_list(TypesAndResults, ",\n ",
			(pred((Type - Result)::in, di, uo) is det -->
				output_boxed_rval(Type, Result))),
		io__write_string("};\n")
	),
	{ ExitMethods = set__make_singleton_set(can_return) }.

output_stmt(Indent, _FuncInfo, do_commit(Ref), _, ExitMethods) -->
	indent_line(Indent),
	output_rval(Ref),
	io__write_string(" = new mercury.runtime.Commit();\n"),
	indent_line(Indent),
	io__write_string("throw "),
	output_rval(Ref),
	io__write_string(";\n"),
	{ ExitMethods = set__make_singleton_set(can_throw) }.

output_stmt(Indent, FuncInfo, try_commit(_Ref, Stmt, Handler), _,
		ExitMethods) -->
	indent_line(Indent),
	io__write_string("try\n"),
	indent_line(Indent),
	io__write_string("{\n"),
	output_statement(Indent + 1, FuncInfo, Stmt, TryExitMethods0),
	indent_line(Indent),
	io__write_string("}\n"),
	indent_line(Indent),
	io__write_string("catch (mercury.runtime.Commit commit_variable)\n"),
	indent_line(Indent),
	io__write_string("{\n"),
	indent_line(Indent + 1),
	output_statement(Indent + 1, FuncInfo, Handler, CatchExitMethods),
	indent_line(Indent),
	io__write_string("}\n"),
	{ ExitMethods = (TryExitMethods0 `set__delete` can_throw)
				         `set__union`  CatchExitMethods }.



:- func remove_dummy_vars(list(mlds__rval)) = list(mlds__rval).
:- mode remove_dummy_vars(in) = out is det.

remove_dummy_vars([]) = [].
remove_dummy_vars([Var|Vars0]) = VarList :-
	Vars = remove_dummy_vars(Vars0),
	( 
   		Var = mlds__lval(Lval),
   		Lval = var(_VarName, VarType),
		VarType = mercury_type(ProgDataType, _, _),
   		type_util__is_dummy_argument_type(ProgDataType)
	->
		VarList = Vars
	;
		VarList = [Var|Vars]	
	).


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

:- pred output_switch_cases(indent, func_info, mlds__context,
		list(mlds__switch_case), mlds__switch_default, exit_methods,
		io__state, io__state).
:- mode output_switch_cases(in, in, in, in, in, out, di, uo) is det.

output_switch_cases(Indent, FuncInfo, Context, [], Default, ExitMethods) -->
	output_switch_default(Indent, FuncInfo, Context, Default, ExitMethods).
output_switch_cases(Indent, FuncInfo, Context, [Case|Cases], Default,
		ExitMethods) -->
	output_switch_case(Indent, FuncInfo, Context, Case, CaseExitMethods0),
	output_switch_cases(Indent, FuncInfo, Context, Cases, Default, 
			CasesExitMethods),
	( { set__member(can_break, CaseExitMethods0) } ->
		{ CaseExitMethods = (CaseExitMethods0 `set__delete` can_break)
				`set__insert` can_fall_through }
	;
		{ CaseExitMethods = CaseExitMethods0 }
	),
	{ ExitMethods = CaseExitMethods `set__union` CasesExitMethods }.


:- pred output_switch_case(indent, func_info, mlds__context,
		mlds__switch_case, exit_methods, io__state, io__state).
:- mode output_switch_case(in, in, in, in, out, di, uo) is det.

output_switch_case(Indent, FuncInfo, Context, Case, ExitMethods) -->
	{ Case = (Conds - Statement) },
	list__foldl(output_case_cond(Indent, Context), Conds),
	output_statement(Indent + 1, FuncInfo, Statement, StmtExitMethods),
	( { set__member(can_fall_through, StmtExitMethods) } ->
		indent_line(Context, Indent + 1),
		io__write_string("break;\n"),
		{ ExitMethods = (StmtExitMethods `set__insert` can_break)
				`set__delete` can_fall_through }
	;
		% Don't output `break' since it would be unreachable.
		{ ExitMethods = StmtExitMethods }
	).

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
		mlds__switch_default, exit_methods, io__state, io__state).
:- mode output_switch_default(in, in, in, in, out, di, uo) is det.

output_switch_default(_Indent, _FuncInfo, _Context, default_do_nothing,
		ExitMethods) -->
	{ ExitMethods = set__make_singleton_set(can_fall_through) }.
output_switch_default(Indent, FuncInfo, Context, default_case(Statement),
		ExitMethods) -->
	indent_line(Context, Indent),
	io__write_string("default:\n"),
	output_statement(Indent + 1, FuncInfo, Statement, ExitMethods).
output_switch_default(Indent, _FuncInfo, Context, default_is_unreachable,
		ExitMethods) -->
	indent_line(Context, Indent),
	io__write_string("default: /*NOTREACHED*/\n"), 
	indent_line(Context, Indent + 1),
	io__write_string("throw new mercury.runtime.UnreachableDefault();\n"),
	{ ExitMethods = set__make_singleton_set(can_throw) }.

%-----------------------------------------------------------------------------%

	%
	% exception handling
	%

	/* XXX not yet implemented */


	%
	% atomic statements
	%
output_stmt(Indent, FuncInfo, atomic(AtomicStatement), Context,
		ExitMethods) -->
	output_atomic_stmt(Indent, FuncInfo, AtomicStatement, Context),
	{ ExitMethods = set__make_singleton_set(can_fall_through) }.

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
	{ NewObject = new_object(Target, _MaybeTag, HasSecTag, Type,
		_MaybeSize, MaybeCtorName, Args, ArgTypes) },
	
	indent_line(Indent),
	io__write_string("{\n"),
	indent_line(Context, Indent + 1),
	output_lval(Target),
	io__write_string(" = new "),
	%
	% Generate class constructor name.
	%
	( { MaybeCtorName = yes(QualifiedCtorId) } ->
		output_type(Type),
		io__write_char('.'),
		{ QualifiedCtorId = qual(_ModuleName, CtorDefn) },
		{ CtorDefn = ctor_id(CtorName, CtorArity) },
		{ llds_out__name_mangle(CtorName, MangledCtorName) },
		io__format("%s_%d", [s(MangledCtorName), i(CtorArity)])
	;
		output_type(Type)
	),
	(
		{ Type = mlds__array_type(_Type)
		; Type = mlds__mercury_type(_Type, pred_type, _)
		} 
	->
		%
		% The new object will be an array of java.lang.Object, so we
		% need to initialise it using array literals syntax.
		%
		io__write_string(" {"),
		output_init_args(Args, ArgTypes, 0, HasSecTag),
		io__write_string("};\n") 
	;
		%
		% Generate constructor arguments.
		%
		io__write_string("("),
		output_init_args(Args, ArgTypes, 0, HasSecTag),
		io__write_string(");\n")
	),
	indent_line(Indent),
	io__write_string("}\n").
	

output_atomic_stmt(_Indent, _FuncInfo, gc_check, _) -->
	{ error("mlds_to_java.m: sorry, gc_check not implemented") }.

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

	% Output initial values of an object's fields as arguments for the
	% object's class constructor.
	%
:- pred output_init_args(list(mlds__rval), list(mlds__type), int, bool,
		io__state, io__state).
:- mode output_init_args(in, in, in, in, di, uo) is det.

output_init_args([], [], _, _) --> [].
output_init_args([_|_], [], _, _) -->
	{ error("output_init_args: length mismatch") }.
output_init_args([], [_|_], _i, _) -->
	{ error("output_init_args: length mismatch") }.
output_init_args([Arg|Args], [_ArgType|ArgTypes], ArgNum, HasSecTag) -->
	(
		{ ArgNum = 0 },
		{ HasSecTag = yes }
	->
		% This first argument is a `data_tag', It is set by
		% the class constructor so this argument can be discarded.
		[]
	;
		output_rval(Arg),
		( { Args = [] } ->
			[]
		;
			io__write_string(", ")
		)
	),
	output_init_args(Args, ArgTypes, ArgNum + 1, HasSecTag).


%-----------------------------------------------------------------------------%
%
% Code to output expressions
%

:- pred output_lval(mlds__lval, io__state, io__state).
:- mode output_lval(in, di, uo) is det.

output_lval(field(_MaybeTag, Rval, offset(OffsetRval), FieldType,
		_ClassType)) -->
	(
		{ FieldType = mlds__generic_type
		; FieldType = mlds__mercury_type(term__variable(_), _, _)
		}
	->
		[]
	;
		% The field type for field(_, _, offset(_), _, _) lvals
		% must be something that maps to MR_Box.
		{ error("unexpected field type") }
	),
	% XXX We shouldn't need this cast here, but there are cases where 
	%     it is needed and the MLDS doesn't seem to generate it.
	io__write_string("((java.lang.Object[]) "),
	output_rval(Rval),
	io__write_string(")["),
	output_rval(OffsetRval),
	io__write_string("]").


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


:- pred output_call_rval(mlds__rval, io__state, io__state).
:- mode output_call_rval(in, di, uo) is det.

output_call_rval(Rval) -->
	(
		{ Rval = mlds__const(Const),
		Const = mlds__code_addr_const(CodeAddr) }
	->
		{ IsCall = yes },
		mlds_output_code_addr(CodeAddr, IsCall)
	;
		output_bracketed_rval(Rval)
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
	Type = mlds__mercury_type(term__functor(term__atom("int"),
		[], _), _, _).
java_builtin_type(Type, "double", "java.lang.Double", "doubleValue") :-
	Type = mlds__native_float_type.
java_builtin_type(Type, "double", "java.lang.Double", "doubleValue") :-
	Type = mlds__mercury_type(term__functor(term__atom("float"),
		[], _), _, _).
java_builtin_type(Type, "char", "java.lang.Character", "charValue") :-
	Type = mlds__native_char_type.
java_builtin_type(Type, "char", "java.lang.Character", "charValue") :-
	Type = mlds__mercury_type(term__functor(term__atom("character"),
		[], _), _, _).
java_builtin_type(Type, "boolean", "java.lang.Boolean", "booleanValue") :-
	Type = mlds__native_bool_type.
	
	% io__state and store__store(S) are dummy variables 
	% for which we pass an arbitrary integer. For this
	% reason they should have the Java type `int'.
	% 
java_builtin_type(Type, "int", "java.lang.Integer", "intValue") :-
	Type = mlds__mercury_type(term__functor(term__atom(":"), _, _), _, _),
	Type = mlds__mercury_type(MercuryType, _, _),
	type_util__is_dummy_argument_type(MercuryType).

:- pred output_std_unop(builtin_ops__unary_op, mlds__rval, 
		io__state, io__state).
:- mode output_std_unop(in, in, di, uo) is det.
	
	%
	% For the Java back-end, there are no tags,
	% so all the tagging operators are no-ops,
	% except for `tag', which always returns zero
	% (a tag of zero means there's no tag).
	%
output_std_unop(UnaryOp, Exprn) -->
	( { UnaryOp = tag } ->
		io__write_string("/* tag */  0")
	;	
		{ java_util__unary_prefix_op(UnaryOp, UnaryOpString) },
		io__write_string(UnaryOpString),
		io__write_string("("),
		output_rval(Exprn),
		io__write_string(")")
	).


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
	{ IsCall = no },
	mlds_output_code_addr(CodeAddr, IsCall).

output_rval_const(data_addr_const(DataAddr)) -->
	mlds_output_data_addr(DataAddr).

output_rval_const(null(_)) -->
       io__write_string("null").

%-----------------------------------------------------------------------------%

:- pred mlds_output_code_addr(mlds__code_addr, bool, io__state, io__state).
:- mode mlds_output_code_addr(in, in, di, uo) is det.

mlds_output_code_addr(proc(Label, _Sig), IsCall) --> 
	( { IsCall = no } ->
		%
		% Not a function call, so we are taking the address of the
		% wrapper for that function (method).
		% 
		io__write_string("new AddrOf__"),
		output_fully_qualified_proc_label(Label, "__"),
		io__write_string("_0()")
	;
		output_fully_qualified_proc_label(Label, ".")
	).
mlds_output_code_addr(internal(Label, SeqNum, _Sig), IsCall) -->
	( { IsCall = no } ->
		%
		% Not a function call, so we are taking the address of the
		% wrapper for that function (method).
		% 
		io__write_string("new AddrOf__"),
		output_fully_qualified_proc_label(Label, "__"),
		io__write_string("_"),
		io__write_int(SeqNum),
		io__write_string("_0()")
	;
		output_fully_qualified_proc_label(Label, "."),
		io__write_string("_"),
		io__write_int(SeqNum)
	).

:- pred mlds_output_proc_label(mlds__proc_label, io__state, io__state).
:- mode mlds_output_proc_label(in, di, uo) is det.

mlds_output_proc_label(PredLabel - ProcId) -->
	output_pred_label(PredLabel),
	{ proc_id_to_int(ProcId, ModeNum) },
	io__format("_%d", [i(ModeNum)]).

:- pred mlds_output_data_addr(mlds__data_addr, io__state, io__state).
:- mode mlds_output_data_addr(in, di, uo) is det.

mlds_output_data_addr(data_addr(ModuleQualifier, DataName)) -->
	{ SymName = mlds_module_name_to_sym_name(ModuleQualifier) },	
	{ mangle_mlds_sym_name_for_java(SymName, ".", ModuleName) },
	io__write_string(ModuleName),
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
