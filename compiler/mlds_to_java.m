%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% mlds_to_java - Convert MLDS to Java code.
% Main authors: juliensf, mjwybrow, fjh.
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
%	RTTI generation
%	handle foreign code written in Java
% 	Support for Java in Mmake and mmc --make (except for nested modules)
%
% TODO:
% 	Fix problem with type names and constructor names that are the same
% 		(Java does not allow the name of a nested class to be
% 		the same as its enclosing class)
%	Support nested modules
%		(The problem with current code generation scheme for nested
%		modules is that Java does not allow the name of a class to
%		be the same as the name of its enclosing package.)
% 	Support for Java in Mmake and mmc --make, for Mercury code using
%		nested modules.
%	Generate names of classes etc. correctly (mostly same as IL backend)
%	General code cleanup
%	handle static ground terms(?)
%	RTTI: XXX There are problems with the RTTI definitions for the Java
%		back-end.  Under the current system, the definitions are output
%		as static variables with static initializers, ordered so that
%		subdefinitions always appear before the definition which uses
%		them.  This is necessary because in Java, static initializers
%		are performed at runtime in textual order, and if a definition
%		relies on another static variable for its constructor but said
%		variable has not been initialized, then it is treated as `null'
%		by the JVM with no warning.
%		The problem with this approach is that it won't work for cyclic
%		definitions.  eg:
%			:- type foo ---> f(bar) ; g.
%			:- type bar ---> f2(foo) ; g2
%		At some point this should be changed so that initialization is
%		performed by 2 phases: first allocate all of the objects, then
%		fill in the fields.
%
%	support foreign_import_module for Java
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
%	transformation, preferably in a separate module. Unfortunately this
%	is not possible due to the fact that the back-end generates `break'
%	statements for cases in switches as they are output, meaning that we
%	can't remove them in a pass over the MLDS.
%
%-----------------------------------------------------------------------------%

:- module ml_backend__mlds_to_java.

:- interface.

:- import_module ml_backend__mlds.

:- import_module io.

:- pred mlds_to_java__output_mlds(mlds::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

	% XXX needed for c_util__output_quoted_string,
	%     c_util__output_quoted_multi_string, and
	%     c_util__make_float_literal.
:- import_module backend_libs__builtin_ops.
:- import_module backend_libs__c_util.
:- import_module backend_libs__export.	% for export__type_to_type_string
:- import_module backend_libs__foreign.
:- import_module backend_libs__name_mangle.
:- import_module backend_libs__rtti.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_pred.	% for pred_proc_id.
:- import_module hlds__passes_aux.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module mdbcomp__prim_data.
:- import_module ml_backend__java_util.
:- import_module ml_backend__ml_code_util. % for ml_gen_local_var_decl_flags.
:- import_module ml_backend__ml_type_gen.	% for ml_gen_type_name
:- import_module ml_backend__ml_util.
:- import_module ml_backend__rtti_to_mlds.	% for mlds_rtti_type_name.
:- import_module parse_tree__error_util.
:- import_module parse_tree__modules.       % for mercury_std_library_name.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_foreign.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_type.

:- import_module bool, int, char, string, library, list, map, set.
:- import_module assoc_list, term, std_util, require.

%-----------------------------------------------------------------------------%

mlds_to_java__output_mlds(MLDS, !IO) :-
	% Note that the Java file name that we use for modules in the
	% Mercury standard library do not include a "mercury." prefix;
	% that's why we don't call mercury_module_name_to_mlds here.
	ModuleName = mlds__get_module_name(MLDS),
	JavaSafeModuleName = valid_module_name(ModuleName),
	module_name_to_file_name(JavaSafeModuleName, ".java", yes,
		JavaSourceFile, !IO),
	Indent = 0,
	output_to_file(JavaSourceFile, output_java_src_file(Indent, MLDS),
		!IO).

%-----------------------------------------------------------------------------%
%
% Utility predicates for various purposes.
%

	% Succeeds iff the given qualified name is part of the standard
	% library (as listed in compiler/modules.m).
	%
:- pred qualified_name_is_stdlib(mercury_module_name::in) is semidet.

qualified_name_is_stdlib(unqualified(_)) :- fail.
qualified_name_is_stdlib(qualified(Module, Name)) :-
	 (
	 	mercury_std_library_module(Name),
		Module = unqualified("mercury")
	 ;
	 	qualified_name_is_stdlib(Module)
	 ).

	% Succeeds iff this definition is a data definition which
	% defines RTTI.
	%
:- pred defn_is_rtti_data(mlds__defn::in) is semidet.

defn_is_rtti_data(Defn) :-
	Defn = mlds__defn(_Name, _Context, _Flags, Body),
	Body = mlds__data(Type, _, _),
	Type = mlds__rtti_type(_).

	% Succeeds iff this type is a enumeration.
	%
:- pred type_is_enum(mlds__type::in) is semidet.

type_is_enum(Type) :-
	Type = mercury_type(_, Builtin, _),
	Builtin = enum_type.

	%  Succeeds iff this type is something that
	%  the Java backend will represent as an object
	%  i.e. something created using the new operator.
	%
:- pred type_is_object(mlds__type::in) is semidet.

type_is_object(Type) :-
	Type = mercury_type(_, TypeCategory, _),
	type_category_is_object(TypeCategory) = yes.

:- func type_category_is_object(type_category) = bool.

type_category_is_object(int_type) = no.
type_category_is_object(char_type) = no.
type_category_is_object(str_type) = no.
type_category_is_object(float_type) = no.
type_category_is_object(higher_order_type) = no.
type_category_is_object(tuple_type) = no.
type_category_is_object(enum_type) = yes.
type_category_is_object(variable_type) = yes.
type_category_is_object(type_info_type) = yes.
type_category_is_object(type_ctor_info_type) = yes.
type_category_is_object(typeclass_info_type) = yes.
type_category_is_object(base_typeclass_info_type) = yes.
type_category_is_object(void_type) = no.
type_category_is_object(user_ctor_type) = yes.

	% Given an lval, return its type.
	%
:- func mlds_lval_type(mlds__lval) = mlds__type.

mlds_lval_type(var(_, VarType)) = VarType.
mlds_lval_type(field(_, _, _, FieldType, _)) = FieldType.
mlds_lval_type(mem_ref(_, PtrType)) =
	( PtrType = mlds__ptr_type(Type) ->
		Type
	;
		func_error("mlds_lval_type: mem_ref of non-pointer")
	).

	% Succeeds iff the Rval represents an integer constant.
	%
:- pred rval_is_int_const(mlds__rval::in) is semidet.

rval_is_int_const(Rval) :-
	Rval = const(int_const(_)).

	% Succeeds iff the Rval represents an enumeration
	% object in the Java backend.  We need to check both Rval's
	% that are variables and Rval's that are casts.
	% We need to know this in order to append the field name
	% to the object so we can access the value of the enumeration object.
	%
:- pred rval_is_enum_object(mlds__rval::in) is semidet.

rval_is_enum_object(Rval) :-
	Rval = lval(Lval),
	(
		Lval = var(_, VarType),
		type_is_enum(VarType)
	;
		Lval = field(_, _, _, FieldType, _),
		type_is_enum(FieldType)
	).

	% Succeeds iff a given string matches the unqualified
	% interface name of a interface in Mercury's Java runtime system.
	%
:- pred interface_is_special(string::in) is semidet.

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
% ever LLDS module it's hiding in and put in a separate one.
%

	% XXX This won't work if we start using the Java
	% coding conventions for all names.  At the moment
	% it only affects library modules.
	%
:- pred enforce_java_names(string::in, string::out) is det.

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

:- pred reverse_string(string::in, string::out) is det.

reverse_string(String0, String) :-
	string__to_char_list(String0, String1),
	string__from_rev_char_list(String1, String).

:- pred mangle_mlds_sym_name_for_java(sym_name::in, qual_kind::in, string::in,
	string::out) is det.

mangle_mlds_sym_name_for_java(unqualified(Name), QualKind, _QualifierOp,
		JavaSafeName) :-
	(
		QualKind = module_qual,
		FlippedName = Name
	;
		QualKind = type_qual,
		FlippedName = flip_initial_case(Name)
	),
	MangledName = name_mangle(FlippedName),
	JavaSafeName = valid_symbol_name(MangledName).
mangle_mlds_sym_name_for_java(qualified(ModuleName0, PlainName), QualKind,
		QualifierOp, JavaSafeName) :-
	mangle_mlds_sym_name_for_java(ModuleName0, module_qual, QualifierOp,
		MangledModuleName),
	(
		QualKind = module_qual,
		FlippedPlainName = PlainName
	;
		QualKind = type_qual,
		FlippedPlainName = flip_initial_case(PlainName)
	),
	MangledPlainName = name_mangle(FlippedPlainName),
	JavaSafePlainName = valid_symbol_name(MangledPlainName),
	string__append_list(
		[MangledModuleName, QualifierOp, JavaSafePlainName],
		JavaSafeName).

%-----------------------------------------------------------------------------%
%
% Name mangling code to fix problem of mercury modules having the same name
% as reserved Java words such as `char' and `int'.
%

	% If the given name conficts with a reserved Java word we must add a
	% prefix to it to avoid compilation errors.
:- func valid_symbol_name(string) = string.

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

:- type java_module_name == sym_name.

:- func valid_module_name(java_module_name) = java_module_name.

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

:- pred output_imports(mlds__imports::in, io::di, io::uo) is det.

output_imports(Imports, !IO) :-
	list__foldl(output_import, Imports, !IO),
	%
	% We should always import the mercury.runtime classes.
	%
	io__write_string("import mercury.runtime.*;\n\n", !IO).

:- pred output_import(mlds__import::in, io::di, io::uo) is det.

output_import(Import, !IO) :-
	% XXX Handle `:- pragma export' to Java.
	(
		Import = mercury_import(ImportType, ImportName),
		(
			ImportType = user_visible_interface,
			unexpected(this_file,
		"import_type `user_visible_interface' in Java backend")
		;
			ImportType = compiler_visible_interface
		)
	;
		Import = foreign_import(_),
		unexpected(this_file, "foreign import in Java backend")
	),
	SymName = mlds_module_name_to_sym_name(ImportName),
	JavaSafeSymName = valid_module_name(SymName),
	mdbcomp__prim_data__sym_name_to_string(JavaSafeSymName, ".", File),
	% XXX Name mangling code should be put here when we start enforcing
	%     Java's naming conventions.
	ClassFile = File,
	% There are issues related to using import statements and Java's
	% naming conventions.  To avoid these problems, we output
	% dependencies as comments only.  This is ok, since we always use
	% fully qualified names anyway.
	io__write_strings(["// import ", ClassFile, ";\n"], !IO).

%-----------------------------------------------------------------------------%
%
% Code to generate the `.java' file.
%

:- pred output_java_src_file(indent::in, mlds::in, io::di, io::uo) is det.

output_java_src_file(Indent, MLDS, !IO) :-
	%
	% Run further transformations on the MLDS.
	%
	MLDS = mlds(ModuleName, AllForeignCode, Imports, Defns0),
	MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
	%
	% Find and build list of all methods which would have their addresses
	% taken to be used as a function pointer.
	%
	find_pointer_addressed_methods(Defns0, [], CodeAddrs0),
	CodeAddrs = list__sort_and_remove_dups(CodeAddrs0),
	%
	% Create wrappers in MLDS for all pointer addressed methods.
	%
	generate_code_addr_wrappers(Indent + 1, CodeAddrs, [],
			WrapperDefns),
	Defns = WrapperDefns ++ Defns0,
	%
	% Get the foreign code for Java
	% XXX We should not ignore _RevImports and _ExportDefns
	%
	ForeignCode = mlds_get_java_foreign_code(AllForeignCode),
	ForeignCode = mlds__foreign_code(RevForeignDecls, _RevImports,
		RevBodyCode, _ExportDefns),
	ForeignDecls = list__reverse(RevForeignDecls),
	ForeignBodyCode = list__reverse(RevBodyCode),
	%
	% Output transformed MLDS as Java source.
	%
	% The order is important here, because Java requires static constants
	% be defined before they can be used in static initializers.
	% We start with the Java foreign code declarations, since for
	% library/private_builtin.m they contain static constants
	% that will get used in the RTTI definitions.
	%
	output_src_start(Indent, ModuleName, Imports, ForeignDecls, Defns,
		!IO),
	io__write_list(ForeignBodyCode, "\n", output_java_body_code(Indent),
		!IO),
	CtorData = none,  % Not a constructor.
	% XXX do we need to split this into RTTI and non-RTTI defns???
	list__filter(defn_is_rtti_data, Defns, RttiDefns, NonRttiDefns),
	output_defns(Indent + 1, MLDS_ModuleName, CtorData, RttiDefns, !IO),
	output_defns(Indent + 1, MLDS_ModuleName, CtorData, NonRttiDefns, !IO),
	output_src_end(Indent, ModuleName, !IO).
	% XXX Need to handle non-Java foreign code at this point.

%-----------------------------------------------------------------------------%
%
% Code for working with Java `foreign_code'.
%

:- pred output_java_decl(indent::in, foreign_decl_code::in, io::di, io::uo)
	is det.

output_java_decl(Indent, DeclCode, !IO) :-
	DeclCode = foreign_decl_code(Lang, _IsLocal, Code, Context),
	( Lang = java ->
		indent_line(make_context(Context), Indent, !IO),
		io__write_string(Code, !IO),
		io__nl(!IO)
	;
		sorry(this_file, "foreign code other than Java")
	).

:- pred output_java_body_code(indent::in, user_foreign_code::in, io::di,
	io__state::uo) is det.

output_java_body_code(Indent, user_foreign_code(Lang, Code, Context), !IO) :-
	% only output Java code
	( Lang = java ->
		indent_line(make_context(Context), Indent, !IO),
		io__write_string(Code, !IO),
		io__nl(!IO)
	;
		sorry(this_file, "foreign code other than Java")
	).

:- func mlds_get_java_foreign_code(map(foreign_language, mlds__foreign_code))
		= mlds__foreign_code.

	% Get the foreign code for Java
mlds_get_java_foreign_code(AllForeignCode) = ForeignCode :-
	( map__search(AllForeignCode, java, ForeignCode0) ->
		ForeignCode = ForeignCode0
	;
		ForeignCode = foreign_code([], [], [], [])
	).

%-----------------------------------------------------------------------------%
%
% Code to search MLDS for all uses of function pointers.
%

	% Returns code-address information (function label and signature)
	% for each method/function which has its address taken in the MLDS.
	%
:- pred find_pointer_addressed_methods(mlds__defns::in,
	list(mlds__code_addr)::in, list(mlds__code_addr)::out) is det.

find_pointer_addressed_methods([], !CodeAddrs).
find_pointer_addressed_methods([Defn | Defns], !CodeAddrs) :-
	Defn  = mlds__defn(_Name, _Context, _Flags, Body),
	method_ptrs_in_entity_defn(Body, !CodeAddrs),
	find_pointer_addressed_methods(Defns, !CodeAddrs).

:- pred method_ptrs_in_entity_defn(mlds__entity_defn::in,
	list(mlds__code_addr)::in, list(mlds__code_addr)::out) is det.

method_ptrs_in_entity_defn(mlds__function(_MaybeID, _Params, Body,
		_Attributes), !CodeAddrs) :-
	(
		Body = mlds__defined_here(Statement),
		method_ptrs_in_statement(Statement, !CodeAddrs)
	;
		Body = mlds__external
	).
method_ptrs_in_entity_defn(mlds__data(_Type, Initializer, _GC_TraceCode),
		!CodeAddrs) :-
	method_ptrs_in_initializer(Initializer, !CodeAddrs).
method_ptrs_in_entity_defn(mlds__class(ClassDefn), !CodeAddrs) :-
	ClassDefn = mlds__class_defn(_, _, _, _, Ctors, Members),
	method_ptrs_in_defns(Ctors, !CodeAddrs),
	method_ptrs_in_defns(Members, !CodeAddrs).

:- pred method_ptrs_in_statements(mlds__statements::in,
	list(mlds__code_addr)::in, list(mlds__code_addr)::out) is det.

method_ptrs_in_statements([], !CodeAddrs).
method_ptrs_in_statements([Statement | Statements], !CodeAddrs) :-
	method_ptrs_in_statement(Statement, !CodeAddrs),
	method_ptrs_in_statements(Statements, !CodeAddrs).

:- pred method_ptrs_in_statement(mlds__statement::in,
	list(mlds__code_addr)::in, list(mlds__code_addr)::out) is det.

method_ptrs_in_statement(mlds__statement(Stmt, _Context), !CodeAddrs) :-
	method_ptrs_in_stmt(Stmt, !CodeAddrs).

:- pred method_ptrs_in_stmt(mlds__stmt::in,
	list(mlds__code_addr)::in, list(mlds__code_addr)::out) is det.

method_ptrs_in_stmt(mlds__block(Defns, Statements), !CodeAddrs) :-
	method_ptrs_in_defns(Defns, !CodeAddrs),
	method_ptrs_in_statements(Statements, !CodeAddrs).
method_ptrs_in_stmt(mlds__while(Rval, Statement, _Bool), !CodeAddrs) :-
	method_ptrs_in_rval(Rval, !CodeAddrs),
	method_ptrs_in_statement(Statement, !CodeAddrs).
method_ptrs_in_stmt(mlds__if_then_else(Rval, StatementThen,
		MaybeStatementElse), !CodeAddrs) :-
	method_ptrs_in_rval(Rval, !CodeAddrs),
	method_ptrs_in_statement(StatementThen, !CodeAddrs),
	( MaybeStatementElse = yes(StatementElse) ->
		method_ptrs_in_statement(StatementElse, !CodeAddrs)
	; % MaybeStatementElse = no
		true
	).
method_ptrs_in_stmt(mlds__switch(_Type, Rval, _Range, Cases, Default),
		!CodeAddrs) :-
	method_ptrs_in_rval(Rval, !CodeAddrs),
	method_ptrs_in_switch_cases(Cases, !CodeAddrs),
	method_ptrs_in_switch_default(Default, !CodeAddrs).
method_ptrs_in_stmt(mlds__label(_), _, _) :-
	unexpected(this_file,
		"method_ptrs_in_stmt: labels not supported in Java.").
method_ptrs_in_stmt(mlds__goto(break), !CodeAddrs).
method_ptrs_in_stmt(mlds__goto(continue), !CodeAddrs).
method_ptrs_in_stmt(mlds__goto(label(_)), _, _) :-
	unexpected(this_file,
		"method_ptrs_in_stmt: goto label not supported in Java.").
method_ptrs_in_stmt(mlds__computed_goto(_, _), _, _) :-
	unexpected(this_file,
		"method_ptrs_in_stmt: computed gotos not supported in Java.").
method_ptrs_in_stmt(mlds__try_commit(_Lval, StatementGoal,
		StatementHandler), !CodeAddrs) :-
	% We don't check "_Lval" here as we expect it to be a local variable
	% of type mlds__commit_type.
	method_ptrs_in_statement(StatementGoal, !CodeAddrs),
	method_ptrs_in_statement(StatementHandler, !CodeAddrs).
method_ptrs_in_stmt(mlds__do_commit(_Rval), !CodeAddrs).
	% We don't check "_Rval" here as we expect it to be a local variable
	% of type mlds__commit_type.
method_ptrs_in_stmt(mlds__return(Rvals), !CodeAddrs) :-
	method_ptrs_in_rvals(Rvals, !CodeAddrs).
method_ptrs_in_stmt(mlds__call(_FuncSig, _Rval, _MaybeThis, Rvals, _ReturnVars,
		_IsTailCall), !CodeAddrs) :-
	% We don't check "_Rval" - it may be a code address but is a
	% standard call rather than a function pointer use.
	method_ptrs_in_rvals(Rvals, !CodeAddrs).
method_ptrs_in_stmt(mlds__atomic(AtomicStatement), !CodeAddrs) :-
	(
		AtomicStatement = mlds__new_object(Lval, _MaybeTag, _Bool,
			_Type, _MemRval, _MaybeCtorName, Rvals, _Types)
	->
	 	% We don't need to check "_MemRval" since this just stores
		% the amount of memory needed for the new object.
		method_ptrs_in_lval(Lval, !CodeAddrs),
		method_ptrs_in_rvals(Rvals, !CodeAddrs)
	; AtomicStatement = mlds__assign(Lval, Rval) ->
		method_ptrs_in_lval(Lval, !CodeAddrs),
		method_ptrs_in_rval(Rval, !CodeAddrs)
	;
		true
	).

:- pred method_ptrs_in_switch_default(mlds__switch_default::in,
	list(mlds__code_addr)::in, list(mlds__code_addr)::out) is det.

method_ptrs_in_switch_default(mlds__default_is_unreachable, !CodeAddrs).
method_ptrs_in_switch_default(mlds__default_do_nothing, !CodeAddrs).
method_ptrs_in_switch_default(mlds__default_case(Statement), !CodeAddrs) :-
	method_ptrs_in_statement(Statement, !CodeAddrs).

:- pred method_ptrs_in_switch_cases(mlds__switch_cases::in,
	list(mlds__code_addr)::in, list(mlds__code_addr)::out) is det.

method_ptrs_in_switch_cases([], !CodeAddrs).
method_ptrs_in_switch_cases([Case | Cases], !CodeAddrs) :-
	Case = _Conditions - Statement,
	method_ptrs_in_statement(Statement, !CodeAddrs),
	method_ptrs_in_switch_cases(Cases, !CodeAddrs).

:- pred method_ptrs_in_defns(mlds__defns::in, list(mlds__code_addr)::in,
	list(mlds__code_addr)::out) is det.

method_ptrs_in_defns([], !CodeAddrs).
method_ptrs_in_defns([Defn | Defns], !CodeAddrs) :-
	method_ptrs_in_defn(Defn, !CodeAddrs),
	method_ptrs_in_defns(Defns, !CodeAddrs).

:- pred method_ptrs_in_defn(mlds__defn::in, list(mlds__code_addr)::in,
	list(mlds__code_addr)::out) is det.

method_ptrs_in_defn(mlds__defn(_Name, _Context, _Flags, Body), !CodeAddrs) :-
	method_ptrs_in_entity_defn(Body, !CodeAddrs).

:- pred method_ptrs_in_initializer(mlds__initializer::in,
	list(mlds__code_addr)::in, list(mlds__code_addr)::out) is det.

method_ptrs_in_initializer(mlds__no_initializer, !CodeAddrs).
method_ptrs_in_initializer(mlds__init_struct(_Type, Initializers),
		!CodeAddrs) :-
	method_ptrs_in_initializers(Initializers, !CodeAddrs).
method_ptrs_in_initializer(mlds__init_array(Initializers), !CodeAddrs) :-
	method_ptrs_in_initializers(Initializers, !CodeAddrs).
method_ptrs_in_initializer(mlds__init_obj(Rval), !CodeAddrs) :-
	method_ptrs_in_rval(Rval, !CodeAddrs).

:- pred method_ptrs_in_initializers(list(mlds__initializer)::in,
	list(mlds__code_addr)::in, list(mlds__code_addr)::out) is det.

method_ptrs_in_initializers([], !CodeAddrs).
method_ptrs_in_initializers([Initializer | Initializers], !CodeAddrs) :-
	method_ptrs_in_initializer(Initializer, !CodeAddrs),
	method_ptrs_in_initializers(Initializers, !CodeAddrs).

:- pred method_ptrs_in_rvals(list(mlds__rval)::in, list(mlds__code_addr)::in,
	list(mlds__code_addr)::out) is det.

method_ptrs_in_rvals([], !CodeAddrs).
method_ptrs_in_rvals([Rval | Rvals], !CodeAddrs) :-
	method_ptrs_in_rval(Rval, !CodeAddrs),
	method_ptrs_in_rvals(Rvals, !CodeAddrs).

:- pred method_ptrs_in_rval(mlds__rval::in, list(mlds__code_addr)::in,
	list(mlds__code_addr)::out) is det.

method_ptrs_in_rval(mlds__lval(Lval), !CodeAddrs) :-
	method_ptrs_in_lval(Lval, !CodeAddrs).
method_ptrs_in_rval(mlds__mkword(_Tag, Rval), !CodeAddrs) :-
	method_ptrs_in_rval(Rval, !CodeAddrs).
method_ptrs_in_rval(mlds__const(RvalConst), !CodeAddrs) :-
	( RvalConst = mlds__code_addr_const(CodeAddr) ->
		!:CodeAddrs = !.CodeAddrs ++ [CodeAddr]
	;
		true
	).
method_ptrs_in_rval(mlds__unop(_UnaryOp, Rval), !CodeAddrs) :-
	method_ptrs_in_rval(Rval, !CodeAddrs).
method_ptrs_in_rval(mlds__binop(_BinaryOp, Rval1, Rval2), !CodeAddrs) :-
	method_ptrs_in_rval(Rval1, !CodeAddrs),
	method_ptrs_in_rval(Rval2, !CodeAddrs).
method_ptrs_in_rval(mlds__mem_addr(_Address), !CodeAddrs).
method_ptrs_in_rval(mlds__self(_Type), !CodeAddrs).

:- pred method_ptrs_in_lval(mlds__lval::in, list(mlds__code_addr)::in,
	list(mlds__code_addr)::out) is det.

	% Here, "_Rval" is the address of a variable so we don't check it.
method_ptrs_in_lval(mlds__mem_ref(_Rval, _Type), !CodeAddrs).
	% Here, "_Rval" is a pointer to a cell on the heap, and doesn't need
	% to be considered.
method_ptrs_in_lval(mlds__field(_MaybeTag, _Rval, _FieldId, _FieldType,
		_PtrType), !CodeAddrs).
method_ptrs_in_lval(mlds__var(_Variable, _Type), !CodeAddrs).

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
:- pred generate_code_addr_wrappers(indent::in, list(mlds__code_addr)::in,
	mlds__defns::in, mlds__defns::out) is det.

generate_code_addr_wrappers(_, [], !Defns).
generate_code_addr_wrappers(Indent, [CodeAddr | CodeAddrs], !Defns) :-
	%
	% XXX We should fill in the Context properly. This would probably
	%     involve also returning context information for each "code_addr"
	%     returned by the "method_ptrs_*" predicates above.
	%
	Context = mlds__make_context(term__context_init),
	InterfaceModuleName = mercury_module_name_to_mlds(
			qualified(unqualified("mercury"), "runtime")),
	Interface = qual(InterfaceModuleName, module_qual, "MethodPtr"),
	generate_addr_wrapper_class(Interface, Context, CodeAddr, ClassDefn),
	!:Defns = [ ClassDefn | !.Defns ],
	generate_code_addr_wrappers(Indent, CodeAddrs, !Defns).

	% Generate the MLDS wrapper class for a given code_addr.
:- pred generate_addr_wrapper_class(mlds__class::in,
	mlds__context::in, mlds__code_addr::in, mlds__defn::out) is det.

generate_addr_wrapper_class(Interface, Context, CodeAddr, ClassDefn) :-
	(
		CodeAddr = mlds__proc(ProcLabel, _FuncSig),
		MaybeSeqNum = no
	;
		CodeAddr = mlds__internal(ProcLabel, SeqNum, _FuncSig),
		MaybeSeqNum = yes(SeqNum)
	),
	ProcLabel = mlds__qual(ModuleQualifier, QualKind, PredLabel - ProcID),
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
	ModuleQualifierSym = mlds_module_name_to_sym_name(ModuleQualifier),
	mangle_mlds_sym_name_for_java(ModuleQualifierSym, QualKind, "__",
		ModuleNameStr),
	ClassEntityName = "addrOf__" ++ ModuleNameStr ++
		"__" ++ PredName,
	MangledClassEntityName = name_mangle(ClassEntityName),
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
:- pred generate_call_method(mlds__code_addr::in, mlds__defn::out) is det.

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
	ModuleName = ProcLabel ^ mod_name,
	PredID = hlds_pred__initial_pred_id,
	ProcID = initial_proc_id,
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
	ReturnVar = mlds__qual(ModuleName, module_qual, ReturnVarName),
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
	CallArgLabel = mlds__qual(ModuleName, module_qual, MethodArgVariable),
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

:- pred generate_call_method_args(list(mlds__type)::in, mlds__var::in, int::in,
	list(mlds__rval)::in, list(mlds__rval)::out) is det.

generate_call_method_args([], _, _, Args, Args).
generate_call_method_args([Type|Types], Variable, Counter, Args0, Args) :-
	ArrayRval = mlds__lval(mlds__var(Variable, mlds__native_int_type)),
	IndexRval = mlds__const(int_const(Counter)),
	Rval = binop(array_index(elem_type_generic), ArrayRval, IndexRval),
	UnBoxedRval = unop(unbox(Type), Rval),
	Args1 = Args0 ++ [UnBoxedRval],
	generate_call_method_args(Types, Variable, Counter + 1, Args1, Args).

:- func mlds_module_name_to_string(mlds__mlds_module_name) = string.

mlds_module_name_to_string(MldsModuleName) = ModuleNameStr :-
	ModuleName = mlds_module_name_to_sym_name(MldsModuleName),
	ModuleNameStr = symbol_name_to_string(ModuleName, "").

:- func symbol_name_to_string(sym_name, string) = string.

symbol_name_to_string(unqualified(SymName), SymNameStr0) = SymNameStr :-
	SymNameStr = SymNameStr0 ++ SymName.
symbol_name_to_string(qualified(Qualifier, SymName),
		SymNameStr0) = SymNameStr :-
	SymNameStr1 = symbol_name_to_string(Qualifier, SymNameStr0),
	SymNameStr = SymNameStr1 ++ "__" ++ SymName.

:- func make_pred_name_string(mlds__pred_label, proc_id,
	maybe(mlds__func_sequence_num)) = string.

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

pred_label_string(pred(PredOrFunc, MaybeDefiningModule, Name, PredArity,
		_CodeModel, _NonOutputFunc)) = PredLabelStr :-
	( PredOrFunc = predicate, Suffix = "p", OrigArity = PredArity
	; PredOrFunc = function, Suffix = "f", OrigArity = PredArity - 1
	),
	MangledName = name_mangle(Name),
	PredLabelStr0 = MangledName ++ "_"
			++ string__int_to_string(OrigArity) ++ "_"
			++ Suffix,
	( MaybeDefiningModule = yes(DefiningModule) ->
		MangledModuleName = sym_name_mangle(DefiningModule),
		PredLabelStr = PredLabelStr0 ++ "_in__" ++ MangledModuleName
	;
		PredLabelStr = PredLabelStr0
	).
pred_label_string(special_pred(PredName, MaybeTypeModule,
		TypeName, TypeArity)) = PredLabelStr :-
	MangledPredName = name_mangle(PredName),
	MangledTypeName = name_mangle(TypeName),
	PredLabelStr0 = MangledPredName ++ "__",
	( MaybeTypeModule = yes(TypeModule) ->
		MangledModuleName = sym_name_mangle(TypeModule),
		PredLabelStr1 = PredLabelStr0 ++ "__" ++ MangledModuleName
	;
		PredLabelStr1 = PredLabelStr0
	),
	PredLabelStr = PredLabelStr1 ++ MangledTypeName ++ "_" ++
		string__int_to_string(TypeArity).

%-----------------------------------------------------------------------------%
%
% Code to output the start and end of a source file.
%

:- pred output_src_start(indent::in, mercury_module_name::in,
	mlds__imports::in, list(foreign_decl_code)::in, mlds__defns::in,
	io::di, io::uo) is det.

output_src_start(Indent, MercuryModuleName, Imports, ForeignDecls, Defns,
		!IO) :-
	MLDSModuleName = mercury_module_name_to_mlds(MercuryModuleName),
	ModuleSymName = mlds_module_name_to_sym_name(MLDSModuleName),
	JavaSafeModuleName = valid_module_name(ModuleSymName),
	output_auto_gen_comment(MercuryModuleName, !IO),
	indent_line(Indent, !IO),
	io__write_string("/* :- module ", !IO),
	prog_out__write_sym_name(MercuryModuleName, !IO),
	io__write_string(". */\n\n", !IO),
	output_package_info(JavaSafeModuleName, !IO),
	output_imports(Imports, !IO),
	io__write_list(ForeignDecls, "\n", output_java_decl(Indent), !IO),
	io__write_string("public class ", !IO),
	unqualify_name(JavaSafeModuleName, ClassName),
	io__write_string(ClassName, !IO),
	io__write_string(" {\n", !IO),
	maybe_write_main_driver(Indent + 1, JavaSafeModuleName, Defns, !IO).

	% Output a `package' directive at the top of the Java source file,
	% if necessary.
	%
:- pred output_package_info(sym_name::in, io::di, io::uo) is det.

output_package_info(unqualified(_), !IO).
output_package_info(qualified(Module, _), !IO) :-
	io__write_string("package ", !IO),
	mdbcomp__prim_data__sym_name_to_string(Module, ".", Package),
	io__write_string(Package, !IO),
	io__write_string(";\n", !IO).

	% Check if this module contains a `main' predicate and if it does insert
	% a `main' method in the resulting Java class that calls the
	% `main' predicate. Save the command line arguments in the class
	% variable `args' in the class `mercury.runtime.JavaInternal'.
	%
:- pred maybe_write_main_driver(indent::in, java_module_name::in,
	mlds__defns::in, io::di, io::uo) is det.

maybe_write_main_driver(Indent, JavaSafeModuleName, Defns, !IO) :-
	( defns_contain_main(Defns) ->
		indent_line(Indent, !IO),
		io__write_string("public static void main", !IO),
		io__write_string("(java.lang.String[] args)\n", !IO),
		indent_line(Indent, !IO),
		io__write_string("{\n", !IO),
		%
		% Save the progname and command line arguments in the class
		% variables of `mercury.runtime.JavaInternal', as well as
		% setting the default exit status.
		%
		unqualify_name(JavaSafeModuleName, ClassName),
		indent_line(Indent + 1, !IO),
		io__write_string("mercury.runtime.JavaInternal.progname = """,
			!IO),
		io__write_string(ClassName, !IO),
		io__write_string(""";\n", !IO),
		indent_line(Indent + 1, !IO),
		io__write_string("mercury.runtime.JavaInternal.args = args;\n",
			!IO),
		indent_line(Indent + 1, !IO),
		io__write_string("mercury.runtime.JavaInternal.exit_status = ",
			!IO),
		io__write_string("0;\n", !IO),
		indent_line(Indent + 1, !IO),
		prog_out__write_sym_name(JavaSafeModuleName, !IO),
		io__write_string(".main_2_p_0();\n", !IO),
		indent_line(Indent + 1, !IO),
		io__write_string("java.lang.System.exit", !IO),
		io__write_string("(mercury.runtime.JavaInternal.exit_status);",
			!IO),
		io__nl(!IO),
		indent_line(Indent, !IO),
		io__write_string("}\n", !IO)
	;
		true
	),
	io__nl(!IO).

:- pred output_src_end(indent::in, mercury_module_name::in, io::di, io::uo)
	is det.

output_src_end(Indent, ModuleName, !IO) :-
	io__write_string("}\n", !IO),
	indent_line(Indent, !IO),
	io__write_string("// :- end_module ", !IO),
	prog_out__write_sym_name(ModuleName, !IO),
	io__write_string(".\n", !IO).

	% Output a Java comment saying that the file was automatically
	% generated and give details such as the compiler version.
	%
:- pred output_auto_gen_comment(mercury_module_name::in, io::di, io::uo) is det.

output_auto_gen_comment(ModuleName, !IO)  :-
	library__version(Version),
	module_name_to_file_name(ModuleName, ".m", no, SourceFileName, !IO),
	io__write_string("//\n//\n// Automatically generated from ", !IO),
	io__write_string(SourceFileName, !IO),
	io__write_string(" by the Mercury Compiler,\n", !IO),
	io__write_string("// version ", !IO),
	io__write_string(Version, !IO),
	io__nl(!IO),
	io__write_string("//\n", !IO),
	io__write_string("//\n", !IO),
	io__nl(!IO).

%-----------------------------------------------------------------------------%
%
% Code to output declarations and definitions.
%

	% Discriminated union which allows us to pass down the class name if
	% a definition is a constructor; this is needed since the class name
	% is not available for a constructor in the mlds.
:- type ctor_data
	--->	none				% not a constructor
	;	cname(mlds__entity_name).	% constructor class name

:- pred output_defns(indent::in, mlds_module_name::in, ctor_data::in,
	mlds__defns::in, io::di, io::uo) is det.

output_defns(Indent, ModuleName, CtorData, Defns, !IO) :-
	OutputDefn = output_defn(Indent, ModuleName, CtorData),
	list__foldl(OutputDefn, Defns, !IO).

:- pred output_defn(indent::in, mlds_module_name::in, ctor_data::in,
	mlds__defn::in, io::di, io::uo) is det.

output_defn(Indent, ModuleName, CtorData, Defn, !IO) :-
	Defn = mlds__defn(Name, Context, Flags, DefnBody),
	indent_line(Context, Indent, !IO),
	( DefnBody = mlds__function(_, _, external, _) ->
		% This is just a function declaration, with no body.
		% Java doesn't support separate declarations and definitions,
		% so just output the declaration as a comment.
		% (Note that the actual definition of an external procedure
		% must be given in `pragma java_code' in the same module.)
		io__write_string("/* external:\n", !IO),
		output_decl_flags(Flags, Name, !IO),
		output_defn_body(Indent, qual(ModuleName, module_qual, Name),
			CtorData, Context, DefnBody, !IO),
		io__write_string("*/\n", !IO)
	;
		output_decl_flags(Flags, Name, !IO),
		output_defn_body(Indent, qual(ModuleName, module_qual, Name),
			CtorData, Context, DefnBody, !IO)
	).

:- pred output_defn_body(indent::in, mlds__qualified_entity_name::in,
	ctor_data::in, mlds__context::in, mlds__entity_defn::in, io::di,
	io::uo) is det.

output_defn_body(_, Name, _, _, mlds__data(Type, Initializer, _), !IO) :-
	output_data_defn(Name, Type, Initializer, !IO).
output_defn_body(Indent, Name, CtorData, Context,
		mlds__function(MaybePredProcId, Signature, MaybeBody,
		_Attributes), !IO) :-
	output_maybe(MaybePredProcId, output_pred_proc_id, !IO),
	output_func(Indent, Name, CtorData, Context, Signature, MaybeBody,
		!IO).
output_defn_body(Indent, Name, _, Context, mlds__class(ClassDefn), !IO) :-
	output_class(Indent, Name, Context, ClassDefn, !IO).

%-----------------------------------------------------------------------------%
%
% Code to output classes.
%

:- pred output_class(indent::in, mlds__qualified_entity_name::in,
	mlds__context::in, mlds__class_defn::in, io::di, io::uo) is det.

output_class(Indent, Name, _Context, ClassDefn, !IO) :-
	Name = qual(ModuleName, _QualKind, UnqualName),
	( UnqualName = type(_, _) ->
		true
	;
		unexpected(this_file, "output_class")
	),
	ClassDefn = class_defn(Kind, _Imports, BaseClasses, Implements,
		Ctors, AllMembers),
	( Kind = mlds__interface ->
		io__write_string("interface ", !IO)
	;
		io__write_string("class ", !IO)
	),
	output_class_name_and_arity(UnqualName, !IO),
	io__nl(!IO),
	output_extends_list(Indent + 1, BaseClasses, !IO),
	output_implements_list(Indent + 1, Implements, !IO),
	indent_line(Indent, !IO),
	io__write_string("{\n", !IO),
	output_class_body(Indent + 1, Kind, Name, AllMembers, ModuleName, !IO),
	io__nl(!IO),
	output_defns(Indent + 1, ModuleName, cname(UnqualName),	Ctors, !IO),
	indent_line(Indent, !IO),
	io__write_string("}\n\n", !IO).

	% Output superclass that this class extends.  Java does
	% not support multiple inheritance, so more than one superclass
	% is an error.
	%
:- pred output_extends_list(indent::in, list(mlds__class_id)::in,
	io::di, io::uo) is det.

output_extends_list(_, [], !IO).
output_extends_list(Indent, [SuperClass], !IO) :-
	indent_line(Indent, !IO),
	io__write_string("extends ", !IO),
	output_type(SuperClass, !IO),
	io__nl(!IO).
output_extends_list(_, [_, _ | _], _, _) :-
	unexpected(this_file,
		"output_extends_list: multiple inheritance not supported in Java").

	% Output list of interfaces that this class implements.
	%
:- pred output_implements_list(indent::in, list(mlds__interface_id)::in,
	io::di, io::uo) is det.

output_implements_list(Indent, InterfaceList, !IO)  :-
	( InterfaceList = []  ->
		true
	;
		indent_line(Indent, !IO),
		io__write_string("implements ", !IO),
		io__write_list(InterfaceList, ",", output_interface, !IO),
		io__nl(!IO)
	).

:- pred output_interface(mlds__interface_id::in, io::di, io::uo) is det.

output_interface(Interface, !IO) :-
	(
		Interface = class_type(qual(ModuleQualifier, QualKind, Name),
	 		Arity, _)
	->
		SymName = mlds_module_name_to_sym_name(ModuleQualifier),
		mangle_mlds_sym_name_for_java(SymName, QualKind, ".",
			ModuleName),
		io__format("%s.%s", [s(ModuleName), s(Name)], !IO),
		%
		% Check if the interface is one of the ones in the runtime
		% system.  If it is we don't need to output the arity.
		%
		( interface_is_special(Name) ->
			true
		;
			io__format("%d", [i(Arity)], !IO)
		)
	;
		unexpected(this_file,
			"output_interface: interface was not a class.")
	).

:- pred output_class_body(indent::in, mlds__class_kind::in,
	mlds__qualified_entity_name::in, mlds__defns::in,
	mlds_module_name::in, io::di, io::uo) is det.

output_class_body(Indent, mlds__class, _, AllMembers, Module, !IO) :-
	CtorData = none,  	% Not a constructor.
	output_defns(Indent, Module, CtorData, AllMembers, !IO).

output_class_body(_Indent, mlds__package, _Name, _AllMembers, _, _, _) :-
	unexpected(this_file, "cannot use package as a type.").

output_class_body(Indent, mlds__interface, _, AllMembers, Module, !IO) :-
	CtorData = none,  % Not a constructor.
	output_defns(Indent, Module, CtorData, AllMembers, !IO).

output_class_body(_Indent, mlds__struct, _, _AllMembers, _, _, _) :-
	unexpected(this_file,
		"output_class_body: structs not supported in Java.").

output_class_body(Indent, mlds__enum, Name, AllMembers, _, !IO) :-
	list__filter(defn_is_const, AllMembers, EnumConsts),
	Name = qual(ModuleName, _QualKind, UnqualName),
	output_enum_constants(Indent + 1, ModuleName, EnumConsts, !IO),
	indent_line(Indent + 1, !IO),
	io__write_string("public int value;\n\n", !IO),
	output_enum_ctor(Indent + 1, UnqualName, !IO).

%-----------------------------------------------------------------------------%
%
% Additional code for generating enumerations
%
% Enumerations are a bit different from normal classes because although the
% ml code generator generates them as classes, it treats them as integers.
% Here we treat them as objects (instantiations of the classes) rather than
% just as integers.
%

:- pred defn_is_const(mlds__defn::in) is semidet.

defn_is_const(Defn) :-
	Defn = mlds__defn(_Name, _Context, Flags, _DefnBody),
	constness(Flags) = const.

	% Output a (Java) constructor for the class representing
	% the enumeration.
	%
:- pred output_enum_ctor(indent::in, mlds__entity_name::in, io::di, io::uo)
	is det.

output_enum_ctor(Indent, UnqualName, !IO) :-
	indent_line(Indent, !IO),
	io__write_string("public ", !IO),
	output_name(UnqualName, !IO),
	io__write_string("(int val) {\n", !IO),
	indent_line(Indent + 1, !IO),
	%
	% The use of `value' is hardcoded into ml_type_gen.m.  Any
	% changes there should probably be reflected here.
	%
	io__write_string("this.value = val;\n", !IO),
	indent_line(Indent + 1, !IO),
	io__write_string("return;\n", !IO),
	indent_line(Indent, !IO),
	io__write_string("}\n", !IO).

:- pred output_enum_constants(indent::in, mlds_module_name::in,
	mlds__defns::in, io::di, io::uo) is det.

output_enum_constants(Indent, EnumModuleName, EnumConsts, !IO) :-
	io__write_list(EnumConsts, "\n",
		output_enum_constant(Indent, EnumModuleName), !IO),
	io__nl(!IO).

:- pred output_enum_constant(indent::in, mlds_module_name::in,
	mlds__defn::in, io::di, io::uo) is det.

output_enum_constant(Indent, EnumModuleName, Defn, !IO) :-
	Defn = mlds__defn(Name, _Context, _Flags, DefnBody),
	(
		DefnBody = data(Type, Initializer, _GC_TraceCode)
	->
		indent_line(Indent, !IO),
		io__write_string("public static final int ", !IO),
		output_name(Name, !IO),
		output_initializer(EnumModuleName, Type, Initializer, !IO),
		io__write_char(';', !IO)
	;
		unexpected(this_file,
			"output_enum_constant: definition body was not data.")
	).

%-----------------------------------------------------------------------------%
%
% Code to output data declarations/definitions
%

:- pred output_data_decl(mlds__qualified_entity_name::in, mlds__type::in,
	io::di, io::uo) is det.

output_data_decl(qual(_, _, Name), Type, !IO) :-
	output_type(Type, !IO),
	io__write_char(' ', !IO),
	output_name(Name, !IO).

:- pred output_data_defn(mlds__qualified_entity_name::in, mlds__type::in,
	mlds__initializer::in, io::di, io::uo) is det.

output_data_defn(Name, Type, Initializer, !IO) :-
	output_data_decl(Name, Type, !IO),
	output_initializer(Name ^ mod_name, Type, Initializer, !IO),
	io__write_string(";\n", !IO).

	% We need to provide initializers for local variables
	% to avoid problems with Java's rules for definite assignment.
	% This mirrors the default Java initializers for class and
	% instance variables.
	%
:- func get_java_type_initializer(mlds__type) = string.

get_java_type_initializer(mercury_type(_, int_type, _)) = "0".
get_java_type_initializer(mercury_type(_, char_type, _)) = "0".
get_java_type_initializer(mercury_type(_, float_type, _)) = "0".
get_java_type_initializer(mercury_type(_, str_type, _)) = "null".
get_java_type_initializer(mercury_type(_, void_type, _)) = "0".
get_java_type_initializer(mercury_type(_, type_info_type, _)) = "null".
get_java_type_initializer(mercury_type(_, type_ctor_info_type, _)) = "null".
get_java_type_initializer(mercury_type(_, typeclass_info_type, _)) = "null".
get_java_type_initializer(mercury_type(_, base_typeclass_info_type, _))
	= "null".
get_java_type_initializer(mercury_type(_, higher_order_type, _)) = "null".
get_java_type_initializer(mercury_type(_, tuple_type, _)) = "null".
get_java_type_initializer(mercury_type(_, enum_type, _)) = "null".
get_java_type_initializer(mercury_type(_, variable_type, _)) = "null".
get_java_type_initializer(mercury_type(_, user_ctor_type, _)) = "null".
get_java_type_initializer(mlds__mercury_array_type(_)) = "null".
get_java_type_initializer(mlds__cont_type(_)) = "null".
get_java_type_initializer(mlds__commit_type) = "null".
get_java_type_initializer(mlds__native_bool_type) = "false".
get_java_type_initializer(mlds__native_int_type) = "0".
get_java_type_initializer(mlds__native_float_type) = "0".
get_java_type_initializer(mlds__native_char_type) = "0".
get_java_type_initializer(mlds__foreign_type(_)) = "null".
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

:- pred output_maybe(maybe(T)::in,
	pred(T, io, io)::pred(in, di, uo) is det, io::di,
	io::uo) is det.

output_maybe(MaybeValue, OutputAction, !IO) :-
	( MaybeValue = yes(Value) ->
		OutputAction(Value, !IO)
	;
		true
	).

:- pred output_initializer(mlds_module_name::in, mlds__type::in,
	mlds__initializer::in, io::di, io::uo) is det.

output_initializer(ModuleName, Type, Initializer, !IO) :-
	io__write_string(" = ", !IO),
	( needs_initialization(Initializer) = yes ->
		output_initializer_body(Initializer, yes(Type), ModuleName, !IO)
	;
		%
		% If we are not provided with an initializer we just,
		% supply the default java values -- note: this is strictly
		% only necessary for local variables, but it's not going
		% to hurt anything else.
		%
		io__write_string(get_java_type_initializer(Type), !IO)
	).

:- func needs_initialization(mlds__initializer) = bool.

needs_initialization(no_initializer) = no.
needs_initialization(init_obj(_)) = yes.
needs_initialization(init_struct(_Type, [])) = no.
needs_initialization(init_struct(_Type, [_|_])) = yes.
needs_initialization(init_array(_)) = yes.

:- pred output_initializer_body(mlds__initializer::in, maybe(mlds__type)::in,
	mlds_module_name::in, io::di, io::uo) is det.

output_initializer_body(no_initializer, _, _, _, _) :-
	unexpected(this_file, "output_initializer_body: no_initializer").
output_initializer_body(init_obj(Rval), MaybeType, ModuleName, !IO) :-
	(
		MaybeType = yes(Type),
		type_is_object(Type),
		rval_is_int_const(Rval)
	->
		%
		% If it is a enumeration object
		% create new object.
		%
		io__write_string("new ", !IO),
		output_type(Type, !IO),
		io__write_char('(', !IO),
		output_rval_maybe_with_enum(Rval, ModuleName, !IO),
		io__write_char(')', !IO)
	;
		MaybeType = yes(Type)
	->
		% If it is an non-enumeration
		% object, insert appropriate
		% cast.
		% XXX The logic of this is a bit
		% wrong.  Fixing it would eliminate
		% some of the unecessary casting
		% that happens
		%
		io__write_string("(", !IO),
		output_type(Type, !IO),
		io__write_string(") ", !IO),
		output_rval(Rval, ModuleName, !IO)
	;
		output_rval_maybe_with_enum(Rval, ModuleName, !IO)
	).

output_initializer_body(init_struct(StructType, FieldInits), _MaybeType,
		ModuleName, !IO) :-
	io__write_string("new ", !IO),
	output_type(StructType, !IO),
	IsArray = type_is_array(StructType),
	io__write_string(if IsArray = yes then " {" else "(", !IO),
	io__write_list(FieldInits, ",\n\t\t",
		(pred(FieldInit::in, !.IO::di, !:IO::uo) is det :-
			output_initializer_body(FieldInit, no, ModuleName,
				!IO)
		), !IO),
	io__write_char(if IsArray = yes then '}' else ')', !IO).
output_initializer_body(init_array(ElementInits), MaybeType, ModuleName, !IO) :-
	io__write_string("new ", !IO),
	( MaybeType = yes(Type) ->
		output_type(Type, !IO)
	;
		% XXX we need to know the type here
		io__write_string("/* XXX init_array */ Object[]", !IO)
	),
	io__write_string(" {\n\t\t", !IO),
	io__write_list(ElementInits, ",\n\t\t",
		(pred(ElementInit::in, !.IO::di, !:IO::uo) is det :-
			output_initializer_body(ElementInit, no, ModuleName,
				!IO)),
		!IO),
	io__write_string("}", !IO).

%-----------------------------------------------------------------------------%
%
% Code to output function declarations/definitions
%

:- pred output_pred_proc_id(pred_proc_id::in, io::di, io::uo) is det.

output_pred_proc_id(proc(PredId, ProcId), !IO) :-
	globals__io_lookup_bool_option(auto_comments, AddComments, !IO),
	( AddComments = yes ->
		io__write_string("// pred_id: ", !IO),
		pred_id_to_int(PredId, PredIdNum),
		io__write_int(PredIdNum, !IO),
		io__write_string(", proc_id: ", !IO),
		proc_id_to_int(ProcId, ProcIdNum),
		io__write_int(ProcIdNum, !IO),
		io__nl(!IO)
	;
		true
	).

:- pred output_func(indent::in, qualified_entity_name::in, ctor_data::in,
	mlds__context::in, func_params::in, function_body::in, io::di,
	io::uo) is det.

output_func(Indent, Name, CtorData, Context, Signature, MaybeBody, !IO) :-
	(
		MaybeBody = defined_here(Body),
		output_func_decl(Indent, Name, CtorData, Context, Signature,
			!IO),
		io__write_string("\n", !IO),
		indent_line(Context, Indent, !IO),
		io__write_string("{\n", !IO),
		FuncInfo = func_info(Name, Signature),
		output_statement(Indent + 1, FuncInfo, Body, _ExitMethods, !IO),
		indent_line(Context, Indent, !IO),
		io__write_string("}\n", !IO)	% end the function
	;
		MaybeBody = external
	).

:- pred output_func_decl(indent::in, qualified_entity_name::in, ctor_data::in,
	mlds__context::in, func_params::in, io::di, io::uo) is det.

output_func_decl(Indent, QualifiedName, cname(CtorName), Context, Signature,
		!IO) :-
	Signature = mlds__func_params(Parameters, _RetTypes),
	output_name(CtorName, !IO),
	output_params(Indent, QualifiedName ^ mod_name, Context, Parameters, !IO).

output_func_decl(Indent, QualifiedName, none, Context, Signature, !IO) :-
	Signature = mlds__func_params(Parameters, RetTypes),
	( RetTypes = [] ->
		io__write_string("void", !IO)
	; RetTypes = [RetType] ->
		output_type(RetType, !IO)
	;
		% for multiple outputs, we return an array of objects.
		io__write_string("java.lang.Object []", !IO)
	),
	io__write_char(' ', !IO),
	QualifiedName = qual(ModuleName, _QualKind, Name),
	output_name(Name, !IO),
	output_params(Indent, ModuleName, Context, Parameters, !IO).

:- pred output_params(indent::in, mlds_module_name::in, mlds__context::in,
	mlds__arguments::in, io::di, io::uo) is det.

output_params(Indent, ModuleName, Context, Parameters, !IO) :-
	io__write_char('(', !IO),
	( Parameters = [] ->
		true
	;
		io__nl(!IO),
		io__write_list(Parameters, ",\n",
			output_param(Indent + 1, ModuleName, Context),
			!IO)
	),
	io__write_char(')', !IO).

:- pred output_param(indent::in, mlds_module_name::in, mlds__context::in,
	mlds__argument::in, io::di, io::uo) is det.

output_param(Indent, _ModuleName, Context, Arg, !IO) :-
	Arg = mlds__argument(Name, Type, _GC_TraceCode),
	indent_line(Context, Indent, !IO),
	output_type(Type, !IO),
	io__write_char(' ', !IO),
	output_name(Name, !IO).

%-----------------------------------------------------------------------------%
%
% Code to output names of various entities
% XXX Much of the code in this section will not work when we
%     start enforcing names properly.

:- pred output_maybe_qualified_name(mlds__qualified_entity_name::in,
	mlds_module_name::in, io::di, io::uo) is det.

output_maybe_qualified_name(QualifiedName, CurrentModuleName, !IO) :-
	%
	% Don't module qualify names which are defined in the
	% current module.  This avoids unnecessary verbosity,
	% and is also necessary in the case of local variables
	% and function parameters, which must not be qualified.
	%
	QualifiedName = qual(ModuleName, _QualKind, Name),
	( ModuleName = CurrentModuleName ->
		output_name(Name, !IO)
	;
		output_fully_qualified(QualifiedName, output_name, ".", !IO)
	).

:- pred output_fully_qualified_name(mlds__qualified_entity_name::in,
	io::di, io::uo) is det.

output_fully_qualified_name(QualifiedName, !IO) :-
	output_fully_qualified(QualifiedName, output_name, ".", !IO).

:- pred output_fully_qualified_proc_label(mlds__qualified_proc_label::in,
	string::in, io::di, io::uo) is det.

output_fully_qualified_proc_label(QualifiedName, Qualifier, !IO) :-
	output_fully_qualified(QualifiedName, mlds_output_proc_label,
		Qualifier, !IO).

:- pred output_fully_qualified(mlds__fully_qualified_name(T)::in,
	pred(T, io, io)::pred(in, di, uo) is det, string::in, io::di,
	io::uo) is det.

output_fully_qualified(qual(ModuleName, QualKind, Name), OutputFunc,
		Qualifier, !IO) :-
	SymName = mlds_module_name_to_sym_name(ModuleName),
	mangle_mlds_sym_name_for_java(SymName, QualKind, Qualifier,
		MangledModuleName),
	io__write_string(MangledModuleName, !IO),
	io__write_string(Qualifier, !IO),
	OutputFunc(Name, !IO).

:- pred output_module_name(mercury_module_name::in, io::di, io::uo) is det.

output_module_name(ModuleName, !IO) :-
	io__write_string(sym_name_mangle(ModuleName), !IO).

:- pred output_class_name_and_arity(mlds__entity_name::in, io::di, io::uo)
	is det.

output_class_name_and_arity(type(Name, Arity)) -->
	output_class_name(Name),
	io__format("_%d", [i(Arity)]).
output_class_name_and_arity(data(_)) -->
	{ unexpected(this_file, "output_class_name_and_arity") }.
output_class_name_and_arity(function(_, _, _, _)) -->
	{ unexpected(this_file, "output_class_name_and_arity") }.
output_class_name_and_arity(export(_)) -->
	{ unexpected(this_file, "output_class_name_and_arity") }.

:- pred output_class_name(mlds__class_name::in, io::di, io::uo) is det.

output_class_name(Name, !IO) :-
	MangledName = name_mangle(Name),
	% By convention, class names should start with a capital letter.
	UppercaseMangledName = flip_initial_case(MangledName),
	io__write_string(UppercaseMangledName, !IO).

:- pred output_name(mlds__entity_name::in, io::di, io::uo) is det.

output_name(type(Name, Arity), !IO) :-
	output_class_name_and_arity(type(Name, Arity), !IO).
output_name(data(DataName), !IO) :-
	output_data_name(DataName, !IO).
output_name(function(PredLabel, ProcId, MaybeSeqNum, _PredId), !IO) :-
	output_pred_label(PredLabel, !IO),
	proc_id_to_int(ProcId, ModeNum),
	io__format("_%d", [i(ModeNum)], !IO),
	( MaybeSeqNum = yes(SeqNum) ->
		io__format("_%d", [i(SeqNum)], !IO)
	;
		true
	).
output_name(export(Name), !IO) :-
	io__write_string(Name, !IO).

:- pred output_pred_label(mlds__pred_label::in, io::di, io::uo) is det.

output_pred_label(pred(PredOrFunc, MaybeDefiningModule, Name,
		PredArity, _, _), !IO) :-
	( PredOrFunc = predicate, Suffix = "p", OrigArity = PredArity
	; PredOrFunc = function, Suffix = "f", OrigArity = PredArity - 1
	),
	MangledName = name_mangle(Name),
	io__format("%s_%d_%s", [s(MangledName), i(OrigArity), s(Suffix)], !IO),
	( MaybeDefiningModule = yes(DefiningModule) ->
		io__write_string("_in__", !IO),
		output_module_name(DefiningModule, !IO)
	;
		true
	).

output_pred_label(special_pred(PredName, MaybeTypeModule, TypeName,
		TypeArity), !IO) :-
	MangledPredName = name_mangle(PredName),
	MangledTypeName = name_mangle(TypeName),
	io__write_string(MangledPredName, !IO),
	io__write_string("__", !IO),
	( MaybeTypeModule = yes(TypeModule) ->
		output_module_name(TypeModule, !IO),
		io__write_string("__", !IO)
	;
		true
	),
	io__format("%s_%d", [s(MangledTypeName), i(TypeArity)], !IO).

:- pred output_data_name(mlds__data_name::in, io::di, io::uo) is det.

output_data_name(var(VarName), !IO) :-
	output_mlds_var_name(VarName, !IO).
output_data_name(common(Num), !IO) :-
	io__write_string("common_", !IO),
	io__write_int(Num, !IO).
output_data_name(rtti(RttiId), !IO) :-
	rtti__id_to_c_identifier(RttiId, RttiAddrName),
	io__write_string(RttiAddrName, !IO).
output_data_name(module_layout, !IO) :-
	unexpected(this_file, "NYI: module_layout").
output_data_name(proc_layout(_ProcLabel), !IO) :-
	unexpected(this_file, "NYI: proc_layout").
output_data_name(internal_layout(_ProcLabel, _FuncSeqNum), !IO) :-
	unexpected(this_file, "NYI: internal_layout").
output_data_name(tabling_pointer(ProcLabel), !IO) :-
	io__write_string("table_for_", !IO),
	mlds_output_proc_label(ProcLabel, !IO).

:- pred output_mlds_var_name(mlds__var_name::in, io::di, io::uo) is det.

output_mlds_var_name(var_name(Name, no), !IO) :-
	output_valid_mangled_name(Name, !IO).
output_mlds_var_name(var_name(Name, yes(Num)), !IO) :-
	output_mangled_name(string__format("%s_%d", [s(Name), i(Num)]), !IO).

%-----------------------------------------------------------------------------%
%
% Code to output types
%

:- pred output_type(mlds__type::in, io::di, io::uo) is det.

output_type(mercury_type(Type, TypeCategory, _), !IO) :-
	( Type = c_pointer_type ->
		% The c_pointer type is used in the c back-end as a
		% generic way to pass foreign types to automatically
		% generated Compare and Unify code.  When compiling to
		% Java we must instead use java.lang.Object.
		io__write_string("/* c_pointer */ java.lang.Object", !IO)
	;
		% We need to handle type_info (etc.) types
		% specially -- they get mapped to types in the
		% runtime rather than in private_builtin.
		hand_defined_type(TypeCategory, SubstituteName)
	->
		io__write_string(SubstituteName, !IO)
	;
		output_mercury_type(Type, TypeCategory, !IO)
	).

output_type(mercury_array_type(ElementType), !IO) :-
	( ElementType = mlds__mercury_type(_, variable_type, _) ->
		% We can't use `java.lang.Object []', since we want
		% a generic type that is capable of holding any kind
		% of array, including e.g. `int []'.
		% Java doesn't have any equivalent of .NET's System.Array
		% class, so we just use the universal base `java.lang.Object'.
		io__write_string("/* Array */ java.lang.Object", !IO)
	;
		output_type(ElementType, !IO),
		io__write_string("[]", !IO)
	).
output_type(mlds__native_int_type, !IO) :- io__write_string("int", !IO).
output_type(mlds__native_float_type, !IO) :- io__write_string("double", !IO).
output_type(mlds__native_bool_type, !IO) :- io__write_string("boolean", !IO).
output_type(mlds__native_char_type, !IO)  :- io__write_string("char", !IO).
output_type(mlds__foreign_type(ForeignType), !IO) :-
	(
		ForeignType = java(java(Name)),
		io__write_string(Name, !IO)
	;
		ForeignType = c(_),
		unexpected(this_file, "output_type: c foreign_type")
	;
		ForeignType = il(_),
		unexpected(this_file, "output_type: il foreign_type")
	).
output_type(mlds__class_type(Name, Arity, _ClassKind), !IO) :-
	% We used to treat enumerations specially here, outputting
	% them as "int", but now we do the same for all classes.
	output_fully_qualified(Name, output_class_name, ".", !IO),
	io__format("_%d", [i(Arity)], !IO).
output_type(mlds__ptr_type(Type), !IO) :-
	% XXX should we report an error here, if the type pointed to
	%     is not a class type?
	output_type(Type, !IO).
output_type(mlds__array_type(Type), !IO) :-
	output_type(Type, !IO),
	io__write_string("[]", !IO).
output_type(mlds__func_type(_FuncParams), !IO) :-
	io__write_string("mercury.runtime.MethodPtr", !IO).
output_type(mlds__generic_type, !IO) :-
	io__write_string("java.lang.Object", !IO).
output_type(mlds__generic_env_ptr_type, !IO) :-
	io__write_string("/* env_ptr */ java.lang.Object", !IO).
output_type(mlds__type_info_type, !IO) :-
	io__write_string("mercury.runtime.TypeInfo", !IO).
output_type(mlds__pseudo_type_info_type, !IO) :-
	io__write_string("mercury.runtime.PseudoTypeInfo", !IO).
output_type(mlds__cont_type(_), !IO) :-
	% XXX Should this actually be a class that extends MethodPtr?
	io__write_string("mercury.runtime.MethodPtr", !IO).
output_type(mlds__commit_type, !IO) :-
	io__write_string("mercury.runtime.Commit", !IO).
output_type(mlds__rtti_type(RttiIdMaybeElement), !IO) :-
	rtti_id_maybe_element_java_type(RttiIdMaybeElement, JavaTypeName,
		IsArray),
	io__write_string(JavaTypeName, !IO),
	( IsArray = yes ->
		io__write_string("[]", !IO)
	;
		true
	).
output_type(mlds__unknown_type, !IO) :-
	unexpected(this_file, "output_type: unknown type").


:- pred output_mercury_type(mercury_type::in, type_category::in,
	io::di, io::uo) is det.

output_mercury_type(Type, TypeCategory, !IO) :-
	(
		TypeCategory = char_type,
		io__write_string("char", !IO)
	;
		TypeCategory = int_type,
		io__write_string("int", !IO)
	;
		TypeCategory = str_type,
		io__write_string("java.lang.String", !IO)
	;
		TypeCategory = float_type,
		io__write_string("double", !IO)
	;
		TypeCategory = void_type,
		% Shouldn't matter what we put here.
		io__write_string("int", !IO)
	;
		TypeCategory = type_info_type,
		output_mercury_user_type(Type, user_ctor_type, !IO)
	;
		TypeCategory = type_ctor_info_type,
		output_mercury_user_type(Type, user_ctor_type, !IO)
	;
		TypeCategory = typeclass_info_type,
		output_mercury_user_type(Type, user_ctor_type, !IO)
	;
		TypeCategory = base_typeclass_info_type,
		output_mercury_user_type(Type, user_ctor_type, !IO)
	;
		TypeCategory = variable_type,
		io__write_string("java.lang.Object", !IO)
	;
		TypeCategory = tuple_type,
		io__write_string("/* tuple */ java.lang.Object[]", !IO)
	;
		TypeCategory = higher_order_type,
		io__write_string("/* closure */ java.lang.Object[]", !IO)
	;
		TypeCategory = enum_type,
		output_mercury_user_type(Type, TypeCategory, !IO)
	;
		TypeCategory = user_ctor_type,
		output_mercury_user_type(Type, TypeCategory, !IO)
	).

:- pred output_mercury_user_type(mercury_type::in, type_category::in,
	io::di, io::uo) is det.

output_mercury_user_type(Type, TypeCategory, !IO) :-
	( type_to_ctor_and_args(Type, TypeCtor, _ArgsTypes) ->
		ml_gen_type_name(TypeCtor, ClassName, ClassArity),
		( TypeCategory = enum_type ->
			MLDS_Type = mlds__class_type(ClassName,
				ClassArity, mlds__enum)
		;
			MLDS_Type = mlds__class_type(
				ClassName, ClassArity, mlds__class)
		),
		output_type(MLDS_Type, !IO)
	;
		unexpected(this_file,
			"output_mercury_user_type: not a user type")
	).

	% return yes if the corresponding Java type is an array type.
:- func type_is_array(mlds__type) = bool.

type_is_array(Type) = IsArray :-
	( Type = mlds__array_type(_) ->
		IsArray = yes
	; Type = mlds__mercury_array_type(_) ->
		IsArray = yes
	; Type = mercury_type(_, TypeCategory, _) ->
		IsArray = type_category_is_array(TypeCategory)
	; Type = mlds__rtti_type(RttiIdMaybeElement) ->
		rtti_id_maybe_element_java_type(RttiIdMaybeElement,
			_JavaTypeName, IsArray)
	;
		IsArray = no
	).

	% return yes if the corresponding Java type is an array type.
:- func type_category_is_array(type_category) = bool.

type_category_is_array(int_type) = no.
type_category_is_array(char_type) = no.
type_category_is_array(str_type) = no.
type_category_is_array(float_type) = no.
type_category_is_array(higher_order_type) = yes.
type_category_is_array(tuple_type) = yes.
type_category_is_array(enum_type) = no.
type_category_is_array(variable_type) = no.
type_category_is_array(type_info_type) = no.
type_category_is_array(type_ctor_info_type) = no.
type_category_is_array(typeclass_info_type) = yes.
type_category_is_array(base_typeclass_info_type) = yes.
type_category_is_array(void_type) = no.
type_category_is_array(user_ctor_type) = no.

	% We need to handle type_info (etc.) types
	% specially -- they get mapped to types in the
	% runtime rather than in private_builtin.
	%
	% hand_defined_type(Type, SubstituteName):
:- pred hand_defined_type(type_category::in, string::out) is semidet.

hand_defined_type(type_info_type, "mercury.runtime.TypeInfo_Struct").
hand_defined_type(type_ctor_info_type, "mercury.runtime.TypeCtorInfo_Struct").
hand_defined_type(base_typeclass_info_type,
	"/* base_typeclass_info */ java.lang.Object[]").
hand_defined_type(typeclass_info_type,
	"/* typeclass_info */ java.lang.Object[]").

%-----------------------------------------------------------------------------%
%
% Code to output declaration specifiers
%

:- pred output_decl_flags(mlds__decl_flags::in, mlds__entity_name::in,
	io::di, io::uo) is det.

output_decl_flags(Flags, _Name, !IO) :-
	output_access(access(Flags), !IO),
	output_per_instance(per_instance(Flags), !IO),
	output_virtuality(virtuality(Flags), !IO),
	output_finality(finality(Flags), !IO),
	output_constness(constness(Flags), !IO),
	output_abstractness(abstractness(Flags), !IO).

:- pred output_access(access::in, io::di, io::uo) is det.

output_access(public, !IO) :- io__write_string("public ", !IO).
output_access(private, !IO) :- io__write_string("private ", !IO).
output_access(protected, !IO) :-io__write_string("protected ", !IO).
output_access(default, !IO) :- maybe_output_comment("default", !IO).
output_access(local, !IO).

:- pred output_per_instance(per_instance::in, io::di, io::uo) is det.

output_per_instance(per_instance, !IO).
output_per_instance(one_copy, !IO) :- io__write_string("static ", !IO).

:- pred output_virtuality(virtuality::in, io::di, io::uo) is det.

output_virtuality(virtual, !IO) :- maybe_output_comment("virtual", !IO).
output_virtuality(non_virtual, !IO).

:- pred output_finality(finality::in, io::di, io::uo) is det.

output_finality(final, !IO) :- io__write_string("final ", !IO).
output_finality(overridable, !IO).

:- pred output_constness(constness::in, io::di, io::uo) is det.

output_constness(const, !IO) :- maybe_output_comment("const", !IO).
output_constness(modifiable, !IO).

:- pred output_abstractness(abstractness::in, io::di, io::uo) is det.

output_abstractness(abstract, !IO) :- io__write_string("abstract ", !IO).
output_abstractness(concrete, !IO).

:- pred maybe_output_comment(string::in, io::di, io::uo) is det.

maybe_output_comment(Comment, !IO) :-
	globals__io_lookup_bool_option(auto_comments, AddComments, !IO),
	( AddComments = yes ->
		io__write_string("/* ", !IO),
		io__write_string(Comment, !IO),
		io__write_string(" */", !IO)
	;
		true
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
	;	can_fall_through.	% Where the instruction can complete
					% normally and execution can continue
					% with the following statement.

:- type func_info
	--->	func_info(
			func_info_name :: mlds__qualified_entity_name,
			func_info_params :: mlds__func_params
	).

:- func mod_name(mlds__fully_qualified_name(T)) = mlds_module_name.

mod_name(qual(ModuleName, _, _)) = ModuleName.

:- pred output_statements(indent::in, func_info::in,
	list(mlds__statement)::in, exit_methods::out, io::di, io::uo) is det.

output_statements(_, _, [], set__make_singleton_set(can_fall_through), !IO).
output_statements(Indent, FuncInfo, [Statement | Statements], ExitMethods,
		!IO) :-
	output_statement(Indent, FuncInfo, Statement, StmtExitMethods, !IO),
	( set__member(can_fall_through, StmtExitMethods) ->
		output_statements(Indent, FuncInfo, Statements,
			StmtsExitMethods, !IO),
		ExitMethods0 = StmtExitMethods `set__union`
			StmtsExitMethods,
		( set__member(can_fall_through, StmtsExitMethods) ->
			ExitMethods = ExitMethods0
		;
			% If the last statement could not complete normally
			% the current block can no longer complete normally.
			ExitMethods = ExitMethods0 `set__delete`
				can_fall_through
		)
	;
		% Don't output any more statements from the current list since
		% the preceeding statement cannot complete.
		ExitMethods = StmtExitMethods
	).

:- pred output_statement(indent::in, func_info::in, mlds__statement::in,
	exit_methods::out, io::di, io::uo) is det.

output_statement(Indent, FuncInfo, mlds__statement(Statement, Context),
		ExitMethods, !IO) :-
	output_context(Context, !IO),
	output_stmt(Indent, FuncInfo, Statement, Context, ExitMethods, !IO).

:- pred output_stmt(indent::in, func_info::in, mlds__stmt::in,
	mlds__context::in, exit_methods::out, io::di, io::uo) is det.

	%
	% sequence
	%
output_stmt(Indent, FuncInfo, block(Defns, Statements), Context,
		ExitMethods, !IO) :-
	indent_line(Indent, !IO),
	io__write_string("{\n", !IO),
	( Defns \= [] ->
		ModuleName = FuncInfo ^ func_info_name ^ mod_name,
		CtorData = none,  % Not a constructor.
		output_defns(Indent + 1, ModuleName, CtorData, Defns, !IO),
		io__write_string("\n", !IO)
	;
		true
	),
	output_statements(Indent + 1, FuncInfo, Statements, ExitMethods, !IO),
	indent_line(Context, Indent, !IO),
	io__write_string("}\n", !IO).

	%
	% iteration
	%
output_stmt(Indent, FuncInfo, while(Cond, Statement, no), _, ExitMethods,
		!IO) :-
	indent_line(Indent, !IO),
	io__write_string("while (", !IO),
	output_rval(Cond, FuncInfo ^ func_info_name ^ mod_name, !IO),
	io__write_string(")\n", !IO),
	% The contained statement is reachable iff the while statement is
	% reachable and the condition expression is not a constant expression
	% whose value is false.
	( Cond = const(false) ->
		indent_line(Indent, !IO),
		io__write_string("{  /* Unreachable code */  }\n", !IO),
		ExitMethods = set__make_singleton_set(can_fall_through)
	;
		output_statement(Indent + 1, FuncInfo, Statement,
			StmtExitMethods, !IO),
		ExitMethods = while_exit_methods(Cond, StmtExitMethods)
	).
output_stmt(Indent, FuncInfo, while(Cond, Statement, yes), Context,
		ExitMethods, !IO) :-
	indent_line(Indent, !IO),
	io__write_string("do\n", !IO),
	output_statement(Indent + 1, FuncInfo, Statement, StmtExitMethods, !IO),
	indent_line(Context, Indent, !IO),
	io__write_string("while (", !IO),
	output_rval(Cond, FuncInfo ^ func_info_name ^ mod_name, !IO),
	io__write_string(");\n", !IO),
	ExitMethods = while_exit_methods(Cond, StmtExitMethods).

	%
	% selection (if-then-else)
	%
output_stmt(Indent, FuncInfo, if_then_else(Cond, Then0, MaybeElse),
		Context, ExitMethods, !IO) :-
	%
	% we need to take care to avoid problems caused by the
	% dangling else ambiguity
	%
	(
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
	),

	indent_line(Indent, !IO),
	io__write_string("if (", !IO),
	output_rval(Cond, FuncInfo ^ func_info_name ^ mod_name, !IO),
	io__write_string(")\n", !IO),
	output_statement(Indent + 1, FuncInfo, Then, ThenExitMethods, !IO),
	( MaybeElse = yes(Else) ->
		indent_line(Context, Indent, !IO),
		io__write_string("else\n", !IO),
		output_statement(Indent + 1, FuncInfo, Else, ElseExitMethods,
			!IO),
		% An if-then-else statement can complete normally iff the
		% then-statement can complete normally or the else-statement
		% can complete normally.
		ExitMethods = ThenExitMethods `set__union` ElseExitMethods
	;
		% An if-then statement can complete normally iff it is
		% reachable.
		ExitMethods = ThenExitMethods `set__union`
			set__make_singleton_set(can_fall_through)
	).

	%
	% selection (switch)
	%
output_stmt(Indent, FuncInfo, switch(_Type, Val, _Range, Cases, Default),
		Context, ExitMethods, !IO) :-
	indent_line(Context, Indent, !IO),
	io__write_string("switch (", !IO),
	output_rval_maybe_with_enum(Val, FuncInfo ^ func_info_name ^ mod_name,
		!IO),
	io__write_string(") {\n", !IO),
	output_switch_cases(Indent + 1, FuncInfo, Context, Cases, Default,
		ExitMethods, !IO),
	indent_line(Context, Indent, !IO),
	io__write_string("}\n", !IO).

	%
	% transfer of control
	%
output_stmt(_, _, label(_), _, _, _, _)  :-
	unexpected(this_file,
		"output_stmt: labels not supported in Java.").
output_stmt(_, _, goto(label(_)), _, _, _, _) :-
	unexpected(this_file,
		"output_stmt: gotos not supported in Java.").
output_stmt(Indent, _FuncInfo, goto(break), _Context, ExitMethods, !IO) :-
	indent_line(Indent, !IO),
	io__write_string("break;\n", !IO),
	ExitMethods = set__make_singleton_set(can_break).
output_stmt(Indent, _FuncInfo, goto(continue), _Context, ExitMethods, !IO)  :-
	indent_line(Indent, !IO),
	io__write_string("continue;\n", !IO),
	ExitMethods = set__make_singleton_set(can_continue).
output_stmt(_, _, computed_goto(_, _), _, _, _, _) :-
	unexpected(this_file,
		"output_stmt: computed gotos not supported in Java.").
	%
	% function call/return
	%
output_stmt(Indent, CallerFuncInfo, Call, Context, ExitMethods, !IO) :-
	Call = call(Signature, FuncRval, MaybeObject, CallArgs,
		Results, _IsTailCall),
	Signature = mlds__func_signature(ArgTypes, RetTypes),
	ModuleName = CallerFuncInfo ^ func_info_name ^ mod_name,
	indent_line(Indent, !IO),
	io__write_string("{\n", !IO),
	indent_line(Context, Indent + 1, !IO),
	( Results = [] ->
		true
	; Results = [Lval] ->
		output_lval(Lval, ModuleName, !IO),
		io__write_string(" = ", !IO)
	;
		% for multiple return values,
		% we generate the following code:
		%	{ java.lang.Object [] result = <func>(<args>);
		%	  <output1> = (<type1>) result[0];
		%	  <output2> = (<type2>) result[1];
		%	  ...
		%	}
		%
		io__write_string("java.lang.Object [] result = ", !IO)
	),
	( FuncRval = const(code_addr_const(_)) ->
		% This is a standard method call.
		( MaybeObject = yes(Object) ->
			output_bracketed_rval(Object, ModuleName, !IO),
			io__write_string(".", !IO)
		;
			true
		),
		% This is a standard function call:
		%
		output_call_rval(FuncRval, ModuleName, !IO),
		io__write_string("(", !IO),
		io__write_list(CallArgs, ", ",
			(pred(CallArg::in, !.IO::di, !:IO::uo) is det :-
				output_rval(CallArg, ModuleName, !IO)), !IO),
		io__write_string(")", !IO)
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
		( RetTypes = [] ->
			true
		; RetTypes = [RetType] ->
			( java_builtin_type(RetType, _, JavaBoxedName, _) ->
				io__write_string("((", !IO),
				io__write_string(JavaBoxedName, !IO),
				io__write_string(") ", !IO)
			;
				io__write_string("((", !IO),
				output_type(RetType, !IO),
				io__write_string(") ", !IO)
			)
		;
			io__write_string("((java.lang.Object[]) ", !IO)
		),
		( MaybeObject = yes(Object) ->
			output_bracketed_rval(Object, ModuleName, !IO),
			io__write_string(".", !IO)
		;
			true
		),
		output_bracketed_rval(FuncRval, ModuleName, !IO),
		io__write_string(".call___0_0(", !IO),
		%
		% We need to pass the arguments as a single array of
		% java.lang.Object.
		%
		output_args_as_array(CallArgs, ArgTypes, ModuleName, !IO),
		%
		% Closes brackets, and calls unbox methods for downcasting.
		%
		% XXX This is a hack, see the above comment.
		%
		io__write_string(")", !IO),
		( RetTypes = [] ->
			true
		; RetTypes = [RetType2] ->
			( java_builtin_type(RetType2, _, _, UnboxMethod) ->
				io__write_string(").", !IO),
				io__write_string(UnboxMethod, !IO),
				io__write_string("()", !IO)
			;
				io__write_string(")", !IO)
			)
		;
			io__write_string(")", !IO)
		)
	),
	io__write_string(";\n", !IO),

	( Results = [_, _ | _] ->
		% Copy the results from the "result" array into the Result
		% lvals (unboxing them as we go).
		output_assign_results(Results, RetTypes, 0, ModuleName,
			Indent + 1, Context, !IO)
	;
		true
	),
	% XXX Is this needed? If present, it causes compiler errors for a
	%     couple of files in the benchmarks directory.  -mjwybrow
	%
	% ( IsTailCall = tail_call, Results = [] ->
	%	indent_line(Context, Indent + 1, !IO),
	%	io__write_string("return;\n", !IO)
	% ;
	%	true
	% ),
	%
	indent_line(Indent, !IO),
	io__write_string("}\n", !IO),
	ExitMethods = set__make_singleton_set(can_fall_through).

output_stmt(Indent, FuncInfo, return(Results0), _, ExitMethods, !IO) :-
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
	Results = remove_dummy_vars(Results0),
	( Results = [] ->
		indent_line(Indent, !IO),
		io__write_string("return;\n", !IO)
	; Results = [Rval] ->
		indent_line(Indent, !IO),
		io__write_string("return ", !IO),
		output_rval(Rval, FuncInfo ^ func_info_name ^ mod_name, !IO),
		io__write_string(";\n", !IO)
	;
		FuncInfo = func_info(FuncName, Params),
		Params = mlds__func_params(_Args, ReturnTypes),
		TypesAndResults = assoc_list__from_corresponding_lists(
			ReturnTypes, Results),
		io__write_string("return new java.lang.Object[] {\n", !IO),
		indent_line(Indent + 1, !IO),
		Separator = ",\n" ++ duplicate_char(' ', (Indent + 1) * 2),
		io__write_list(TypesAndResults, Separator,
			(pred((Type - Result)::in, !.IO::di,
					!:IO::uo) is det :-
				output_boxed_rval(Type, Result,
					FuncName ^ mod_name, !IO)),
			!IO),
		io__write_string("\n", !IO),
		indent_line(Indent, !IO),
		io__write_string("};\n", !IO)
	),
	ExitMethods = set__make_singleton_set(can_return).

output_stmt(Indent, FuncInfo, do_commit(Ref), _, ExitMethods, !IO) :-
	indent_line(Indent, !IO),
	output_rval(Ref, FuncInfo ^ func_info_name ^ mod_name, !IO),
	io__write_string(" = new mercury.runtime.Commit();\n", !IO),
	indent_line(Indent, !IO),
	io__write_string("throw ", !IO),
	output_rval(Ref, FuncInfo ^ func_info_name ^ mod_name, !IO),
	io__write_string(";\n", !IO),
	ExitMethods = set__make_singleton_set(can_throw).

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
	%
	% exception handling
	%

	/* XXX not yet implemented */

	%
	% atomic statements
	%
output_stmt(Indent, FuncInfo, atomic(AtomicStatement), Context,
		ExitMethods, !IO) :-
	output_atomic_stmt(Indent, FuncInfo, AtomicStatement, Context, !IO),
	ExitMethods = set__make_singleton_set(can_fall_through).
	% Returns a set of exit_methods that describes whether the while
	% statement can complete normally.

%-----------------------------------------------------------------------------%
%
% Extra code for handling while-loops.
%

:- func while_exit_methods(mlds__rval, exit_methods) = exit_methods.

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

%-----------------------------------------------------------------------------%
%
% Extra code for handling function calls/returns.
%

:- pred output_args_as_array(list(mlds__rval)::in, list(mlds__type)::in,
	mlds_module_name::in, io::di, io::uo) is det.

output_args_as_array(CallArgs, CallArgTypes, ModuleName, !IO) :-
	io__write_string("new java.lang.Object[] { ", !IO),
	output_boxed_args(CallArgs, CallArgTypes, ModuleName, !IO),
	io__write_string("} ", !IO).

:- pred output_boxed_args(list(mlds__rval)::in, list(mlds__type)::in,
	mlds_module_name::in, io::di, io::uo) is det.

output_boxed_args([], [], _, !IO).
output_boxed_args([_|_], [], _, _, _) :-
	unexpected(this_file, "output_boxed_args: length mismatch.").
output_boxed_args([], [_|_], _, _, _) :-
	unexpected(this_file, "output_boxed_args: length mismatch.").
output_boxed_args([CallArg|CallArgs], [CallArgType | CallArgTypes],
		ModuleName, !IO) :-
	output_boxed_rval(CallArgType, CallArg, ModuleName, !IO),
	( CallArgs = [] ->
		true
	;
		io__write_string(", ", !IO),
		output_boxed_args(CallArgs, CallArgTypes, ModuleName, !IO)
	).

:- func remove_dummy_vars(list(mlds__rval)) = list(mlds__rval).

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
% Code for handling multiple return values.
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
:- pred output_assign_results(list(mlds__lval)::in, list(mlds__type)::in,
	int::in, mlds_module_name::in, indent::in, mlds__context::in, io::di,
	io::uo) is det.

output_assign_results([], [], _, _, _, _, !IO).
output_assign_results([Lval | Lvals], [Type | Types], ResultIndex, ModuleName,
		Indent, Context, !IO) :-
	indent_line(Context, Indent, !IO),
	output_lval(Lval, ModuleName, !IO),
	io__write_string(" = ", !IO),
	output_unboxed_result(Type, ResultIndex, !IO),
	io__write_string(";\n", !IO),
	output_assign_results(Lvals, Types, ResultIndex + 1, ModuleName,
		Indent, Context, !IO).
output_assign_results([_|_], [], _, _, _, _, _, _) :-
	unexpected(this_file, "output_assign_results: list length mismatch.").
output_assign_results([], [_|_], _, _, _, _, _, _) :-
	unexpected(this_file, "output_assign_results: list lenght mismatch.").

:- pred output_unboxed_result(mlds__type::in, int::in, io::di, io::uo) is det.

output_unboxed_result(Type, ResultIndex, !IO) :-
	( java_builtin_type(Type, _, JavaBoxedName, UnboxMethod) ->
		io__write_string("((", !IO),
		io__write_string(JavaBoxedName, !IO),
		io__write_string(") ", !IO),
		io__format("result[%d]).%s()", [i(ResultIndex), s(UnboxMethod)],
			!IO)
	;
		io__write_string("(", !IO),
		output_type(Type, !IO),
		io__write_string(") ", !IO),
		io__format("result[%d]", [i(ResultIndex)], !IO)
	).

%-----------------------------------------------------------------------------%
%
% Extra code for outputting switch statements
%

:- pred output_switch_cases(indent::in, func_info::in, mlds__context::in,
	list(mlds__switch_case)::in, mlds__switch_default::in, exit_methods::out,
	io::di, io::uo) is det.

output_switch_cases(Indent, FuncInfo, Context, [], Default, ExitMethods, !IO) :-
	output_switch_default(Indent, FuncInfo, Context, Default, ExitMethods,
		!IO).
output_switch_cases(Indent, FuncInfo, Context, [Case | Cases], Default,
		ExitMethods, !IO) :-
	output_switch_case(Indent, FuncInfo, Context, Case, CaseExitMethods0,
		!IO),
	output_switch_cases(Indent, FuncInfo, Context, Cases, Default,
		CasesExitMethods, !IO),
	( set__member(can_break, CaseExitMethods0) ->
		CaseExitMethods = (CaseExitMethods0 `set__delete` can_break)
			`set__insert` can_fall_through
	;
		CaseExitMethods = CaseExitMethods0
	),
	ExitMethods = CaseExitMethods `set__union` CasesExitMethods.

:- pred output_switch_case(indent::in, func_info::in, mlds__context::in,
	mlds__switch_case::in, exit_methods::out, io::di, io::uo) is det.

output_switch_case(Indent, FuncInfo, Context, Case, ExitMethods, !IO) :-
	Case = (Conds - Statement),
	ModuleName = FuncInfo ^ func_info_name ^ mod_name,
	list__foldl(output_case_cond(Indent, ModuleName, Context), Conds, !IO),
	output_statement(Indent + 1, FuncInfo, Statement, StmtExitMethods, !IO),
	( set__member(can_fall_through, StmtExitMethods) ->
		indent_line(Context, Indent + 1, !IO),
		io__write_string("break;\n", !IO),
		ExitMethods = (StmtExitMethods `set__insert` can_break)
			`set__delete` can_fall_through
	;
		% Don't output `break' since it would be unreachable.
		ExitMethods = StmtExitMethods
	).

:- pred output_case_cond(indent::in, mlds_module_name::in, mlds__context::in,
	mlds__case_match_cond::in, io::di, io::uo) is det.

output_case_cond(Indent, ModuleName, Context, match_value(Val), !IO) :-
	indent_line(Context, Indent, !IO),
	io__write_string("case ", !IO),
	output_rval(Val, ModuleName, !IO),
	io__write_string(":\n", !IO).
output_case_cond(_Indent, _ModuleName, _Context, match_range(_, _), _, _) :-
	unexpected(this_file,
		"output_case_cond: cannot match ranges in Java cases").

:- pred output_switch_default(indent::in, func_info::in, mlds__context::in,
	mlds__switch_default::in, exit_methods::out, io::di, io::uo) is det.

output_switch_default(_Indent, _FuncInfo, _Context, default_do_nothing,
		ExitMethods, !IO) :-
	ExitMethods = set__make_singleton_set(can_fall_through).
output_switch_default(Indent, FuncInfo, Context, default_case(Statement),
		ExitMethods, !IO) :-
	indent_line(Context, Indent, !IO),
	io__write_string("default:\n", !IO),
	output_statement(Indent + 1, FuncInfo, Statement, ExitMethods, !IO).
output_switch_default(Indent, _FuncInfo, Context, default_is_unreachable,
		ExitMethods, !IO) :-
	indent_line(Context, Indent, !IO),
	io__write_string("default: /*NOTREACHED*/\n", !IO),
	indent_line(Context, Indent + 1, !IO),
	io__write_string("throw new mercury.runtime.UnreachableDefault();\n",
		!IO),
	ExitMethods = set__make_singleton_set(can_throw).

%-----------------------------------------------------------------------------%
%
% Code for outputting atomic statements.
%

:- pred output_atomic_stmt(indent::in, func_info::in,
	mlds__atomic_statement::in, mlds__context::in, io::di, io::uo) is det.

	%
	% comments
	%
output_atomic_stmt(Indent, _FuncInfo, comment(Comment), _, !IO) :-
	% XXX we should escape any "*/"'s in the Comment.
	%     we should also split the comment into lines and indent
	%     each line appropriately.
	indent_line(Indent, !IO),
	io__write_string("/* ", !IO),
	io__write_string(Comment, !IO),
	io__write_string(" */\n", !IO).

	%
	% assignment
	%
output_atomic_stmt(Indent, FuncInfo, assign(Lval, Rval), _, !IO) :-
	ModuleName = FuncInfo ^ func_info_name ^ mod_name,
	indent_line(Indent, !IO),
	output_lval(Lval, ModuleName, !IO),
	io__write_string(" = ", !IO),
	(
		LvalType = mlds_lval_type(Lval),
	    	type_is_object(LvalType)
	->
		% If the Lval is a an object.

		( rval_is_int_const(Rval) ->
			io__write_string("new ", !IO),
			output_type(LvalType, !IO),
			io__write_string("(", !IO),
			output_rval(Rval, ModuleName, !IO),
			io__write_string(")", !IO)
		;
			output_rval(Rval, ModuleName, !IO)
		)
	;
		output_rval_maybe_with_enum(Rval, ModuleName, !IO)
	),
	io__write_string(";\n", !IO).

	%
	% heap management
	%
output_atomic_stmt(_Indent, _FuncInfo, delete_object(_Lval), _, _, _) :-
	unexpected(this_file, "delete_object not supported in Java.").

output_atomic_stmt(Indent, FuncInfo, NewObject, Context, !IO) :-
	NewObject = new_object(Target, _MaybeTag, HasSecTag, Type,
		_MaybeSize, MaybeCtorName, Args, ArgTypes),
	ModuleName = FuncInfo ^ func_info_name ^ mod_name,

	indent_line(Indent, !IO),
	io__write_string("{\n", !IO),
	indent_line(Context, Indent + 1, !IO),
	output_lval(Target, ModuleName, !IO),
	io__write_string(" = new ", !IO),
	%
	% Generate class constructor name.
	%
	(
		MaybeCtorName = yes(QualifiedCtorId),
		\+ (Type = mercury_type(_, TypeCategory, _),
		      hand_defined_type(TypeCategory, _))
	->
		output_type(Type, !IO),
		io__write_char('.', !IO),
		QualifiedCtorId = qual(_ModuleName, _QualKind, CtorDefn),
		CtorDefn = ctor_id(CtorName, CtorArity),
		output_class_name_and_arity(type(CtorName, CtorArity), !IO)
	;
		output_type(Type, !IO)
	),
	( type_is_array(Type) = yes ->
		%
		% The new object will be an array, so we
		% need to initialise it using array literals syntax.
		%
		io__write_string(" {", !IO),
		output_init_args(Args, ArgTypes, 0, HasSecTag, ModuleName, !IO),
		io__write_string("};\n", !IO)
	;
		%
		% Generate constructor arguments.
		%
		io__write_string("(", !IO),
		output_init_args(Args, ArgTypes, 0, HasSecTag, ModuleName, !IO),
		io__write_string(");\n", !IO)
	),
	indent_line(Indent, !IO),
	io__write_string("}\n", !IO).


output_atomic_stmt(_Indent, _FuncInfo, gc_check, _, _, _) :-
	unexpected(this_file, "gc_check not implemented.").

output_atomic_stmt(_Indent, _FuncInfo, mark_hp(_Lval), _, _, _) :-
	unexpected(this_file, "mark_hp not implemented.").

output_atomic_stmt(_Indent, _FuncInfo, restore_hp(_Rval), _, _, _) :-
	unexpected(this_file, "restore_hp not implemented.").

	%
	% trail management
	%
output_atomic_stmt(_Indent, _FuncInfo, trail_op(_TrailOp), _, _, _) :-
	unexpected(this_file, "trail_ops not implemented.").

	%
	% foreign language interfacing
	%
output_atomic_stmt(Indent, FuncInfo,
		inline_target_code(TargetLang, Components), Context, !IO) :-
	( TargetLang = lang_java ->
		indent_line(Indent, !IO),
		ModuleName = FuncInfo ^ func_info_name ^ mod_name,
		list__foldl(output_target_code_component(ModuleName, Context),
			Components, !IO)
	;
		unexpected(this_file,
			"inline_target_code only works for lang_java")
	).

output_atomic_stmt(_Indent, _FuncInfo,
		outline_foreign_proc(_TargetLang, _Vs, _Lvals, _Code),
		_Context, _, _)  :-
	unexpected(this_file, "foreign language interfacing not implemented").

%------------------------------------------------------------------------------%

:- pred output_target_code_component(mlds_module_name::in, mlds__context::in,
	target_code_component::in, io::di, io::uo) is det.

output_target_code_component(_ModuleName, _Context,
		user_target_code(CodeString, _MaybeUserContext, _Attrs), !IO) :-
	% XXX Java does not have an equivalent of the C #line preprocessor
	%     directive.  If it did, we should use it here.
	io__write_string(CodeString, !IO).
output_target_code_component(_ModuleName, _Context, raw_target_code(CodeString,
		_Attrs), !IO) :-
	io__write_string(CodeString, !IO).
output_target_code_component(ModuleName, _Context, target_code_input(Rval),
		!IO) :-
	output_rval(Rval, ModuleName, !IO).
output_target_code_component(ModuleName, _Context, target_code_output(Lval),
		!IO) :-
	output_lval(Lval, ModuleName, !IO).
output_target_code_component(ModuleName, _Context, name(Name), !IO) :-
	output_maybe_qualified_name(Name, ModuleName, !IO).

%------------------------------------------------------------------------------%

	% Output initial values of an object's fields as arguments for the
	% object's class constructor.
	%
:- pred output_init_args(list(mlds__rval)::in, list(mlds__type)::in, int::in,
	bool::in, mlds_module_name::in, io::di, io::uo) is det.

output_init_args([], [], _, _, _, !IO).
output_init_args([_|_], [], _, _, _, _, _) :-
	unexpected(this_file, "output_init_args: length mismatch.").
output_init_args([], [_|_], _, _, _, _, _) :-
	unexpected(this_file, "output_init_args: length mismatch.").
output_init_args([Arg | Args], [_ArgType | ArgTypes], ArgNum, HasSecTag,
		ModuleName, !IO) :-
	(
		ArgNum = 0,
		HasSecTag = yes
	->
		% This first argument is a `data_tag', It is set by
		% the class constructor so this argument can be discarded.
		true
	;
		output_rval(Arg, ModuleName, !IO),
		( Args = [] ->
			true
		;
			io__write_string(", ", !IO)
		)
	),
	output_init_args(Args, ArgTypes, ArgNum + 1, HasSecTag, ModuleName,
		!IO).

%-----------------------------------------------------------------------------%
%
% Code to output expressions
%

:- pred output_lval(mlds__lval::in, mlds_module_name::in, io::di,
	io::uo) is det.

output_lval(field(_MaybeTag, Rval, offset(OffsetRval), FieldType, _),
		ModuleName, !IO) :-
	(
		( FieldType = mlds__generic_type
		; FieldType = mlds__mercury_type(term__variable(_), _, _))
	->
		true
	;
		% The field type for field(_, _, offset(_), _, _) lvals
		% must be something that maps to MR_Box.
		unexpected(this_file, "unexpected field type.")
	),
	% XXX We shouldn't need this cast here, but there are cases where
	%     it is needed and the MLDS doesn't seem to generate it.
	io__write_string("((java.lang.Object[]) ", !IO),
	output_rval(Rval, ModuleName, !IO),
	io__write_string(")[", !IO),
	output_rval(OffsetRval, ModuleName, !IO),
	io__write_string("]", !IO).


output_lval(field(_, PtrRval, named_field(FieldName, CtorType), _, _),
		ModuleName, !IO) :-
	(
		FieldName = qual(_, _, UnqualFieldName),
	 	MangledFieldName = name_mangle(UnqualFieldName),
	  	MangledFieldName = "data_tag"
	->
		%
		% If the field we are trying to access is just a `data_tag'
		% then it is a member of the base class.
		%
		output_bracketed_rval(PtrRval, ModuleName, !IO),
		io__write_string(".", !IO)
	;
		%
		% Otherwise the field we are trying to access may be in
		% a derived class.  Objects are manipulated as instances
		% of their base class, so we need to downcast to the derived
		% class to access some fields.
		%
		io__write_string("((", !IO),
		output_type(CtorType, !IO),
		io__write_string(") ", !IO),
		output_bracketed_rval(PtrRval, ModuleName, !IO),
			% the actual variable
		io__write_string(").", !IO)
	),
	FieldName = qual(_, _, UnqualFieldName),
	output_valid_mangled_name(UnqualFieldName, !IO).    % the field name

output_lval(mem_ref(Rval, _Type), ModuleName, !IO) :-
	output_bracketed_rval(Rval, ModuleName, !IO).

output_lval(var(qual(ModName, QualKind, Name), _), CurrentModuleName, !IO) :-
	output_maybe_qualified_name(qual(ModName, QualKind, data(var(Name))),
		CurrentModuleName, !IO).

:- pred output_mangled_name(string::in, io::di, io::uo) is det.

output_mangled_name(Name, !IO) :-
	MangledName = name_mangle(Name),
	io__write_string(MangledName, !IO).

:- pred output_valid_mangled_name(string::in, io::di, io::uo) is det.

output_valid_mangled_name(Name, !IO) :-
	MangledName = name_mangle(Name),
	JavaSafeName = valid_symbol_name(MangledName),
	io__write_string(JavaSafeName, !IO).

:- pred mlds_output_bracketed_lval(mlds__lval::in, mlds_module_name::in,
	io::di, io::uo) is det.

mlds_output_bracketed_lval(Lval, ModuleName, !IO) :-
	(
		% if it's just a variable name, then we don't need parentheses
		Lval = var(_,_)
	->
		output_lval(Lval, ModuleName, !IO)
	;
		io__write_char('(', !IO),
		output_lval(Lval, ModuleName, !IO),
		io__write_char(')', !IO)
	).


:- pred output_call_rval(mlds__rval::in, mlds_module_name::in, io::di,
	io::uo) is det.

output_call_rval(Rval, ModuleName, !IO) :-
	(
		Rval = mlds__const(Const),
		Const = mlds__code_addr_const(CodeAddr)
	->
		IsCall = yes,
		mlds_output_code_addr(CodeAddr, IsCall, !IO)
	;
		output_bracketed_rval(Rval, ModuleName, !IO)
	).


:- pred output_bracketed_rval(mlds__rval::in, mlds_module_name::in,
	io::di, io::uo) is det.

output_bracketed_rval(Rval, ModuleName, !IO) :-
	(
		% if it's just a variable name, then we don't need parentheses
		( Rval = lval(var(_,_))
		; Rval = const(code_addr_const(_)))
	->
		output_rval(Rval, ModuleName, !IO)
	;
		io__write_char('(', !IO),
		output_rval(Rval, ModuleName, !IO),
		io__write_char(')', !IO)
	).

:- pred output_rval(mlds__rval::in, mlds_module_name::in, io::di, io::uo) is det.

output_rval(lval(Lval), ModuleName, !IO) :-
	output_lval(Lval, ModuleName, !IO).

output_rval(mkword(_, _), _, _, _) :-
	unexpected(this_file,
		"output_rval: tags not supported in Java").

output_rval(const(Const), _, !IO) :-
	output_rval_const(Const, !IO).

output_rval(unop(Op, Rval), ModuleName, !IO) :-
	output_unop(Op, Rval, ModuleName, !IO).

output_rval(binop(Op, Rval1, Rval2), ModuleName, !IO) :-
	output_binop(Op, Rval1, Rval2, ModuleName, !IO).

output_rval(mem_addr(_Lval), _, !IO) :-
	unexpected(this_file, "output_rval: mem_addr(_) not supported").

output_rval(self(_), _, !IO) :-
	io__write_string("this", !IO).

:- pred output_unop(mlds__unary_op::in, mlds__rval::in, mlds_module_name::in,
	io::di, io::uo) is det.

output_unop(cast(Type), Exprn, ModuleName, !IO) :-
	% rtti_to_mlds.m generates casts from int to
	% mercury.runtime.PseudoTypeInfo, but for Java
	% we need to treat these as constructions, not casts.
	% Similarly for conversions from TypeCtorInfo to TypeInfo.
	(
		Type = mlds__pseudo_type_info_type,
		Exprn = const(int_const(_))
	->
		maybe_output_comment("cast", !IO),
		io__write_string("new mercury.runtime.PseudoTypeInfo(", !IO),
		output_rval(Exprn, ModuleName, !IO),
		io__write_string(")", !IO)
	;
		( Type = mlds__mercury_type(_, type_info_type, _)
		; Type = mlds__type_info_type )
	->
		maybe_output_comment("cast", !IO),
		io__write_string("new mercury.runtime.TypeInfo_Struct(", !IO),
		output_rval(Exprn, ModuleName, !IO),
		io__write_string(")", !IO)
	;
		output_cast_rval(Type, Exprn, ModuleName, !IO)
	).
output_unop(box(Type), Exprn, ModuleName, !IO) :-
	output_boxed_rval(Type, Exprn, ModuleName, !IO).
output_unop(unbox(Type), Exprn, ModuleName, !IO) :-
	output_unboxed_rval(Type, Exprn, ModuleName, !IO).
output_unop(std_unop(Unop), Exprn, ModuleName, !IO) :-
	output_std_unop(Unop, Exprn, ModuleName, !IO).

:- pred output_cast_rval(mlds__type::in, mlds__rval::in,
	mlds_module_name::in, io::di, io::uo) is det.

output_cast_rval(Type, Exprn, ModuleName, !IO) :-
	io__write_string("(", !IO),
	output_type(Type, !IO),
	io__write_string(") ", !IO),
	( java_builtin_type(Type, "int", _, _) ->
		output_rval_maybe_with_enum(Exprn, ModuleName, !IO)
	;
		output_rval(Exprn, ModuleName, !IO)
	).

:- pred output_boxed_rval(mlds__type::in, mlds__rval::in,
	 mlds_module_name::in, io::di, io::uo) is det.

output_boxed_rval(Type, Exprn, ModuleName, !IO) :-
	( java_builtin_type(Type, _JavaName, JavaBoxedName, _) ->
		io__write_string("new ", !IO),
		io__write_string(JavaBoxedName, !IO),
		io__write_string("(", !IO),
		output_rval(Exprn, ModuleName, !IO),
		io__write_string(")", !IO)
	;
		io__write_string("((java.lang.Object) (", !IO),
		output_rval(Exprn, ModuleName, !IO),
		io__write_string("))", !IO)
	).

:- pred output_unboxed_rval(mlds__type::in, mlds__rval::in,
	mlds_module_name::in, io::di, io::uo) is det.

output_unboxed_rval(Type, Exprn, ModuleName, !IO) :-
	( java_builtin_type(Type, _, JavaBoxedName, UnboxMethod) ->
		io__write_string("((", !IO),
		io__write_string(JavaBoxedName, !IO),
		io__write_string(") ", !IO),
		output_bracketed_rval(Exprn, ModuleName, !IO),
		io__write_string(").", !IO),
		io__write_string(UnboxMethod, !IO),
		io__write_string("()", !IO)
	;
		io__write_string("((", !IO),
		output_type(Type, !IO),
		io__write_string(") ", !IO),
		output_rval(Exprn, ModuleName, !IO),
		io__write_string(")", !IO)
	).

	% java_builtin_type(MLDS_Type, JavaUnboxedType, JavaBoxedType,
	%	UnboxMethod):
	% For a given Mercury type, check if this corresponds to a
	% Java type which has both unboxed (builtin) and boxed (class)
	% versions, and if so, return their names, and the name of
	% the method to get the unboxed value from the boxed type.
	%
:- pred java_builtin_type(mlds__type::in, string::out, string::out,
	string::out) is semidet.

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
java_builtin_type(Type, "int", "java.lang.Integer", "intValue") :-
	Type = mlds__mercury_type(term__functor(term__atom("."), _, _), _, _),
	Type = mlds__mercury_type(MercuryType, _, _),
	type_util__is_dummy_argument_type(MercuryType).

:- pred output_std_unop(builtin_ops__unary_op::in, mlds__rval::in,
	mlds_module_name::in, io::di, io::uo) is det.

	%
	% For the Java back-end, there are no tags,
	% so all the tagging operators are no-ops,
	% except for `tag', which always returns zero
	% (a tag of zero means there's no tag).
	%
output_std_unop(UnaryOp, Exprn, ModuleName, !IO) :-
	( UnaryOp = tag ->
		io__write_string("/* tag */  0", !IO)
	;
		java_util__unary_prefix_op(UnaryOp, UnaryOpString),
		io__write_string(UnaryOpString, !IO),
		io__write_string("(", !IO),
		output_rval(Exprn, ModuleName, !IO),
		io__write_string(")", !IO)
	).


:- pred output_binop(binary_op::in, mlds__rval::in, mlds__rval::in,
	mlds_module_name::in, io::di, io::uo) is det.

output_binop(Op, X, Y, ModuleName, !IO) :-
	( Op = array_index(_Type) ->
		output_bracketed_rval(X, ModuleName, !IO),
		io__write_string("[", !IO),
		output_rval(Y, ModuleName, !IO),
		io__write_string("]", !IO)
	; java_util__string_compare_op(Op, OpStr) ->
		io__write_string("(", !IO),
		output_rval(X, ModuleName, !IO),
		io__write_string(".compareTo(", !IO),
		output_rval(Y, ModuleName, !IO),
		io__write_string(") ", !IO),
		io__write_string(OpStr, !IO),
		io__write_string(" 0)", !IO)
	;
		( java_util__float_compare_op(Op, OpStr1) ->
			OpStr = OpStr1
		; java_util__float_op(Op, OpStr2) ->
			OpStr = OpStr2
		;
			fail
		)
	->
		io__write_string("(", !IO),
		output_rval_maybe_with_enum(X, ModuleName, !IO),
		io__write_string(" ", !IO),
		io__write_string(OpStr, !IO),
		io__write_string(" ", !IO),
		output_rval_maybe_with_enum(Y, ModuleName, !IO),
		io__write_string(")", !IO)
	;
		io__write_string("(", !IO),
		output_rval_maybe_with_enum(X, ModuleName, !IO),
		io__write_string(" ", !IO),
		output_binary_op(Op, !IO),
		io__write_string(" ", !IO),
		output_rval_maybe_with_enum(Y, ModuleName, !IO),
		io__write_string(")", !IO)
	).

	% Output an Rval and if the Rval is an enumeration object
	% append the string ".value", so we can access its value
	% field.
	% XXX	Note that this is necessary in some places, but not in others.
	%	For example, it is important to do so for switch statements, as
	%	the argument of a switch _must_ be an integer in Java. However,
	%	adding the .value to assignments breaks some casting...
	%	At some point, we need to go through all the places where
	%	output_rval and output_rval_maybe_with_enum are called and make
	%	sure the correct one is being used.
	%
:- pred output_rval_maybe_with_enum(mlds__rval::in, mlds_module_name::in,
	io::di, io::uo) is det.

output_rval_maybe_with_enum(Rval, ModuleName, !IO) :-
	output_rval(Rval, ModuleName, !IO),
	( rval_is_enum_object(Rval) ->
		io__write_string(".value", !IO)
	;
		true
	).

:- pred output_binary_op(binary_op::in, io::di, io::uo) is det.

output_binary_op(Op, !IO) :-
	( java_util__binary_infix_op(Op, OpStr) ->
		io__write_string(OpStr, !IO)
	;
		unexpected(this_file,
			"output_binary_op: invalid binary operator")
	).

:- pred output_rval_const(mlds__rval_const::in, io::di, io::uo) is det.

output_rval_const(true, !IO) :-
	io__write_string("true", !IO).

output_rval_const(false, !IO) :-
	io__write_string("false", !IO).

output_rval_const(int_const(N), !IO) :-
	io__write_int(N, !IO).

output_rval_const(float_const(FloatVal), !IO) :-
	c_util__output_float_literal(FloatVal, !IO).

output_rval_const(string_const(String), !IO) :-
	io__write_string("""", !IO),
	c_util__output_quoted_string(String, !IO),
	io__write_string("""", !IO).

output_rval_const(multi_string_const(Length, String), !IO) :-
	io__write_string("""", !IO),
	c_util__output_quoted_multi_string(Length, String, !IO),
	io__write_string("""", !IO).

output_rval_const(code_addr_const(CodeAddr), !IO) :-
	IsCall = no,
	mlds_output_code_addr(CodeAddr, IsCall, !IO).

output_rval_const(data_addr_const(DataAddr), !IO) :-
	mlds_output_data_addr(DataAddr, !IO).

output_rval_const(null(_), !IO) :-
       io__write_string("null", !IO).

%-----------------------------------------------------------------------------%

:- pred mlds_output_code_addr(mlds__code_addr::in, bool::in, io::di,
	io::uo) is det.

mlds_output_code_addr(proc(Label, _Sig), IsCall, !IO) :-
	( IsCall = no ->
		%
		% Not a function call, so we are taking the address of the
		% wrapper for that function (method).
		%
		io__write_string("new AddrOf__", !IO),
		output_fully_qualified_proc_label(Label, "__", !IO),
		io__write_string("_0()", !IO)
	;
		output_fully_qualified_proc_label(Label, ".", !IO)
	).
mlds_output_code_addr(internal(Label, SeqNum, _Sig), IsCall, !IO) :-
	( IsCall = no ->
		%
		% Not a function call, so we are taking the address of the
		% wrapper for that function (method).
		%
		io__write_string("new AddrOf__", !IO),
		output_fully_qualified_proc_label(Label, "__", !IO),
		io__write_string("_", !IO),
		io__write_int(SeqNum, !IO),
		io__write_string("_0()", !IO)
	;
		output_fully_qualified_proc_label(Label, ".", !IO),
		io__write_string("_", !IO),
		io__write_int(SeqNum, !IO)
	).

:- pred mlds_output_proc_label(mlds__proc_label::in, io::di, io::uo) is det.

mlds_output_proc_label(PredLabel - ProcId, !IO) :-
	output_pred_label(PredLabel, !IO),
	proc_id_to_int(ProcId, ModeNum),
	io__format("_%d", [i(ModeNum)], !IO).

:- pred mlds_output_data_addr(mlds__data_addr::in, io::di, io::uo) is det.

mlds_output_data_addr(data_addr(ModuleQualifier, DataName), !IO) :-
	SymName = mlds_module_name_to_sym_name(ModuleQualifier),
	mangle_mlds_sym_name_for_java(SymName, module_qual, ".", ModuleName),
	io__write_string(ModuleName, !IO),
	io__write_string(".", !IO),
	output_data_name(DataName, !IO).

%-----------------------------------------------------------------------------%
%
% Miscellaneous stuff to handle indentation and generation of
% source context annotations.  (XXX This can probably be simplified
% since Java doesn't have an equivalent of #line directives.)
%

:- pred output_context(mlds__context::in, io::di, io::uo) is det.

output_context(_Context, !IO).

:- pred indent_line(mlds__context::in, indent::in, io::di, io::uo) is det.

indent_line(Context, N, !IO) :-
	output_context(Context, !IO),
	indent_line(N, !IO).

% A value of type `indent' records the number of levels
% of indentation to indent the next piece of code.
% Currently we output two spaces for each level of indentation.
% XXX There is a small amount of code duplication with mlds_to_c.m here.
:- type indent == int.

:- pred indent_line(indent::in, io::di, io::uo) is det.

indent_line(N, !IO) :-
	( N =< 0 ->
		true
	;
		io__write_string("  ", !IO),
		indent_line(N - 1, !IO)
	).

:- func this_file = string.

this_file = "mlds_to_java.m".

%-----------------------------------------------------------------------------%
:- end_module mlds_to_java.
%-----------------------------------------------------------------------------%
