%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% MLDS - The Medium-Level Data Structure.
% Main author: fjh.

% This module defines the MLDS data structure itself.
% The MLDS is an intermediate data structure used in compilation;
% we compile Mercury source -> parse tree -> HLDS -> MLDS -> target (e.g. C).
% See notes/compiler_design.html for more information about the MLDS & LLDS.
%
% The MLDS is intended to be suitable for generating target code in
% languages such as Java, Java bytecode, high-level C, C++, or C--, etc.
% This is as opposed to the LLDS, which is better suited for generating
% assembler or the very low-level C or GNU C (lower level than C--) that
% the original Mercury compiler backend generates.  In the LLDS, we are
% doing all our own register allocation and stack manipulation, but with
% MLDS, those tasks are the responsibility of the target.
% The one really important simplification that we do make relative to the
% HLDS is that MLDS does not have any support for non-determinism, so the
% HLDS->MLDS compiler must compile non-deterministic code into code that
% uses continuation passing.

% The MLDS data structure is quite full-featured, including for example
% support for arbitrary nesting, multiple return values, and tagged pointers.
% However, many
% of the intended target languages don't support all of those features. 
% Therefore the HLDS->MLDS compiler must ensure that the final MLDS code that
% it eventually generates does not use features which the target does not
% support.  This will (presumably) be accomplished by having handle_options.m
% set various flags according to the selected target, and having the
% HLDS->MLDS compiler take account of those flags and either generate simpler
% MLDS code in the first place or run some extra simplification passes over
% the MLDS code before invoking the MLDS->target compiler.
% 

%-----------------------------------------------------------------------------%
%
% Mapping Mercury names to MLDS names
%

% 1. Modules
%
% Mercury module names map directly to MLDS package names, except that
% modules in the Mercury standard library map get a `mercury' prefix,
% e.g. `mercury.builtin', `mercury.io', `mercury.std_util', etc.

% 2. Procedures names
%
% HLDS procedure names map directly to MLDS function names.
% MLDS function names are structured terms that include sufficient
% information (arity, pred_or_func indicator, mode number, etc.)
% to avoid any ambiguities.
%
% [Rationale: the reason for keeping structured names rather than flattened
% names at the MLDS level is that we don't want to do name mangling at
% the HLDS -> MLDS stage, since we don't know what restrictions the target
% language will impose.  For example, some target languages (e.g. C++, Java)
% will support overloading, while others (e.g. C) will not.]

% 3. Procedures signatures
%
% MLDS function signatures are determined by the HLDS procedure's
% argument types, modes, and determinism.
% Procedures arguments with type `io__state' or `store(_)' are not passed.
% Procedures arguments with top_in modes are passed as input.
% Procedures arguments with top_out modes may be returned by value
% or alternatively may be passed by reference.
% Procedure arguments with top_unused modes should not be passed
% (except possibly in the case where they are polymorphically typed,
% and even then, depending on how polymorphism is handled).]
% Procedures with determinism model_det need no special handling.
% Procedures with determinism model_semi must return a boolean.
% Procedures with determinism model_non get passed a continuation;
% if the procedure succeeds, it must call the continuation, and if
% it fails, it must return.

% 4. Variables
%
% MLDS variable names are determined by the HLDS variable name and
% (to avoid ambiguity) variable number.
% All references to MLDS variables must however be fully qualified
% with the name of the enclosing entity that defines them.

% 5. Global data
%
% Is there any use in hanging on to the structure of the names for
% compiler-generated global data?  Currently we do.  I'm not sure
% if it would be better to flatten it (using mangled names) at this point.

% 6. Types
%
% If there is an MLDS type corresponding to a Mercury type, then
% the Mercury type name maps directly to the MLDS type name,
% suitable module-qualified of course. 
% The MLDS type name includes the type arity (arity overloading is allowed).
% However, if a low-level data representation scheme is used,
% then some Mercury types may not have corresponding MLDS type names
% (that is, the corresponding MLDS type may be just `Word' or its equivalent).

% 7.  Data constructors.
%
% [XXX Not yet documented or implemented]
% [XXX Also need to think about equivalence types and no_tag types]

% 8.  Insts and modes
%
% Inst and mode definitions do not get translated into MLDS.
%
% [Inst and mode definitions do however affect the signatures of the
% MLDS functions used to implement each procedure.]

% 9. Type classes.
% 
% Currently type classes are handled early and at a fairly low level.
% It's not yet clear how easy it will be to convert this to MLDS.
% It may depend to some degree on the target language.
% But if it is possible, then when it is done the target language
% generator will be able to ignore the issue of type classes.
%
% For language interoperability, however, it might be nice if the
% translation were done at higher level.
%
% Mercury type classes should map directly to MLDS interfaces.
%
% Mercury instance definitions should map to classes which implement the
% corresponding interface.  Note that if there is an instance declaration
% `:- instance foo(bar)', then the MLDS type for `bar' will *not* implement 
% the MLDS interface for `foo' -- instead, there will be a new MLDS type
% for `instance foo(bar)' which implements that interface.

%-----------------------------------------------------------------------------%
%
% Mapping MLDS names to the target language
%

% Ultimately the exact choice of how MLDS names are mapped to
% the target language is up to the target language generator.
% So the remarks here are just guidelines, not strict rules.

% 1.  Names.
%
% If the target language has standard conventions about certain categories
% of names beginning with uppercase or lowercase letters, then the target
% language generator should generate names that respect those conventions.
%
% An MLDS name may contain arbitrary characters.
% If the target language has restrictions on what names can be used
% in identifiers, then it is the responsibility of the target language
% generator to mangle MLDS names accordingly to ensure that they abide
% by those restrictions.

% 2. Packages.
%
% MLDS packages should be mapped directly to target packages, if possible.
% If the target doesn't have a notion of packages, then they should be
% mapped to names of the form "foo.bar.baz" or if dots are not allowed
% then to "foo__bar__baz".

% 3. Overloading
%
% If the target does not support overloading, then any Mercury names which
% are overloaded within a single Mercury module must be qualified to avoid
% ambiguity.  However, Mercury names which are not overloaded should not
% be qualified.  If a name is overloaded but occurs only once in the module's
% interface then the version in the interface should not be qualified.
% The arity-zero version of type should not be arity-qualified
% unless this would cause ambiguity with an unqualified name generated by
% the previous rule.  Likewise, the arity-zero version of function
% (i.e. a constant) should not be function-qualified or arity-qualified
% unless this would cause ambiguity with an unqualified name generated
% the aforementioned rule.
% The first mode of a predicate should not be mode-qualified.
%
% [Rationale: name mangling should be avoided where possible, because
% this makes it easier for the user to interoperate with other languages
% and to use tools which do not properly demangle Mercury names.]

% 4. Procedures.
%
% If a procedure name needs to be qualified, the qualification should be
% done by appending "_f" for functions or "_p" for predicates,
% optionally followed by the arity,
% optionally followed by an underscore and then the mode number,
% optionally followed by "_i" and then the MLDS function sequence number
% (for internal MLDS functions, used e.g. to implement backtracking).
%
% [Rationale: any qualifiers should go at the end of a name, so that
% command-line completion works even for mangled names (and hopefully
% even shows all the possible alternatives...).]
%
% To avoid ambiguity as to whether a name is qualified or not,,
% any procedures whose unqualified name matches the pattern for qualified
% names, i.e. the regular expression `.*_[fp][0-9]*(_[0-9]+)?(_i[0-9]+)?',
% should always be qualified (even if not overloaded).

% 5. Types.
%
% If a type name needs to be qualified, the qualification should be
% done by appending an underscore followed by the arity.
%
% To avoid ambiguity as to whether a name is qualified or not,
% any types whose unqualified name matches the pattern for qualified
% names, i.e. the regular expression `.*_[0-9]+',
% should always be qualified (even if not overloaded).

%-----------------------------------------------------------------------------%
%
% Notes on garbage collection and liveness.
% 

% "Liveness-accurate GC" is GC in which the collector does not trace local
% variables which are definitely not live according to a straight-forward
% static analysis of definite-deadness/possible-liveness.  Liveness-accurate
% GC is desirable, in general, since tracing through variables which are
% no longer live may lead to excessive memory retention for some programs.
% However these programs are relatively rare, so liveness-accurate GC
% is not always worth the extra complication.
%
% The MLDS is therefore designed to optionally support liveness-accurate
% GC, if the target language supports it.  If liveness-accurate GC is
% supported and enabled, then it is the responsibility of the target
% language implementation to do whatever is needed to avoid having the GC
% trace variables which have gone out of scope.
%
% That means that to support liveness-accurate the HLDS->MLDS code
% generator just needs to cover the cases where a straight-forward liveness
% calculation on the generated MLDS does not match up with the desired
% result of a straight-forward liveness calculation on the HLDS.  That is,
% the HLDS->MLDS code generator must generate code to clobber variables
% which are no longer live according to a straight-forward liveness
% calculation on the HLDS but which have not gone out of scope in
% the generated MLDS.  For example, with our current HLDS->MLDS code
% generation scheme, this is the case for variables in the `else' of a
% nondet if-then-else once the `then' has been entered.
% (XXX Currently ml_code_gen.m does _not_ clobber those variables, though.)

% The rationale for leaving most of the responsibility for liveness-accurate
% GC up to the MLDS back-end is as follows: at very least we need the
% target language implementation's _cooperation_, since if the MLDS code
% generator inserts statements to clobber variables that are no longer live,
% then an uncooperative target language implementation could just optimize
% them away, since they are assignments to dead variables.  Given this need
% for the MLDS target back-end's cooperation, it makes sense to assign as
% much of the responsibily for this task as is possible to the MLDS target
% back-end, to keep the front-end simple and to keep the responsibility
% for this task in one place.
%
% But in the cases such as nondet if-then-else, where HLDS-liveness does not
% match MLDS-liveness, we can't just leave it to the MLDS target back-end,
% because that would require assuming an unreasonably smart liveness
% calculation in the MLDS target back-end, so in such cases we do need
% to handle it in the HLDS->MLDS code generator.

%-----------------------------------------------------------------------------%

:- module mlds.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_data.
:- import_module prog_data, builtin_ops, rtti.
:- import_module type_util.

% To avoid duplication, we use a few things from the LLDS
% (specifically stuff for the C interface).
% It would be nice to avoid this dependency...
:- import_module llds.

:- import_module bool, list, assoc_list, std_util.

%-----------------------------------------------------------------------------%

:- type mercury_module_name == prog_data__module_name.

%
% The type `mlds' is the actual MLDS.
% XXX we ought to make this type abstract
%
:- type mlds
	---> mlds(
		mercury_module_name,	% The Mercury module name

		mlds__foreign_code,	% Code defined in some other language,
					% e.g. for `pragma c_header_code', etc.

		% The MLDS code itself
		mlds__imports,		% Packages/classes to import
		mlds__defns		% Definitions of code and data
	).

:- func mlds__get_module_name(mlds) = mercury_module_name.

:- type mlds__imports == list(mlds__import).

% Currently an import just gives the name of the package to be imported.
% This might perhaps need to be expanded to cater to different kinds of
% imports, e.g. imports with wild-cards ("import java.lang.*").
:- type mlds__import == mlds_module_name.
					% Specifies the name of a package or
					% class to import.

% An mlds_module_name specifies the name of an mlds package or class.
:- type mlds_module_name.

% An mlds__package_name specifies the name of an mlds package.
:- type mlds__package_name == mlds_module_name.

% Given the name of a Mercury module, return the name of the corresponding
% MLDS package.
:- func mercury_module_name_to_mlds(mercury_module_name) = mlds__package_name.

% Given the name of a Mercury module, return the name of the corresponding
% MLDS package.
:- func mlds_module_name_to_sym_name(mlds__package_name) = sym_name.

% Given an MLDS module name (e.g. `foo.bar'), append another class qualifier
% (e.g. for a class `baz'), and return the result (e.g. `foo.bar.baz').
% The `arity' argument specifies the arity of the class.
:- func mlds__append_class_qualifier(mlds_module_name, mlds__class_name, arity) =
	mlds_module_name.

:- type mlds__defns == list(mlds__defn).
:- type mlds__defn
	---> mlds__defn(
		mlds__entity_name,	% the name of the entity being declared
		mlds__context,		% the source location
		mlds__decl_flags,	% these contain the following:
			% mlds__access,		% public/private/protected
			% mlds__member_type,	% static/per_instance
			% mlds__virtuality,	% virtual/non_virtual
			% mlds__finality,	% final/overridable (funcs only)
			% mlds__constness,	% const/modifiable  (data only)
			% mlds__is_abstract,	% abstract/concrete
			% etc.
		mlds__entity_defn	% the definition of the entity
	).

% An mlds name may contain arbitrary characters.
% If the target language has restrictions on what names can be used
% in identifiers, then it is the responsibility of the target language
% generator to mangle these names accordingly.
:- type mlds__fully_qualified_name(T)
	---> 	qual(mlds_module_name, T).
:- type mlds__qualified_entity_name
	==	mlds__fully_qualified_name(mlds__entity_name).

:- type mlds__entity_name
	--->	type(mlds__class_name, arity)	% Name, arity.
	;	data(mlds__data_name)
	;	function(
			mlds__pred_label,	% Identifies the source code
						% predicate or function.

			proc_id,		% Mode number.

				% A sequence number used to distinguish 
				% different MLDS functions when compiling a
				% single HLDS predicate into multiple MLDS
				% functions (e.g. to handle backtracking).
			maybe(mlds__func_sequence_num),

				% This should generally not be needed much,
				% since all the necessary information should
				% be in the mlds__pred_label and/or in the
				% mlds__entity_defn.  However, the target
				% generator may want to refer to the HLDS
				% for additional information.
			pred_id			% Specifies the HLDS pred_id.
		)
	;	export(string)	% A pragma export name.
	.

:- type mlds__func_sequence_num == int.

	% This specifies information about some entity being defined
	% The entity may be any of the following:
	%	constant or variable
	%	function
	%	class, including
	%		package (class with only static members)
	%		interface (abstract class, no data members)
	%		struct (value class)
	%		enum
:- type mlds__entity_defn
		% constants or variables
	--->	mlds__data(
			mlds__type,
			mlds__initializer
		)
		% functions
	;	mlds__function(
			maybe(pred_proc_id),	% identifies the original
						% Mercury procedure, if any
			mlds__func_params,	% the arguments & return types
			maybe(mlds__statement)	% the function body, or `no'
						% if the function is abstract
		)
		% packages, classes, interfaces, structs, enums
	;	mlds__class(
			mlds__class_defn
		).

:- type mlds__initializer
	--->	init_obj(mlds__rval)
	;	init_struct(list(mlds__initializer))
	;	init_array(list(mlds__initializer))
	;	no_initializer
	.

:- type mlds__func_params
	---> mlds__func_params(
		mlds__arguments,	% names and types of arguments (inputs)
		list(mlds__type)	% types of return values (outputs)
	).

:- type mlds__arguments == assoc_list(mlds__entity_name, mlds__type).

	% An mlds__func_signature is like an mlds__func_params
	% except that it only includes the function's type, not
	% the parameter names.
:- type mlds__func_signature
	---> mlds__func_signature(
		list(mlds__type),	% argument types
		list(mlds__type)	% return types
	).

:- func mlds__get_func_signature(mlds__func_params) = mlds__func_signature.

:- type mlds__class_kind
	--->	mlds__class		% A generic class:
					% can inherit other classes and
					% interfaces
					% (but most targets will only support
					% single inheritence, so usually there
					% will be at most one class).
	;	mlds__package		% A class with only static members
					% (can only inherit other packages).
					% Unlike other kinds of classes,
					% packages should not be used as types.
	;	mlds__interface		% A class with no variable data members
					% (can only inherit other interfaces)
	;	mlds__struct		% A value class
					% (can only inherit other structs).
	;	mlds__enum		% A class with one integer member and
					% a bunch of static consts
					% (cannot inherit anything).
	.


:- type mlds__class_name == string.
:- type mlds__class == mlds__fully_qualified_name(mlds__class_name).

:- type mlds__class_defn
	---> mlds__class_defn(
		kind	::	mlds__class_kind,
		imports	::	mlds__imports,	% imports these classes (or
						% modules, packages, ...)
		inherits ::	list(mlds__class_id),
						% inherits these base classes
		implements ::	list(mlds__interface_id),
						% implements these interfaces
		members ::	mlds__defns	% contains these members
	).

	% Note: the definition of the `mlds__type' type is subject to change.
	% In particular, we might add new alternatives here, so try to avoid
	% switching on this type.
:- type mlds__type
	--->	% Mercury data types
		mercury_type(
			prog_data__type,	% the exact Mercury type
			builtin_type		% what kind of type it is:
						% enum, float, etc.
		)

		% The type for the continuation functions used
		% to handle nondeterminism
	;	mlds__cont_type

		% The type used for storing information about a commit.
		% This may be `jmp_buf' or `__label__'.
	;	mlds__commit_type

		% MLDS native builtin types.
		% These are the builtin types of the MLDS target language,
		% whatever that may be.
		% Currently we don't actually use many of these.
	;	mlds__native_bool_type
	;	mlds__native_int_type
	;	mlds__native_float_type
	;	mlds__native_char_type

		% MLDS types defined using mlds__class_defn
	;	mlds__class_type(
			mlds__class,		% name
			arity,
			mlds__class_kind
		)

		% MLDS array types.
		% These are single-dimensional, and can be indexed
		% using the `field' lval with an `offset' field_id;
		% indices start at zero.
		% Currently these are used for static constants
		% that would otherwise be allocated with a `new_object'
		% statement.
	;	mlds__array_type(mlds__type)

		% Pointer types.
		% Currently these are used for handling output arguments.
	;	mlds__ptr_type(mlds__type)

		% Function types.
		% For the C back-end, these are mapped to function
		% pointer types, since C doesn't have first-class
		% function types.
	;	mlds__func_type(mlds__func_params)

		% A generic type (e.g. `Word') that can hold any Mercury value.
		% This is used for implementing polymorphism.
	;	mlds__generic_type

		% A generic pointer type (e.g. `void *' in C)
		% that can be used to point to the environment
		% (set of local variables) of the containing function.
		% This is used for handling nondeterminism,
		% if the target language doesn't support
		% nested functions, and also for handling
		% closures for higher-order code.
	;	mlds__generic_env_ptr_type

	;	mlds__pseudo_type_info_type
	
	;	mlds__rtti_type(rtti_name).

:- type mercury_type == prog_data__type.

:- func mercury_type_to_mlds_type(module_info, mercury_type) = mlds__type.

% Hmm... this is tentative.
:- type mlds__class_id == mlds__type.
:- type mlds__interface_id == mlds__type.

%-----------------------------------------------------------------------------%

:- type mlds__decl_flags.

:- type access
	--->	public
	;	protected
	;	private
	;	default.	% Java "default" access: accessible to anything
				% defined in the same package.

:- type per_instance
	--->	one_copy	% i.e. "static" storage duration
				% (but not necessarily static linkage)
				% or static member function
	;	per_instance.	% i.e. "auto" local variable in function,
				% or non-static member of class.

:- type virtuality
	--->	non_virtual
	;	virtual.

:- type finality
	--->	overridable
	;	final.

:- type constness
	--->	modifiable
	;	const.

:- type abstractness
	--->	concrete
	;	abstract.

:- func access(mlds__decl_flags) = access.
:- func per_instance(mlds__decl_flags) = per_instance.
:- func virtuality(mlds__decl_flags) = virtuality.
:- func finality(mlds__decl_flags) = finality.
:- func constness(mlds__decl_flags) = constness.
:- func abstractness(mlds__decl_flags) = abstractness.

:- func set_access(mlds__decl_flags, access) = mlds__decl_flags.
:- func set_per_instance(mlds__decl_flags, per_instance) = mlds__decl_flags.
:- func set_virtuality(mlds__decl_flags, virtuality) = mlds__decl_flags.
:- func set_finality(mlds__decl_flags, finality) = mlds__decl_flags.
:- func set_constness(mlds__decl_flags, constness) = mlds__decl_flags.
:- func set_abstractness(mlds__decl_flags, abstractness) = mlds__decl_flags.

:- func init_decl_flags(access, per_instance, virtuality, finality,
		constness, abstractness) = mlds__decl_flags.

%-----------------------------------------------------------------------------%

	%
	% C code required for the C interface.
	% When compiling to a language other than C,
	% this part still needs to be generated as C code
	% and compiled with a C compiler.
	%
:- type mlds__foreign_code
	---> mlds__foreign_code(
		c_header_info,
		list(user_c_code),
		list(mlds__pragma_export)
	).

	%
	% Information required to generate code for each
	% `pragma export'.
	%
:- type mlds__pragma_export
	---> ml_pragma_export(
		string,			% Exported name
		mlds__entity_name,	% MLDS name for exported entity
		mlds__func_params,	% MLDS function parameters
		mlds__context,
		bool			% is a det function with the
					% final args mode top_out.
	).


%-----------------------------------------------------------------------------%

	% mlds__context is probably == prog_context,
	% but might also contain goal_path or other information.
:- type mlds__context.

:- func mlds__make_context(prog_context) = mlds__context.

:- func mlds__get_prog_context(mlds__context) = prog_context.

%-----------------------------------------------------------------------------%

:- type mlds__statements == list(mlds__statement).

:- type mlds__statement
	--->	mlds__statement(
			mlds__stmt,
			mlds__context
		).

:- type mlds__stmt
	--->

	%
	% sequence
	%
		block(mlds__defns, list(mlds__statement))

	%
	% iteration
	%
	;	while(mlds__rval, mlds__statement, bool)
			% the `bool' is true iff the loop is guaranteed
			% to iterate at least once -- in that case,
			% the compiler can generate a `do...while' loop
			% rather than a `while...' loop.

	%
	% selection (see also computed_goto)
	%
	;	if_then_else(mlds__rval, mlds__statement,
			maybe(mlds__statement))

/******
	% Is it worth including this?  We already have `computed_goto'...
	;	switch(
			mlds__rval,

			% other representations might be better...
			assoc_list(mlds__rval, mlds__statement),
			mlds__statement		% the default case
		)
******/

	%
	% transfer of control
	%

	;	label(mlds__label)
			% Defines a label that can be used as the
			% target of calls, gotos, etc.

	;	goto(mlds__label)
			% goto(Target)
			% Branch to the specified address.

	;	computed_goto(mlds__rval, list(mlds__label))
			% Evaluate rval, which should be an integer,
			% and jump to the (rval+1)th label in the list.
			% e.g. computed_goto(2, [A, B, C, D])
			% will branch to label C.


	%
	% function call/return
	%

	;	call(
			mlds__func_signature,	% signature of the func
			mlds__rval,		% the function to call
			maybe(mlds__rval),	% for method calls, this field
						% specifies the `this' object
			list(mlds__rval),	% ordinary function arguments
			list(mlds__lval),	% places to store the
						% function return value(s)
			is_tail_call		% indicates whether this
						% call is a tail call
		)

	;	return(list(mlds__rval))	% Some targets will not support
						% returning more than one value
						
	%
	% commits (a specialized form of exception handling)
	%

		% try_commit(Ref, GoalToTry, CommitHandlerGoal):
		%	Execute GoalToTry.  If GoalToTry exits via a
		%	`commit(Ref)' instruction, then execute
		%	CommitHandlerGoal.
		%
		% do_commit(Ref):
		%	Unwind the stack to the corresponding `try_commit'
		%	statement for Ref, and branch to the CommitHandlerGoal
		%	that was specified in that try_commit instruction.
		%
		% For both try_commit and commit instructions,
		% Ref should be the name of a local variable of type
		% mlds__commit_type.  There should be exactly
		% one try_commit instruction for each Ref.
		% do_commit(Ref) instructions should only be used
		% in goals called from the GoalToTry goal in the
		% try_commit instruction with the same Ref.
		%	
	;	try_commit(mlds__lval, mlds__statement, mlds__statement)
	;	do_commit(mlds__rval)

	%
	% exception handling
	%
/*********
XXX Full exception handling support is not yet implemented.

	% We use C++-style exceptions.
	% For C, the back-end can simulate them using setjmp/longjmp.
	%
	% XXX This is tentative -- the current definition may be
	% a bit too specific to C++-style exceptions.
	% It might not be a good choice for different target languages.

		% throw the specified exception
	;	throw(mlds__type, mlds__rval)

		% rethrow the current exception
		% (only valid inside an exception handler)
	;	rethrow

		% Execute the specified statement, and if it throws an exception,
		% and the exception matches any of the exception handlers,
		% then execute the first matching exception handler.
	;	try_catch(
			mlds__statement,
			list(mlds__exception_handler)
		)
**********/

	%
	% atomic statements
	%

	;	atomic(mlds__atomic_statement)
	
	.


:- type mlds__label == string.

:- type is_tail_call
	--->	tail_call	% a tail call
	;	call		% just an ordinary call
	.


	% XXX This is tentative -- the current definition may be
	% a bit too specific to C++-style exceptions.
	% It might not be a good choice for different target languages.
:- type mlds__exception_handler
	--->	handler(
			maybe(mlds__type),
				% if `yes(T)', specifies the type of exceptions to catch
				% if `no', it means catch all exceptions

			maybe(string)
				% if `yes(Name)', gives the variable name to use for
				%	the exception value
				% if `no', then exception value will not be used
		).


	%
	% atomic statements
	%
:- type mlds__atomic_statement

	--->	comment(string)
			% Insert a comment into the output code.

	;	assign(mlds__lval, mlds__rval)
			% assign(Location, Value):
			% Assign the value specified by rval to the location
			% specified by lval.

	%
	% heap management
	%

		% XXX the following is still quite tentative
			% new_object(Target, Tag, Type,
			%	Size, CtorName, Args, ArgTypes):
			% Allocate a memory block of the given size,
			% initialize it with a new object of the given
			% type by calling the constructor with the specified
			% arguments, and put its address in the given lval,
			% possibly after tagging the address with a given tag.
			% (Some targets might not support tags.)
	;	new_object(
			mlds__lval,	% The target to assign the new object's
					% address to.
			maybe(mlds__tag),
					% A tag to tag the address with
					% before assigning the result to the
					% target.
			mlds__type,	% The type of the object being
					% allocated.
			maybe(mlds__rval),
					% The amount of memory that needs to
					% be allocated for the new object,
					% measured in bytes.
					% (XXX would it be better to measure
					% this in bits or words rather than
					% bytes?)
			maybe(ctor_name),
					% The name of the constructor to
					% invoke.
			list(mlds__rval),
					% The arguments to the constructor.
			list(mlds__type)
					% The types of the arguments to the
					% constructor. 
					%
					% Note that currently we store all 
					% fields as type mlds__generic_type.
					% But the type here is the actual
					% argument type, which does not
					% have to be mlds__generic_type.
					% It is the responsibility of the
					% MLDS->target code output phase
					% to box the arguments if necessary.
					% 
		)

	;	mark_hp(mlds__lval)
			% Tell the heap sub-system to store a marker
			% (for later use in restore_hp/1 instructions)
			% in the specified lval
			%
			% It's OK for the target to treat this as a no-op,
			% and probably that is what most targets will do.

	;	restore_hp(mlds__rval)
			% The rval must be a marker as returned by mark_hp/1.
			% The effect is to deallocate all the memory which
			% was allocated since that call to mark_hp.
			%
			% It's OK for the target to treat this as a no-op,
			% and probably that is what most targets will do.

	%
	% trail management
	%

	;	trail_op(trail_op)

	%
	% foreign language interfacing
	%

	;	target_code(target_lang, list(target_code_component))
			% Do whatever is specified by the target_code_compoenents,
			% which can be any piece of code in the specified
			% target language (C, assembler, or whatever)
			% that does not have any non-local flow of control.
	.

	%
	% This is just a random selection of possible languages
	% that we might want to target...
	%
:- type target_lang
	--->	lang_C
	;	lang_GNU_C
	;	lang_C_minus_minus
	;	lang_asm
	;	lang_java_asm
	;	lang_java_bytecode
	.

:- type target_code_component
	--->	user_target_code(string, maybe(prog_context))
			% user_target_code holds C code from
			% the user's `pragma c_code' declaration
	;	raw_target_code(string)
			% raw_target_code holds C code that the
			% compiler has generated.  To ensure that
			% following `#line' directives work OK,
			% the string in a raw_target_code must
			% end in `\n' (or `\n' followed by whitespace).
	;	target_code_input(mlds__rval)
	;	target_code_output(mlds__lval)
	;	name(mlds__entity_name)
	.

	% XXX I'm not sure what representation we should use here
:- type ctor_name == string.

	%
	% trail management
	% For documentation, see the corresponding LLDS instructions
	% in llds.m.
	%
:- type trail_op
	--->	store_ticket(mlds__lval)
	;	reset_ticket(mlds__rval, mlds__reset_trail_reason)
	;	discard_ticket
	;	prune_ticket
	;	mark_ticket_stack(mlds__lval)
	;	prune_tickets_to(mlds__rval)
% 	;	discard_tickets_to(mlds__rval)	% used only by the library
	.

%-----------------------------------------------------------------------------%

	%
	% A field_id represents some data within an object
	%

:- type field_id 
	--->		% offset(N) represents the field
			% at offset N Words.
	 	offset(mlds__rval)
	;		% named_field(Name, CtorType) represents the field
			% with the specified name.  The CtorType gives the
			% MLDS type for this particular constructor.
			% The type of the object is given by the PtrType
			% in the field(..) lval; CtorType may either be
			% the same as PtrType, or it may be a pointer to
			% a derived class.  In the latter case, the
			% MLDS->target code back-end is responsible
			% for inserting a downcast from PtrType to CtorType
			% before accessing the field.
		named_field(mlds__fully_qualified_name(field_name), mlds__type)
	.

:- type field_name == string.

	%
	% An mlds__var represents a variable or constant.
	%
:- type mlds__var == mlds__fully_qualified_name(mlds__var_name).
:- type mlds__var_name == string.

	%
	% An lval represents a data location or variable that can be used
	% as the target of an assignment.
	%
:- type mlds__lval 

	%
	% values on the heap
	% or fields of a structure
	%
	--->	field(maybe(mlds__tag), mlds__rval, field_id, 
			mlds__type, mlds__type)
				% field(Tag, Address, FieldName, FieldType,
				%	PtrType)
				% selects a field of a compound term.
				% Address is a tagged pointer to a cell
				% on the heap; the offset into the cell
				% is FieldNum words. If Tag is yes, the
				% arg gives the value of the tag; if it is
				% no, the tag bits will have to be masked off.
				% The value of the tag should be given if
				% it is known, since this will lead to
				% faster code.
				% The FieldType is the type of the field.
				% The PtrType is the type of the pointer
				% from which we are fetching the field.
				%
				% Note that currently we store all fields
				% of objects created with new_object
				% as type mlds__generic_type. For such objects,
				% the type here should be mlds__generic_type,
				% not the actual type of the field.
				% If the actual type is different, then it
				% is the HLDS->MLDS code generator's
				% responsibility to insert the necessary
				% code to handle boxing/unboxing.

	%
	% values somewhere in memory
	% this is the deference operator (e.g. unary `*' in C)
	%
	;	mem_ref(mlds__rval, mlds__type)	
				% The rval should have originally come
				% from a mem_addr rval.
				% The type is the type of the value being
				% dereferenced

	%
	% variables
	% these may be local or they may come from some enclosing scope
	% the variable name should be fully qualified
	%
	;	var(mlds__var)
	
	.

%-----------------------------------------------------------------------------%

	% XXX this probably needs work

	% An rval is an expression that represents a value.
:- type mlds__rval	
	--->	lval(mlds__lval)
		% The value of an `lval' rval is just the value stored in
		% the specified lval.

	;	mkword(mlds__tag, mlds__rval)
		% Given a pointer and a tag, mkword returns a tagged pointer.
		%
		% (XXX It might be more consistent to make this a binary_op,
		% with the tag argument just being an rval, rather than
		% having `mkword' be a separate kind of rval.)

	;	const(mlds__rval_const)

	;	unop(mlds__unary_op, mlds__rval)

	;	binop(binary_op, mlds__rval, mlds__rval)

	;	mem_addr(mlds__lval).
		% The address of a variable, etc.

:- type mlds__unary_op
	--->	box(mlds__type)
	;	unbox(mlds__type)
	;	cast(mlds__type) % XXX it might be worthwhile adding the 
				 % type that we cast from.
	;	std_unop(builtin_ops__unary_op).

:- type mlds__rval_const
	--->	true
	;	false
	;	int_const(int)
	;	float_const(float)
	;	string_const(string)
			% A multi_string_const is a string containing
			% embedded NULs, whose real length is given
			% by the integer, and not the location of the
			% first null character.
	;	multi_string_const(int, string)
	;	code_addr_const(mlds__code_addr)
	;	data_addr_const(mlds__data_addr).

:- type mlds__code_addr
	--->	proc(mlds__qualified_proc_label, mlds__func_signature)
	;	internal(mlds__qualified_proc_label, mlds__func_sequence_num,
			mlds__func_signature).

:- type mlds__data_addr
	--->	data_addr(mlds_module_name, mlds__data_name).
			% module name; which var

:- type mlds__data_name
	--->	var(mlds__var_name)
			% ordinary variables
	;	common(int)
			% Compiler-introduced constants representing
			% global constants.  These are called "common"
			% because they may be common sub-expressions.
	%
	% Stuff for handling polymorphism/RTTI and type classes.
	%
	;	rtti(rtti_type_id, rtti_name)
	;	base_typeclass_info(hlds_data__class_id, string)
			% class name & class arity, names and arities of the
			% types
	%
	% Stuff for handling debugging and accurate garbage collection.
	% (Those features are not yet implemented for the MLDS back-end,
	% so these data_names are not yet used.)
	%
	;	module_layout
			% Layout information for the current module.
	;	proc_layout(mlds__proc_label)
			% Layout structure for the given procedure.
	;	internal_layout(mlds__proc_label, mlds__func_sequence_num)
			% Layout structure for the given internal MLDS func.
	%
	% Stuff for tabling
	%
	;	tabling_pointer(mlds__proc_label).
			% A variable that contains a pointer that points to
			% the table used to implement memoization, loopcheck
			% or minimal model semantics for the given procedure.

%-----------------------------------------------------------------------------%

%
% Note: the types `tag' and `reset_trail_reason' here are all
% defined exactly the same as the ones in llds.m.  The definitions are
% duplicated here because we don't want mlds.m to depend on llds.m.
% (Alternatively, we could move both these definitions into a new module
% imported by both mlds.m and llds.m, but these definitions are small enough
% and simple enough that I don't think it is worth creating a new module
% just for them.)
%

	% A tag should be a small non-negative integer.
:- type tag == int.

	% see runtime/mercury_trail.h
:- type reset_trail_reason
	--->	undo
	;	commit
	;	solve
	;	exception
	;	gc
	.

%-----------------------------------------------------------------------------%

:- type mlds__qualified_proc_label
	==	mlds__fully_qualified_name(mlds__proc_label).
:- type mlds__proc_label
	==	pair(mlds__pred_label, proc_id).

:- type mlds__qualified_pred_label
	==	mlds__fully_qualified_name(mlds__pred_label).

	% An mlds__pred_label is a structure containing information that
	% uniquely identifies a HLDS predicate within a given module.
	%
	% Note that a predicate can have both a declaring and a defining module.
	% The defining module is the module that provides the code for the
	% predicate, the declaring module contains the `:- pred' declaration.
	% When these are different, as for specialised versions of predicates
	% from `.opt' files, the defining module's name is added as a
	% qualifier to the pred name.
:- type mlds__pred_label
	--->	pred(
			pred_or_func,		% predicate/function
				% The declaring module,
				% if different to the defining module
			maybe(mercury_module_name),
			string,			% name
			arity			% arity
		)
			
	;	special_pred(
			string,			% pred name
			maybe(mercury_module_name),
				% The module declaring the type,
				% if this is different to module defining
				% the special_pred.
			string,			% the type name
			arity			% the type arity
		).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module modules.
:- import_module int, term, string, require.

%-----------------------------------------------------------------------------%

mlds__get_module_name(mlds(ModuleName, _, _, _)) = ModuleName.

%-----------------------------------------------------------------------------%

% Currently mlds__contexts just contain a prog_context.

:- type mlds__context ---> mlds__context(prog_context).

mlds__make_context(Context) = mlds__context(Context).

mlds__get_prog_context(mlds__context(Context)) = Context.

%-----------------------------------------------------------------------------%

% Currently we return mlds__types that are just the same as Mercury types,
% except that we also store the type category, so that we
% can tell if the type is an enumeration or not, without
% needing to refer to the HLDS type_table.
% XXX It might be a better idea to get rid of the mercury_type/2
% MLDS type and instead fully convert all Mercury types to MLDS types.

mercury_type_to_mlds_type(ModuleInfo, Type) = mercury_type(Type, Category) :-
	classify_type(Type, ModuleInfo, Category).

%-----------------------------------------------------------------------------%

mlds__get_func_signature(func_params(Parameters, RetTypes)) =
		func_signature(ParamTypes, RetTypes) :-
	assoc_list__values(Parameters, ParamTypes).

%-----------------------------------------------------------------------------%

% Mercury module names are the same as MLDS package names, except that
% modules in the Mercury standard library map get a `mercury' prefix
% e.g. `mercury.builtin', `mercury.io', `mercury.std_util', etc.,
% when mapped to MLDS package names.

:- type mlds_module_name == prog_data__module_name.

mercury_module_name_to_mlds(MercuryModule) = MLDS_Package :-
	(
		MercuryModule = unqualified(ModuleName),
		mercury_std_library_module(ModuleName)
	->
		MLDS_Package = qualified(unqualified("mercury"), ModuleName)
	;
		MLDS_Package = MercuryModule
	).

mlds_module_name_to_sym_name(MLDS_Package) = MLDS_Package.

mlds__append_class_qualifier(Package, ClassName, ClassArity) =
		qualified(Package, ClassQualifier) :-
	string__format("%s_%d", [s(ClassName), i(ClassArity)],
		ClassQualifier).

%-----------------------------------------------------------------------------%

%
% We represent the set of declaration flags as a bunch of bit-fields packed
% into a single int.
%
:- type mlds__decl_flags == int.

%
% Here we define which bits are used to store each bitfield.
%
% It would be nicer to use a language builtin, e.g. index/2, for these.
% But currently builtin__index/2 does not work in the reverse mode,
% and you can't use std_util__construct/4 since that numbers the
% alternatives in a different order than builtin__index/2.
%
% It would also be nice to use a typeclass:
%	:- typeclass bitfield(T) where [
%		func bits(T) = int,
%		func mask(T::unused) = (int::out) is det
%	].
% But currently that is too cumbersome, since you can't define class
% methods inline.
%
% On the other hand, doing it manually may be more efficient than either
% of those two approaches.
%

:- func access_bits(access) = int.
:- mode access_bits(in) = out is det.
:- mode access_bits(out) = in is semidet.
access_bits(public)  	= 0x00.
access_bits(private) 	= 0x01.
access_bits(protected)	= 0x02.
access_bits(default)	= 0x03.
% 0x4 - 0x7 reserved

:- func access_mask = int.
access_mask = 0x07.

:- func per_instance_bits(per_instance) = int.
:- mode per_instance_bits(in) = out is det.
:- mode per_instance_bits(out) = in is semidet.
per_instance_bits(one_copy) 	= 0x00.
per_instance_bits(per_instance)	= 0x08.

:- func per_instance_mask = int.
per_instance_mask = per_instance_bits(per_instance).

:- func virtuality_bits(virtuality) = int.
:- mode virtuality_bits(in) = out is det.
:- mode virtuality_bits(out) = in is semidet.
virtuality_bits(non_virtual) 	= 0x00.
virtuality_bits(virtual)	= 0x10.

:- func virtuality_mask = int.
virtuality_mask = virtuality_bits(virtual).

:- func finality_bits(finality) = int.
:- mode finality_bits(in) = out is det.
:- mode finality_bits(out) = in is semidet.
finality_bits(overridable) 	= 0x00.
finality_bits(final)		= 0x20.

:- func finality_mask = int.
finality_mask = finality_bits(final).

:- func constness_bits(constness) = int.
:- mode constness_bits(in) = out is det.
:- mode constness_bits(out) = in is semidet.
constness_bits(modifiable) 	= 0x00.
constness_bits(const)		= 0x40.

:- func constness_mask = int.
constness_mask = constness_bits(const).

:- func abstractness_bits(abstractness) = int.
:- mode abstractness_bits(in) = out is det.
:- mode abstractness_bits(out) = in is semidet.
abstractness_bits(abstract) 	= 0x00.
abstractness_bits(concrete)	= 0x80.

:- func abstractness_mask = int.
abstractness_mask = abstractness_bits(concrete).

%
% Here we define the functions to lookup a member of the set.
%

access(Flags) = promise_det(pred(Access::out) is semidet :-
	Flags /\ access_mask = access_bits(Access)).

per_instance(Flags) = promise_det(pred(PerInstance::out) is semidet :-
	Flags /\ per_instance_mask = per_instance_bits(PerInstance)).

virtuality(Flags) = promise_det(pred(Virtuality::out) is semidet :-
	Flags /\ virtuality_mask = virtuality_bits(Virtuality)).

finality(Flags) = promise_det(pred(Finality::out) is semidet :-
	Flags /\ finality_mask = finality_bits(Finality)).

constness(Flags) = promise_det(pred(Constness::out) is semidet :-
	Flags /\ constness_mask = constness_bits(Constness)).

abstractness(Flags) = promise_det(pred(Abstractness::out) is semidet :-
	Flags /\ abstractness_mask = abstractness_bits(Abstractness)).

:- func promise_det(pred(T)) = T.
:- mode promise_det(pred(out) is semidet) = out is det.

promise_det(Pred) = X :-
	(if Pred(X0) then X = X0 else error("promise_det failed")).


%
% Here we define the functions to set a member of the set.
%

set_access(Flags, Access) =
	Flags /\ \access_mask \/ access_bits(Access).

set_per_instance(Flags, PerInstance) =
	Flags /\ \per_instance_mask \/ per_instance_bits(PerInstance).

set_virtuality(Flags, Virtuality) =
	Flags /\ \virtuality_mask \/ virtuality_bits(Virtuality).

set_finality(Flags, Finality) =
	Flags /\ \finality_mask \/ finality_bits(Finality).

set_constness(Flags, Constness) =
	Flags /\ \constness_mask \/ constness_bits(Constness).

set_abstractness(Flags, Abstractness) =
	Flags /\ \abstractness_mask \/ abstractness_bits(Abstractness).

init_decl_flags(Access, PerInstance, Virtuality, Finality, Constness,
		Abstractness) =
	access_bits(Access) \/
	per_instance_bits(PerInstance) \/
	virtuality_bits(Virtuality) \/
	finality_bits(Finality) \/
	constness_bits(Constness) \/
	abstractness_bits(Abstractness).

%-----------------------------------------------------------------------------%
