%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_data.m.
% Main author: fjh.
%
% This module defines a data structure for representing Mercury programs.
%
% This data structure specifies basically the same information as is
% contained in the source code, but in a parse tree rather than a flat file.
% Simplifications are done only by make_hlds.m, which transforms
% the parse tree which we built here into the HLDS.

:- module parse_tree__prog_data.

:- interface.

:- import_module libs__globals.
:- import_module libs__options.
:- import_module libs__rat.

:- import_module mdbcomp__prim_data.
:- import_module recompilation.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

	% This is how programs (and parse errors) are represented.

:- type message_list	==	list(pair(string, term)).
				% the error/warning message, and the
				% term to which it relates

:- type compilation_unit
	--->	module(
			module_name,
			item_list
		).

:- type item_list	==	list(item_and_context).

:- type item_and_context ==	pair(item, prog_context).

:- type item
	--->	clause(
			cl_varset		:: prog_varset,
			cl_pred_or_func		:: pred_or_func,
			cl_predname		:: sym_name,
			cl_head_args		:: list(prog_term),
			cl_body			:: goal
		)

		% `:- type ...':
		% a definition of a type, or a declaration of an abstract type.
	; 	type_defn(
			td_tvarset		:: tvarset,
			td_ctor_name		:: sym_name,
			td_ctor_args		:: list(type_param),
			td_ctor_defn		:: type_defn,
			td_cond			:: condition
		)

		% `:- inst ... = ...':
		% a definition of an inst.
	; 	inst_defn(
			id_varset		:: inst_varset,
			id_inst_name		:: sym_name,
			id_inst_args		:: list(inst_var),
			id_inst_defn		:: inst_defn,
			id_cond			:: condition
		)

		% `:- mode ... = ...':
		% a definition of a mode.
	; 	mode_defn(
			md_varset		:: inst_varset,
			md_mode_name		:: sym_name,
			md_mode_args		:: list(inst_var),
			md_mode_defn		:: mode_defn,
			md_cond			:: condition
		)

	; 	module_defn(
			module_defn_varset	:: prog_varset,
			module_defn_module_defn	:: module_defn
		)

		% `:- pred ...' or `:- func ...':
		% a predicate or function declaration.
		% This specifies the type of the predicate or function,
		% and it may optionally also specify the mode and determinism.
	; 	pred_or_func(
			pf_tvarset		:: tvarset,
			pf_instvarset		:: inst_varset,
			pf_existqvars		:: existq_tvars,
			pf_which		:: pred_or_func,
			pf_name			:: sym_name,
			pf_arg_decls		:: list(type_and_mode),
			pf_maybe_with_type	:: maybe(type),
			pf_maybe_with_inst	:: maybe(inst),
			pf_maybe_detism		:: maybe(determinism),
			pf_cond			:: condition,
			pf_purity		:: purity,
			pf_class_context	:: prog_constraints
		)
		%	The WithType and WithInst fields hold the `with_type`
		% 	and `with_inst` annotations, which are syntactic
		%	sugar that is expanded by equiv_type.m
		%	equiv_type.m will set these fields to `no'.

		% `:- mode ...':
		% a mode declaration for a predicate or function.
	; 	pred_or_func_mode(
			pfm_instvarset		:: inst_varset,
			pfm_which		:: maybe(pred_or_func),
			pfm_name		:: sym_name,
			pfm_arg_modes		:: list(mode),
			pfm_maybe_with_inst	:: maybe(inst),
			pfm_maybe_detism	:: maybe(determinism),
			pfm_cond		:: condition
		)
		%	The WithInst field holds the `with_inst` annotation,
		%	which is syntactic sugar that is expanded by
		%	equiv_type.m. equiv_type.m will set the field to `no'.

	;	pragma(
			pragma_type		:: pragma_type
		)

	;	promise(
			prom_type		:: promise_type,
			prom_clause		:: goal,
			prom_varset		:: prog_varset,
			prom_univ_quant_vars	:: prog_vars
		)

	;	typeclass(
			tc_constraints		:: list(prog_constraint),
			tc_fundeps		:: list(prog_fundep),
			tc_class_name		:: class_name,
			tc_class_params		:: list(tvar),
			tc_class_methods	:: class_interface,
			tc_varset		:: tvarset
		)

	;	instance(
			ci_deriving_class	:: list(prog_constraint),
			ci_class_name		:: class_name,
			ci_types		:: list(type),
			ci_method_instances	:: instance_body,
			ci_varset		:: tvarset,
			ci_module_containing_instance :: module_name
		)

	;	nothing(
			nothing_maybe_warning	:: maybe(item_warning)
		).
		% used for items that should be ignored (for the
		% purposes of backwards compatibility etc)

	% indicates the type of information the compiler should get from the
	% declaration's clause
:- type promise_type
		% promise ex declarations
	--->	exclusive		% each disjunct is mutually exclusive
	;	exhaustive		% disjunction cannot fail
	;	exclusive_exhaustive	% both of the above

		% assertions
	; 	true.			% promise goal is true

:- type type_and_mode
	--->	type_only(type)
	;	type_and_mode(type, mode).

	% Purity indicates whether a goal can have side effects or can
	% depend on global state.  See purity.m and the "Purity" section
	% of the Mercury language reference manual.
:- type purity		--->	pure
			;	(semipure)
			;	(impure).

	% The `determinism' type specifies how many solutions a given
	% procedure may have.  Procedures for manipulating this type
	% are defined in det_analysis.m and hlds_data.m.
:- type determinism
	--->	det
	;	semidet
	;	nondet
	;	multidet
	;	cc_nondet
	;	cc_multidet
	;	erroneous
	;	failure.

	% The `is_solver_type' type specifies whether a type is a "solver" type,
	% for which `any' insts are interpreted as "don't know", or a non-solver
	% type for which `any' is the same as `bound(...)'.
:- type is_solver_type
	--->	non_solver_type
			% The inst `any' is always `bound' for this type.
	;	solver_type.
			% The inst `any' is not always `bound' for this type
			% (i.e. the type was declared with
			% `:- solver type ...').

:- type item_warning
	--->	item_warning(
			maybe(option),	% Option controlling whether the
					% warning should be reported.
			string,		% The warning.
			term		% The term to which it relates.
		).

%-----------------------------------------------------------------------------%
%
% Pragmas
%

:- type foreign_decl_is_local
	--->	foreign_decl_is_local
	;	foreign_decl_is_exported.

:- type pragma_type
	%
	% Foreign language interfacing pragmas
	%
			% a foreign language declaration, such as C
			% header code.
	--->	foreign_decl(
			decl_lang	:: foreign_language,
			decl_is_local	:: foreign_decl_is_local,
			decl_decl	:: string
		)

	;	foreign_code(
			code_lang	:: foreign_language,
			code_code	:: string)

	;	foreign_proc(
			proc_attrs	:: pragma_foreign_proc_attributes,
			proc_name	:: sym_name,
			proc_p_or_f	:: pred_or_func,
			proc_vars	:: list(pragma_var),
			proc_varset	:: prog_varset,
			proc_impl	:: pragma_foreign_code_impl
			% Set of foreign proc attributes, eg.:
			%	what language this code is in
			%	whether or not the code may call Mercury,
			%	whether or not the code is thread-safe
			% PredName, Predicate or Function, Vars/Mode,
			% VarNames, Foreign Code Implementation Info
		)

	;	foreign_import_module(
			imp_lang	:: foreign_language,
			imp_module	:: module_name
			% Equivalent to
			% `:- pragma foreign_decl(Lang, "#include <module>.h").'
			% except that the name of the header file is not
			% hard-coded, and mmake can use the dependency
			% information.
		)

	;	export(
			exp_predname	:: sym_name,
			exp_p_or_f	:: pred_or_func,
			exp_modes	:: list(mode),
			exp_foreign_name :: string
			% Predname, Predicate/function, Modes,
			% foreign function name.
		)

	;	import(
			import_pred_name :: sym_name,
			import_p_or_f	:: pred_or_func,
			import_modes	:: list(mode),
			import_attrs	:: pragma_foreign_proc_attributes,
			import_foreign_name :: string
			% Predname, Predicate/function, Modes,
			% Set of foreign proc attributes, eg.:
			%    whether or not the foreign code may call Mercury,
			%    whether or not the foreign code is thread-safe
			% foreign function name.
		)
	%
	% Optimization pragmas
	%
	;	type_spec(
			tspec_pred_name	:: sym_name,
			tspec_new_name	:: sym_name,
			tspec_arity	::  arity,
			tspec_p_or_f	:: maybe(pred_or_func),
			tspec_modes	:: maybe(list(mode)),
			tspec_tsubst	:: type_subst,
			tspec_tvarset	:: tvarset,
			tspec_items	:: set(item_id)
			% PredName, SpecializedPredName, Arity,
			% PredOrFunc, Modes if a specific procedure was
			% specified, type substitution (using the variable
			% names from the pred declaration), TVarSet,
			% Equivalence types used
		)

	;	inline(
			inline_name	:: sym_name,
			inline_arity	:: arity
			% Predname, Arity
		)

	;	no_inline(
			noinline_name	:: sym_name,
			noinline_arity	:: arity
			% Predname, Arity
		)

	;	unused_args(
			unused_p_or_f	:: pred_or_func,
			unused_name	:: sym_name,
			unused_arity	:: arity,
			unused_mode	:: mode_num,
			unused_args	:: list(int)
			% PredName, Arity, Mode number, Removed arguments.
			% Used for inter-module unused argument
			% removal, should only appear in .opt files.
		)
	;
		exceptions(
			exceptions_p_or_f :: pred_or_func,
			exceptions_name	  :: sym_name,
			exceptions_arity  :: arity,
			exceptions_mode	  :: mode_num,
			exceptions_status :: exception_status
			% PredName, Arity, Mode number, Exception status.
			% Should only appear in `.opt' or `.trans_opt' files.
		)
	%
	% Diagnostics pragmas (pragmas related to compiler warnings/errors)
	%

	;	obsolete(
			obsolete_name	:: sym_name,
			obsolete_arity	:: arity
			% Predname, Arity
		)

	;	source_file(
			source_file	:: string
			% Source file name.
		)

	%
	% Evaluation method pragmas
	%

	;	tabled(
			tabled_method	:: eval_method,
			tabled_name	:: sym_name,
			tabled_arity	:: int,
			tabled_p_or_f	:: maybe(pred_or_func),
			tabled_mode	:: maybe(list(mode))
			% Tabling type, Predname, Arity, PredOrFunc?, Mode?
		)

	;	fact_table(
			fact_table_name	:: sym_name,
			fact_table_arity :: arity,
			fact_table_file	:: string
			% Predname, Arity, Fact file name.
		)

	;	reserve_tag(
			restag_type	:: sym_name,
			restag_arity	:: arity
			% Typename, Arity
		)

	%
	% Aditi pragmas
	%
	;	aditi(
			aditi_name	:: sym_name,
			aditi_arity	:: arity
			% Predname, Arity
		)

	;	base_relation(
			baserel_name	:: sym_name,
			baserel_arity	:: arity
			% Predname, Arity
			%
			% Eventually, these should only occur in
			% automatically generated database interface
			% files, but for now there's no such thing,
			% so they can occur in user programs.
		)

	;	aditi_index(
			index_name	:: sym_name,
			index_arity	:: arity,
			index_spec	:: index_spec
			% PredName, Arity, IndexType, Attributes
			%
			% Specify an index on a base relation.
		)

	;	naive(
			naive_name	:: sym_name,
			naive_arity	:: arity
			% Predname, Arity
			% Use naive evaluation.
		)

	;	psn(
			psn_name	:: sym_name,
			psn_arity	:: arity
			% Predname, Arity
			% Use predicate semi-naive evaluation.
		)

	;	aditi_memo(
			aditimemo_name	:: sym_name,
			aditimemo_arity	:: arity
			% Predname, Arity
		)

	;	aditi_no_memo(
			aditinomemo_name :: sym_name,
			aditinomemo_arity :: arity
			% Predname, Arity
		)

	;	supp_magic(
			suppmagic_name	:: sym_name,
			suppmagic_arity	:: arity
			% Predname, Arity
		)

	;	context(
			context_name	:: sym_name,
			context_arity	:: arity
			% Predname, Arity
		)

	;	owner(
			owner_name	:: sym_name,
			owner_arity	:: arity,
			owner_id	:: string
			% PredName, Arity, String.
		)

	%
	% Purity pragmas
	%

	;	promise_pure(
			pure_name	:: sym_name,
			pure_arity	:: arity
			% Predname, Arity
		)

	;	promise_semipure(
			semipure_name	:: sym_name,
			semipure_arity	:: arity
			% Predname, Arity
		)

	%
	% Termination analysis pragmas
	%

	;	termination_info(
			terminfo_p_or_f	:: pred_or_func,
			terminfo_name	:: sym_name,
			terminfo_mode	:: list(mode),
			terminfo_args	:: maybe(pragma_arg_size_info),
			terminfo_term	:: maybe(pragma_termination_info)
			% the list(mode) is the declared argmodes of the
			% procedure, unless there are no declared argmodes,
			% in which case the inferred argmodes are used.
			% This pragma is used to define information about a
			% predicates termination properties.  It is most
			% useful where the compiler has insufficient
			% information to be able to analyse the predicate.
			% This includes c_code, and imported predicates.
			% termination_info pragmas are used in opt and
			% trans_opt files.
		)

	;	termination2_info(
			pred_or_func, 
			sym_name, 
			list(mode),
			maybe(pragma_constr_arg_size_info),
			maybe(pragma_constr_arg_size_info),
			maybe(pragma_termination_info)
		)

	;	terminates(
			term_name	:: sym_name,
			term_arity	:: arity
			% Predname, Arity
		)

	;	does_not_terminate(
			noterm_name	:: sym_name,
			noterm_arity	:: arity
			% Predname, Arity
		)

	;	check_termination(
			checkterm_name	:: sym_name,
			checkterm_arity	:: arity
			% Predname, Arity
		)

	;	mode_check_clauses(
			mode_check_clause_name	:: sym_name,
			mode_check_clause_arity	:: arity
		).

%
% Stuff for the foreign interfacing pragmas.
%

	%
	% A foreign_language_type represents a type that is defined in a
	% foreign language and accessed in Mercury (most likely through
	% pragma foreign_type).
	% Currently we only support foreign_language_types for IL.
	%

	%
	% It is important to distinguish between IL value types and
	% reference types, the compiler may need to generate different code
	% for each of these cases.
	%

:- type foreign_language_type
	--->	il(il_foreign_type)
	;	c(c_foreign_type)
	;	java(java_foreign_type).

:- type il_foreign_type
	--->	il(
			ref_or_val,	% An indicator of whether the type is a
					% reference of value type.
			string,		% The location of the .NET name (the
					% assembly)
			sym_name	% The .NET type name
		).

:- type c_foreign_type
	--->	c(
			string		% The C type name
		).

:- type java_foreign_type
	--->	java(
			string		% The Java type name
		).

:- type ref_or_val
	--->	reference
	;	value.

%
% Stuff for tabling pragmas
%

:- type eval_minimal_method
	--->	stack_copy		% saving and restoring stack segments
					% as necessary
	;	own_stacks.		% each generator has own stacks

	% The evaluation method that should be used for a procedure.
	% Ignored for Aditi procedures.
:- type eval_method
	--->	eval_normal		% normal mercury
					% evaluation
	;	eval_loop_check		% loop check only
	;	eval_memo(call_table_strictness)
					% memoing + loop check
	;	eval_table_io(		% memoing I/O actions for debugging
			table_io_is_decl,
			table_io_is_unitize
		)
	;	eval_minimal(eval_minimal_method).
					% minimal model evaluation

:- type call_table_strictness
	--->	strict
	;	fast_loose.

:- type table_io_is_decl
	--->	table_io_decl		% The procedure is tabled for
					% declarative debugging.
	;	table_io_proc.		% The procedure is tabled only for
					% procedural debugging.

:- type table_io_is_unitize
	--->	table_io_unitize	% The procedure is tabled for I/O
					% together with its Mercury
					% descendants.
	;	table_io_alone.		% The procedure is tabled for I/O
					% by itself; it can have no Mercury
					% descendants.

%
% Stuff for the `aditi_index' pragma
%

	% For Aditi base relations, an index_spec specifies how the base
	% relation is indexed.
:- type index_spec
	--->	index_spec(
			index_type,
			list(int)	% which attributes are being indexed on
					% (attribute numbers start at 1)
		).

	% Hash indexes?
:- type index_type
	--->	unique_B_tree
	;	non_unique_B_tree.

%
% Stuff for the `termination_info' pragma.
% See term_util.m.
%

:- type generic_arg_size_info(ErrorInfo)
	--->	finite(int, list(bool))
				% The termination constant is a finite integer.
				% The list of bool has a 1:1 correspondence
				% with the input arguments of the procedure.
				% It stores whether the argument contributes
				% to the size of the output arguments.
	;	infinite(ErrorInfo).
				% There is no finite integer for which the
				% above equation is true.

:- type generic_termination_info(TermInfo, ErrorInfo)
	--->	cannot_loop(TermInfo)	% This procedure definitely terminates
					% for all possible inputs.
	;	can_loop(ErrorInfo).
				% This procedure might not terminate.

:- type pragma_arg_size_info	== generic_arg_size_info(unit).
:- type pragma_termination_info	== generic_termination_info(unit, unit).

%
% Stuff for the `termination2_info' pragma.
%
	
	% This is the form in which termination information from other 
	% modules (imported via `.opt' or `.trans_opt' files) comes.
	% We convert this to an intermediate form and let the termination
	% analyser convert it to the correct form.

:- type arg_size_constr
	--->	le(list(arg_size_term), rat)
	;	eq(list(arg_size_term), rat).

:- type arg_size_term == pair(int, rat).

:- type pragma_constr_arg_size_info == list(arg_size_constr).

%
% Stuff for the `unused_args' pragma.
%

	% This `mode_num' type is only used for mode numbers written out in
	% automatically-generated `pragma unused_args' pragmas in `.opt'
	% files.
	% The mode_num gets converted to an HLDS proc_id by make_hlds.m.
	% We don't want to use the `proc_id' type here since the parse tree
	% (prog_data.m) should not depend on the HLDS.
:- type mode_num == int.

%
% Stuff for the `exceptions' pragma.
%

:- type exception_status 
		---> 	will_not_throw 
				% This procedure will not throw an
				% exception.
		
		;	may_throw(exception_type)
				% This procedure may throw an exception
				% The exception is classified by the 
				% `exception_type' type.
		;	conditional.
				% Whether the procedure will not throw an
				% exception depends upon the value of one
				% or more polymorphic arguments.
				% XXX This needs to be extended for ho
				% preds.  (See exception_analysis.m for
				% more details).
:- type exception_type	
		--->	user_exception
				% The exception that might be thrown is of
				% a result of some code calling
				% exception.throw/1.
		;	type_exception.	
				% The exception is a result of a compiler
				% introduced unification/comparison maybe 
				% throwing an exception (in the case of 
				% user-defined equality or comparison) or
				% propagating an exception from them.

%
% Stuff for the `type_spec' pragma.
%

	% The type substitution for a `pragma type_spec' declaration.
	% Elsewhere in the compiler we generally use the `tsubst' type
	% which is a map rather than an assoc_list.
:- type type_subst == assoc_list(tvar, type).

%
% Stuff for `foreign_code' pragma.
%

	% This type holds information about the implementation details
	% of procedures defined via `pragma foreign_code'.
	%
	% All the strings in this type may be accompanied by the context
	% of their appearance in the source code. These contexts are
	% used to tell the foreign language compiler where the included
	% code comes from, to allow it to generate error messages that
	% refer to the original appearance of the code in the Mercury
	% program.
	% The context is missing if the foreign code was constructed by
	% the compiler.
	% Note that nondet pragma foreign definitions might not be
	% possible in all foreign languages.
:- type pragma_foreign_code_impl
	--->	ordinary(		% This is a foreign language
					% definition of a model_det
					% or model_semi procedure. (We
					% also allow model_non, until
					% everyone has had time to adapt
					% to the new way
					% of handling model_non pragmas.)
			string,		% The code of the procedure.
			maybe(prog_context)
		)
	;	nondet(			% This is a foreign language
					% definition of a model_non
					% procedure.
			string,
			maybe(prog_context),
					% The info saved for the time when
					% backtracking reenters this procedure
					% is stored in a data structure.
					% This arg contains the field
					% declarations.

			string,
			maybe(prog_context),
					% Gives the code to be executed when
					% the procedure is called for the first
					% time. This code may access the input
					% variables.

			string,
			maybe(prog_context),
					% Gives the code to be executed when
					% control backtracks into the procedure.
					% This code may not access the input
					% variables.

			pragma_shared_code_treatment,
					% How should the shared code be
					% treated during code generation.
			string,
			maybe(prog_context)
					% Shared code that is executed after
					% both the previous code fragments.
					% May not access the input variables.
		)
	;	import(
			string,		% Pragma imported C func name
			string,		% Code to handle return value
			string,		% Comma separated variables which
					% the import function is called
					% with.

			maybe(prog_context)
		).

	% The use of this type is explained in the comment at the top of
	% pragma_c_gen.m.
:- type pragma_shared_code_treatment
	--->	duplicate
	;	share
	;	automatic.

:- type foreign_import_module_info	== list(foreign_import_module).
					% in reverse order

:- type foreign_import_module
	--->	foreign_import_module(
			foreign_language,
			module_name,
			prog_context
		).

%-----------------------------------------------------------------------------%
%
% Stuff for type classes
%

	% A class constraint represents a constraint that a given
	% list of types is a member of the specified type class.
	% It is an invariant of this data structure that
	% the types in a class constraint do not contain any
	% information in their prog_context fields.
	% This invariant is needed to ensure that we can do
	% unifications, map__lookups, etc., and get the
	% expected semantics.
	% (This invariant now applies to all types, but is
	% especially important here.)
	%
:- type prog_constraint
	--->	constraint(
			class_name,
			list(type)
		).

:- type prog_constraints
	--->	constraints(
			univ_constraints	:: list(prog_constraint),
						% universally quantified
						% constraints
			exist_constraints	:: list(prog_constraint)
						% existentially quantified
						% constraints
		).

	% A functional dependency on the variables in the head of a class
	% declaration.  This asserts that, given the complete set of
	% instances of this class, the binding of the range variables
	% can be uniquely determined from the binding of the domain
	% variables.
	%
:- type prog_fundep
	--->	fundep(
			domain			:: list(tvar),
			range			:: list(tvar)
		).

:- type class_name == sym_name.
:- type class_id
	--->	class_id(class_name, arity).

:- type class_interface
	--->	abstract
	;	concrete(list(class_method)).

	% The name class_method is a slight misnomer;
	% this type actually represents any declaration
	% that occurs in the body of a type class definition.
	% Such declarations may either declare class methods,
	% or they may declare modes of class methods.
	%
:- type class_method
		% pred_or_func(...) here represents a `pred ...' or `func ...'
		% declaration in a type class body, which declares
		% a predicate or function method.  Such declarations
		% specify the type of the predicate or function,
		% and may optionally also specify the mode and determinism.
		%
	--->	pred_or_func(
			tvarset,		% type variables
			inst_varset,		% inst variables
			existq_tvars,		% existentially quantified
						% type variables
			pred_or_func,
			sym_name,		% name of the pred or func
			list(type_and_mode),	% the arguments' types and
						% modes
			maybe(type),		% any `with_type` annotation
			maybe(inst),		% any `with_inst` annotation
			maybe(determinism),	% any determinism declaration
			condition,		% any attached declaration
			purity,			% any purity annotation
			prog_constraints,	% the typeclass constraints on
						% the declaration
			prog_context		% the declaration's context
		)

		% pred_or_func_mode(...) here represents a `mode ...'
		% declaration in a type class body.  Such a declaration
		% declares a mode for one of the type class methods.
		%
	; 	pred_or_func_mode(
			inst_varset,		% inst variables
			maybe(pred_or_func),	% whether the method is a pred
						% or a func; for declarations
						% using `with_inst`, we don't
						% know which until we've
						% expanded the inst.
			sym_name,		% the method name
			list(mode),		% the arguments' modes
			maybe(inst),		% any `with_inst` annotation
			maybe(determinism),	% any determinism declaration
			condition,		% any attached condition
			prog_context		% the declaration's context
		).

:- type instance_method
	--->	instance_method(
			pred_or_func,
			sym_name,		% method name
			instance_proc_def,
			arity,
			prog_context		% context of the instance
						% declaration
		).

:- type instance_proc_def
		% defined using the `pred(...) is <Name>' syntax
	--->	name(
			sym_name
		)

		% defined using clauses
	;	clauses(
			list(item)	% the items must be either
					% pred_clause or func_clause items
		).

:- type instance_body
	--->	abstract
	;	concrete(instance_methods).

:- type instance_methods ==	list(instance_method).

%-----------------------------------------------------------------------------%
%
% Some more stuff for `pragma c_code'.
%

		% an abstract type for representing a set of
		% `pragma_c_code_attribute's.
:- type pragma_foreign_proc_attributes.

:- func default_attributes(foreign_language) = pragma_foreign_proc_attributes.
:- func may_call_mercury(pragma_foreign_proc_attributes) = may_call_mercury.
:- func thread_safe(pragma_foreign_proc_attributes) = thread_safe.
:- func purity(pragma_foreign_proc_attributes) = purity.
:- func terminates(pragma_foreign_proc_attributes) = terminates.
:- func foreign_language(pragma_foreign_proc_attributes) = foreign_language.
:- func tabled_for_io(pragma_foreign_proc_attributes) = tabled_for_io.
:- func legacy_purity_behaviour(pragma_foreign_proc_attributes) = bool.
:- func may_throw_exception(pragma_foreign_proc_attributes) =
	may_throw_exception.
:- func ordinary_despite_detism(pragma_foreign_proc_attributes) = bool.
:- func extra_attributes(pragma_foreign_proc_attributes)
	= pragma_foreign_proc_extra_attributes.

:- pred set_may_call_mercury(may_call_mercury::in,
	pragma_foreign_proc_attributes::in,
	pragma_foreign_proc_attributes::out) is det.

:- pred set_thread_safe(thread_safe::in,
	pragma_foreign_proc_attributes::in,
	pragma_foreign_proc_attributes::out) is det.

:- pred set_foreign_language(foreign_language::in,
	pragma_foreign_proc_attributes::in,
	pragma_foreign_proc_attributes::out) is det.

:- pred set_tabled_for_io(tabled_for_io::in,
	pragma_foreign_proc_attributes::in,
	pragma_foreign_proc_attributes::out) is det.

:- pred set_purity(purity::in,
	pragma_foreign_proc_attributes::in,
	pragma_foreign_proc_attributes::out) is det.

:- pred set_terminates(terminates::in,
	pragma_foreign_proc_attributes::in,
	pragma_foreign_proc_attributes::out) is det.

:- pred set_may_throw_exception(may_throw_exception::in,	
	pragma_foreign_proc_attributes::in,
	pragma_foreign_proc_attributes::out) is det.

:- pred set_legacy_purity_behaviour(bool::in,
	pragma_foreign_proc_attributes::in,
	pragma_foreign_proc_attributes::out) is det.

:- pred set_ordinary_despite_detism(bool::in,
	pragma_foreign_proc_attributes::in,
	pragma_foreign_proc_attributes::out) is det.

:- pred add_extra_attribute(pragma_foreign_proc_extra_attribute::in,
	pragma_foreign_proc_attributes::in,
	pragma_foreign_proc_attributes::out) is det.

	% For pragma c_code, there are two different calling conventions,
	% one for C code that may recursively call Mercury code, and another
	% more efficient one for the case when we know that the C code will
	% not recursively invoke Mercury code.
:- type may_call_mercury
	--->	may_call_mercury
	;	will_not_call_mercury.

	% If thread_safe execution is enabled, then we need to put a mutex
	% around the C code for each `pragma c_code' declaration, unless
	% it's declared to be thread_safe.  If a piece of foreign code is
	% declared to be maybe_thread_safe whether we put the mutex around
	% the foreign code depends upon the `--maybe-thread-safe' compiler
	% flag.
	%
:- type thread_safe
	--->	not_thread_safe
	;	thread_safe
	;	maybe_thread_safe.

:- type tabled_for_io
	--->	not_tabled_for_io
	;	tabled_for_io
	;	tabled_for_io_unitize
	;	tabled_for_descendant_io.

:- type pragma_var
	--->	pragma_var(prog_var, string, mode).
	  	% variable, name, mode
		% we explicitly store the name because we need the real
		% name in code_gen

	% This type specifies the termination property of a procedure
	% defined using pragma c_code or pragma foreign_proc.
:- type terminates
	--->	terminates
			% The foreign code will terminate for all input.
			% (assuming any input streams are finite).

	;	does_not_terminate
			% The foreign code will not necessarily terminate for
			% some (possibly all) input.

	;	depends_on_mercury_calls.
			% The termination of the foreign code depends
			% on whether the code makes calls back to Mercury
			% (See termination.m for details).

:- type may_throw_exception
	--->	will_not_throw_exception
			% The foreign code will not result in an 
			% exception being thrown.
	
	;	default_exception_behaviour.
			% If the foreign proc. is erroneous then
			% mark it as throwing an exception.  Otherwise
			% mark it as throwing an exception if it makes
			% calls back to Mercury and not throwing an
			% exception otherwise.	

:- type pragma_foreign_proc_extra_attribute
	--->	max_stack_size(int)
	;	backend(backend).

:- type pragma_foreign_proc_extra_attributes ==
	list(pragma_foreign_proc_extra_attribute).

	% Convert the foreign code attributes to their source code
	% representations suitable for placing in the attributes list of
	% the pragma (not all attributes have one).
	% In particular, the foreign language attribute needs to be
	% handled separately as it belongs at the start of the pragma.
:- func attributes_to_strings(pragma_foreign_proc_attributes) = list(string).

%-----------------------------------------------------------------------------%
%
% Goals
%

	% Here's how clauses and goals are represented.
	% a => b --> implies(a, b)
	% a <= b --> implies(b, a) [just flips the goals around!]
	% a <=> b --> equivalent(a, b)

% clause/4 defined above

:- type goal		==	pair(goal_expr, prog_context).

:- type goal_expr
	% conjunctions
	--->	(goal , goal)	% (non-empty) conjunction
	;	true		% empty conjunction
	;	{goal & goal}	% parallel conjunction
				% (The curly braces just quote the '&'/2.)

	% disjunctions
	;	{goal ; goal}	% (non-empty) disjunction
				% (The curly braces just quote the ';'/2.)
	;	fail		% empty disjunction

	% quantifiers
	;	{ some(prog_vars, goal) }
				% existential quantification
				% (The curly braces just quote the 'some'/2.)
	;	all(prog_vars, goal)	% universal quantification
	;	some_state_vars(prog_vars, goal)
	;	all_state_vars(prog_vars, goal)
				% state variables extracted from
				% some/2 and all/2 quantifiers.

	% other scopes
	;	promise_purity(implicit_purity_promise, purity, goal)
	;	promise_equivalent_solutions(prog_vars, prog_vars, prog_vars,
			goal)	% (OrdinaryVars, DotStateVars, ColonStateVars,
				% Goal)

	% implications
	;	implies(goal, goal)	% A => B
	;	equivalent(goal, goal)	% A <=> B

	% negation and if-then-else
	;	not(goal)
	;	if_then(prog_vars, prog_vars, goal, goal)
				% if_then(SomeVars, StateVars, If, Then)
	;	if_then_else(prog_vars, prog_vars, goal, goal, goal)
				% if_then_else(SomeVars, StateVars,
				% 			If, Then, Else)

	% atomic goals
	;	call(sym_name, list(prog_term), purity)
	;	unify(prog_term, prog_term, purity).


:- type implicit_purity_promise
	--->	make_implicit_promises
	;	dont_make_implicit_promises.

:- type goals		==	list(goal).

	% These type equivalences are for the type of program variables
	% and associated structures.

:- type prog_var_type	--->	prog_var_type.
:- type prog_var	==	var(prog_var_type).
:- type prog_varset	==	varset(prog_var_type).
:- type prog_substitution ==	substitution(prog_var_type).
:- type prog_term	==	term(prog_var_type).
:- type prog_vars	==	list(prog_var).

	% A prog_context is just a term__context.

:- type prog_context	==	term__context.

%-----------------------------------------------------------------------------%
%
% Cons ids
%

	% The representation of cons_ids below is a compromise. The cons_id
	% type must be defined here, in a submodule of parse_tree.m, because
	% it is a component of insts. However, after the program has been read
	% in, the cons_ids cons, int_const, string_const and float_const,
	% which can appear in user programs, may also be augmented by the other
	% cons_ids, which can only be generated by the compiler.
	%
	% The problem is that some of these compiler generated cons_ids
	% refer to procedures, and the natural method of identifying
	% procedures requires the types pred_id and proc_id, defined
	% in hlds_pred.m, which we don't want to import here.
	%
	% We could try to avoid this problem using two different types
	% for cons_ids, one defined here for use in the parse tree and one
	% defined in hlds_data.m for use in the HLDS. We could distinguish
	% the two by having the HLDS cons_id have a definition such as
	% hlds_cons_id ---> parse_cons_id(parse_cons_id) ; ...
	% or, alternatively, by making cons_id parametric in the type of
	% constants, and substitute different constant types (since all the
	% cons_ids that refer to HLDS concepts are constants).
	%
	% Using two different types requires a translation from one to the
	% other. While the runtime cost would be acceptable, the cost in code
	% complexity isn't, since the translation isn't confined to
	% make_hlds.m. (I found this out the hard way.) This is especially so
	% if we want to use in each case only the tightest possible type.
	% For example, while construct goals can involve all cons_ids,
	% deconstruct goals and switches can currently involve only the
	% cons_ids that can appear in parse trees.
	%
	% The solution we have chosen is to exploit the fact that pred_ids
	% and proc_ids are integers. Those types are private to hlds_pred.m,
	% but hlds_pred.m also contains functions for translating them to and
	% from the shrouded versions defined below. The next three types are
	% designed to be used in only two ways: for translation to their HLDS
	% equivalents by the unshroud functions in hlds_pred.m, and for
	% printing for diagnostics.

:- type shrouded_pred_id	---> shrouded_pred_id(int).
:- type shrouded_proc_id	---> shrouded_proc_id(int).
:- type shrouded_pred_proc_id	---> shrouded_pred_proc_id(int, int).

:- type cons_id
	--->	cons(sym_name, arity)	% name, arity
		% Tuples have cons_id `cons(unqualified("{}"), Arity)'.

	;	int_const(int)
	;	string_const(string)
	;	float_const(float)
	;	pred_const(shrouded_pred_proc_id, lambda_eval_method)
		% Note that a pred_const represents a closure,
		% not just a code address.
	;	type_ctor_info_const(module_name, string, int)
		% module name, type name, type arity
	;	base_typeclass_info_const(module_name, class_id, int, string)
		% module name of instance declaration
		% (not filled in so that link errors result
		% from overlapping instances),
		% class name and arity,
		% class instance, a string encoding the type
		% names and arities of the arguments to the
		% instance declaration
	;	type_info_cell_constructor(type_ctor)
	;	typeclass_info_cell_constructor
	;	tabling_pointer_const(shrouded_pred_proc_id)
		% The address of the static variable
		% that points to the table that implements
		% memoization, loop checking or the minimal
		% model semantics for the given procedure.
	;	deep_profiling_proc_layout(shrouded_pred_proc_id)
		% The Proc_Layout structure of a procedure. Its proc_static
		% field is used by deep profiling, as documented in the deep
		% profiling paper.
	;	table_io_decl(shrouded_pred_proc_id).
		% The address of a structure that describes
		% the layout of the answer block used by
		% I/O tabling for declarative debugging.

	% Describe how a lambda expression is to be evaluated.
	%
	% `normal' is the top-down Mercury execution algorithm.
	%
	% `lambda_eval_method's other than `normal' are used for lambda
	% expressions constructed for arguments of the builtin Aditi
	% update constructs.
	%
	% `aditi_bottom_up' expressions are used as database queries to
	% produce a set of tuples to be inserted or deleted.
:- type lambda_eval_method
	--->	normal
	;	(aditi_bottom_up).

%-----------------------------------------------------------------------------%
%
% Types
%

	% This is how types are represented.

			% one day we might allow types to take
			% value parameters as well as type parameters.

% type_defn/3 is defined above as a constructor for item/0

:- type type_defn
	--->	du_type(
			list(constructor),
			maybe(unify_compare)
		)
	;	eqv_type(
			type
		)
	;	abstract_type(
			is_solver_type
		)
	;	solver_type(
			solver_type_details,
			maybe(unify_compare)
		)
	;	foreign_type(
			foreign_language_type,
			maybe(unify_compare),
			list(foreign_type_assertion)
		).

:- type foreign_type_assertion
	--->	can_pass_as_mercury_type
	;	stable.

:- type constructor
	--->	ctor(
			cons_exist		:: existq_tvars,
			cons_constraints	:: list(prog_constraint),
						% existential constraints
			cons_name		:: sym_name,
			cons_args		:: list(constructor_arg)
		).

:- type constructor_arg	==
		pair(
			maybe(ctor_field_name),
			type
		).

:- type ctor_field_name == sym_name.

	% unify_compare gives the user-defined unification and/or comparison
	% predicates for a noncanonical type, if they are known.  The value
	% `abstract_noncanonical_type' represents a type whose definition uses
	% the syntax `where type_is_abstract_noncanonical' and has been read
	% from a .int2 file.  This means we know that the type has a
	% noncanonical representation, but we don't know what the
	% unification/comparison predicates are.
	%
:- type unify_compare
	--->	unify_compare(
			unify		:: maybe(equality_pred),
			compare		:: maybe(comparison_pred)
		)
	;	abstract_noncanonical_type(is_solver_type).

	% The `where' attributes of a solver type definition must begin
	% with
	% 	representation is <<representation type>>,
	% 	initialisation is <<init pred name>>,
	% 	ground         is <<ground inst>>,
	% 	any            is <<any inst>>
	% 
:- type solver_type_details
	--->	solver_type_details(
			representation_type :: (type),
			init_pred           :: init_pred,
			ground_inst         :: (inst),
			any_inst            :: (inst)
		).

	% An init_pred specifies the name of an impure user-defined predicate
	% used to initialise solver type values (the compiler will insert
	% calls to this predicate to convert free solver type variables to
	% inst any variables where necessary.)
	%
:- type init_pred	==	sym_name.

	% An equality_pred specifies the name of a user-defined predicate
	% used for equality on a type.  See the chapter on them in the
	% Mercury Language Reference Manual.
:- type equality_pred	==	sym_name.

	 % The name of a user-defined comparison predicate.
:- type comparison_pred	==	sym_name.

	% probably type parameters should be variables not terms.
:- type type_param	==	term(tvar_type).

	% Module qualified types are represented as ':'/2 terms.
	% Use type_util:type_to_ctor_and_args to convert a type to a qualified
	% type_ctor and a list of arguments.
	% type_util:construct_type to construct a type from a type_ctor
	% and a list of arguments.
	%
	% The `term__context's of the type terms must be empty (as
	% returned by term__context_init). prog_io_util__convert_type
	% ensures this is the case. There are at least two reasons that this
	% is required:
	% - Various parts of the code to handle typeclasses create maps
	%   indexed by `prog_constraint's, which contain types.
	% - Smart recompilation requires that the items which occur in
	%   interface files can be unified using the builtin unification
	%   operation.
:- type (type)		==	term(tvar_type).
:- type type_term	==	term(tvar_type).

:- type tvar_type	--->	type_var.
:- type tvar		==	var(tvar_type).
					% used for type variables
:- type tvarset		==	varset(tvar_type).
					% used for sets of type variables
:- type tsubst		==	map(tvar, type). % used for type substitutions

:- type type_ctor	==	pair(sym_name, arity).

:- type tvar_name_map	==	map(string, tvar).

	% existq_tvars is used to record the set of type variables which are
	% existentially quantified
:- type existq_tvars	==	list(tvar).

	% Types may have arbitrary assertions associated with them
	% (eg. you can define a type which represents sorted lists).
	% Similarly, pred declarations can have assertions attached.
	% The compiler will ignore these assertions - they are intended
	% to be used by other tools, such as the debugger.

:- type condition
	--->	true
	;	where(term).

%-----------------------------------------------------------------------------%
%
% insts and modes
%

	% This is how instantiatednesses and modes are represented.
	% Note that while we use the normal term data structure to represent
	% type terms (see above), we need a separate data structure for inst
	% terms.

:- type (inst)
	--->		any(uniqueness)
	;		free
	;		free(type)
	;		bound(uniqueness, list(bound_inst))
				% The list(bound_inst) must be sorted
	;		ground(uniqueness, ground_inst_info)
				% The ground_inst_info holds extra information
				% about the ground inst.
	;		not_reached
	;		inst_var(inst_var)
				% constrained_inst_vars is a set of inst
				% variables that are constrained to have the
				% same uniqueness as and to match_final the
				% specified inst.
	;		constrained_inst_vars(set(inst_var), inst)
				% A defined_inst is possibly recursive
				% inst whose value is stored in the
				% inst_table.  This is used both for
				% user-defined insts and for
				% compiler-generated insts.
	;		defined_inst(inst_name)
				% An abstract inst is a defined inst which
				% has been declared but not actually been
				% defined (yet).
	;		abstract_inst(sym_name, list(inst)).

:- type uniqueness
	--->		shared		% there might be other references
	;		unique		% there is only one reference
	;		mostly_unique	% there is only one reference
					% but there might be more on
					% backtracking
	;		clobbered	% this was the only reference, but
					% the data has already been reused
	;		mostly_clobbered.
					% this was the only reference, but
					% the data has already been reused;
					% however, there may be more references
					% on backtracking, so we will need to
					% restore the old value on backtracking

	% The ground_inst_info type gives extra information about ground insts.
:- type ground_inst_info
	--->	higher_order(pred_inst_info)
			% The ground inst is higher-order.
	;	none.
			% No extra information is available.

	% higher-order predicate terms are given the inst
	%	`ground(shared, higher_order(PredInstInfo))'
	% where the PredInstInfo contains the extra modes and the determinism
	% for the predicate.  Note that the higher-order predicate term
	% itself must be ground.

:- type pred_inst_info
	--->	pred_inst_info(
			pred_or_func,		% is this a higher-order func
						% mode or a higher-order pred
						% mode?
			list(mode),		% the modes of the additional
						% (i.e. not-yet-supplied)
						% arguments of the pred;
						% for a function, this includes
						% the mode of the return value
						% as the last element of the
						% list.
			determinism		% the determinism of the
						% predicate or function
	).

:- type inst_id		==	pair(sym_name, arity).

:- type bound_inst	--->	functor(cons_id, list(inst)).

:- type inst_var_type	--->	inst_var_type.
:- type inst_var	==	var(inst_var_type).
:- type inst_term	==	term(inst_var_type).
:- type inst_varset	==	varset(inst_var_type).

:- type inst_var_sub	==	map(inst_var, inst).

% inst_defn/3 defined above

:- type inst_defn
	--->	eqv_inst(inst)
	;	abstract_inst.

	% An `inst_name' is used as a key for the inst_table.
	% It is either a user-defined inst `user_inst(Name, Args)',
	% or some sort of compiler-generated inst, whose name
	% is a representation of it's meaning.
	%
	% For example, `merge_inst(InstA, InstB)' is the name used for the
	% inst that results from merging InstA and InstB using `merge_inst'.
	% Similarly `unify_inst(IsLive, InstA, InstB, IsReal)' is
	% the name for the inst that results from a call to
	% `abstractly_unify_inst(IsLive, InstA, InstB, IsReal)'.
	% And `ground_inst' and `any_inst' are insts that result
	% from unifying an inst with `ground' or `any', respectively.
	% `typed_inst' is an inst with added type information.
	% `typed_ground(Uniq, Type)' a equivalent to
	% `typed_inst(ground(Uniq, no), Type)'.
	% Note that `typed_ground' is a special case of `typed_inst',
	% and `ground_inst' and `any_inst' are special cases of `unify_inst'.
	% The reason for having the special cases is efficiency.

:- type inst_name
	--->	user_inst(sym_name, list(inst))
	;	merge_inst(inst, inst)
	;	unify_inst(is_live, inst, inst, unify_is_real)
	;	ground_inst(inst_name, is_live, uniqueness, unify_is_real)
	;	any_inst(inst_name, is_live, uniqueness, unify_is_real)
	;	shared_inst(inst_name)
	;	mostly_uniq_inst(inst_name)
	;	typed_ground(uniqueness, type)
	;	typed_inst(type, inst_name).

	% Note: `is_live' records liveness in the sense used by
	% mode analysis.  This is not the same thing as the notion of liveness
	% used by code generation.  See compiler/notes/glossary.html.
:- type is_live		--->	live ; dead.

	% Unifications of insts fall into two categories, "real" and "fake".
	% The "real" inst unifications correspond to real unifications,
	% and are not allowed to unify with `clobbered' insts (unless
	% the unification would be `det').
	% Any inst unification which is associated with some code that
	% will actually examine the contents of the variables in question
	% must be "real".  Inst unifications that are not associated with
	% some real code that examines the variables' values are "fake".
	% "Fake" inst unifications are used for procedure calls in implied
	% modes, where the final inst of the var must be computed by
	% unifying its initial inst with the procedure's final inst,
	% so that if you pass a ground var to a procedure whose mode
	% is `free -> list_skeleton', the result is ground, not list_skeleton.
	% But these fake unifications must be allowed to unify with `clobbered'
	% insts. Hence we pass down a flag to `abstractly_unify_inst' which
	% specifies whether or not to allow unifications with clobbered values.

:- type unify_is_real
	--->	real_unify
	;	fake_unify.

:- type mode_id		==	pair(sym_name, arity).

% mode_defn/3 defined above

:- type mode_defn
	--->	eqv_mode(mode).

:- type (mode)
	--->	((inst) -> (inst))
	;	user_defined_mode(sym_name, list(inst)).

% mode/4 defined above

%-----------------------------------------------------------------------------%
%
% Module system
%

	% This is how module-system declarations (such as imports
	% and exports) are represented.

:- type module_defn
	--->	module(module_name)
	;	end_module(module_name)

	;	interface
	;	implementation

	;	private_interface
		% This is used internally by the compiler,
		% to identify items which originally
		% came from an implementation section
		% for a module that contains sub-modules;
		% such items need to be exported to the
		% sub-modules.

	;	imported(import_locn)
		% This is used internally by the compiler,
		% to identify declarations which originally
		% came from some other module imported with
		% a `:- import_module' declaration, and which
		% section the module was imported.
	;	used(import_locn)
		% This is used internally by the compiler,
		% to identify declarations which originally
		% came from some other module and for which
		% all uses must be module qualified. This
		% applies to items from modules imported using
		% `:- use_module', and items from `.opt'
		% and `.int2' files. It also records from which
		% section the module was imported.
	;	abstract_imported
		% This is used internally by the compiler,
		% to identify items which originally
		% came from the implementation section
		% of an interface file; usually type
		% declarations (especially equivalence types)
		% which should be used in code generation
		% but not in type checking.
	;	opt_imported
		% This is used internally by the compiler,
		% to identify items which originally
		% came from a .opt file.
	;	transitively_imported
		% This is used internally by the compiler,
		% to identify items which originally
		% came from a `.opt' or `.int2' file.
		% These should not be allowed to
		% match items in the current module.
		% Note that unlike `:- interface', `:- implementation'
		% and the other pseudo-declarations `:- imported(interface)',
		% etc., a `:- transitively_imported' declaration
		% applies to all of the following items in the list,
		% not just up to the next pseudo-declaration.

	;	external(maybe(backend), sym_name_specifier)

	;	export(sym_list)
	;	import(sym_list)
	;	use(sym_list)

	;	include_module(list(module_name))

		% This is used to represent the version numbers
		% of items in an interface file for use in
		% smart recompilation.
	;	version_numbers(module_name, recompilation__version_numbers).

:- type backend
	--->	high_level_backend
	;	low_level_backend.

:- type section
	--->	implementation
	;	interface.

	% An import_locn is used to describe the place where an item was
	% imported from.
:- type import_locn
	--->
		% The item is from a module imported in the implementation.
		implementation

		% The item is from a module imported in the interface.
	;	interface

		% The item is from a module imported by an ancestor.
	;	ancestor

		% The item is from the private interface of an ancestor module.
	;	ancestor_private_interface.

:- type sym_list
	--->	sym(list(sym_specifier))
	;	pred(list(pred_specifier))
	;	func(list(func_specifier))
	;	cons(list(cons_specifier))
	;	op(list(op_specifier))
	;	adt(list(adt_specifier))
	;	type(list(type_specifier))
	;	module(list(module_specifier)).

:- type sym_specifier
	--->	sym(sym_name_specifier)
	;	typed_sym(typed_cons_specifier)
	;	pred(pred_specifier)
	;	func(func_specifier)
	;	cons(cons_specifier)
	;	op(op_specifier)
	;	adt(adt_specifier)
	;	type(type_specifier)
	;	module(module_specifier).
:- type pred_specifier
	--->	sym(sym_name_specifier)
	;	name_args(sym_name, list(type)).
:- type func_specifier	==	cons_specifier.
:- type cons_specifier
	--->	sym(sym_name_specifier)
	;	typed(typed_cons_specifier).
:- type typed_cons_specifier
	--->	name_args(sym_name, list(type))
	;	name_res(sym_name_specifier, type)
	;	name_args_res(sym_name, list(type), type).
:- type adt_specifier	==	sym_name_specifier.
:- type type_specifier	==	sym_name_specifier.
:- type op_specifier
	--->	sym(sym_name_specifier)
	% operator fixity specifiers not yet implemented
	;	fixity(sym_name_specifier, fixity).
:- type fixity
	--->	infix
	; 	prefix
	; 	postfix
	; 	binary_prefix
	; 	binary_postfix.
:- type sym_name_specifier
	--->	name(sym_name)
	;	name_arity(sym_name, arity).
:- type sym_name_and_arity
	--->	sym_name / arity.

:- type module_specifier ==	sym_name.
:- type arity		==	int.

	% Describes whether an item can be used without an
	% explicit module qualifier.
:- type need_qualifier
	--->	must_be_qualified
	;	may_be_unqualified.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

:- type pragma_foreign_proc_attributes
	--->	attributes(
			foreign_language 	:: foreign_language,
			may_call_mercury	:: may_call_mercury,
			thread_safe		:: thread_safe,
			tabled_for_io		:: tabled_for_io,
			purity			:: purity,
			terminates		:: terminates,
				% there is some special case behaviour for
				% pragma c_code and pragma import purity
				% if legacy_purity_behaviour is `yes'
			may_throw_exception	:: may_throw_exception,
			legacy_purity_behaviour	:: bool,
			ordinary_despite_detism	:: bool,
			extra_attributes	::
				list(pragma_foreign_proc_extra_attribute)
		).

default_attributes(Language) =
	attributes(Language, may_call_mercury, not_thread_safe,
		not_tabled_for_io, impure, depends_on_mercury_calls,
		default_exception_behaviour, no, no, []).

set_may_call_mercury(MayCallMercury, Attrs0, Attrs) :-
	Attrs = Attrs0 ^ may_call_mercury := MayCallMercury.
set_thread_safe(ThreadSafe, Attrs0, Attrs) :-
	Attrs = Attrs0 ^ thread_safe := ThreadSafe.
set_foreign_language(ForeignLanguage, Attrs0, Attrs) :-
	Attrs = Attrs0 ^ foreign_language := ForeignLanguage.
set_tabled_for_io(TabledForIo, Attrs0, Attrs) :-
	Attrs = Attrs0 ^ tabled_for_io := TabledForIo.
set_purity(Purity, Attrs0, Attrs) :-
	Attrs = Attrs0 ^ purity := Purity.
set_terminates(Terminates, Attrs0, Attrs) :-
	Attrs = Attrs0 ^ terminates := Terminates.
set_may_throw_exception(MayThrowException, Attrs0, Attrs) :-
	Attrs = Attrs0 ^ may_throw_exception := MayThrowException.
set_legacy_purity_behaviour(Legacy, Attrs0, Attrs) :-
	Attrs = Attrs0 ^ legacy_purity_behaviour := Legacy.
set_ordinary_despite_detism(OrdinaryDespiteDetism, Attrs0, Attrs) :-
	Attrs = Attrs0 ^ ordinary_despite_detism := OrdinaryDespiteDetism.

attributes_to_strings(Attrs) = StringList :-
	% We ignore Lang because it isn't an attribute that you can put
	% in the attribute list -- the foreign language specifier string
	% is at the start of the pragma.
	Attrs = attributes(_Lang, MayCallMercury, ThreadSafe, TabledForIO,
		Purity,	Terminates, Exceptions, _LegacyBehaviour,
		OrdinaryDespiteDetism, ExtraAttributes),
	(
		MayCallMercury = may_call_mercury,
		MayCallMercuryStr = "may_call_mercury"
	;
		MayCallMercury = will_not_call_mercury,
		MayCallMercuryStr = "will_not_call_mercury"
	),
	(
		ThreadSafe = not_thread_safe,
		ThreadSafeStr = "not_thread_safe"
	;
		ThreadSafe = thread_safe,
		ThreadSafeStr = "thread_safe"
	;
		ThreadSafe = maybe_thread_safe,
		ThreadSafeStr = "maybe_thread_safe"
	),
	(
		TabledForIO = tabled_for_io,
		TabledForIOStr = "tabled_for_io"
	;
		TabledForIO = tabled_for_io_unitize,
		TabledForIOStr = "tabled_for_io_unitize"
	;
		TabledForIO = tabled_for_descendant_io,
		TabledForIOStr = "tabled_for_descendant_io"
	;
		TabledForIO = not_tabled_for_io,
		TabledForIOStr = "not_tabled_for_io"
	),
	(
		Purity = pure,
		PurityStrList = ["promise_pure"]
	;
		Purity = (semipure),
		PurityStrList = ["promise_semipure"]
	;
		Purity = (impure),
		PurityStrList = []
	),
	(
		Terminates = terminates,
		TerminatesStrList = ["terminates"]
	;
		Terminates = does_not_terminate,
		TerminatesStrList = ["does_not_terminate"]
	;
		Terminates = depends_on_mercury_calls,
		TerminatesStrList = []
	),
	(
		Exceptions = will_not_throw_exception,
		ExceptionsStrList = ["will_not_throw_exception"]
	;
		Exceptions = default_exception_behaviour,
		ExceptionsStrList = []
	),
	(
		OrdinaryDespiteDetism = yes,
		OrdinaryDespiteDetismStrList = ["ordinary_despite_detism"]
	;
		OrdinaryDespiteDetism = no,
		OrdinaryDespiteDetismStrList = []
	),
	StringList = [MayCallMercuryStr, ThreadSafeStr, TabledForIOStr |
			PurityStrList] ++ TerminatesStrList ++
			ExceptionsStrList ++
			OrdinaryDespiteDetismStrList ++
		list__map(extra_attribute_to_string, ExtraAttributes).

add_extra_attribute(NewAttribute, Attributes0,
	Attributes0 ^ extra_attributes :=
		[NewAttribute | Attributes0 ^ extra_attributes]).

:- func extra_attribute_to_string(pragma_foreign_proc_extra_attribute)
	= string.

extra_attribute_to_string(backend(low_level_backend)) = "low_level_backend".
extra_attribute_to_string(backend(high_level_backend)) = "high_level_backend".
extra_attribute_to_string(max_stack_size(Size)) =
	"max_stack_size(" ++ string__int_to_string(Size) ++ ")".

%-----------------------------------------------------------------------------%
:- end_module prog_data.
%-----------------------------------------------------------------------------%
