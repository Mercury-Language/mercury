%---------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: private_builtin.m.
% Main authors: fjh, zs.
% Stability: low.

% This file is automatically imported, as if via `use_module', into every
% module.  It is intended for builtins that are just implementation details,
% such as procedures that the compiler generates implicit calls to when
% implementing polymorphism, unification, compare/3, etc.
% Note that the builtins used for tabling are in a separate module
% (table_builtin.m).

% This module is a private part of the Mercury implementation;
% user modules should never explicitly import this module.
% The interface for this module does not get included in the
% Mercury library reference manual.

% Many of the predicates defined in this module are builtin -
% they do not have definitions because the compiler generates code
% for them inline. Some others are implemented in the runtime.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module private_builtin.

%-----------------------------------------------------------------------------%

:- interface.

	% This section of the module contains predicates that are used
	% by the compiler, to implement polymorphism. These predicates
	% should not be used by user programs directly.

	% Changes here may also require changes in compiler/polymorphism.m,
	% compiler/unify_proc.m, compiler/higher_order.m and
	% runtime/mercury_type_info.{c,h}.

:- pred builtin_unify_int(int::in, int::in) is semidet.
:- pred builtin_compare_int(comparison_result::uo, int::in, int::in) is det.

:- pred builtin_unify_character(character::in, character::in) is semidet.
:- pred builtin_compare_character(comparison_result::uo, character::in,
	character::in) is det.

:- pred builtin_unify_string(string::in, string::in) is semidet.
:- pred builtin_compare_string(comparison_result::uo, string::in, string::in)
	is det.

:- pred builtin_unify_float(float::in, float::in) is semidet.
:- pred builtin_compare_float(comparison_result::uo, float::in, float::in)
	is det.

:- pred builtin_unify_pred((pred)::in, (pred)::in) is semidet.
:- pred builtin_compare_pred(comparison_result::uo, (pred)::in, (pred)::in)
	is det.

	% These should never be called -- the compiler never
	% specializes them because the generic compare is just
	% as good as anything we could put here.
:- pred builtin_unify_tuple(T::in, T::in) is semidet.
:- pred builtin_compare_tuple(comparison_result::uo, T::in, T::in) is det.

	% The following pred is used for compare/3
	% on non-canonical types (types for which there is a
	% `where equality is ...' declaration).
:- pred builtin_compare_non_canonical_type(comparison_result::uo,
		T::in, T::in) is det.

	% Compare_error is used in the code generated for compare/3 preds.
:- pred compare_error is erroneous.

	% The builtin < operator on ints, used in the code generated
	% for compare/3 preds.
:- pred builtin_int_lt(int, int).
:- mode builtin_int_lt(in, in) is semidet.

	% The builtin > operator on ints, used in the code generated
	% for compare/3 preds.
:- pred builtin_int_gt(int, int).
:- mode builtin_int_gt(in, in) is semidet.

	% A "typed" version of unify/2 -- i.e. one that can handle arguments
	% of different types.  It first unifies their types, and then if
	% the types are equal it unifies the values.
:- pred typed_unify(T1, T2).
:- mode typed_unify(in, in) is semidet.
:- mode typed_unify(in, out) is semidet.

	% A "typed" version of compare/3 -- i.e. one that can handle arguments
	% of different types.  It first compares the types, and then if the
	% types are equal it compares the values.
:- pred typed_compare(comparison_result, T1, T2).
:- mode typed_compare(uo, in, in) is det.

	% N.B. interface continued below.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, string, std_util, int, float, char, string, list.

:- pragma foreign_code("MC++", "

// The dummy_var is used to represent io__states and other Mercury
// parameters that are not really passed around.  Occasionally a dummy variable
// will be used by the code generator as an lval, so we use
// private_builtin:dummy_var as that lval.

static MR_Word dummy_var;

").

:- pragma inline(builtin_compare_int/3).
:- pragma inline(builtin_compare_character/3).
:- pragma inline(builtin_compare_string/3).
:- pragma inline(builtin_compare_float/3).

builtin_unify_int(X, X).

builtin_compare_int(R, X, Y) :-
	( X < Y ->
		R = (<)
	; X = Y ->
		R = (=)
	;
		R = (>)
	).

builtin_unify_character(C, C).

builtin_compare_character(R, X, Y) :-
	char__to_int(X, XI),
	char__to_int(Y, YI),
	( XI < YI ->
		R = (<)
	; XI = YI ->
		R = (=)
	;
		R = (>)
	).

builtin_unify_string(S, S).

builtin_compare_string(R, S1, S2) :-
	builtin_strcmp(Res, S1, S2),
	( Res < 0 ->
		R = (<)
	; Res = 0 ->
		R = (=)
	;
		R = (>)
	).

:- pred builtin_strcmp(int, string, string).
:- mode builtin_strcmp(out, in, in) is det.

:- pragma foreign_proc("C", builtin_strcmp(Res::out, S1::in, S2::in),
	[will_not_call_mercury, promise_pure, thread_safe],
	"Res = strcmp(S1, S2);").

:- pragma foreign_proc("MC++", builtin_strcmp(Res::out, S1::in, S2::in),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Res = System::String::Compare(S1, S2);
").
	

builtin_unify_float(F, F).

builtin_compare_float(R, F1, F2) :-
	( F1 < F2 ->
		R = (<)
	; F1 > F2 ->
		R = (>)
	;
		R = (=)
	).

builtin_unify_tuple(_, _) :-
	( semidet_succeed ->
		% The generic unification function in the runtime
		% should handle this itself.
		error("builtin_unify_tuple called")
	;
		% the following is never executed
		semidet_succeed
	).

builtin_compare_tuple(Res, _, _) :-
	( semidet_succeed ->
		% The generic comparison function in the runtime
		% should handle this itself.
		error("builtin_compare_tuple called")
	;
		% the following is never executed
		Res = (<)
	).

:- pragma no_inline(builtin_unify_pred/2).
builtin_unify_pred(_X, _Y) :-
	( semidet_succeed ->
		error("attempted higher-order unification")
	;
		% the following is never executed
		semidet_succeed
	).

:- pragma no_inline(builtin_compare_pred/3).
builtin_compare_pred(Result, _X, _Y) :-
	( semidet_succeed ->
		error("attempted higher-order comparison")
	;
		% the following is never executed
		Result = (<)
	).

:- pragma no_inline(builtin_compare_non_canonical_type/3).
builtin_compare_non_canonical_type(Res, X, _Y) :-
	% suppress determinism warning
	( semidet_succeed ->
		string__append_list([
			"call to compare/3 for non-canonical type `",
			type_name(type_of(X)),
			"'"],
			Message),
		error(Message)
	;
		% the following is never executed
		Res = (<)
	).

	% This is used by the code that the compiler generates for compare/3.
:- pragma no_inline(compare_error/0).
compare_error :-
	error("internal error in compare/3").

%-----------------------------------------------------------------------------%

typed_unify(X, Y) :-
	( type_of(X) = type_of(Y) ->
		unsafe_type_cast(X, Y)
	;
		fail
	).

typed_compare(R, X, Y) :-
	compare(R0, type_of(X), type_of(Y)),
	( R0 = (=) ->
		unsafe_type_cast(X, Z),
		compare(R, Z, Y)
	;
		R = R0
	).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% This section of the module handles the runtime representation of
	% type information.

	% The code generated by polymorphism.m always requires
	% the existence of a type_info functor, and requires
	% the existence of a type_ctor_info functor as well
	% when using --type-info {shared-,}one-or-two-cell.
	%
	% The actual arities of these two function symbols are variable;
	% they depend on the number of type parameters of the type represented
	% by the type_info, and how many predicates we associate with each
	% type.
	%
	% Note that, since these types look to the compiler as though they
	% are candidates to become no_tag types, special code is required
	% to handle them in type_util:type_is_no_tag_type/3.

:- type type_info(T) ---> type_info(type_ctor_info(T) /*, ... */).
:- type type_ctor_info(T) ---> type_ctor_info(int /*, ... */).

	% The type variable in these types isn't really a type variable,
	% it is a place for polymorphism.m to put a representation of the
	% class constraint about which the typeclass_info carries information.
	%
	% Note that, since these types look to the compiler as though they
	% are candidates to become no_tag types, special code is required
	% to handle them in type_util:type_is_no_tag_type/3.

:- type typeclass_info(T) ---> typeclass_info(base_typeclass_info(T)
						/*, ... */).
:- type base_typeclass_info(_) ---> typeclass_info(int /*, ... */).

	% The following types are used by compiler/ml_code_util.m
	% as the types used for copying type_info/1 and typeclass_info/1
	% types.  XXX Document me better
:- type sample_type_info ---> sample_type_info(type_info(int)).
:- type sample_typeclass_info ---> sample_typeclass_info(typeclass_info(int)).

	% type_info_from_typeclass_info(TypeClassInfo, Index, TypeInfo)
	% extracts TypeInfo from TypeClassInfo, where TypeInfo is the Indexth
	% type_info in the typeclass_info.
	%
	% Note: Index must be equal to the number of the desired type_info
	% plus the number of superclasses for this class.
:- pred type_info_from_typeclass_info(typeclass_info(_), int, type_info(T)).
:- mode type_info_from_typeclass_info(in, in, out) is det.

	% unconstrained_type_info_from_typeclass_info(TypeClassInfo, 
	%               Index, TypeInfo)
	% extracts the TypeInfo for the Indexth unconstrained type variable
	% from the instance represented by TypeClassInfo.
:- pred unconstrained_type_info_from_typeclass_info(typeclass_info(_),
		int, type_info(_)).
:- mode unconstrained_type_info_from_typeclass_info(in, in, out) is det.

	% superclass_from_typeclass_info(TypeClassInfo, Index, SuperClass)
	% extracts SuperClass from TypeClassInfo where SuperClass is the
	% Indexth superclass of the class.
:- pred superclass_from_typeclass_info(typeclass_info(_),
		int, typeclass_info(_)).
:- mode superclass_from_typeclass_info(in, in, out) is det.

	% instance_constraint_from_typeclass_info(TypeClassInfo, Index,
	%       InstanceConstraintTypeClassInfo)
	% extracts the typeclass_info for the Indexth typeclass constraint
	% of the instance described by TypeClassInfo.
	%
	% Note: Index must be equal to the number of the desired constraint
	% plus the number of unconstrained type variables for this instance.
:- pred instance_constraint_from_typeclass_info(
		typeclass_info(_), int, typeclass_info(_)).
:- mode instance_constraint_from_typeclass_info(in, in, out) is det.

	% N.B. interface continued below.

%-----------------------------------------------------------------------------%

:- implementation.

	% The definitions for type_ctor_info/1 and type_info/1.

:- pragma c_header_code("
#ifdef MR_DEEP_PROFILING
#include ""mercury_deep_profiling.h""
#endif
").

:- pragma foreign_code("C", "

/* forward decls, to suppress gcc -Wmissing-decl warnings */
void sys_init_type_info_module_init(void);
void sys_init_type_info_module_init_type_tables(void);
#ifdef	MR_DEEP_PROFILING
void sys_init_type_info_module_write_out_proc_statics(FILE *fp);
#endif

#ifndef MR_HIGHLEVEL_CODE

	/*
	** For most purposes, type_ctor_info can be treated just like
	** type_info.  The code that handles type_infos can also handle
	** type_ctor_infos.
	*/

#ifdef	MR_DEEP_PROFILING
MR_proc_static_compiler_empty(private_builtin, __Unify__,   type_info,
	1, 0, ""private_builtin.m"", 0, TRUE);
MR_proc_static_compiler_empty(private_builtin, __Compare__, type_info,
	1, 0, ""private_builtin.m"", 0, TRUE);
MR_proc_static_compiler_empty(private_builtin, __Unify__,   typeclass_info,
	1, 0, ""private_builtin.m"", 0, TRUE);
MR_proc_static_compiler_empty(private_builtin, __Compare__, typeclass_info,
	1, 0, ""private_builtin.m"", 0, TRUE);
#endif

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_PRED(private_builtin, type_ctor_info, 1,
	MR_TYPECTOR_REP_TYPECTORINFO,
	mercury____Unify___private_builtin__type_info_1_0,
	mercury____Compare___private_builtin__type_info_1_0);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(private_builtin, type_info, 1,
	MR_TYPECTOR_REP_TYPEINFO);

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_PRED(private_builtin, base_typeclass_info, 1,
	MR_TYPECTOR_REP_BASETYPECLASSINFO,
	mercury____Unify___private_builtin__typeclass_info_1_0,
	mercury____Compare___private_builtin__typeclass_info_1_0);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(private_builtin, typeclass_info, 1,
	MR_TYPECTOR_REP_TYPECLASSINFO);

MR_define_extern_entry(mercury____Unify___private_builtin__type_info_1_0);
MR_define_extern_entry(mercury____Compare___private_builtin__type_info_1_0);
MR_define_extern_entry(mercury____Unify___private_builtin__typeclass_info_1_0);
MR_define_extern_entry(mercury____Compare___private_builtin__typeclass_info_1_0);

MR_BEGIN_MODULE(type_info_module)
	MR_init_entry(mercury____Unify___private_builtin__type_info_1_0);
	MR_init_entry(mercury____Compare___private_builtin__type_info_1_0);
	MR_init_entry(mercury____Unify___private_builtin__typeclass_info_1_0);
	MR_init_entry(mercury____Compare___private_builtin__typeclass_info_1_0);
#ifdef	MR_DEEP_PROFILING
	MR_init_entry(mercury____Unify___private_builtin__type_info_1_0_i1);
	MR_init_entry(mercury____Unify___private_builtin__type_info_1_0_i2);
	MR_init_entry(mercury____Unify___private_builtin__type_info_1_0_i3);
	MR_init_entry(mercury____Unify___private_builtin__type_info_1_0_i4);
	MR_init_entry(mercury____Compare___private_builtin__type_info_1_0_i1);
	MR_init_entry(mercury____Compare___private_builtin__type_info_1_0_i2);
	MR_init_entry(mercury____Unify___private_builtin__typeclass_info_1_0_i1);
	MR_init_entry(mercury____Unify___private_builtin__typeclass_info_1_0_i2);
	MR_init_entry(mercury____Unify___private_builtin__typeclass_info_1_0_i3);
	MR_init_entry(mercury____Unify___private_builtin__typeclass_info_1_0_i4);
	MR_init_entry(mercury____Compare___private_builtin__typeclass_info_1_0_i1);
	MR_init_entry(mercury____Compare___private_builtin__typeclass_info_1_0_i2);
#endif
MR_BEGIN_CODE

#define	proc_label	mercury____Unify___private_builtin__type_info_1_0
#define	proc_static	MR_proc_static_compiler_name(private_builtin,	\
				__Unify__, type_info, 1, 0)
#define	body_code	do {						\
				int	comp;				\
									\
				MR_save_transient_registers();		\
				comp = MR_compare_type_info(		\
					(MR_TypeInfo) MR_r1,		\
					(MR_TypeInfo) MR_r2);		\
				MR_restore_transient_registers();	\
				MR_r1 = (comp == MR_COMPARE_EQUAL);	\
			} while (0)

#include ""mercury_hand_unify_body.h""

#undef	proc_label
#undef	proc_static
#undef	body_code

#define	proc_label	mercury____Compare___private_builtin__type_info_1_0
#define	proc_static	MR_proc_static_compiler_name(private_builtin,	\
				__Compare__, type_info, 1, 0)
#define	body_code	do {						\
				int	comp;				\
									\
				MR_save_transient_registers();		\
				comp = MR_compare_type_info(		\
					(MR_TypeInfo) MR_r1,		\
					(MR_TypeInfo) MR_r2);		\
				MR_restore_transient_registers();	\
				MR_r1 = comp;				\
			} while (0)

#include ""mercury_hand_compare_body.h""

#undef	proc_label
#undef	proc_static
#undef	body_code

#define	proc_label	mercury____Unify___private_builtin__typeclass_info_1_0
#define	proc_static	MR_proc_static_compiler_name(private_builtin,	\
				__Unify__, typeclass_info, 1, 0)
#define	body_code	do {						\
				MR_fatal_error(""attempt to unify typeclass_info""); \
			} while (0)

#include ""mercury_hand_unify_body.h""

#undef	proc_label
#undef	proc_static
#undef	body_code

#define	proc_label	mercury____Compare___private_builtin__typeclass_info_1_0
#define	proc_static	MR_proc_static_compiler_name(private_builtin,	\
				__Compare__, typeclass_info, 1, 0)
#define	body_code	do {						\
				MR_fatal_error(""attempt to compare typeclass_info""); \
			} while (0)

#include ""mercury_hand_compare_body.h""

#undef	proc_label
#undef	proc_static
#undef	body_code

MR_END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_type_info_module
*/
MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc type_info_module;

#endif /* ! MR_HIGHLEVEL_CODE */

void
sys_init_type_info_module_init(void)
{
#ifndef	MR_HIGHLEVEL_CODE
	type_info_module();

	MR_INIT_TYPE_CTOR_INFO(
	  mercury_data_private_builtin__type_ctor_info_type_ctor_info_1,
	  private_builtin__type_info_1_0);
	MR_INIT_TYPE_CTOR_INFO(
	  mercury_data_private_builtin__type_ctor_info_type_info_1,
	  private_builtin__type_info_1_0);
	MR_INIT_TYPE_CTOR_INFO(
	  mercury_data_private_builtin__type_ctor_info_base_typeclass_info_1,
	  private_builtin__typeclass_info_1_0);
	MR_INIT_TYPE_CTOR_INFO(
	  mercury_data_private_builtin__type_ctor_info_typeclass_info_1,
	  private_builtin__typeclass_info_1_0);
#endif
}

void
sys_init_type_info_module_init_type_tables(void)
{
#ifndef	MR_HIGHLEVEL_CODE
	MR_register_type_ctor_info(
	  &mercury_data_private_builtin__type_ctor_info_type_ctor_info_1);
	MR_register_type_ctor_info(
	  &mercury_data_private_builtin__type_ctor_info_type_info_1);
	MR_register_type_ctor_info(
	  &mercury_data_private_builtin__type_ctor_info_base_typeclass_info_1);
	MR_register_type_ctor_info(
	  &mercury_data_private_builtin__type_ctor_info_typeclass_info_1);
#endif
}

#ifdef	MR_DEEP_PROFILING
void
sys_init_type_info_module_write_out_proc_statics(FILE *fp)
{
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_compiler_name(private_builtin,
			__Unify__, type_info, 1, 0));
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_compiler_name(private_builtin,
			__Compare__, type_info, 1, 0));
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_compiler_name(private_builtin,
			__Unify__, typeclass_info, 1, 0));
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_compiler_name(private_builtin,
			__Compare__, typeclass_info, 1, 0));
}
#endif

").


:- pragma foreign_code("MC++", "

static MR_TypeInfo MR_typeclass_info_type_info(
	MR_TypeClassInfo tcinfo, int index)
{
	MR_Word tmp;
	int t1;

	tmp = dynamic_cast<MR_Word> (tcinfo[0]);
	t1 = System::Convert::ToInt32(tmp[0]) + index;
	return dynamic_cast<MR_Word> (tcinfo[t1]);
}
static MR_TypeInfo MR_typeclass_info_unconstrained_type_info(
	MR_TypeClassInfo tcinfo, int index) 
{
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
	return 0;
}

static MR_TypeClassInfo MR_typeclass_info_superclass_info(
	MR_TypeClassInfo tcinfo, int index)
{
	MR_Word tmp;
	int t1;

	tmp = dynamic_cast<MR_Word> (tcinfo[0]);
	t1 = System::Convert::ToInt32(tmp[0]) + index;
	return dynamic_cast<MR_Word> (tcinfo[t1]);
}

static MR_TypeClassInfo MR_typeclass_info_arg_typeclass_info(
	MR_TypeClassInfo tcinfo, int index) 
{
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
	return 0;
}

").

:- pragma foreign_code("MC++", "

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(private_builtin, type_ctor_info, 1,
	MR_TYPECTOR_REP_TYPECTORINFO) 
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(private_builtin, type_info, 1,
	MR_TYPECTOR_REP_TYPEINFO) 
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(private_builtin, base_typeclass_info, 1,
	MR_TYPECTOR_REP_BASETYPECLASSINFO) 
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(private_builtin, typeclass_info, 1,
	MR_TYPECTOR_REP_TYPECLASSINFO) 

	// XXX These static constants are duplicated both here and in
	// mercury_mcpp.cpp.

	// This is because other library modules reference them
	// from MC++ code (so they depend on the versions in the runtime to
	// make the dependencies simple) whereas the compiler generates
	// references to the ones here. 

	// See runtime/mercury_mcpp.cpp for discussion of why we aren't using
	// enums or const static ints here.

static int MR_TYPECTOR_REP_ENUM 		= MR_TYPECTOR_REP_ENUM_val;
static int MR_TYPECTOR_REP_ENUM_USEREQ 		= MR_TYPECTOR_REP_ENUM_USEREQ_val;
static int MR_TYPECTOR_REP_DU			= MR_TYPECTOR_REP_DU_val;
static int MR_TYPECTOR_REP_DU_USEREQ		= 3;
static int MR_TYPECTOR_REP_NOTAG		= 4;
static int MR_TYPECTOR_REP_NOTAG_USEREQ		= 5;
static int MR_TYPECTOR_REP_EQUIV		= 6;
static int MR_TYPECTOR_REP_FUNC			= 7;
static int MR_TYPECTOR_REP_INT		    	= 8;
static int MR_TYPECTOR_REP_CHAR		    	= 9;
static int MR_TYPECTOR_REP_FLOAT		=10;
static int MR_TYPECTOR_REP_STRING		=11;
static int MR_TYPECTOR_REP_PRED		    	=12;
static int MR_TYPECTOR_REP_UNIV		    	=13;
static int MR_TYPECTOR_REP_VOID		    	=14;
static int MR_TYPECTOR_REP_C_POINTER		=15;
static int MR_TYPECTOR_REP_TYPEINFO		=16;
static int MR_TYPECTOR_REP_TYPECLASSINFO	=17;
static int MR_TYPECTOR_REP_ARRAY		=18;
static int MR_TYPECTOR_REP_SUCCIP		=19;
static int MR_TYPECTOR_REP_HP			=20;
static int MR_TYPECTOR_REP_CURFR		=21;
static int MR_TYPECTOR_REP_MAXFR		=22;
static int MR_TYPECTOR_REP_REDOFR		=23;
static int MR_TYPECTOR_REP_REDOIP		=24;
static int MR_TYPECTOR_REP_TRAIL_PTR		=25;
static int MR_TYPECTOR_REP_TICKET		=26;
static int MR_TYPECTOR_REP_NOTAG_GROUND		=27;
static int MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ	=28;
static int MR_TYPECTOR_REP_EQUIV_GROUND		=29;
static int MR_TYPECTOR_REP_TUPLE		=30;
static int MR_TYPECTOR_REP_RESERVED_ADDR	=31;
static int MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ	=32;
static int MR_TYPECTOR_REP_TYPECTORINFO		=33;
static int MR_TYPECTOR_REP_BASETYPECLASSINFO	=34;
static int MR_TYPECTOR_REP_UNKNOWN		=35;

static int MR_SECTAG_NONE				= 0;
static int MR_SECTAG_LOCAL				= 1;
static int MR_SECTAG_REMOTE				= 2;
static int MR_SECTAG_VARIABLE				= 3;


static int
__Unify____type_info_1_0(
	MR_Word type_info, MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::SORRY(""unify for type_info"");
	return 0;
}

static int
__Unify____typeclass_info_1_0(
	MR_Word type_info, MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::SORRY(""unify for typeclass_info"");
	return 0;
}

static int
__Unify____base_typeclass_info_1_0(
	MR_Word type_info, MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::SORRY(""unify for base_typeclass_info"");
	return 0;
}

static int
__Unify____type_ctor_info_1_0(
	MR_Word type_info, MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::SORRY(""unify for type_ctor_info"");
	return 0;
}

static void
__Compare____type_ctor_info_1_0(
	MR_Word type_info, MR_Word_Ref result, MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::SORRY(""compare for type_ctor_info"");
}

static void
__Compare____type_info_1_0(
	MR_Word type_info, MR_Word_Ref result, MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::SORRY(""compare for type_info"");
}

static void
__Compare____typeclass_info_1_0(
	MR_Word type_info, MR_Word_Ref result, MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::SORRY(""compare for typeclass_info"");
}

static void
__Compare____base_typeclass_info_1_0(
	MR_Word type_info, MR_Word_Ref result, MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::SORRY(""compare for base_typeclass_info"");
}

static int
do_unify__type_ctor_info_1_0(
	MR_Word type_info, MR_Box x, MR_Box y)
{
	return mercury::private_builtin__cpp_code::mercury_code::__Unify____type_ctor_info_1_0(
		type_info, 
		dynamic_cast<MR_Word>(x),
		dynamic_cast<MR_Word>(y));
}

static int
do_unify__type_info_1_0(
	MR_Word type_info, MR_Box x, MR_Box y)
{
	return mercury::private_builtin__cpp_code::mercury_code::__Unify____type_info_1_0(
		type_info,
		dynamic_cast<MR_Word>(x),
		dynamic_cast<MR_Word>(y));
}

static int
do_unify__typeclass_info_1_0(
	MR_Word type_info, MR_Box x, MR_Box y)
{
	return mercury::private_builtin__cpp_code::mercury_code::__Unify____typeclass_info_1_0(
		type_info, 
		dynamic_cast<MR_Word>(x),
		dynamic_cast<MR_Word>(y));
}

static int
do_unify__base_typeclass_info_1_0(
	MR_Word type_info, MR_Box x, MR_Box y)
{
	return
	mercury::private_builtin__cpp_code::mercury_code::__Unify____base_typeclass_info_1_0(
		type_info,
		dynamic_cast<MR_Word>(x),
		dynamic_cast<MR_Word>(y));
}

static void
do_compare__type_ctor_info_1_0(
	MR_Word type_info, MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::private_builtin__cpp_code::mercury_code::__Compare____type_ctor_info_1_0(
		type_info, result, 
		dynamic_cast<MR_Word>(x),
		dynamic_cast<MR_Word>(y));
}

static void
do_compare__type_info_1_0(
	MR_Word type_info, MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::private_builtin__cpp_code::mercury_code::__Compare____type_info_1_0(
		type_info, result,
		dynamic_cast<MR_Word>(x),
		dynamic_cast<MR_Word>(y));
}

static void
do_compare__typeclass_info_1_0(
	MR_Word type_info, MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::private_builtin__cpp_code::mercury_code::__Compare____typeclass_info_1_0(
		type_info, result,
		dynamic_cast<MR_Word>(x),
		dynamic_cast<MR_Word>(y));
}

static void
do_compare__base_typeclass_info_1_0(
	MR_Word type_info, MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::private_builtin__cpp_code::mercury_code::__Compare____base_typeclass_info_1_0(
		type_info, result,
		dynamic_cast<MR_Word>(x),
		dynamic_cast<MR_Word>(y));
}

static void init_runtime(void)
{
	mercury::runtime::Init::init_runtime();
}

").

:- pragma foreign_proc("C",
	type_info_from_typeclass_info(TypeClassInfo::in, Index::in,
		TypeInfo::out),
			[will_not_call_mercury, promise_pure, thread_safe],
"
	TypeInfo = MR_typeclass_info_type_info(TypeClassInfo, Index);
").

:- pragma foreign_proc("C",
	unconstrained_type_info_from_typeclass_info(TypeClassInfo::in,
		Index::in, TypeInfo::out),
			[will_not_call_mercury, promise_pure, thread_safe],
"
	TypeInfo = MR_typeclass_info_unconstrained_type_info(TypeClassInfo,
			Index);
").

:- pragma foreign_proc("C",
	superclass_from_typeclass_info(TypeClassInfo0::in, Index::in,
		TypeClassInfo::out),
			[will_not_call_mercury, promise_pure, thread_safe],
"
	TypeClassInfo =
		MR_typeclass_info_superclass_info(TypeClassInfo0, Index);
").

:- pragma foreign_proc("C",
	instance_constraint_from_typeclass_info(TypeClassInfo0::in,
		Index::in, TypeClassInfo::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	TypeClassInfo =
		MR_typeclass_info_arg_typeclass_info(TypeClassInfo0, Index);
").

:- pragma foreign_proc("MC++",
	type_info_from_typeclass_info(TypeClassInfo::in, Index::in,
		TypeInfo::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	TypeInfo = MR_typeclass_info_type_info(TypeClassInfo, Index);
").

:- pragma foreign_proc("MC++",
	unconstrained_type_info_from_typeclass_info(TypeClassInfo::in,
		Index::in, TypeInfo::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	TypeInfo = MR_typeclass_info_unconstrained_type_info(TypeClassInfo,
			Index);
").

:- pragma foreign_proc("MC++",
	superclass_from_typeclass_info(TypeClassInfo0::in, Index::in,
		TypeClassInfo::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	TypeClassInfo =
		MR_typeclass_info_superclass_info(TypeClassInfo0, Index);
").

:- pragma foreign_proc("MC++",
	instance_constraint_from_typeclass_info(TypeClassInfo0::in,
		Index::in, TypeClassInfo::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	TypeClassInfo =
		MR_typeclass_info_arg_typeclass_info(TypeClassInfo0, Index);
").


%-----------------------------------------------------------------------------%

	% This section of the module contains predicates that are used
	% by the MLDS back-end, to implement trailing.
	% (The LLDS back-end does not use these; instead it inserts
	% the corresponding LLDS instructions directly during code
	% generation.)
	% These predicates should not be used by user programs directly.

:- interface.

:- type ticket == c_pointer.
:- type ticket_counter == c_pointer.

	% For documentation, see the corresponding LLDS instructions
	% in compiler/llds.m.  See also compiler/notes/trailing.html.

:- impure pred store_ticket(ticket::out) is det.
:- impure pred reset_ticket_undo(ticket::in) is det.
:- impure pred reset_ticket_commit(ticket::in) is det.
:- impure pred reset_ticket_solve(ticket::in) is det.
:- impure pred discard_ticket is det.
:- impure pred prune_ticket is det.
:- impure pred mark_ticket_stack(ticket_counter::out) is det.
:- impure pred prune_tickets_to(ticket_counter::in) is det.

	% XXX currently we don't support nondet pragma
	% foreign_code when trailing is enabled.
	% Instead we generate code which calls this procedure,
	% which will call error/1 with an appropriate message.
:- pred trailed_nondet_pragma_foreign_code is erroneous.

	% N.B. interface continued below.

:- implementation.

% Default (Mercury) implementations.
% These should be overridden by the appropriate foreign language implementation.
store_ticket(_Ticket::out) :-
	sorry("private_builtin__store_ticket/1").
reset_ticket_undo(_Ticket::in) :-
	sorry("private_builtin__reset_ticket_undo/1").
reset_ticket_commit(_Ticket::in) :-
	sorry("private_builtin__reset_ticket_commit/1").
reset_ticket_solve(_Ticket::in) :-
	sorry("private_builtin__reset_ticket_solve/1").
mark_ticket_stack(_TicketCounter::out) :-
	sorry("private_builtin__mark_ticket_stack/1").
prune_tickets_to(_TicketCounter::in) :-
	sorry("private_builtin__prune_tickets_to/1").
/****
% XXX we can't give default Mercury implementations for these,
% because you can't write a mode-specific clause for a zero-arity
% procedure.
discard_ticket :-
	sorry("private_builtin__discard_ticket/0").
prune_ticket :-
	sorry("private_builtin__prune_ticket/0").
****/

:- pragma foreign_proc("C", store_ticket(Ticket::out),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	MR_store_ticket(Ticket);
#else
	Ticket = 0;
#endif
").

:- pragma foreign_proc("C", reset_ticket_undo(Ticket::in),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	MR_reset_ticket(Ticket, MR_undo);
#endif
").

:- pragma foreign_proc("C", reset_ticket_commit(Ticket::in),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	MR_reset_ticket(Ticket, MR_commit);
#endif
").

:- pragma foreign_proc("C", reset_ticket_solve(Ticket::in),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	MR_reset_ticket(Ticket, MR_solve);
#endif
").

:- pragma foreign_proc("C", discard_ticket,
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	MR_discard_ticket();
#endif
").

:- pragma foreign_proc("C", prune_ticket,
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	MR_prune_ticket();
#endif
").

:- pragma foreign_proc("C", mark_ticket_stack(TicketCounter::out),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	MR_mark_ticket_stack(TicketCounter);
#else
	TicketCounter = 0;
#endif
").

:- pragma foreign_proc("C", prune_tickets_to(TicketCounter::in),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	MR_prune_tickets_to(TicketCounter);
#endif
").

:- pragma foreign_proc("MC++", store_ticket(Ticket::out),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
	// MR_store_ticket(Ticket);
#else
	Ticket = 0;
#endif
").

:- pragma foreign_proc("MC++", reset_ticket_undo(Ticket::in),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
	// MR_reset_ticket(Ticket, MR_undo);
#endif
").

:- pragma foreign_proc("MC++", reset_ticket_commit(Ticket::in),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
	// MR_reset_ticket(Ticket, MR_commit);
#endif
").

:- pragma foreign_proc("MC++", reset_ticket_solve(Ticket::in),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
	// MR_reset_ticket(Ticket, MR_solve);
#endif
").

:- pragma foreign_proc("MC++", discard_ticket,
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
	// MR_discard_ticket();
#endif
").

:- pragma foreign_proc("MC++", prune_ticket,
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
	// MR_prune_ticket();
#endif
").

:- pragma foreign_proc("MC++", mark_ticket_stack(TicketCounter::out),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
	// MR_mark_ticket_stack(TicketCounter);
#else
	TicketCounter = 0;
#endif
").

:- pragma foreign_proc("MC++", prune_tickets_to(TicketCounter::in),
	[will_not_call_mercury, thread_safe],
"
#ifdef MR_USE_TRAIL
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
	// MR_prune_tickets_to(TicketCounter);
#endif
").


trailed_nondet_pragma_foreign_code :-
	Msg = string__append_list([
		"Sorry, not implemented:\n",
		"for the MLDS back-end (`--high-level-code')\n",
		"nondet `pragma c_code' or `pragma foreign_code'\n",
		"is not supported when trailing (`--use-trail') is enabled."
	]),
	error(Msg).

%-----------------------------------------------------------------------------%

	% This section of the module contains predicates that are used
	% internally by the compiler for manipulating the heap.
	% These predicates should not be used by user programs directly.

:- interface.

	% free_heap/1 is used internally by the compiler to implement
	% compile-time garbage collection.
	% (Note that currently compile-time garbage collection
	% is not yet fully implemented.)
	% free_heap/1 explicitly deallocates a cell on the heap.
	% It works by calling GC_free(), which will put the cell
	% on the appropriate free list.
	% It can only be used when doing conservative GC,
	% since with `--gc none' or `--gc accurate',
	% allocation does not use a free list.
	% The `di' mode on the argument is overly conservative -- only
	% the top-level cell is clobbered. This is handled correctly by
	% mode_util__recompute_instmap_delta.
	% XXX Why isn't this marked as `impure'?
:- pred free_heap(_T).
:- mode free_heap(di) is det.

	% gc_trace/1 is used for accurate garbage collection in the
	% the MLDS->C backend.  It takes as parameters a pointer to
	% a variable (normally on the stack) and, implicitly,
	% a type_info which describes the type of that variable.
	% It traverses the heap object(s) pointed to by that variable,
	% copying them to the new heap area, and updating the
	% variable to point to the new copy.  This is done by calling
	% MR_agc_deep_copy() (from runtime/mercury_deep_copy*).

:- type mutvar(T) ---> mutvar(c_pointer).
	% a no_tag type, i.e. the representation is just a c_pointer.

:- impure pred gc_trace(mutvar(T)::in) is det.

	% mark_hp/1 and restore_hp/1 are used by the MLDS back-end,
	% to implement heap reclamation on failure.
	% (The LLDS back-end does not use these; instead it inserts
	% the corresponding LLDS instructions directly during code
	% generation.)
	% For documentation, see the corresponding LLDS instructions
	% in compiler/llds.m.  See also compiler/notes/trailing.html.

:- type heap_pointer == c_pointer.

:- impure pred mark_hp(heap_pointer::out) is det.
:- impure pred restore_hp(heap_pointer::in) is det.

	% XXX currently we don't support nondet pragma
	% foreign_code when trailing is enabled.
	% Instead we generate code which calls this procedure,
	% which will call error/1 with an appropriate message.
:- pred reclaim_heap_nondet_pragma_foreign_code is erroneous.

	% N.B. interface continued below.

:- implementation.

:- pragma foreign_decl("C", "
	#include ""mercury_heap.h""	/* for MR_free_heap() */
").

% default (Mercury) implementation for gc_trace/1
% This should be overridden by the appropriate foreign language implementation.
gc_trace(_::in) :-
	sorry("private_builtin__gc_trace/1").

:- pragma foreign_proc("C", gc_trace(Pointer::in),
	[will_not_call_mercury, thread_safe],
"
#ifdef NATIVE_GC
	*(MR_Word *)Pointer =
		MR_agc_deep_copy((MR_Word *) Pointer,
			(MR_TypeInfo) TypeInfo_for_T,
			MR_ENGINE(MR_eng_heap_zone2->min),
                        MR_ENGINE(MR_eng_heap_zone2->hardmax));
#else
	MR_fatal_error(""private_builtin__gc_trace/2: ""
		""called when accurate GC not enabled"");
#endif
").

% default (Mercury) implementation for free_heap/1
% This should be overridden by the appropriate foreign language implementation.
free_heap(_::di) :-
	sorry("private_builtin__free_heap/1").

:- pragma foreign_proc("C", free_heap(Val::di),
	[will_not_call_mercury, promise_pure, thread_safe],
	"MR_free_heap((void *) Val);").

% default (Mercury) implementations for mark_hp/1 and restore_hp/1.
% This should be overridden by the appropriate foreign language implementation.
mark_hp(_::out) :-
	sorry("private_builtin__mark_hp/1").
restore_hp(_::in) :-
	sorry("private_builtin__restore_hp/1").

:- pragma foreign_proc("C", mark_hp(SavedHeapPointer::out),
	[will_not_call_mercury, thread_safe],
"
#ifndef MR_CONSERVATIVE_GC
	MR_mark_hp(SavedHeapPointer);
#else
	/* We can't do heap reclamation with conservative GC. */
	SavedHeapPointer = 0;
#endif
").

:- pragma foreign_proc("C", restore_hp(SavedHeapPointer::in),
	[will_not_call_mercury, thread_safe],
"
#ifndef MR_CONSERVATIVE_GC
	MR_restore_hp(SavedHeapPointer);
#endif
").

:- pragma foreign_proc("MC++", mark_hp(SavedHeapPointer::out),
	[will_not_call_mercury, thread_safe],
"
	/* We can't do heap reclamation on failure in the .NET back-end. */
	SavedHeapPointer = 0;
").

:- pragma foreign_proc("MC++", restore_hp(SavedHeapPointer::in),
	[will_not_call_mercury, thread_safe],
"
	/* We can't do heap reclamation on failure in the .NET back-end. */
").

reclaim_heap_nondet_pragma_foreign_code :-
	Msg = string__append_list([
		"Sorry, not implemented:\n",
		"for the MLDS back-end (`--high-level-code')\n",
		"nondet `pragma c_code' or `pragma foreign_code'\n",
		"is not supported when `--reclaim-heap-on-failure' is enabled."
	]),
	error(Msg).

%-----------------------------------------------------------------------------%

:- interface.

	% This section of the module is for miscellaneous predicates
	% that sometimes have calls to them emitted by the compiler.

	% unsafe_type_cast/2 is used internally by the compiler. Bad things
	% will happen if this is used in programs.
	% With the LLDS back-end, it has no definition,
	% since for efficiency the code generator treats it as a builtin.
	% With the MLDS back-end, it is defined in runtime/mercury.h.

:- pred unsafe_type_cast(T1, T2).
:- mode unsafe_type_cast(in, out) is det.

:- pred unused is det.

	% N.B. interface continued below.

:- implementation.

% unsafe_type_cast is a builtin; the compiler generates inline code for it

unused :-
	( semidet_succeed ->
		error("attempted use of dead predicate")
	;
		% the following is never executed
		true
	).

%-----------------------------------------------------------------------------%

:- interface.

% var/1 is intended to make it possible to write code that effectively
% has different implementations for different modes (see type_to_univ
% in std_util.m as an example).
% It has to be impure to ensure that reordering doesn't cause the wrong
% mode to be selected.

:- impure pred var(T).
:- 	  mode var(ui) is failure.
:- 	  mode var(in) is failure.
:- 	  mode var(unused) is det.

:- impure pred nonvar(T).
:- 	  mode nonvar(ui) is det.
:- 	  mode nonvar(in) is det.
:- 	  mode nonvar(unused) is failure.

% sorry/1 is used to apologize about the fact that we have not implemented
% some predicate or function in the library for a given back end. The argument
% should give the name of the predicate or function.

:- pred sorry(string::in) is erroneous.

%-----------------------------------------------------------------------------%

:- implementation.

var(_::ui) :- fail.
var(_::in) :- fail.
var(_::unused) :- true.

nonvar(_::ui) :- true.
nonvar(_::in) :- true.
nonvar(_::unused) :- fail.

sorry(PredName) :-
	error("sorry, `" ++ PredName ++ "' not implemented\n" ++
		"for this target language (or compiler back-end).").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
