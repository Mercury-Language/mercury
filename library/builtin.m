%---------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: builtin.m.
% Main author: fjh.
% Stability: low.

% This file is automatically imported into every module.
% It is intended for things that are part of the language,
% but which are implemented just as normal user-level code
% rather than with special coding in the compiler.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module builtin.
:- interface.

%-----------------------------------------------------------------------------%

% TYPES.

% The types `character', `int', `float', and `string',
% and tuple types `{}', `{T}', `{T1, T2}', ...
% and the types `pred', `pred(T)', `pred(T1, T2)', `pred(T1, T2, T3)', ...
% and `func(T1) = T2', `func(T1, T2) = T3', `func(T1, T2, T3) = T4', ...
% are builtin and are implemented using special code in the
% type-checker.  (XXX TODO: report an error for attempts to redefine
% these types.)

% The type c_pointer can be used by predicates which use the C interface.
:- type c_pointer.

%-----------------------------------------------------------------------------%

% INSTS.

% The standard insts `free', `ground', and `bound(...)' are builtin
% and are implemented using special code in the parser and mode-checker.

% So are the standard unique insts `unique', `unique(...)',
% `mostly_unique', `mostly_unique(...)', and `clobbered'.
% The name `dead' is allowed as a synonym for `clobbered'.
% Similarly `mostly_dead' is a synonym for `mostly_clobbered'.

:- inst dead = clobbered.
:- inst mostly_dead = mostly_clobbered.

% The `any' inst used for the constraint solver interface is also builtin.

% Higher-order predicate insts `pred(<modes>) is <detism>'
% and higher-order functions insts `func(<modes>) = <mode> is det'
% are also builtin.

%-----------------------------------------------------------------------------%

% MODES.

% The standard modes.

:- mode unused :: (free -> free).
:- mode output :: (free -> ground).
:- mode input :: (ground -> ground).

:- mode in :: (ground -> ground).
:- mode out :: (free -> ground).

:- mode in(Inst) :: (Inst -> Inst).
:- mode out(Inst) :: (free -> Inst).
:- mode di(Inst) :: (Inst -> clobbered).
:- mode mdi(Inst) :: (Inst -> mostly_clobbered).

% Unique modes.  These are still not fully implemented.

% unique output
:- mode uo :: free -> unique.

% unique input
:- mode ui :: unique -> unique.

% destructive input
:- mode di :: unique -> clobbered.

% "Mostly" unique modes (unique except that that may be referenced
% again on backtracking).

% mostly unique output
:- mode muo :: free -> mostly_unique.

% mostly unique input
:- mode mui :: mostly_unique -> mostly_unique.

% mostly destructive input
:- mode mdi :: mostly_unique -> mostly_clobbered.

% Higher-order predicate modes are builtin.

%-----------------------------------------------------------------------------%

% PREDICATES.

% Most of these probably ought to be moved to another
% module in the standard library such as std_util.m.

% copy/2 makes a deep copy of a data structure.  The resulting copy is a
% `unique' value, so you can use destructive update on it.

:- pred copy(T, T).
:- mode copy(ui, uo) is det.
:- mode copy(in, uo) is det.

% unsafe_promise_unique/2 is used to promise the compiler that you have a
% `unique' copy of a data structure, so that you can use destructive update.
% It is used to work around limitations in the current support for unique
% modes.  `unsafe_promise_unique(X, Y)' is the same as `Y = X' except that
% the compiler will assume that `Y' is unique.
%
% Note that misuse of this predicate may lead to unsound results:
% if there is more than one reference to the data in question,
% i.e. it is not `unique', then the behaviour is undefined.
% (If you lie to the compiler, the compiler will get its revenge!)

:- pred unsafe_promise_unique(T, T).
:- mode unsafe_promise_unique(in, uo) is det.

%-----------------------------------------------------------------------------%

% A call to the function `promise_only_solution(Pred)' constitutes a
% promise on the part of the caller that `Pred' has at most one solution,
% i.e. that `not some [X1, X2] (Pred(X1), Pred(X2), X1 \= X2)'.
% `promise_only_solution(Pred)' presumes that this assumption is
% satisfied, and returns the X for which Pred(X) is true, if
% there is one.
%
% You can use `promise_only_solution' as a way of 
% introducing `cc_multi' or `cc_nondet' code inside a
% `det' or `semidet' procedure.
%
% Note that misuse of this function may lead to unsound results:
% if the assumption is not satisfied, the behaviour is undefined.
% (If you lie to the compiler, the compiler will get its revenge!)

:- func promise_only_solution(pred(T)) = T.
:- mode promise_only_solution(pred(out) is cc_multi) = out is det.
:- mode promise_only_solution(pred(out) is cc_nondet) = out is semidet.

% `promise_only_solution_io' is like `promise_only_solution', but
% for procedures with unique modes (e.g. those that do IO).
%
% A call to `promise_only_solution_io(P, X, IO0, IO)' constitutes
% a promise on the part of the caller that for the given IO0,
% there is only one value of `X' and `IO' for which `P(X, IO0, IO)' is true.
% `promise_only_solution_io(P, X, IO0, IO)' presumes that this assumption
% is satisfied, and returns the X and IO for which `P(X, IO0, IO)' is true.
%
% Note that misuse of this predicate may lead to unsound results:
% if the assumption is not satisfied, the behaviour is undefined.
% (If you lie to the compiler, the compiler will get its revenge!)

:- pred promise_only_solution_io(pred(T, IO, IO), T, IO, IO).
:- mode promise_only_solution_io(pred(out, di, uo) is cc_multi,
		out, di, uo) is det.

%-----------------------------------------------------------------------------%


% We define !/0 (and !/2 for dcgs) to be equivalent to `true'.  This is for
% backwards compatibility with Prolog systems.  But of course it only works
% if all your cuts are green cuts.

:- pred ! is det.

:- pred !(T, T).
:- mode !(di, uo) is det.
:- mode !(in, out) is det.

%-----------------------------------------------------------------------------%

	% unify(X, Y) is true iff X = Y.
:- pred unify(T::in, T::in) is semidet.

:- type comparison_result ---> (=) ; (<) ; (>).

	% compare(Res, X, Y) binds Res to =, <, or >
	% depending on wheither X is =, <, or > Y in the
	% standard ordering.
:- pred compare(comparison_result, T, T).
	% Note to implementors: this mode must be first --
	% compiler/higher_order.m depends on it.
:- mode compare(uo, in, in) is det.
:- mode compare(uo, ui, ui) is det.
:- mode compare(uo, ui, in) is det.
:- mode compare(uo, in, ui) is det.

% In addition, the following predicate-like constructs are builtin:
%
%	:- pred (T = T).
%	:- pred (T \= T).
%	:- pred (pred , pred).
%	:- pred (pred ; pred).
%	:- pred (\+ pred).
%	:- pred (not pred).
%	:- pred (pred -> pred).
%	:- pred (if pred then pred).
%	:- pred (if pred then pred else pred).
%	:- pred (pred => pred).
%	:- pred (pred <= pred).
%	:- pred (pred <=> pred).
%
%	(pred -> pred ; pred).
%	some Vars pred
%	all Vars pred
%	call/N

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, string, std_util, int, float, char, string, list.

%-----------------------------------------------------------------------------%

promise_only_solution(Pred) = OutVal :-
        call(cc_cast(Pred), OutVal).

:- func cc_cast(pred(T)) = pred(T).
:- mode cc_cast(pred(out) is cc_nondet) = out(pred(out) is semidet) is det.
:- mode cc_cast(pred(out) is cc_multi) = out(pred(out) is det) is det.

:- pragma foreign_proc("C", cc_cast(X :: (pred(out) is cc_multi)) =
                        (Y :: out(pred(out) is det)),
                [will_not_call_mercury, thread_safe],
                "Y = X;").
:- pragma foreign_proc("C", cc_cast(X :: (pred(out) is cc_nondet)) =
                        (Y :: out(pred(out) is semidet)),
                [will_not_call_mercury, thread_safe],
                "Y = X;").
:- pragma foreign_proc("C#", cc_cast(X :: (pred(out) is cc_multi)) =
                        (Y :: out(pred(out) is det)),
                [will_not_call_mercury, thread_safe],
                "Y = X;").
:- pragma foreign_proc("C#", cc_cast(X :: (pred(out) is cc_nondet)) =
                        (Y :: out(pred(out) is semidet)),
                [will_not_call_mercury, thread_safe],
                "Y = X;").

promise_only_solution_io(Pred, X) -->
	call(cc_cast_io(Pred), X).

:- func cc_cast_io(pred(T, IO, IO)) = pred(T, IO, IO).
:- mode cc_cast_io(pred(out, di, uo) is cc_multi) =
	out(pred(out, di, uo) is det) is det.

:- pragma foreign_proc("C",
	cc_cast_io(X :: (pred(out, di, uo) is cc_multi)) = 
		(Y :: out(pred(out, di, uo) is det)),
                [will_not_call_mercury, thread_safe],
                "Y = X;").
:- pragma foreign_proc("C#", 
		cc_cast_io(X :: (pred(out, di, uo) is cc_multi)) =
		(Y :: out(pred(out, di, uo) is det)),
                [will_not_call_mercury, thread_safe],
                "Y = X;").

%-----------------------------------------------------------------------------%

!.
!(X, X).

%-----------------------------------------------------------------------------%

:- external(unify/2).
:- external(compare/3).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "#include ""mercury_type_info.h""").

:- pragma foreign_code("C", "

/* forward decls, to suppress gcc -Wmissing-decl warning */
void sys_init_builtin_types_module_init(void);
void sys_init_builtin_types_module_init_type_tables(void);
#ifdef MR_DEEP_PROFILING
void sys_init_builtin_types_module_write_out_proc_statics(FILE *fp);
#endif

#ifndef MR_HIGHLEVEL_CODE

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_NOCM(builtin, int, 0,
	MR_TYPECTOR_REP_INT,
	mercury__builtin_unify_int_2_0,
	mercury__builtin_compare_int_3_0);

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_NOCM(builtin, character, 0,
	MR_TYPECTOR_REP_CHAR,
	mercury__builtin_unify_character_2_0,
	mercury__builtin_compare_character_3_0);

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_NOCM(builtin, string, 0,
	MR_TYPECTOR_REP_STRING,
	mercury__builtin_unify_string_2_0,
	mercury__builtin_compare_string_3_0);

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_NOCM(builtin, float, 0,
	MR_TYPECTOR_REP_FLOAT,
	mercury__builtin_unify_float_2_0,
	mercury__builtin_compare_float_3_0);

	/* 
	** One of the following two is used for all higher-order types.
	** Note that they use the same unify and compare predicates.
	*/

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_NOCM(builtin, func, 0,
	MR_TYPECTOR_REP_PRED,
	mercury__builtin_unify_pred_2_0,
	mercury__builtin_compare_pred_3_0);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_NOCM(builtin, pred, 0,
	MR_TYPECTOR_REP_PRED,
	mercury__builtin_unify_pred_2_0,
	mercury__builtin_compare_pred_3_0);

	/*
	** All tuple types use the following type_ctor_info.
	*/
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_NOCM(builtin, tuple, 0,
	MR_TYPECTOR_REP_TUPLE,
	mercury__builtin_unify_tuple_2_0,
	mercury__builtin_compare_tuple_3_0);

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_UNUSED(void, 0, MR_TYPECTOR_REP_VOID);

#ifdef	NATIVE_GC

/*
** The following type_ctor_infos are used only by accurate gc.
*/

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_UNUSED(succip, 0, MR_TYPECTOR_REP_SUCCIP);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_UNUSED(hp, 0, MR_TYPECTOR_REP_HP);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_UNUSED(curfr, 0, MR_TYPECTOR_REP_CURFR);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_UNUSED(maxfr, 0, MR_TYPECTOR_REP_MAXFR);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_UNUSED(redofr, 0, MR_TYPECTOR_REP_REDOFR);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_UNUSED(redoip, 0, MR_TYPECTOR_REP_REDOIP);

#endif /* NATIVE_GC */

/*
** The following type_ctor_infos are used both accurate gc and by the debugger.
*/

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_UNUSED(trailptr, 0, MR_TYPECTOR_REP_TRAIL_PTR);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_UNUSED(ticket, 0, MR_TYPECTOR_REP_TICKET);

/*
INIT sys_init_builtin_types_module
*/
MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc builtin_types_module;
extern void mercury__private_builtin__init(void);

#endif /* ! HIGHLEVEL_CODE */

void
sys_init_builtin_types_module_init(void)
{
#ifndef	MR_HIGHLEVEL_CODE
	/* 
	** We had better call this init() because we use the
	** labels for the special preds of int, float, pred, 
	** character and string. If they aren't initialized,
	** we might initialize the type_ctor_info with
	** garbage.
	*/
	mercury__private_builtin__init();

	MR_INIT_BUILTIN_TYPE_CTOR_INFO(
		mercury_data___type_ctor_info_int_0, _int_);
	MR_INIT_BUILTIN_TYPE_CTOR_INFO(
		mercury_data___type_ctor_info_float_0, _float_);
	MR_INIT_BUILTIN_TYPE_CTOR_INFO(
		mercury_data___type_ctor_info_character_0, _character_);
	MR_INIT_BUILTIN_TYPE_CTOR_INFO(
		mercury_data___type_ctor_info_string_0, _string_);
	MR_INIT_BUILTIN_TYPE_CTOR_INFO(
		mercury_data___type_ctor_info_pred_0, _pred_);
	MR_INIT_BUILTIN_TYPE_CTOR_INFO(
		mercury_data___type_ctor_info_func_0, _pred_);
	MR_INIT_BUILTIN_TYPE_CTOR_INFO(
		mercury_data___type_ctor_info_tuple_0, _tuple_);
	MR_INIT_TYPE_CTOR_INFO_WITH_PRED(
		mercury_data___type_ctor_info_void_0, mercury__unused_0_0);
#endif
}

void
sys_init_builtin_types_module_init_type_tables(void)
{
#ifndef	MR_HIGHLEVEL_CODE
	MR_register_type_ctor_info(
		&mercury_data___type_ctor_info_int_0);
	MR_register_type_ctor_info(
		&mercury_data___type_ctor_info_float_0);
	MR_register_type_ctor_info(
		&mercury_data___type_ctor_info_character_0);
	MR_register_type_ctor_info(
		&mercury_data___type_ctor_info_string_0);
	MR_register_type_ctor_info(
		&mercury_data___type_ctor_info_pred_0);
	MR_register_type_ctor_info(
		&mercury_data___type_ctor_info_func_0);
	MR_register_type_ctor_info(
		&mercury_data___type_ctor_info_tuple_0);
	MR_register_type_ctor_info(
		&mercury_data___type_ctor_info_void_0);
#endif
}

#ifdef	MR_DEEP_PROFILING
void
sys_init_builtin_types_module_write_out_proc_statics(FILE *fp)
{
	/* no proc_statics to write out */
}
#endif

").

:- pragma foreign_code("MC++", "

static void compare_3(MR_TypeInfo TypeInfo_for_T, MR_Word_Ref Res, 
		MR_Box X, MR_Box Y) 
{

        MR_TypeInfo             type_info;
        MR_TypeCtorInfo         type_ctor_info;
        int                     arity;
        MR_TypeInfoParams       params;
        MR_Word                 *args;
        MR_Box                  ComparePred;

        type_info = (MR_TypeInfo) TypeInfo_for_T;
        type_ctor_info = dynamic_cast<MR_Word> (type_info->GetValue(
		MR_TYPEINFO_TYPE_CTOR_INFO_SLOT));

        if (type_ctor_info == 0) {
            type_ctor_info = type_info;
        }

        if (0) {
            // XXX code for higher order still needs to be written...
        } else {
            arity = System::Convert::ToInt32(
		type_ctor_info->GetValue(MR_TYPE_CTOR_INFO_ARITY_SLOT));
        }

	ComparePred = type_ctor_info->GetValue(
		MR_TYPE_CTOR_INFO_COMPARE_PRED_SLOT);

        switch(arity) {
	case 0: 
		mercury::runtime::GenericCall::result_call_4(
			ComparePred,
			Res, X, Y);
	break;
	case 1:
		mercury::runtime::GenericCall::result_call_5(
			ComparePred,
			type_info->GetValue(1), 
			Res, X, Y);
	break;
	case 2:
		mercury::runtime::GenericCall::result_call_6(
			ComparePred,
			type_info->GetValue(1), 
			type_info->GetValue(2), 
			Res, X, Y);
	break;
	case 3:
		mercury::runtime::GenericCall::result_call_7(
			ComparePred,
			type_info->GetValue(1), 
			type_info->GetValue(2), 
			type_info->GetValue(3), 
			Res, X, Y);
	break;
	case 4:
		mercury::runtime::GenericCall::result_call_8(
			ComparePred,
			type_info->GetValue(1), 
			type_info->GetValue(2), 
			type_info->GetValue(3), 
			type_info->GetValue(4), 
			Res, X, Y);
	break;
	case 5:
		mercury::runtime::GenericCall::result_call_9(
			ComparePred,
			type_info->GetValue(1), 
			type_info->GetValue(2), 
			type_info->GetValue(3), 
			type_info->GetValue(4), 
			type_info->GetValue(5), 
			Res, X, Y);
	break; 
	default:
		mercury::runtime::Errors::fatal_error(
			""compare/3: type arity > 5 not supported"");
	}
}

void compare_3_m1(MR_TypeInfo TypeInfo_for_T, MR_Word_Ref Res, 
		MR_Box X, MR_Box Y) 
{
	compare_3(TypeInfo_for_T, Res, X, Y);
}

void compare_3_m2(MR_TypeInfo TypeInfo_for_T, MR_Word_Ref Res, 
		MR_Box X, MR_Box Y) 
{
	compare_3(TypeInfo_for_T, Res, X, Y);
}

void compare_3_m3(MR_TypeInfo TypeInfo_for_T, MR_Word_Ref Res, 
		MR_Box X, MR_Box Y) 
{
	compare_3(TypeInfo_for_T, Res, X, Y);
}

void copy_2(MR_TypeInfo TypeInfo_for_T,
		MR_Box X, MR_Ref(MR_Box) Y) 
{
	// XXX this needs to be implemented -- just using Clone() won't work
	// because it often does shallow copies.
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
}

void copy_2_m1(MR_TypeInfo TypeInfo_for_T,
		MR_Box X, MR_Ref(MR_Box) Y) 
{
	copy_2(TypeInfo_for_T, X, Y);
}

").

:- pragma foreign_code("MC++", "

static MR_Integer unify_2_p(MR_TypeInfo TypeInfo_for_T, MR_Box X, MR_Box Y) 
{
	int			SUCCESS_INDICATOR;
        MR_TypeInfo             type_info;
        MR_TypeCtorInfo         type_ctor_info;
        MR_Box                  tmp;
        int                     arity;
        MR_TypeInfoParams       params;
        MR_Box       		UnifyPred;
	
        type_info = (MR_TypeInfo) TypeInfo_for_T;

        type_ctor_info = dynamic_cast<MR_Word> (type_info->GetValue(
		MR_TYPEINFO_TYPE_CTOR_INFO_SLOT));
        if (type_ctor_info == 0) {
            type_ctor_info = type_info;
        }

        // XXX insert code to handle higher order....
        if (0) {

        } else {
            arity = System::Convert::ToInt32(
		type_ctor_info->GetValue(MR_TYPE_CTOR_INFO_ARITY_SLOT));
        }

	UnifyPred = type_ctor_info->GetValue(
		MR_TYPE_CTOR_INFO_UNIFY_PRED_SLOT);

	switch(arity) {
	case 0: 
                SUCCESS_INDICATOR = 
			mercury::runtime::GenericCall::semidet_call_3(
				UnifyPred,
				X, Y);
	break;
	case 1:
                SUCCESS_INDICATOR = 
			mercury::runtime::GenericCall::semidet_call_4(
				UnifyPred,
				type_info->GetValue(1), 
				X, Y);
	break;
	case 2:
		SUCCESS_INDICATOR = 
			mercury::runtime::GenericCall::semidet_call_5(
				UnifyPred,
				type_info->GetValue(1), 
				type_info->GetValue(2), 
				X, Y);
	break;
	case 3:
		SUCCESS_INDICATOR =
			mercury::runtime::GenericCall::semidet_call_6(
				UnifyPred,
				type_info->GetValue(1), 
				type_info->GetValue(2), 
				type_info->GetValue(3), 
				X, Y);
	break;
	case 4:
		SUCCESS_INDICATOR =
			mercury::runtime::GenericCall::semidet_call_7(
				UnifyPred,
				type_info->GetValue(1), 
				type_info->GetValue(2), 
				type_info->GetValue(3), 
				type_info->GetValue(4), 
				X, Y);
	break;
	case 5:
		SUCCESS_INDICATOR = 
			mercury::runtime::GenericCall::semidet_call_8(
				UnifyPred,
				type_info->GetValue(1), 
				type_info->GetValue(2), 
				type_info->GetValue(3), 
				type_info->GetValue(4), 
				type_info->GetValue(5), 
				X, Y);
	break;
	default:
		mercury::runtime::Errors::fatal_error(
			""unify/2: type arity > 5 not supported"");
	}

	return SUCCESS_INDICATOR;
}

").

:- pragma foreign_code("MC++", "
	
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(builtin, int, 0, MR_TYPECTOR_REP_INT) 
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(builtin, character, 0, MR_TYPECTOR_REP_CHAR) 
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(builtin, string, 0, MR_TYPECTOR_REP_STRING) 
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(builtin, c_pointer, 0,
	MR_TYPECTOR_REP_C_POINTER) 
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(builtin, void, 0, MR_TYPECTOR_REP_VOID) 
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(builtin, float, 0, MR_TYPECTOR_REP_FLOAT) 
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(builtin, func, 0, MR_TYPECTOR_REP_PRED) 
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(builtin, pred, 0, MR_TYPECTOR_REP_PRED) 
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO(builtin, tuple, 0, MR_TYPECTOR_REP_TUPLE) 

static int
__Unify____int_0_0(MR_Integer x, MR_Integer y)
{
	return x == y;
}

static int
__Unify____string_0_0(MR_String x, MR_String y)
{
	return System::String::Equals(x, y);
}

static int
__Unify____character_0_0(MR_Char x, MR_Char y)
{
	return x == y;
}

static int
__Unify____float_0_0(MR_Float x, MR_Float y)
{
	/* XXX what should this function do when x and y are both NaNs? */
	return x == y;
}

static int
__Unify____void_0_0(MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::fatal_error(
		""called unify for type `void'"");
	return 0;
}

static int
__Unify____c_pointer_0_0(MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::fatal_error(
		""called unify for type `c_pointer'"");
	return 0;
}

static int
__Unify____func_0_0(MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::fatal_error(
		""called unify for `func' type"");
	return 0;
}

static int
__Unify____pred_0_0(MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::fatal_error(
		""called unify for `pred' type"");
	return 0;
}

static int
__Unify____tuple_0_0(MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::fatal_error(
		""called unify for `tuple' type"");
	return 0;
}

static void
__Compare____int_0_0(
	MR_Word_Ref result, MR_Integer x, MR_Integer y)
{
	int r = (x > y ? MR_COMPARE_GREATER :
		x == y ? MR_COMPARE_EQUAL :
		MR_COMPARE_LESS);
	MR_newenum(*result, r);
}

static void
__Compare____float_0_0(
	MR_Word_Ref result, MR_Float x, MR_Float y)
{
	/* XXX what should this function do when x and y are both NaNs? */
	int r = (x > y ? MR_COMPARE_GREATER :
		x == y ? MR_COMPARE_EQUAL :
		x < y ? MR_COMPARE_LESS :
		(mercury::runtime::Errors::fatal_error(
			""incomparable floats in compare/3""),
			MR_COMPARE_EQUAL)); 
	MR_newenum(*result, r);
}


static void
__Compare____string_0_0(MR_Word_Ref result,
	MR_String x, MR_String y)
{
	int res = System::String::Compare(x, y);
	int r = (res > 0 ? MR_COMPARE_GREATER :
		res == 0 ? MR_COMPARE_EQUAL :
		MR_COMPARE_LESS);
	MR_newenum(*result, r);
}

static void
__Compare____character_0_0(
	MR_Word_Ref result, MR_Char x, MR_Char y)
{
	int r = (x > y ? MR_COMPARE_GREATER :
		x == y ? MR_COMPARE_EQUAL :
		MR_COMPARE_LESS);
	MR_newenum(*result, r);
}

static void
__Compare____void_0_0(MR_Word_Ref result,
	MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::fatal_error(
		""called compare/3 for type `void'"");
}

static void
__Compare____c_pointer_0_0(
	MR_Word_Ref result, MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::fatal_error(
		""called compare/3 for type `c_pointer'"");
}

static void
__Compare____func_0_0(MR_Word_Ref result,
	MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::fatal_error(
		""called compare/3 for `func' type"");
}

static void
__Compare____pred_0_0(MR_Word_Ref result,
	MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::fatal_error(
		""called compare/3 for `pred' type"");
}

static void
__Compare____tuple_0_0(MR_Word_Ref result,
	MR_Word x, MR_Word y)
{
	mercury::runtime::Errors::fatal_error(
		""called compare/3 for `pred' type"");
}

/*
** Unification procedures with the arguments boxed.
** These are just wrappers which call the unboxed version.
*/

static int
do_unify__int_0_0(MR_Box x, MR_Box y)
{
	return mercury::builtin__cpp_code::mercury_code::__Unify____int_0_0(
		System::Convert::ToInt32(x), 
		System::Convert::ToInt32(y)); 
}

static int
do_unify__string_0_0(MR_Box x, MR_Box y)
{
	return mercury::builtin__cpp_code::mercury_code::__Unify____string_0_0(
		dynamic_cast<MR_String>(x), 
		dynamic_cast<MR_String>(y));
}

static int
do_unify__float_0_0(MR_Box x, MR_Box y)
{
	return mercury::builtin__cpp_code::mercury_code::__Unify____float_0_0(
		System::Convert::ToDouble(x), 
		System::Convert::ToDouble(y)); 
}

static int
do_unify__character_0_0(MR_Box x, MR_Box y)
{
	return mercury::builtin__cpp_code::mercury_code::__Unify____character_0_0(
		System::Convert::ToChar(x), 
		System::Convert::ToChar(y)); 
}

static int
do_unify__void_0_0(MR_Box x, MR_Box y)
{
	mercury::runtime::Errors::fatal_error(
		""called unify for type `void'"");
	return 0;
}

static int
do_unify__c_pointer_0_0(MR_Box x, MR_Box y)
{
	return mercury::builtin__cpp_code::mercury_code::__Unify____c_pointer_0_0(
		dynamic_cast<MR_Word>(x), 
		dynamic_cast<MR_Word>(y)); 
}

static int
do_unify__func_0_0(MR_Box x, MR_Box y)
{
	mercury::runtime::Errors::fatal_error(
		""called unify for `func' type"");
	return 0;
}

static int
do_unify__pred_0_0(MR_Box x, MR_Box y)
{
	mercury::runtime::Errors::fatal_error(
		""called unify for `pred' type"");
	return 0;
}

static int
do_unify__tuple_0_0(MR_Box x, MR_Box y)
{
	mercury::runtime::Errors::fatal_error(
		""called unify for `tuple' type"");
	return 0;
}

/*
** Comparison procedures with the arguments boxed.
** These are just wrappers which call the unboxed version.
*/

static void
do_compare__int_0_0(MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::builtin__cpp_code::mercury_code::__Compare____int_0_0(result,
		System::Convert::ToInt32(x), 
		System::Convert::ToInt32(y)); 
}

static void
do_compare__string_0_0(MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::builtin__cpp_code::mercury_code::__Compare____string_0_0(result,
		dynamic_cast<MR_String>(x),
		dynamic_cast<MR_String>(y));
}

static void
do_compare__float_0_0(MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::builtin__cpp_code::mercury_code::__Compare____float_0_0(result,
		System::Convert::ToDouble(x), 
		System::Convert::ToDouble(y)); 
}

static void
do_compare__character_0_0(
	MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::builtin__cpp_code::mercury_code::__Compare____character_0_0(
		result, 
		System::Convert::ToChar(x), 
		System::Convert::ToChar(y)); 
}

static void
do_compare__void_0_0(MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::runtime::Errors::fatal_error(
		""called compare/3 for type `void'"");
}

static void
do_compare__c_pointer_0_0(
	MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::builtin__cpp_code::mercury_code::__Compare____c_pointer_0_0(
		result, 
		dynamic_cast<MR_Word>(x),
		dynamic_cast<MR_Word>(y));
}

static void
do_compare__func_0_0(MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::runtime::Errors::fatal_error(
		""called compare/3 for func type"");
}

static void
do_compare__pred_0_0(MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::runtime::Errors::fatal_error(
		""called compare/3 for pred type"");
}

static void
do_compare__tuple_0_0(MR_Word_Ref result, MR_Box x, MR_Box y)
{
	mercury::runtime::Errors::fatal_error(
		""called compare/3 for tuple type"");
}


").

%-----------------------------------------------------------------------------%

% unsafe_promise_unique/2 is a compiler builtin.

%-----------------------------------------------------------------------------%

/* copy/2
	:- pred copy(T, T).
	:- mode copy(ui, uo) is det.
	:- mode copy(in, uo) is det.
*/

/*************
Using `pragma c_code' doesn't work, due to the lack of support for
aliasing, and in particular the lack of support for `ui' modes.
:- pragma c_code(copy(Value::ui, Copy::uo), "
	MR_save_transient_registers();
	Copy = MR_deep_copy(&Value, TypeInfo_for_T, NULL, NULL);
	MR_restore_transient_registers();
").
:- pragma c_code(copy(Value::in, Copy::uo), "
	MR_save_transient_registers();
	Copy = MR_deep_copy(&Value, TypeInfo_for_T, NULL, NULL);
	MR_restore_transient_registers();
").
*************/

:- external(copy/2).

:- pragma foreign_decl("C", "
#include ""mercury_deep_copy.h""
#include ""mercury_deep_profiling_hand.h""
").

:- pragma foreign_decl("C", "
#ifdef MR_HIGHLEVEL_CODE
  void MR_CALL mercury__builtin__copy_2_p_0(MR_Mercury_Type_Info, MR_Box, MR_Box *);
  void MR_CALL mercury__builtin__copy_2_p_1(MR_Mercury_Type_Info, MR_Box, MR_Box *);
#endif
").

:- pragma foreign_code("C", "

#ifdef MR_HIGHLEVEL_CODE

void MR_CALL
mercury__builtin__copy_2_p_0(MR_Mercury_Type_Info type_info,
	MR_Box value, MR_Box *copy)
{
	MR_Word val = (MR_Word) value;
	*copy = (MR_Box) MR_deep_copy(&val, (MR_TypeInfo) type_info,
		NULL, NULL);
}

void MR_CALL
mercury__builtin__copy_2_p_1(MR_Mercury_Type_Info type_info,
	MR_Box value, MR_Box *copy)
{
	mercury__builtin__copy_2_p_0(type_info, value, copy);
}

/* forward decl, to suppress gcc -Wmissing-decl warning */
void sys_init_copy_module(void);

#else /* ! MR_HIGHLEVEL_CODE */

#ifdef	MR_DEEP_PROFILING
MR_proc_static_user_builtin_empty(copy, 2, 0, ""builtin.m"", 0, TRUE);
MR_proc_static_user_builtin_empty(copy, 2, 1, ""builtin.m"", 0, TRUE);
#endif

MR_define_extern_entry(mercury__copy_2_0);
MR_define_extern_entry(mercury__copy_2_1);

MR_BEGIN_MODULE(copy_module)
	MR_init_entry(mercury__copy_2_0);
	MR_init_entry(mercury__copy_2_1);
#ifdef	MR_DEEP_PROFILING
	MR_init_label(mercury__copy_2_0_i1);
	MR_init_label(mercury__copy_2_0_i2);
	MR_init_label(mercury__copy_2_1_i1);
	MR_init_label(mercury__copy_2_1_i2);
#endif
MR_BEGIN_CODE

#ifdef	MR_DEEP_PROFILING
  #define call_label(proc_label)	MR_PASTE3(proc_label, _i, 1)
  #define exit_label(proc_label)	MR_PASTE3(proc_label, _i, 2)
  #define first_slot			3

  #define copy_body(proc_label, proc_static)				\
		MR_incr_sp_push_msg(6, ""builtin:copy/2"");		\
		MR_stackvar(6) = (MR_Word) MR_succip;			\
		MR_stackvar(1) = MR_r1;					\
		MR_stackvar(2) = MR_r2;					\
									\
		MR_deep_det_call(proc_label, proc_static, first_slot,	\
			call_label(proc_label));			\
									\
		{							\
		MR_Word		value, copy;				\
		MR_TypeInfo	type_info;				\
									\
		type_info = (MR_TypeInfo) MR_stackvar(1);		\
		value = MR_stackvar(2);					\
									\
		MR_save_transient_registers();				\
		copy = MR_deep_copy(&value, type_info, NULL, NULL);	\
		MR_restore_transient_registers();			\
									\
		MR_stackvar(1) = copy;					\
		}							\
									\
		MR_deep_det_exit(proc_label, first_slot,		\
			exit_label(proc_label));			\
									\
		MR_r1 = MR_stackvar(1);					\
		MR_succip = (MR_Code *) MR_stackvar(6);			\
		MR_decr_sp_pop_msg(6);					\
		MR_proceed();
#else
  #define copy_body(proc_label, proc_static)				\
		{							\
		MR_Word		value, copy;				\
		MR_TypeInfo	type_info;				\
									\
		type_info = (MR_TypeInfo) MR_r1;			\
		value = MR_r2;						\
									\
		MR_save_transient_registers();				\
		copy = MR_deep_copy(&value, type_info, NULL, NULL);	\
		MR_restore_transient_registers();			\
									\
		MR_r1 = copy;						\
		MR_proceed();						\
		}
#endif

MR_define_entry(mercury__copy_2_0);
	copy_body(mercury__copy_2_0,
		MR_proc_static_user_builtin_name(copy, 2, 0))

MR_define_entry(mercury__copy_2_1);
	copy_body(mercury__copy_2_1,
		MR_proc_static_user_builtin_name(copy, 2, 1))

#undef	call_label
#undef	exit_label
#undef	first_slot
#undef	copy_body
MR_END_MODULE

#endif /* ! MR_HIGHLEVEL_CODE */

/* Ensure that the initialization code for the above module gets run. */

/*
INIT sys_init_copy_module
*/

/* suppress gcc -Wmissing-decl warnings */
void sys_init_copy_module_init(void);
void sys_init_copy_module_init_type_tables(void);
#ifdef MR_DEEP_PROFILING
void sys_init_copy_module_write_out_proc_statics(FILE *fp);
#endif

MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc copy_module;

void
sys_init_copy_module_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
	copy_module();
#endif
}

void
sys_init_copy_module_init_type_tables(void)
{
}

#ifdef	MR_DEEP_PROFILING
void
sys_init_copy_module_write_out_proc_statics(FILE *fp)
{
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&mercury_data__proc_static__mercury__copy_2_0);
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&mercury_data__proc_static__mercury__copy_2_1);
}
#endif

").

%-----------------------------------------------------------------------------%

% The type c_pointer can be used by predicates which use the C interface.

:- pragma foreign_code("C", "

#include ""mercury_deep_profiling_hand.h""

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_c_pointer_module
*/

/* duplicate declarations to suppress gcc -Wmissing-decl warning */
void sys_init_c_pointer_module_init(void);
void sys_init_c_pointer_module_init_type_tables(void);
#ifdef	MR_DEEP_PROFILING
void sys_init_c_pointer_module_write_out_proc_statics(FILE *fp);
#endif

#ifndef MR_HIGHLEVEL_CODE

#ifdef	MR_DEEP_PROFILING
MR_proc_static_compiler_empty(builtin, __Unify__,   c_pointer,
	0, 0, ""builtin.m"", 0, TRUE);
MR_proc_static_compiler_empty(builtin, __Compare__, c_pointer,
	0, 0, ""builtin.m"", 0, TRUE);
#endif

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_PRED(builtin, c_pointer, 0,
	MR_TYPECTOR_REP_C_POINTER,
	mercury____Unify___builtin__c_pointer_0_0,
	mercury____Compare___builtin__c_pointer_0_0);

MR_declare_entry(mercury____Unify___builtin__c_pointer_0_0);
MR_declare_entry(mercury____Index___builtin__c_pointer_0_0);
MR_declare_entry(mercury____Compare___builtin__c_pointer_0_0);

MR_BEGIN_MODULE(c_pointer_module)
	MR_init_entry(mercury____Unify___builtin__c_pointer_0_0);
	MR_init_entry(mercury____Compare___builtin__c_pointer_0_0);
#ifdef	MR_DEEP_PROFILING
	MR_init_label(mercury____Unify___builtin__c_pointer_0_0_i1);
	MR_init_label(mercury____Unify___builtin__c_pointer_0_0_i2);
	MR_init_label(mercury____Unify___builtin__c_pointer_0_0_i3);
	MR_init_label(mercury____Unify___builtin__c_pointer_0_0_i4);
	MR_init_label(mercury____Compare___builtin__c_pointer_0_0_i1);
	MR_init_label(mercury____Compare___builtin__c_pointer_0_0_i2);
#endif
MR_BEGIN_CODE

	/*
	** For c_pointer, we assume that equality and comparison
	** can be based on object identity (i.e. using address comparisons).
	** This is correct for types like io__stream, and necessary since
	** the io__state contains a map(io__stream, filename).
	** However, it might not be correct in general...
	*/

#define	proc_label	mercury____Unify___builtin__c_pointer_0_0
#define	proc_static	MR_proc_static_compiler_name(builtin, __Unify__, \
				c_pointer, 0, 0)
#define	body_code	do { MR_r1 = (MR_r1 == MR_r2); } while(0)

#include ""mercury_hand_unify_body.h""

#undef	body_code
#undef	proc_static
#undef	proc_label

#define	proc_label	mercury____Compare___builtin__c_pointer_0_0
#define	proc_static	MR_proc_static_compiler_name(builtin, __Compare__, \
				c_pointer, 0, 0)
#define	body_code	do {						     \
				MR_r1 = (MR_r1 == MR_r2 ? MR_COMPARE_EQUAL : \
					MR_r1 < MR_r2 ? MR_COMPARE_LESS :    \
					MR_COMPARE_GREATER);		     \
			} while (0)

#include ""mercury_hand_compare_body.h""

#undef	body_code
#undef	proc_static
#undef	proc_label

MR_END_MODULE

MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc c_pointer_module;

#endif /* ! MR_HIGHLEVEL_CODE */

void
sys_init_c_pointer_module_init(void)
{
#ifndef	MR_HIGHLEVEL_CODE
	c_pointer_module();

	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_builtin__type_ctor_info_c_pointer_0,
		builtin__c_pointer_0_0);
#endif
}

void
sys_init_c_pointer_module_init_type_tables(void)
{
#ifndef	MR_HIGHLEVEL_CODE
	MR_register_type_ctor_info(
		&mercury_data_builtin__type_ctor_info_c_pointer_0);
#endif
}

#ifdef	MR_DEEP_PROFILING
void
sys_init_c_pointer_module_write_out_proc_statics(FILE *fp)
{
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_compiler_name(builtin, __Unify__, c_pointer,
			0, 0));
	MR_write_out_proc_static(fp, (MR_ProcStatic *)
		&MR_proc_static_compiler_name(builtin, __Compare__, c_pointer,
			0, 0));
}
#endif

").

:- end_module builtin.

%-----------------------------------------------------------------------------%
