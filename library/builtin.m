%---------------------------------------------------------------------------%
% Copyright (C) 1994-2000 The University of Melbourne.
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
:- mode compare(uo, ui, ui) is det.
:- mode compare(uo, ui, in) is det.
:- mode compare(uo, in, ui) is det.
:- mode compare(uo, in, in) is det.

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

:- pragma c_code(cc_cast(X :: (pred(out) is cc_multi)) =
                        (Y :: out(pred(out) is det)),
                [will_not_call_mercury, thread_safe],
                "Y = X;").
:- pragma c_code(cc_cast(X :: (pred(out) is cc_nondet)) =
                        (Y :: out(pred(out) is semidet)),
                [will_not_call_mercury, thread_safe],
                "Y = X;").

%-----------------------------------------------------------------------------%

!.
!(X, X).

%-----------------------------------------------------------------------------%

:- external(unify/2).
:- external(compare/3).

%-----------------------------------------------------------------------------%

:- pragma c_header_code("#include ""mercury_type_info.h""").

:- pragma c_code("

#ifndef HIGHLEVEL_CODE

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_FULL(builtin, , int, 0,
	MR_TYPECTOR_REP_INT,
	mercury__builtin_unify_int_2_0,
	mercury__builtin_compare_int_3_0);

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_FULL(builtin, , character, 0,
	MR_TYPECTOR_REP_CHAR,
	mercury__builtin_unify_character_2_0,
	mercury__builtin_compare_character_3_0);

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_FULL(builtin, , string, 0,
	MR_TYPECTOR_REP_STRING,
	mercury__builtin_unify_string_2_0,
	mercury__builtin_compare_string_3_0);

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_FULL(builtin, , float, 0,
	MR_TYPECTOR_REP_FLOAT,
	mercury__builtin_unify_float_2_0,
	mercury__builtin_compare_float_3_0);

	/* 
	** One of the following two is used for all higher-order types.
	** Note that they use the same three predicates.
	*/

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_FULL(builtin, , func, 0,
	MR_TYPECTOR_REP_PRED,
	mercury__builtin_unify_pred_2_0,
	mercury__builtin_compare_pred_3_0);
MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_FULL(builtin, , pred, 0,
	MR_TYPECTOR_REP_PRED,
	mercury__builtin_unify_pred_2_0,
	mercury__builtin_compare_pred_3_0);

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

Define_extern_entry(mercury__builtin_unify_pred_2_0);
Define_extern_entry(mercury__builtin_compare_pred_3_0);

BEGIN_MODULE(builtin_types_module)
	init_entry_ai(mercury__builtin_unify_pred_2_0);
	init_entry_ai(mercury__builtin_compare_pred_3_0);
BEGIN_CODE
/* code for predicate 'builtin_unify_pred'/2 in mode 0 */
Define_entry(mercury__builtin_unify_pred_2_0);
	MR_incr_sp_push_msg(2, ""private_builtin:builtin_unify_pred"");
	fatal_error(""attempted unification of higher-order terms"");

/* code for predicate 'builtin_compare_pred'/3 in mode 0 */
Define_entry(mercury__builtin_compare_pred_3_0);
	MR_incr_sp_push_msg(2, ""private_builtin:builtin_compare_pred"");
	fatal_error(""attempted comparison of higher-order terms"");
END_MODULE

/*
INIT sys_init_builtin_types_module
*/
MR_MODULE_STATIC_OR_EXTERN ModuleFunc builtin_types_module;
extern void mercury__private_builtin__init(void);

void sys_init_builtin_types_module(void); /* suppress gcc warning */
void sys_init_builtin_types_module(void) {

	builtin_types_module();

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
	MR_INIT_TYPE_CTOR_INFO_WITH_PRED(
		mercury_data___type_ctor_info_void_0, mercury__unused_0_0);
}

#endif /* ! HIGHLEVEL_CODE */

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
	save_transient_registers();
	Copy = deep_copy(&Value, TypeInfo_for_T, NULL, NULL);
	restore_transient_registers();
").
:- pragma c_code(copy(Value::in, Copy::uo), "
	save_transient_registers();
	Copy = deep_copy(&Value, TypeInfo_for_T, NULL, NULL);
	restore_transient_registers();
").
*************/

:- external(copy/2).

:- pragma c_header_code("#include ""mercury_deep_copy.h""").

:- pragma c_code("

#ifdef MR_HIGHLEVEL_CODE

void
mercury__builtin__copy_2_p_0(Word type_info, MR_Box value, MR_Box * copy)
{
	*copy = deep_copy(&value, (Word *) type_info, NULL, NULL);
}

void
mercury__builtin__copy_2_p_1(Word type_info, MR_Box x, MR_Box * y)
{
	mercury__builtin__copy_2_p_0(type_info, x, y);
}

#else /* ! MR_HIGHLEVEL_CODE */

Define_extern_entry(mercury__copy_2_0);
Define_extern_entry(mercury__copy_2_1);

BEGIN_MODULE(copy_module)
	init_entry(mercury__copy_2_0);
	init_entry(mercury__copy_2_1);
BEGIN_CODE

#ifdef PROFILE_CALLS
  #define fallthru(target, caller) { tailcall((target), (caller)); }
#else
  #define fallthru(target, caller)
#endif

Define_entry(mercury__copy_2_0);
fallthru(ENTRY(mercury__copy_2_1), ENTRY(mercury__copy_2_0))
Define_entry(mercury__copy_2_1);
{
	Word value, copy, type_info;

	type_info = r1;
	value = r2;

	save_transient_registers();
	copy = deep_copy(&value, (MR_TypeInfo) type_info, NULL, NULL);
	restore_transient_registers();

	r1 = copy;
	proceed();
}
END_MODULE

/* Ensure that the initialization code for the above module gets run. */

/*
INIT sys_init_copy_module
*/
MR_MODULE_STATIC_OR_EXTERN ModuleFunc copy_module;
void sys_init_copy_module(void);
	/* extra declaration to suppress gcc -Wmissing-decl warning */
void sys_init_copy_module(void) {
	copy_module();
}

#endif /* ! MR_HIGHLEVEL_CODE */
").

%-----------------------------------------------------------------------------%

% The type c_pointer can be used by predicates which use the C interface.

:- pragma c_code("

#ifndef MR_HIGHLEVEL_CODE

MR_DEFINE_BUILTIN_TYPE_CTOR_INFO_PRED(builtin, c_pointer, 0,
	MR_TYPECTOR_REP_C_POINTER,
	mercury____Unify___builtin__c_pointer_0_0,
	mercury____Compare___builtin__c_pointer_0_0);

BEGIN_MODULE(unify_c_pointer_module)
	init_entry(mercury____Unify___builtin__c_pointer_0_0);
	init_entry(mercury____Compare___builtin__c_pointer_0_0);

BEGIN_CODE
Define_entry(mercury____Unify___builtin__c_pointer_0_0);
	/*
	** For c_pointer, we assume that equality and comparison
	** can be based on object identity (i.e. using address comparisons).
	** This is correct for types like io__stream, and necessary since
	** the io__state contains a map(io__stream, filename).
	** However, it might not be correct in general...
	*/
	r1 = (r1 == r2);
	proceed();

Define_entry(mercury____Compare___builtin__c_pointer_0_0);
	r1 = (r1 == r2 ? MR_COMPARE_EQUAL :
			  r1 < r2 ? MR_COMPARE_LESS :
			  MR_COMPARE_GREATER);
	proceed();

END_MODULE

/* Ensure that the initialization code for the above module gets run. */
/*
INIT sys_init_unify_c_pointer_module
*/


MR_MODULE_STATIC_OR_EXTERN ModuleFunc unify_c_pointer_module;
void sys_init_unify_c_pointer_module(void);
	/* duplicate declaration to suppress gcc -Wmissing-decl warning */
void sys_init_unify_c_pointer_module(void) {
	unify_c_pointer_module();
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_builtin__type_ctor_info_c_pointer_0,
		builtin__c_pointer_0_0);
}

#endif /* ! MR_HIGHLEVEL_CODE */

").

:- end_module builtin.

%-----------------------------------------------------------------------------%
