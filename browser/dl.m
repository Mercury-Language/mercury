%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: dl.m.
% Purpose: dynamic linking support.
% Main author: fjh.
% Stability: medium.

% This file provides an interface to the C functions dlopen(), dlsym(),
% and dlclose().  For details about the behaviour of those procedures,
% see the documentation for those procedures (i.e. `man dlopen').

%-----------------------------------------------------------------------------%
:- module mdb__dl.
:- interface.
:- import_module io.
:- import_module mdb__name_mangle.

:- type (mode) ---> lazy ; now.		% RTLD_LAZY or RTLD_NOW
:- type scope ---> local ; global.	% RTLD_GLOBAL or not.
:- type handle.
:- type result(T) ---> ok(T) ; error(string).
:- type result ---> ok ; error(string).

% interface to the C function dlopen()
:- pred dl__open(string::in, (mode)::in, scope::in, dl__result(handle)::out,
	io__state::di, io__state::uo) is det.

% low-level interface to the C function dlsym() -- returns a c_pointer.
:- pred dl__sym(handle::in, string::in, dl__result(c_pointer)::out,
	io__state::di, io__state::uo) is det.

% high-level interface to the C function dlsym().
% This version returns a higher-order predicate or function term.
% The user must use an inst cast (implemented using pragma c_code)
% to cast this term to the appropriate higher-order inst before calling
% it; see dl_test.m for an example of this.
%
% The type `T' below must be a higher-order type whose arity and
% argument types exactly match that of the specified procedure.
% The implementation may check this at runtime, but is not required
% to do so.  (The current implementation checks that the type is a
% higher-order type with the appropriate arity, but it does not
% check the argument types.)
%
% WARNING: for the `--high-level-code' back-end (the `hl*' grades),
% calling dl__mercury_sym for procedures with argument types `float'
% or `char' is not supported.

:- pred dl__mercury_sym(handle::in, mercury_proc::in, dl__result(T)::out,
	io__state::di, io__state::uo) is det.

% interface to the C function dlclose()
%
% WARNING: dlclose() is form of manual memory management.
% You need to make sure that no remaining references to code or
% static data in the dynamically linked module before you call dl__close,
% because if you do reference code or static data from the dynamically
% linked module after dl__close has been called, then the behaviour is
% undefined (and probably harmful!).
% 
% This can be difficult to ensure.  You need to make sure that you
% don't keep any references to the higher-order terms return by dl__sym.
% Furthermore you need to make sure that you don't keep any references
% to terms constructed by procedures in the dynamically loaded module,
% since such terms may contain references to static data in the
% dynamically loaded module.  You must also ensure that you don't keep
% any references to types or instances defined in the dynamically loaded
% module, as might be the case if you're using existentially quantified
% data types, since they too can contain references to static data.
%
% (Note that using builtin__copy/2, to make copies rather than
% keeping references, is *not* guaranteed to work in all cases.)
% 
:- pred dl__close(handle::in, dl__result::out,
	io__state::di, io__state::uo) is det.

:- implementation.
:- import_module std_util, require, string, list, int.

:- pragma c_header_code("
	#include <stdio.h>
	#include ""mercury_conf.h""
	#include ""mercury_string.h""	/* for MR_make_aligned_string_copy() */
#ifdef MR_HAVE_DLFCN_H
	#include <dlfcn.h>
#endif
").

:- type handle ---> handle(c_pointer).

:- pred is_null(c_pointer::in) is semidet.
:- pragma c_code(is_null(Pointer::in),
		[will_not_call_mercury, thread_safe],
		"SUCCESS_INDICATOR = ((void *)Pointer == NULL)").

open(FileName, Mode, Scope, Result) -->
	dlopen(FileName, Mode, Scope, Pointer),
	( { is_null(Pointer) } ->
		dlerror(ErrorMsg),
		{ Result = error(ErrorMsg) }
	;
		{ Result = ok(handle(Pointer)) }
	).

/*
** Note that dlopen() may call startup code (e.g. constructors for global
** variables in C++) which may end up calling Mercury, so it's not safe
** to declare this as `will_not_call_mercury'.
*/

:- pred dlopen(string::in, (mode)::in, scope::in, c_pointer::out,
	io__state::di, io__state::uo) is det.
:- pragma c_code(dlopen(FileName::in, Mode::in, Scope::in, Result::out,
		_IO0::di, _IO::uo), [], "
{
#if defined(MR_HAVE_DLFCN_H) && defined(MR_HAVE_DLOPEN) \
 && defined(RTLD_NOW) && defined(RTLD_LAZY)
	int mode = (Mode ? RTLD_NOW : RTLD_LAZY);
	/* not all systems have RTLD_GLOBAL */
	#ifdef RTLD_GLOBAL
	  if (Scope) mode |= RTLD_GLOBAL;
	#endif
	Result = (MR_Word) dlopen(FileName, mode);
#else
	Result = (MR_Word) NULL;
#endif
}").

mercury_sym(Handle, MercuryProc0, Result) -->
	{ check_proc_spec_matches_result_type(Result, _,
		MercuryProc0, MercuryProc1) },
	{ check_type_is_supported(Result, _, MercuryProc1, MercuryProc) },
	{ MangledName = proc_name_mangle(MercuryProc) },
	sym(Handle, MangledName, Result0),
	{
		Result0 = error(Msg),
		Result = error(Msg)
	;
		Result0 = ok(Address),
		%
		% convert the procedure address to a closure
		%
		( high_level_code ->
			NumCurriedInputArgs = 1,
			ClosureLayout = make_closure_layout,
			HL_Closure = make_closure(ClosureLayout,
				dl__generic_closure_wrapper,
				NumCurriedInputArgs, Address),
			private_builtin__unsafe_type_cast(HL_Closure, Value)
		;
			NumCurriedInputArgs = 0,
			ClosureLayout = make_closure_layout,
			LL_Closure = make_closure(ClosureLayout, Address,
				NumCurriedInputArgs, Address),
			private_builtin__unsafe_type_cast(LL_Closure, Value)
		),
		Result = ok(Value)
	}.

:- pragma foreign_decl("C",
"
#include ""mercury_ho_call.h""
extern	int	ML_DL_closure_counter;
").

:- pragma foreign_code("C",
"
int	ML_DL_closure_counter = 0;
").

:- func make_closure_layout = c_pointer.

:- pragma foreign_proc("C", make_closure_layout = (ClosureLayout::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"{
	MR_Closure_Id			*closure_id;
	MR_Closure_Dyn_Link_Layout	*closure_layout;
	char				buf[80];

	/* create a goal path that encodes a unique id for this closure */
	ML_DL_closure_counter++;
	sprintf(buf, ""@%d;"", ML_DL_closure_counter);

	/*
	** XXX All the allocations in this code should use malloc
	** in deep profiling grades.
	*/

	MR_incr_hp_type(closure_id, MR_Closure_Id);
	closure_id->MR_closure_proc_id.MR_proc_user.MR_user_pred_or_func =
		MR_PREDICATE;
	closure_id->MR_closure_proc_id.MR_proc_user.MR_user_decl_module =
		""unknown"";
	closure_id->MR_closure_proc_id.MR_proc_user.MR_user_def_module =
		""unknown"";
	closure_id->MR_closure_proc_id.MR_proc_user.MR_user_name = ""unknown"";
	closure_id->MR_closure_proc_id.MR_proc_user.MR_user_arity = -1;
	closure_id->MR_closure_proc_id.MR_proc_user.MR_user_mode = -1;
	closure_id->MR_closure_module_name = ""dl"";
	closure_id->MR_closure_file_name = __FILE__;
	closure_id->MR_closure_line_number = __LINE__;
	MR_make_aligned_string_copy(closure_id->MR_closure_goal_path, buf);

	MR_incr_hp_type(closure_layout, MR_Closure_Dyn_Link_Layout);
	closure_layout->MR_closure_dl_id = closure_id;
	closure_layout->MR_closure_dl_type_params = NULL;
	closure_layout->MR_closure_dl_num_all_args = 0;

	ClosureLayout = (MR_Word) closure_layout;
}").

:- func make_closure(c_pointer, c_pointer, int, c_pointer) = c_pointer.

:- pragma foreign_proc("C", make_closure(ClosureLayout::in,
	Address::in, NumArgs::in, FirstArg::in) = (Closure::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"{
	MR_Closure	*closure;
	/*
	** XXX All the allocations in this code should use malloc
	** in deep profiling grades, perhaps?
	*/
	MR_incr_hp(MR_LVALUE_CAST(MR_Word, closure), 3 + NumArgs);
	closure->MR_closure_layout = (MR_Closure_Layout *) ClosureLayout;
	closure->MR_closure_code = (MR_Code *) Address;
	closure->MR_closure_num_hidden_args = NumArgs;
	switch (NumArgs) {
	case 0:
		break;
	case 1:
		closure->MR_closure_hidden_args(1) = FirstArg;
		break;
	default:
		/* Not supported. */
		MR_fatal_error(""dl.m: make_closure: NumArgs > 1"");
	}
	Closure = (MR_Word) closure;
}").

:- pragma c_header_code("
extern MR_Box MR_CALL ML_DL_generic_closure_wrapper(void *closure,
	MR_Box arg1, MR_Box arg2, MR_Box arg3, MR_Box arg4, MR_Box arg5,
	MR_Box arg6, MR_Box arg7, MR_Box arg8, MR_Box arg9, MR_Box arg10,
	MR_Box arg11, MR_Box arg12, MR_Box arg13, MR_Box arg14, MR_Box arg15,
	MR_Box arg16, MR_Box arg17, MR_Box arg18, MR_Box arg19, MR_Box arg20);
").

:- pragma c_code("

/*
** For the --high-level-code grades, the closure will be passed
** as an argument to the wrapper procedure.  The wrapper procedure
** then extracts any needed curried arguments from the closure,
** and calls the real procedure.  Normally the wrapper procedure
** knows which real procedure it will call, but for dl.m we use
** a generic wrapper procedure, and treat the real procedure
** as a curried argument of the generic wrapper.  That is always
** the only curried argument, so all the wrapper needs to do
** is to extract the procedure address from the closure, and
** then call it, passing the same arguments that it was passed,
** except for the closure itself.
**
** XXX Using a single generic wrapper procedure is a nasty hack. 
** We play fast and loose with the C type system here.  In reality
** this will get called with different return type, different
** argument types, and with fewer than 20 arguments.  Likewise, the
** procedure that it calls may actually have different arity, return type
** and argument types than we pass.  So we really ought to have lots of
** different wrapper procedures, for each different return type, number
** of arguments, and even for each different set of argument types.
** Doing it right might require run-time code generation!
** But with traditional C calling conventions, using a single wrapper
** like this will work anyway, at least for arguments whose type is the
** same size as MR_Box.  It fails for arguments of type `char' or `float'.
**
** XXX This will also fail for calling conventions where the callee pops the
** arguments.  To handle that right, we'd need different wrappers for
** each different number of arguments.  (Doing that would also be slightly
** more efficient, so it may worth doing...)
**
** There are also a couple of libraries called `ffcall' and `libffi'
** which we might be able use to do this in a more portable manner.
*/
MR_Box MR_CALL
ML_DL_generic_closure_wrapper(void *closure,
	MR_Box arg1, MR_Box arg2, MR_Box arg3, MR_Box arg4, MR_Box arg5,
	MR_Box arg6, MR_Box arg7, MR_Box arg8, MR_Box arg9, MR_Box arg10,
	MR_Box arg11, MR_Box arg12, MR_Box arg13, MR_Box arg14, MR_Box arg15,
	MR_Box arg16, MR_Box arg17, MR_Box arg18, MR_Box arg19, MR_Box arg20)
{
	typedef MR_Box MR_CALL FuncType(
		MR_Box a1, MR_Box a2, MR_Box a3, MR_Box a4, MR_Box a5,
		MR_Box a6, MR_Box a7, MR_Box a8, MR_Box a9, MR_Box a10,
		MR_Box a11, MR_Box a12, MR_Box a13, MR_Box a14, MR_Box a15,
		MR_Box a16, MR_Box a17, MR_Box a18, MR_Box a19, MR_Box a20);
	FuncType *proc = (FuncType *)
		MR_field(MR_mktag(0), closure, (MR_Integer) 3);
	return (*proc)(arg1, arg2, arg3, arg4, arg5,
		arg6, arg7, arg8, arg9, arg10,
		arg11, arg12, arg13, arg14, arg15,
		arg16, arg17, arg18, arg19, arg20);
}

").
	 
:- func dl__generic_closure_wrapper = c_pointer.
:- pragma c_code(dl__generic_closure_wrapper = (WrapperFuncAddr::out),
	[thread_safe, will_not_call_mercury],
"
	WrapperFuncAddr = (MR_Word) &ML_DL_generic_closure_wrapper;
").

%
% Check that the result type matches the information
% in the procedure specification.
%
:- pred check_proc_spec_matches_result_type(dl__result(T)::unused, T::unused,
		mercury_proc::in, mercury_proc::out) is det.
check_proc_spec_matches_result_type(_Result, Value, Proc0, Proc) :-
	Proc0 = mercury_proc(IsPredOrFunc, _Module, _Name, ProcArity, _Mode),
	ResultType = type_of(Value),
	type_ctor_name_and_arity(type_ctor(ResultType),
		TypeModule, TypeName, TypeArity),
	( TypeName = "func" ->
		TypeProcArity = TypeArity - 1
	;
		TypeProcArity = TypeArity
	),
	(
		( TypeModule \= "builtin"
		; TypeName \= "pred", TypeName \= "func"
		)
	->
		error(
		"dl__mercury_sym: result type (`" ++
		type_name(ResultType) ++
		"') is not a higher-order type")
	;
		IsPredOrFunc = predicate, TypeName \= "pred"
	->
		string__append(
			"dl__mercury_sym: predicate/function mismatch: ",
			"argument is a predicate, result type is a function",
			Msg),
		error(Msg)
	;
		IsPredOrFunc = function, TypeName \= "func"
	->
		string__append(
			"dl__mercury_sym: predicate/function mismatch: ",
			"argument is a function, result type is a predicate",
			Msg),
		error(Msg)
	;
		ProcArity \= TypeProcArity
	->
		string__int_to_string(ProcArity, ProcArityString),
		string__int_to_string(TypeProcArity, TypeArityString),
		string__append_list([
			"dl__mercury_sym: arity mismatch: ",
			"argument has ", ProcArityString, " argument(s), ",
			"result type has ", TypeArityString, " arguments(s)"],
			Msg),
		error(Msg)
	;
		Proc = Proc0
	).

%
% Check that the given higher-order type is supported.
%
% For the MLDS back-end, we normally need wrapper functions
% for closures; the wrapper functions convert from type MR_Box
% to the appropriate argument type, and then call the function
% with the unboxed argument types.  Generating those on-the-fly
% here would be tricky!  Instead, we only try to handle the cases
% where we can use a single generic wrapper, i.e. arguments with
% types other than `char' or `float'.  All other argument types
% are word-sized, and will hopefully be passed in the same way
% by the C compiler.
%
% This procedure checks, for the MLDS back-end, that you're
% not using it on a procedure with argument types `char' or
% `float', and that the procedure doesn't have more arguments
% than the generic wrapper can handle.
%
% XXX this doesn't catch the case of no_tag types that
% end up being equivalent to `float' or `char'.
%
:- pred check_type_is_supported(dl__result(T)::unused, T::unused,
		mercury_proc::in, mercury_proc::out) is det.
check_type_is_supported(_Result, Value, Proc0, Proc) :-
	(
		high_level_code,
		list__member(ArgType, type_args(type_of(Value))),
		% The following line might be more efficient,
		% but is not yet supported by the MLDS back-end
		% ArgType = type_of(_ `with_type` float))
		ArgTypeCtor = type_ctor(ArgType),
		( type_ctor_name(ArgTypeCtor) = "float"
		; type_ctor_name(ArgTypeCtor) = "char"
		),
		type_ctor_module_name(ArgTypeCtor) = "builtin"
	->
		error("sorry, not implemented: dl__mercury_sym for procedure with argument type `float' or `char'")
	;
		high_level_code,
		% The generic wrapper only works for procedures with up to
		% 20 arguments.
		% For nondet procedures, two of the arguments get used up
		% for the continuation function and the environment pointer,
		% so we can only support 18 other arguments.
		type_ctor_arity(type_ctor(type_of(Value))) > 18
	->
		error("sorry, not implemented: dl__mercury_sym for procedure with more than 18 arguments")
	;
		Proc = Proc0
	).

sym(handle(Handle), Name, Result) -->
	dlsym(Handle, Name, Pointer),
	( { is_null(Pointer) } ->
		dlerror(ErrorMsg),
		{ Result = error(ErrorMsg) }
	;
		{ Result = ok(Pointer) }
	).

:- pred dlsym(c_pointer::in, string::in, c_pointer::out,
	io__state::di, io__state::uo) is det.
:- pragma c_code(dlsym(Handle::in, Name::in, Pointer::out,
	_IO0::di, _IO::uo), [will_not_call_mercury], "
{
#if defined(MR_HAVE_DLFCN_H) && defined(MR_HAVE_DLSYM)
	Pointer = (MR_Word) dlsym((void *) Handle, Name);
#else
	Pointer = (MR_Word) NULL;
#endif
}").

:- pred dlerror(string::out, io__state::di, io__state::uo) is det.
:- pragma c_code(dlerror(ErrorMsg::out, _IO0::di, _IO::uo),
	[will_not_call_mercury], "
{
	const char *msg;

#if defined(MR_HAVE_DLFCN_H) && defined(MR_HAVE_DLERROR)
	msg = dlerror();
	if (msg == NULL) msg = """";
#else
	MR_make_aligned_string(msg, ""sorry, not implemented: ""
		""dynamic linking not supported on this platform"");
#endif

	MR_make_aligned_string_copy(ErrorMsg, msg);
}").

close(handle(Handle), Result) -->
	dlclose(Handle), 
	dlerror(ErrorMsg),
	{ Result = (if ErrorMsg = "" then ok else error(ErrorMsg)) }.

/*
** Note that dlclose() may call finalization code (e.g. destructors for global
** variables in C++) which may end up calling Mercury, so it's not safe
** to declare this as `will_not_call_mercury'.
*/
:- pred dlclose(c_pointer::in, io__state::di, io__state::uo) is det.
:- pragma c_code(dlclose(Handle::in, _IO0::di, _IO::uo), [], "
#if defined(MR_HAVE_DLFCN_H) && defined(MR_HAVE_DLCLOSE)
	dlclose((void *)Handle)
#endif
").

%-----------------------------------------------------------------------------%

:- pred high_level_code is semidet.
:- pragma c_code(high_level_code, [will_not_call_mercury, thread_safe], "
#ifdef MR_HIGHLEVEL_CODE
	SUCCESS_INDICATOR = MR_TRUE;
#else
	SUCCESS_INDICATOR = MR_FALSE;
#endif
").

%-----------------------------------------------------------------------------%
