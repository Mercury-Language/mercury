%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000 The University of Melbourne.
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
% argument types match that of the specified procedure.
% The implementation may check this at runtime, but is not required
% to do so.  (The current implementation checks that the type is a
% higher-order type with the appropriate arity, but it does not
% check the argument types.)
:- pred dl__mercury_sym(handle::in, mercury_proc::in, dl__result(T)::out,
	io__state::di, io__state::uo) is det.

% interface to the C function dlclose()
:- pred dl__close(handle::in, dl__result::out,
	io__state::di, io__state::uo) is det.

:- implementation.
:- import_module std_util, require, string, list.

:- pragma c_header_code("
	#include <stdio.h>
	#include ""mercury_conf.h""
#ifdef HAVE_DLFCN_H
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
#if defined(HAVE_DLFCN_H) && defined(HAVE_DLOPEN) \
 && defined(RTLD_NOW) && defined(RTLD_LAZY)
	int mode = (Mode ? RTLD_NOW : RTLD_LAZY);
	/* not all systems have RTLD_GLOBAL */
	#ifdef RTLD_GLOBAL
	  if (Scope) mode |= RTLD_GLOBAL;
	#endif
	Result = (Word) dlopen(FileName, mode);
#else
	Result = (Word) NULL;
#endif
}").

:- type closure_layout
	--->	closure_layout(
			int,
			string,
			string,
			string,
			int,
			int,
			int
		).

:- type closure
	--->	closure(
			closure_layout,
			c_pointer,
			int
		).

mercury_sym(Handle, MercuryProc0, Result) -->
	{ check_proc_spec_matches_result_type(Result, _,
		MercuryProc0, MercuryProc) },
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
		NumCurriedInputArgs = 0,
		ClosureLayout = closure_layout(0, "unknown", "unknown",
			"unknown", -1, -1, -1),
		Closure = closure(ClosureLayout, Address, NumCurriedInputArgs),
		private_builtin__unsafe_type_cast(Closure, Value),
		Result = ok(Value)
	}.
	 
%
% Check that the result type matches the information
% in the procedure specification.
%
:- pred check_proc_spec_matches_result_type(dl__result(T)::unused, T::unused,
		mercury_proc::in, mercury_proc::out) is det.
check_proc_spec_matches_result_type(_Result, Value, Proc0, Proc) :-
	Proc0 = mercury_proc(IsPredOrFunc, _Module, _Name, ProcArity, _Mode),
	type_ctor_name_and_arity(type_ctor(type_of(Value)),
		TypeModule, TypeName, TypeArity),
	(
		( TypeModule \= "builtin"
		; TypeName \= "pred", TypeName \= "func"
		)
	->
		error(
		"dl__mercury_sym: result type is not a higher-order type")
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
		ProcArity \= TypeArity
	->
		string__int_to_string(ProcArity, ProcArityString),
		string__int_to_string(TypeArity, TypeArityString),
		string__append_list([
			"dl__mercury_sym: arity mismatch: ",
			"argument has ", ProcArityString, " argument(s), ",
			"result type has ", TypeArityString, " arguments(s)"],
			Msg),
		error(Msg)
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
#if defined(HAVE_DLFCN_H) && defined(HAVE_DLSYM)
	Pointer = (Word) dlsym((void *) Handle, Name);
#else
	Pointer = (Word) NULL;
#endif
}").

:- pred dlerror(string::out, io__state::di, io__state::uo) is det.
:- pragma c_code(dlerror(ErrorMsg::out, _IO0::di, _IO::uo),
	[will_not_call_mercury], "
{
	const char *msg;

#if defined(HAVE_DLFCN_H) && defined(HAVE_DLERROR)
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
#if defined(HAVE_DLFCN_H) && defined(HAVE_DLCLOSE)
	dlclose((void *)Handle)
#endif
").
