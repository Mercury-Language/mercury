%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% maybe_mlds_to_gcc - Convert MLDS to the GCC back-end representation,
% if the GCC back-end interface has been enabled.
% Main author: fjh.

% This is just a wrapper around mlds_to_gcc.m to enable that
% file to be included iff we were configured with the
% gcc back-end interface enabled.

%-----------------------------------------------------------------------------%

:- module ml_backend__maybe_mlds_to_gcc.
:- interface.

:- import_module aditi_backend__rl_file.
:- import_module ml_backend__mlds.

:- import_module bool.
:- import_module std_util.
:- use_module io.

:- type frontend_callback(T) == pred(T, io__state, io__state).
:- inst frontend_callback == (pred(out, di, uo) is det).

	% Invoke the callback either via gcc__run_backend, or directly,
	% depending on whether the gcc back-end interface has
	% been enabled.
:- pred maybe_mlds_to_gcc__run_gcc_backend(mercury_module_name::in,
	frontend_callback(T)::in(frontend_callback), T::out,
	io__state::di, io__state::uo) is det.

	% Either invoke mlds_to_gcc__compile_to_asm, or report an error
	% message, depending on whether the gcc back-end interface has
	% been enabled.  In the former case,
	% the bool returned is `yes' iff the module contained C code.
:- pred maybe_mlds_to_gcc__compile_to_asm(mlds__mlds::in, maybe(rl_file)::in,
	bool::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

#if ENABLE_GCC_BACK_END

:- use_module mlds_to_gcc.

maybe_mlds_to_gcc__run_gcc_backend(ModuleName, CallBack, CallBackOutput) -->
	mlds_to_gcc__run_gcc_backend(ModuleName, CallBack, CallBackOutput).

maybe_mlds_to_gcc__compile_to_asm(MLDS, RLFile, ContainsCCode) -->
	mlds_to_gcc__compile_to_asm(MLDS, RLFile, ContainsCCode).

#else

:- import_module parse_tree__prog_out.
:- import_module string.

maybe_mlds_to_gcc__run_gcc_backend(_ModuleName, CallBack, CallBackOutput) -->
	CallBack(CallBackOutput).

maybe_mlds_to_gcc__compile_to_asm(_MLDS, _, no) -->
	report_error(
"Sorry, `--target asm' not supported: this installation of the Mercury\n" ++
"compiler was built without support for the GCC back-end interface.").

#endif

%-----------------------------------------------------------------------------%
