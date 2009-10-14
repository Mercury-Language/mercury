%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2003-2006, 2008-2009 The University of Melbourne.
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

:- module ml_backend.maybe_mlds_to_gcc.
:- interface.

:- import_module ml_backend.mlds.

:- import_module bool.
:- import_module io.

:- type frontend_callback(T) == pred(T, io.state, io.state).
:- inst frontend_callback == (pred(out, di, uo) is det).

    % Invoke the callback either via gcc.run_backend, or directly,
    % depending on whether the gcc back-end interface has been enabled.
    %
:- pred maybe_run_gcc_backend(mercury_module_name::in,
    frontend_callback(T)::in(frontend_callback), T::out,
    io::di, io::uo) is det.

    % Either invoke mlds_to_gcc.compile_to_asm, or report an error message,
    % depending on whether the gcc back-end interface has been enabled.
    % In the former case, the bool returned is `yes' iff the module
    % contained C code.
    %
:- pred maybe_compile_to_asm(mlds::in, bool::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

#if ENABLE_GCC_BACK_END

:- use_module mlds_to_gcc.

maybe_run_gcc_backend(ModuleName, CallBack, CallBackOutput, !IO) :-
    mlds_to_gcc.run_gcc_backend(ModuleName, CallBack, CallBackOutput, !IO).

maybe_compile_to_asm(MLDS, ContainsCCode, !IO) :-
    mlds_to_gcc.compile_to_asm(MLDS, ContainsCCode, !IO).

#else

:- import_module libs.file_util.
:- import_module string.

maybe_run_gcc_backend(_ModuleName, CallBack, CallBackOutput, !IO) :-
    CallBack(CallBackOutput, !IO).

maybe_compile_to_asm(_MLDS, no, !IO) :-
    report_error(
"Sorry, `--target asm' not supported: this installation of the Mercury\n" ++
"compiler was built without support for the GCC back-end interface.",
    !IO).

#endif

%-----------------------------------------------------------------------------%
