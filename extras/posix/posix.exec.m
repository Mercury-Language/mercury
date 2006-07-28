%------------------------------------------------------------------------------%
% Copyright (C) 2001, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__exec.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__exec.

:- interface.

:- import_module string, list, map.

:- type argv == list(string).

:- type env == map(string, string).

:- pred exec(string, argv, env, posix__result, io__state, io__state).
:- mode exec(in, in, in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module pair.

:- pragma foreign_decl("C", "#include <unistd.h>").

%------------------------------------------------------------------------------%

exec(Command, Args, Env, Result) -->
	exec0(Command,
	    array(Args ++ [null]),
	    array(list__map(variable, map__to_assoc_list(Env)) ++ [null])
	),
	errno(Err),
	{ Result = error(Err) }.

:- func variable(pair(string)) = string.

variable(Name - Value) = Name ++ "=" ++ Value.

:- func null = string.

:- pragma c_code(null = (Null::out), [will_not_call_mercury, thread_safe],
    "Null = NULL; ").

:- pred exec0(string, array(string), array(string), io__state, io__state).
:- mode exec0(in, array_ui, array_ui, di, uo) is det.

:- pragma c_code(exec0(Command::in, Args::array_ui, Env::array_ui,
		IO0::di, IO::uo), [will_not_call_mercury], "{
	execve(Command,
	    ((MR_ArrayType *)Args)->elements, 
	    ((MR_ArrayType *)Env)->elements);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

