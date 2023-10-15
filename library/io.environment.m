%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: io.environment.m.
%
% This provides read and write access to the environment variables
% inherited by this process from its parent process.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module io.environment.
:- interface.

:- import_module maybe.

%---------------------------------------------------------------------------%

    % The following predicates provide an interface to the environment list.
    % Do not attempt to put spaces or '=' signs in the names of environment
    % variables, or bad things may result!
    %
    % First argument is the name of the environment variable. Returns
    % yes(Value) if the variable was set (Value will be set to the value
    % of the variable) and no if the variable was not set.
    %
:- pred get_environment_var(string::in, maybe(string)::out,
    io::di, io::uo) is det.

    % First argument is the name of the environment variable, second argument
    % is the value to be assigned to that variable. Res is 'ok' on success or
    % 'error(ErrorCode)' if the system runs out of environment space or if
    % the environment cannot be modified.
    %
    % Note that the environment cannot be modified on Java.
    %
:- pred set_environment_var(string::in, string::in, io.res::out,
    io::di, io::uo) is det.

    % Same as set_environment_var/5, but throws an exception if an error
    % occurs.
    %
:- pred set_environment_var(string::in, string::in, io::di, io::uo) is det.

    % Test if the set_environment_var/{4,5} predicates are available.
    % This is false for Java backends.
    %
:- pred have_set_environment_var is semidet.

    % Return a map containing all the environment variables in the current
    % environment, together with their values.
    %
:- pred get_environment_var_map(environment_var_map::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module pair.

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
#if defined(MR_WIN32)
   #include ""mercury_string.h"" // For MR_utf8_to_wide.
#endif
").

%---------------------------------------------------------------------------%

get_environment_var(Var, OptValue, !IO) :-
    promise_pure (
        ( if semipure getenv(Var, Value) then
            OptValue0 = yes(Value)
        else
            OptValue0 = no
        ),
        OptValue = OptValue0
    ).

set_environment_var(Var, Value, Res, !IO) :-
    promise_pure (
        ( if io.environment.have_set_environment_var then
            ( if impure setenv(Var, Value) then
                Res = ok
            else
                string.format("Could not set environment variable `%s'",
                    [s(Var)], Message),
                Res = error(io_error_string(Message))
            )
        else
            Message = "Cannot set environment variables on this platform",
            Res = error(io_error_string(Message))
        )
    ).

set_environment_var(Var, Value, IO0, IO) :-
    io.environment.set_environment_var(Var, Value, Res, IO0, IO1),
    (
        Res = ok,
        IO = IO1
    ;
        Res = error(ErrorCode),
        error(io.error_message(ErrorCode))
    ).

:- pragma foreign_proc("C",
    have_set_environment_var,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = MR_TRUE;
").

:- pragma foreign_proc("Java",
    have_set_environment_var,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("C#",
    have_set_environment_var,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

%---------------------%

get_environment_var_map(EnvVarMap, !IO) :-
    get_environment_var_assoc_list([], EnvVarAL, !IO),
    map.from_assoc_list(EnvVarAL, EnvVarMap).

:- type env_var_assoc_list == assoc_list(string, string).

:- pred get_environment_var_assoc_list(
    env_var_assoc_list::in, env_var_assoc_list::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_environment_var_assoc_list(EnvVarAL0::in, EnvVarAL::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    MR_Word cur_env = EnvVarAL0;
    MR_Word next_env;
    int     i;
    char    **environ_vars;

    // See the comments about the environ global below
    // for an explanation of this.
    #if defined(MR_MAC_OSX)
        environ_vars = (*_NSGetEnviron());
    #else
        environ_vars = environ;
    #endif

    for (i = 0; environ_vars[i] != NULL; i++) {
        ML_record_env_var_equal_value(environ_vars[i], cur_env, &next_env);
        cur_env = next_env;
    }

    EnvVarAL = cur_env;
").

:- pragma foreign_proc("Java",
    get_environment_var_assoc_list(EnvVarAL0::in, EnvVarAL::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure],
"
    EnvVarAL = EnvVarAL0;
    java.util.Map<String, String> env = java.lang.System.getenv();
    for (java.util.Map.Entry<String, String> entry : env.entrySet()) {
        String name = entry.getKey();
        String value = entry.getValue();
        EnvVarAL = jmercury.io__environment.ML_record_env_var_and_value(name,
            value, EnvVarAL);
    }
").

:- pragma foreign_proc("C#",
    get_environment_var_assoc_list(EnvVarAL0::in, EnvVarAL::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure],
"
    EnvVarAL = EnvVarAL0;
    System.Collections.IDictionary env =
        System.Environment.GetEnvironmentVariables();
    foreach (System.Collections.DictionaryEntry entry in env) {
        string name = (string) entry.Key;
        string value = (string) entry.Value;
        EnvVarAL = mercury.io__environment.ML_record_env_var_and_value(name,
            value, EnvVarAL);
    }
").

:- pred record_env_var_equal_value(string::in,
    env_var_assoc_list::in, env_var_assoc_list::out) is det.
:- pragma foreign_export("C", record_env_var_equal_value(in, in, out),
    "ML_record_env_var_equal_value").

record_env_var_equal_value(EnvVarNameEqValue, !EnvVarAL) :-
    ( if
        sub_string_search(EnvVarNameEqValue, "=", IndexOfEq),
        string.split(EnvVarNameEqValue, IndexOfEq, EnvVarName, EqEnvVarValue),
        string.first_char(EqEnvVarValue, _Eq, EnvVarValue)
    then
        !:EnvVarAL = [EnvVarName - EnvVarValue | !.EnvVarAL]
    else
        unexpected($pred, "No = in environment entry")
    ).

:- pred record_env_var_and_value(string::in, string::in,
    env_var_assoc_list::in, env_var_assoc_list::out) is det.
:- pragma foreign_export("C#", record_env_var_and_value(in, in, in, out),
    "ML_record_env_var_and_value").
:- pragma foreign_export("Java", record_env_var_and_value(in, in, in, out),
    "ML_record_env_var_and_value").

record_env_var_and_value(EnvVarName, EnvVarValue, !EnvVarAL) :-
    !:EnvVarAL = [EnvVarName - EnvVarValue | !.EnvVarAL].

%---------------------%

:- pragma foreign_decl("C", "
#include <stdlib.h> // for getenv() and setenv()
").

:- pragma foreign_decl("C", "
// A note regarding the declaration of the environ global variable
// that follows:
//
// The man page (on Linux) says that it should be declared by the user program.
//
// On MinGW, environ is a macro (defined in stdlib.h) that expands to a
// function call that returns the user environment; no additional
// declaration is required.
//
// On Mac OS X, shared libraries do not have direct access to environ.
// The man page for environ(7) says that we should look it up at runtime
// using _NSGetEnviron().

#if defined(MR_HAVE_ENVIRON) && !defined(MR_MAC_OSX)
    #include <unistd.h>

    #if !defined(MR_MINGW)
        extern char **environ;
    #endif
#endif

#if defined(MR_MAC_OSX)
    #include <crt_externs.h>
#endif

#ifdef MR_HAVE_SPAWN_H
    #include <spawn.h>
#endif
").

    % getenv(Var, Value):
    %
    % Gets the value Value associated with the environment variable Var.
    % Fails if the variable was not set.
    %
:- semipure pred getenv(string::in, string::out) is semidet.

:- pragma foreign_proc("C",
    getenv(Var::in, Value::out),
    [promise_semipure, will_not_call_mercury, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_WIN32
    wchar_t *ValueW = _wgetenv(MR_utf8_to_wide(Var));
    if (ValueW != NULL) {
        Value = MR_wide_to_utf8(ValueW, MR_ALLOC_ID);
    } else {
        Value = NULL;
    }
#else
    Value = getenv(Var);
#endif
    SUCCESS_INDICATOR = (Value != 0);
").

:- pragma foreign_proc("C#",
    getenv(Var::in, Value::out),
    [promise_semipure, will_not_call_mercury, tabled_for_io],
"
    Value = System.Environment.GetEnvironmentVariable(Var);
    SUCCESS_INDICATOR = (Value != null);
").

:- pragma foreign_proc("Java",
    getenv(Var::in, Value::out),
    [promise_semipure, will_not_call_mercury, tabled_for_io,
        may_not_duplicate],
"
    Value = System.getenv(Var);
    SUCCESS_INDICATOR = (Value != null);
").

    % setenv(NameString, ValueString):
    %
    % Sets the named environment variable to the specified value.
    % Fails if the operation does not work.
    %
:- impure pred setenv(string::in, string::in) is semidet.

:- pragma foreign_proc("C",
    setenv(Var::in, Value::in),
    [will_not_call_mercury, not_thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_WIN32
    SUCCESS_INDICATOR =
        (_wputenv_s(MR_utf8_to_wide(Var), MR_utf8_to_wide(Value)) == 0);
#else
    SUCCESS_INDICATOR = (setenv(Var, Value, 1) == 0);
#endif
").

:- pragma foreign_proc("C#",
    setenv(Var::in, Value::in),
    [will_not_call_mercury, tabled_for_io],
"
    try {
        System.Environment.SetEnvironmentVariable(Var, Value);
        SUCCESS_INDICATOR = true;
    } catch (System.Exception) {
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("Java",
    setenv(Var::in, Value::in),
    [will_not_call_mercury, tabled_for_io, may_not_duplicate],
"
    // Java does not provide a method to set environment variables, only a way
    // to modify the environment when creating a new process.

    // Avoid warning: Var, Value
    SUCCESS_INDICATOR = false;
").

%---------------------------------------------------------------------------%
:- end_module io.environment.
%---------------------------------------------------------------------------%
