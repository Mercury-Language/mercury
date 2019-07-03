%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2006 The University of Melbourne.
% Copyright (C) 2015-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: dl.m.
% Purpose: dynamic linking support.
% Main author: fjh.
% Stability: medium.
%
% This file provides an interface to the C functions dlopen(), dlsym(),
% and dlclose(). For details about the behaviour of those procedures,
% see the documentation for those procedures (i.e. `man dlopen').
%
%---------------------------------------------------------------------------%

:- module mdb.dl.

:- interface.

:- import_module mdb.name_mangle.
:- import_module io.

:- type link_mode
    --->    lazy        % RTLD_LAZY
    ;       now.        % RTLD_NOW

:- type scope
    --->    scope_local
    ;       scope_global.  % RTLD_GLOBAL or not.

:- type handle.
:- type dl_result(T)
    --->    dl_ok(T)
    ;       dl_error(string).

:- type dl_result
    --->    dl_ok
    ;       dl_error(string).

    % Interface to the C function dlopen().
    %
:- pred open(string::in, link_mode::in, scope::in, dl_result(handle)::out,
    io::di, io::uo) is det.

    % Low-level interface to the C function dlsym() -- returns a c_pointer.
    %
:- pred sym(handle::in, string::in, dl_result(c_pointer)::out,
    io::di, io::uo) is det.

    % High-level interface to the C function dlsym().
    %
    % This version returns a higher-order predicate or function term.
    % The user must use an inst cast (implemented using pragma foreign_proc)
    % to cast this term to the appropriate higher-order inst before calling it;
    % see dl_test.m for an example of this.
    %
    % The type `T' below must be a higher-order type whose arity and
    % argument types exactly match that of the specified procedure.
    % The implementation may check this at runtime, but is not required
    % to do so. (The current implementation checks that the type is a
    % higher-order type with the appropriate arity, but it does not
    % check the argument types.)
    %
    % WARNING: for the `--high-level-code' back-end (the `hl*' grades),
    % calling mercury_sym for procedures with argument types `float'
    % or `char' is not supported.
    %
:- pred mercury_sym(handle::in, mercury_proc::in, dl_result(T)::out,
    io::di, io::uo) is det.

    % Interface to the C function dlclose().
    %
    % WARNING: dlclose() is form of manual memory management.
    % You need to make sure that no remaining references to code or
    % static data in the dynamically linked module before you call close,
    % because if you do reference code or static data from the dynamically
    % linked module after close has been called, then the behaviour is
    % undefined (and probably harmful!).
    %
    % This can be difficult to ensure. You need to make sure that you
    % don't keep any references to the higher-order terms return by sym.
    % Furthermore you need to make sure that you don't keep any references
    % to terms constructed by procedures in the dynamically loaded module,
    % since such terms may contain references to static data in the
    % dynamically loaded module. You must also ensure that you don't keep
    % any references to types or instances defined in the dynamically loaded
    % module, as might be the case if you're using existentially quantified
    % data types, since they too can contain references to static data.
    %
    % (Note that using builtin.copy/2, to make copies rather than
    % keeping references, is *not* guaranteed to work in all cases.)
    %
:- pred close(handle::in, dl_result::out, io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module type_desc.

:- pragma foreign_decl("C", "
    #include <stdio.h>
    #include ""mercury_conf.h""
    #include ""mercury_string.h""   /* for MR_make_aligned_string_copy() */
    #include ""mercury_ho_call.h""
#ifdef MR_HAVE_DLFCN_H
    #include <dlfcn.h>
#endif
").

:- type handle
    --->    handle(c_pointer).

:- pred is_null(c_pointer::in) is semidet.
:- pragma no_determinism_warning(is_null/1).

:- pragma foreign_proc("C",
    is_null(Pointer::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((void *) Pointer == NULL)
").

is_null(_) :-
    private_builtin.sorry("is_null").

open(FileName, Mode, Scope, Result, !IO) :-
    dlopen(FileName, Mode, Scope, Pointer, !IO),
    ( if is_null(Pointer) then
        dlerror(ErrorMsg, !IO),
        Result = dl_error(ErrorMsg)
    else
        Result = dl_ok(handle(Pointer))
    ).

:- pred dlopen(string::in, link_mode::in, scope::in, c_pointer::out,
    io::di, io::uo) is det.
:- pragma no_determinism_warning(dlopen/6).

:- pragma foreign_proc("C",
    dlopen(FileName::in, Mode::in, Scope::in, Result::out, _IO0::di, _IO::uo),
    % Note that dlopen() may call startup code (e.g. constructors for global
    % variables in C++) which may end up calling Mercury, so it is NOT safe
    % to assert that it `will_not_call_mercury'.
    [may_call_mercury, promise_pure, tabled_for_io],
"{
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

dlopen(_, _, _, _, !IO) :-
    private_builtin.sorry("dlopen").

:- pragma foreign_decl("C",
"
#include ""mercury_ho_call.h""
").

    % Convert the given procedure address to a closure.
    %
:- func make_closure(c_pointer) = c_pointer.
:- pragma no_determinism_warning(make_closure/1).

:- pragma foreign_proc("C",
    make_closure(ProcAddr::in) = (Closure::out),
    [will_not_call_mercury, promise_pure],
"{
    MR_save_transient_hp();
    Closure = (MR_Word) MR_make_closure((MR_Code *) ProcAddr);
    MR_restore_transient_hp();
}").

make_closure(_) = _ :-
    private_builtin.sorry("make_closure").

    % Check that the result type matches the information in the
    % procedure specification.
    %
:- pred check_proc_spec_matches_result_type(dl_result(T)::unused, T::unused,
    mercury_proc::in, mercury_proc::out) is det.

check_proc_spec_matches_result_type(_Result, Value, Proc0, Proc) :-
    Proc0 = mercury_proc(IsPredOrFunc, _Module, _Name, ProcArity, _Mode),
    ResultType = type_of(Value),
    type_ctor_name_and_arity(type_ctor(ResultType),
        TypeModule, TypeName, TypeArity),
    ( if TypeName = "func" then
        TypeProcArity = TypeArity - 1
    else
        TypeProcArity = TypeArity
    ),
    ( if
        ( TypeModule \= "builtin"
        ; TypeName \= "pred", TypeName \= "func"
        )
    then
        unexpected($pred,
            "result type (`" ++ type_name(ResultType) ++ "')" ++
            " is not a higher-order type")
    else if
        IsPredOrFunc = predicate, TypeName \= "pred"
    then
        unexpected($pred,
            "predicate/function mismatch: " ++
            "argument is a predicate, result type is a function")
    else if
        IsPredOrFunc = function, TypeName \= "func"
    then
        unexpected($pred,
            "predicate/function mismatch: " ++
            "argument is a function, result type is a predicate")
    else if
        ProcArity \= TypeProcArity
    then
        string.int_to_string(ProcArity, ProcArityString),
        string.int_to_string(TypeProcArity, TypeArityString),
        string.append_list(["arity mismatch: ",
            "argument has ", ProcArityString, " argument(s), ",
            "result type has ", TypeArityString, " arguments(s)"], Msg),
        unexpected($pred, Msg)
    else
        Proc = Proc0
    ).

    % Check that the given higher-order type is supported.
    %
    % For the MLDS back-end, we normally need wrapper functions for closures;
    % the wrapper functions convert from type MR_Box to the appropriate
    % argument type, and then call the function with the unboxed argument
    % types. Generating those on-the-fly here would be tricky! Instead,
    % we only try to handle the cases where we can use a single generic
    % wrapper, i.e. arguments with types other than `char' or `float'.
    % All other argument types are word-sized, and will hopefully be passed
    % in the same way by the C compiler.
    %
    % This procedure checks, for the MLDS back-end, that you are not using it
    % on a procedure with argument types `char' or `float', and that the
    % procedure doesn't have more arguments than the generic wrapper
    % can handle.
    %
    % XXX This doesn't catch the case of no_tag types that end up
    % being equivalent to `float' or `char'.
    %
:- pred check_type_is_supported(dl_result(T)::unused, T::unused,
    mercury_proc::in, mercury_proc::out) is det.

check_type_is_supported(_Result, Value, Proc0, Proc) :-
    ( if
        high_level_code,
        list.member(ArgType, type_args(type_of(Value))),
        % The following line might be more efficient,
        % but is not yet supported by the MLDS back-end
        % ArgType = type_of(_ `with_type` float))
        ArgTypeCtor = type_ctor(ArgType),
        ( type_ctor_name(ArgTypeCtor) = "float"
        ; type_ctor_name(ArgTypeCtor) = "char"
        ),
        type_ctor_module_name(ArgTypeCtor) = "builtin"
    then
        sorry($pred, "procedure with argument type `float' or `char'")
    else if
        high_level_code,
        % The generic wrapper only works for procedures with up to 20
        % arguments. For nondet procedures, two of the arguments get used up
        % for the continuation function and the environment pointer, so we can
        % only support 18 other arguments.
        type_ctor_arity(type_ctor(type_of(Value))) > 18
    then
        sorry($pred, "procedure with more than 18 arguments")
    else
        Proc = Proc0
    ).

sym(handle(Handle), Name, Result, !IO) :-
    dlsym(Handle, Name, Pointer, !IO),
    ( if is_null(Pointer) then
        dlerror(ErrorMsg, !IO),
        Result = dl_error(ErrorMsg)
    else
        Result = dl_ok(Pointer)
    ).

mercury_sym(Handle, MercuryProc0, Result, !IO) :-
    check_proc_spec_matches_result_type(Result, _, MercuryProc0, MercuryProc1),
    check_type_is_supported(Result, _, MercuryProc1, MercuryProc),
    MangledName = proc_name_mangle(MercuryProc),
    sym(Handle, MangledName, Result0, !IO),
    (
        Result0 = dl_error(Msg),
        Result = dl_error(Msg)
    ;
        Result0 = dl_ok(Address),
        private_builtin.unsafe_type_cast(make_closure(Address), Closure),
        Result = dl_ok(Closure)
    ).

:- pred dlsym(c_pointer::in, string::in, c_pointer::out,
    io::di, io::uo) is det.
:- pragma no_determinism_warning(dlsym/5).

:- pragma foreign_proc("C",
    dlsym(Handle::in, Name::in, Pointer::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
#if defined(MR_HAVE_DLFCN_H) && defined(MR_HAVE_DLSYM)
    Pointer = (MR_Word) dlsym((void *) Handle, Name);
#else
    Pointer = (MR_Word) NULL;
#endif
}").

dlsym(_, _, _, !IO) :-
    private_builtin.sorry("dlsym").

:- pred dlerror(string::out, io::di, io::uo) is det.
:- pragma no_determinism_warning(dlerror/3).

:- pragma foreign_proc("C",
    dlerror(ErrorMsg::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
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

dlerror(_, !IO) :-
    private_builtin.sorry("dlerror").

close(handle(Handle), Result, !IO) :-
    dlclose(Handle, !IO),
    dlerror(ErrorMsg, !IO),
    Result = (if ErrorMsg = "" then dl_ok else dl_error(ErrorMsg)).

:- pred dlclose(c_pointer::in, io::di, io::uo) is det.
:- pragma no_determinism_warning(dlclose/3).

:- pragma foreign_proc("C",
    dlclose(Handle::in, _IO0::di, _IO::uo),
    % Note that dlclose() may call finalization code (e.g. destructors
    % for global variables in C++) which may end up calling Mercury,
    % so it is NOT safe to declare this as `will_not_call_mercury'.
    [may_call_mercury, promise_pure, tabled_for_io],
"
#if defined(MR_HAVE_DLFCN_H) && defined(MR_HAVE_DLCLOSE)
    dlclose((void *) Handle);
#endif
").

dlclose(_, !IO) :-
    private_builtin.sorry("dlclose").

%---------------------------------------------------------------------------%

:- pred high_level_code is semidet.
:- pragma no_determinism_warning(high_level_code/0).

:- pragma foreign_proc("C",
    high_level_code,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_HIGHLEVEL_CODE
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

high_level_code :-
    private_builtin.sorry("high_level_code").

%---------------------------------------------------------------------------%
