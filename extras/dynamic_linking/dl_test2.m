%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% Example program using dynamic linking.
% This example tests calling functions with floating point arguments.
%
% This module loads in the object code for the module `hello' from the file
% `libhello.so' (or `libhello.dylib' on OS X), looks up the address of the
% function add3/3 in that module, and then calls that procedure.
%
% This source file is hereby placed in the public domain.  -fjh (the author).
%
%----------------------------------------------------------------------------%

:- module dl_test2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module dl.
:- import_module list.
:- import_module name_mangle.
:- import_module string.

%----------------------------------------------------------------------------%

main(!IO) :-
    %
    % Load in the object code for the module `hello' from
    % the file `libhello.so' (`libhello.dylib' on OS X).
    %
    HelloLib = "./libhello." ++ shared_library_extension,
    dl.open(HelloLib, lazy, scope_local, MaybeHandle, !IO),
    (
        MaybeHandle = dl_error(OpenMsg),
        io.format("dlopen failed: %s\n", [s(OpenMsg)], !IO)
    ;
        MaybeHandle = dl_ok(Handle),
        Add3Proc = mercury_proc(function, unqualified("hello"), "add3", 3, 0),
        dl.mercury_sym(Handle, Add3Proc, MaybeAdd3, !IO),
        (
            MaybeAdd3 = dl_error(Msg3),
            io.format("dlsym failed: %s\n", [s(Msg3)], !IO)
        ;
            MaybeAdd3 = dl_ok(Add3Func0),
            %
            % Cast the higher-order term that we obtained to the correct
            % higher-order inst.
            %
            wrapper(Add3Func) = inst_cast_add3(wrapper(Add3Func0)),
            %
            % Call the procedure whose address we just obtained.
            %
            Sum = Add3Func(1.0, 2.0, 3.0),
            io.format("1.0 + 2.0 + 3.0 = %f\n", [f(Sum)], !IO)
        ),
        %
        % Unload the object code in the libhello.so file.
        %
        dl.close(Handle, CloseResult, !IO),
        (
            CloseResult = dl_error(CloseMsg),
            io.format("dlclose failed: %s\n", [s(CloseMsg)], !IO)
        ;
            CloseResult = dl_ok
        )
    ).

% dl.mercury_sym returns a higher-order term with inst `ground'.
% We need to cast it to the right higher-order inst, which for the
% `add3' function is `func(in, in, in) = out is det', before we can actually
% call it.  The function inst_cast_add3/1 defined below does that.
%
% Note that for arguments of function type, the function type
% normally gets automatically propagated into the inst.
% We use a wrapper type to avoid that.

:- type add3 == (func(float, float, float) = float).
:- type add3_wrapper ---> wrapper(add3).
:- inst add3_wrapper ---> wrapper(func(in, in, in) = out is det).

:- func inst_cast_add3(add3_wrapper::in) = (add3_wrapper::out(add3_wrapper))
    is det.
:- pragma foreign_proc("C",
    inst_cast_add3(X::in) = (Y::out(add3_wrapper)),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Y = X;
").

%-----------------------------------------------------------------------------%

:- func shared_library_extension = string.

shared_library_extension =
   ( if system_is_darwin then "dylib" else "so" ).

:- pred system_is_darwin is semidet.
:- pragma foreign_proc("C",
    system_is_darwin,
    [promise_pure, will_not_call_mercury, thread_safe],
"
#if defined(MR_MAC_OSX)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

%----------------------------------------------------------------------------%
:- end_module dl_test2.
%----------------------------------------------------------------------------%
