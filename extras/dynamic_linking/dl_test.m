%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% Example program using dynamic linking.
% This module loads in the object code for the module `hello'
% from the file `libhello.so' (or `libhello.dylib' on OS X), looks up the
% address of the procedure hello/2 in that module, and then calls that
% procedure.
%
% This source file is hereby placed in the public domain.  -fjh (the author).
%
%----------------------------------------------------------------------------%

:- module dl_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module dl.
:- import_module name_mangle.
:- import_module list.
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
        %
        % Look up the address of the first mode (mode number 0)
        % of the predicate hello/2 in the module hello.
        %
        HelloProc = mercury_proc(predicate, unqualified("hello"), "hello", 2,
            0),
        dl.mercury_sym(Handle, HelloProc, MaybeHello, !IO),
        (
            MaybeHello = dl_error(Msg),
            io.format("dlsym failed: %s\n", [s(Msg)], !IO)
        ;
            MaybeHello = dl_ok(HelloPred0),
            %
            % Cast the higher-order term that we obtained to the correct
            % higher-order inst.
            %
            HelloPred = inst_cast(HelloPred0),
            %
            % Call the procedure whose address we just obtained.
            %
            HelloPred(!IO)
        ),

        Add3IntProc = mercury_proc(function, unqualified("hello"), "add3int",
            3, 0),
        dl.mercury_sym(Handle, Add3IntProc, MaybeAdd3Int, !IO),
        (
            MaybeAdd3Int = dl_error(Msg2),
            io.format("dlsym failed: %s\n", [s(Msg2)], !IO)
        ;
            MaybeAdd3Int = dl_ok(Add3IntFunc0),
            %
            % Cast the higher-order term that we obtained to the correct
            % higher-order inst.
            %
            wrapper(Add3IntFunc) = inst_cast_add3int(wrapper(Add3IntFunc0)),
            %
            % Call the procedure whose address we just obtained.
            %
            SumInt = Add3IntFunc(1, 2, 3),
            io.format("1 + 2 + 3 = %d\n", [i(SumInt)], !IO)
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
% We need to cast it to the right higher-order inst, which for the `hello'
% procedure is `pred(di, uo) is det', before we can actually call it.  The
% function inst_cast/1 defined below does that.
%
:- type io_pred == pred(io, io).
:- inst io_pred == (pred(di, uo) is det).

:- func inst_cast(io_pred::in) = (io_pred::out(io_pred)) is det.
:- pragma foreign_proc("C",
    inst_cast(X::in) = (Y::out(io_pred)),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Y = X;
").

% Likewise for `add3int'.
% Note that for arguments of function type, the function type normally gets
% automatically propagated into the inst.  We use a wrapper type to avoid that.
%
:- type add3int == (func(int, int, int) = int).
:- type add3int_wrapper ---> wrapper(add3int).
:- inst add3int_wrapper ---> wrapper(func(in, in, in) = out is det).

:- func inst_cast_add3int(add3int_wrapper::in)
    = (add3int_wrapper::out(add3int_wrapper)) is det.

:- pragma foreign_proc("C",
    inst_cast_add3int(X::in) = (Y::out(add3int_wrapper)),
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

%-----------------------------------------------------------------------------%
:- end_module dl_test.
%-----------------------------------------------------------------------------%
