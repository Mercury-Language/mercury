% Example program using dynamic linking.
% This module loads in the object code for the module `hello'
% from the file `libhello.so', looks up the address of the
% procedure hello/2 in that module, and then calls that procedure.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module dl_test.
:- interface.
:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module dl, name_mangle, string, list.

main -->
	%
	% Load in the object code for the module `hello' from
	% the file `libhello.so'.
	%
	dl__open("./libhello.so", lazy, local, MaybeHandle),
	(	
		{ MaybeHandle = error(Msg) },
		print("dlopen failed: "), print(Msg), nl
	;
		{ MaybeHandle = ok(Handle) },
		%
		% Look up the address of the first mode (mode number 0)
		% of the predicate hello/2 in the module hello.
		%
		{ HelloProc = mercury_proc(predicate, unqualified("hello"),
					"hello", 2, 0) },
		dl__mercury_sym(Handle, HelloProc, MaybeHello),
		(
			{ MaybeHello = error(Msg) },
			print("dlsym failed: "), print(Msg), nl
		;
			{ MaybeHello = ok(HelloPred0) },
			%
			% Cast the higher-order term that we obtained
			% to the correct higher-order inst.
			%
			{ HelloPred = inst_cast(HelloPred0) },
			%
			% Call the procedure whose address
			% we just obtained.
			%
			HelloPred
		),

		{ Add3IntProc = mercury_proc(function, unqualified("hello"),
					"add3int", 3, 0) },
		dl__mercury_sym(Handle, Add3IntProc, MaybeAdd3Int),
		(
			{ MaybeAdd3Int = error(Msg2) },
			print("dlsym failed: "), print(Msg2), nl
		;
			{ MaybeAdd3Int = ok(Add3IntFunc0) },
			%
			% Cast the higher-order term that we obtained
			% to the correct higher-order inst.
			%
			{ wrapper(Add3IntFunc) =
				inst_cast_add3int(wrapper(Add3IntFunc0)) },
			%
			% Call the procedure whose address
			% we just obtained.
			%
			{ SumInt = Add3IntFunc(1, 2, 3) },
			io__format("1 + 2 + 3 = %d\n", [i(SumInt)])
		),

		%
		% unload the object code in the libhello.so file
		%
		dl__close(Handle, Result),
		(
			{ Result = error(CloseMsg) },
			print("dlclose failed: "), print(CloseMsg), nl
		;
			{ Result = ok }
		)
	).

%
% dl__mercury_sym returns a higher-order term with inst `ground'.
% We need to cast it to the right higher-order inst, which for the
% `hello' procedure is `pred(di, uo) is det', before we can actually
% call it.  The function inst_cast/1 defined below does that.
%

:- type io_pred == pred(io__state, io__state).
:- inst io_pred == (pred(di, uo) is det).

:- func inst_cast(io_pred) = io_pred.
:- mode inst_cast(in) = out(io_pred) is det.
:- pragma c_code(inst_cast(X::in) = (Y::out(io_pred)),
	[will_not_call_mercury, thread_safe], "Y = X").

% Likewise for `add3int'.
% Note that for arguments of function type, the function type
% normally gets automatically propagated into the inst.
% We use a wrapper type to avoid that.

:- type add3int == (func(int, int, int) = int).
:- type add3int_wrapper ---> wrapper(add3int).
:- inst add3int_wrapper ---> wrapper(func(in, in, in) = out is det).

:- func inst_cast_add3int(add3int_wrapper) = add3int_wrapper.
:- mode inst_cast_add3int(in) = out(add3int_wrapper) is det.
:- pragma c_code(inst_cast_add3int(X::in) = (Y::out(add3int_wrapper)),
	[will_not_call_mercury, thread_safe], "Y = X").

