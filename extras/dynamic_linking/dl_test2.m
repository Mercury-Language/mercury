% Example program using dynamic linking.
% This example tests calling functions with floating point arguments.

% This module loads in the object code for the module `hello'
% from the file `libhello.so', looks up the address of the
% function add3/3 in that module, and then calls that procedure.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module dl_test2.
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

		{ Add3Proc = mercury_proc(function, unqualified("hello"),
					"add3", 3, 0) },
		dl__mercury_sym(Handle, Add3Proc, MaybeAdd3),
		(
			{ MaybeAdd3 = error(Msg3) },
			print("dlsym failed: "), print(Msg3), nl
		;
			{ MaybeAdd3 = ok(Add3Func0) },
			%
			% Cast the higher-order term that we obtained
			% to the correct higher-order inst.
			%
			{ wrapper(Add3Func) =
				inst_cast_add3(wrapper(Add3Func0)) },
			%
			% Call the procedure whose address
			% we just obtained.
			%
			{ Sum = Add3Func(1.0, 2.0, 3.0) },
			io__format("1.0 + 2.0 + 3.0 = %f\n", [f(Sum)])
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
% `add3' function is `func(in, in, in) = out is det', before we can actually
% call it.  The function inst_cast_add3/1 defined below does that.
%
% Note that for arguments of function type, the function type
% normally gets automatically propagated into the inst.
% We use a wrapper type to avoid that.

:- type add3 == (func(float, float, float) = float).
:- type add3_wrapper ---> wrapper(add3).
:- inst add3_wrapper ---> wrapper(func(in, in, in) = out is det).

:- func inst_cast_add3(add3_wrapper) = add3_wrapper.
:- mode inst_cast_add3(in) = out(add3_wrapper) is det.
:- pragma c_code(inst_cast_add3(X::in) = (Y::out(add3_wrapper)),
	[will_not_call_mercury, thread_safe], "Y = X").

