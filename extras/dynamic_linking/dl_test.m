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
:- import_module dl, name_mangle.

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
% We need to cast it to the right higher-order inst, namely
% `pred(di, uo) is det' before we can actually call it.
% The function inst_cast/1 defined below does that.
%

:- type io_pred == pred(io__state, io__state).
:- inst io_pred == (pred(di, uo) is det).

:- func inst_cast(io_pred) = io_pred.
:- mode inst_cast(in) = out(io_pred) is det.
:- pragma c_code(inst_cast(X::in) = (Y::out(io_pred)),
	[will_not_call_mercury, thread_safe], "Y = X").
