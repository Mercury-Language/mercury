% Add a test case for yet another instance of the GCC global
% register bug, which causes a GCC abort with message
% "fixed or forbidden register was spilled"
:- module reg_bug.

:- interface.

:- type signal_action ---> signal_action.
:- pragma foreign_type("C", signal_action, "MR_signal_action").

:- func sig_dfl = signal_action.

:- implementation.

:- pragma foreign_decl("C", "#include ""mercury_signal.h""").

sig_dfl = (signal_action::out).

:- pragma foreign_proc("C", sig_dfl = (Result::out),
		[will_not_call_mercury, promise_pure],
	"MR_init_signal_action(&Result, SIG_DFL, MR_FALSE, MR_TRUE);").

