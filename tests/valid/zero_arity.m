% Test separate `:- pred' and `:- mode' declarations
% for predicates with no arguments.
% The compiler of 25/8/1999 reported a spurious duplicate
% mode error for this test case.
:- module zero_arity.

:- interface.

:- pred use_asm_labels.
:- mode use_asm_labels is semidet.

:- implementation.

:- pragma c_code(use_asm_labels, [will_not_call_mercury, thread_safe], "

#ifdef USE_ASM_LABELS
	SUCCESS_INDICATOR = TRUE;
#else
	SUCCESS_INDICATOR = FALSE;
#endif
").


