% Test separate `:- pred' and `:- mode' declarations
% for predicates with no arguments.
% The compiler of 25/8/1999 reported a spurious duplicate
% mode error for this test case.
:- module zero_arity.

:- interface.

% bar has one mode, with determinism semidet.
:- pred bar is semidet.

% baz has one mode, with determinism semidet.
:- pred baz.
:- mode baz is semidet.

:- pred use_asm_labels.
:- mode use_asm_labels is semidet.

:- implementation.

:- import_module std_util.

bar :- semidet_fail.

baz :- semidet_fail.

% foo has no modes, or the modes for foo will be inferred.
:- pred foo.

% The mode error here should not be detected because this predicate
% has no modes and is not called, so no modes will be inferred.
foo :- X = 1, unify(X, _).

% quux has one mode, whose determinism will be inferred.
:- pred quux.
:- mode quux.

quux :- semidet_fail.

:- pragma c_code(use_asm_labels, [will_not_call_mercury, thread_safe], "
#ifdef MR_USE_ASM_LABELS
	SUCCESS_INDICATOR = MR_TRUE;
#else
	SUCCESS_INDICATOR = MR_FALSE;
#endif
").
