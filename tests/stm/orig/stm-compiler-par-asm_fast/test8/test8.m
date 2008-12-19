% vim: ts=2 sw=2

:- module test8.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module string.
:- import_module stm_builtin.
:- import_module exception.
:- import_module univ.
:- import_module int.

%-------- Predicates --------

  % pred id 0
  % mode number 0 of predicate `main'/2 (det):
% variable types map (3 entries):
% IO0 (number 3): (io.state)
% IO (number 4): (io.state)
% STM_BLA (number 5): (stm_builtin.stm)
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
main(IO0, IO) :-
	new_stm_var(123, TVar, IO0, IO0a),
  'StmExpanded_actual_main_2_0_0'(TVar, X, Y, IO0a, IO1),
	io.write_int(X, IO1, IO2),
	io.write_int(Y, IO2, IO).

  % pred id 773
  % mode number 0 of predicate `StmExpanded_rollback_main_0_0_0'/0 (det):
% variable types map (18 entries):
% Stm_Aux_1_STM0 (number 7): (stm_builtin.stm)
% Stm_Aux_2_STM (number 8): (stm_builtin.stm)
% Stm_Aux_3_AtomicClosure (number 9): pred((stm_builtin.stm_dummy_output), (stm_builtin.stm), (stm_builtin.stm))
% TypeCtorInfo_10 (number 10): (private_builtin.type_info)
% Stm_Aux_4_ExceptionResult (number 11): (exception.exception_result((stm_builtin.stm_dummy_output)))
% V_12 (number 12): string
% V_13 (number 13): string
% Stm_Aux_5_BoringResult (number 14): (stm_builtin.stm_dummy_output)
% Stm_Aux_6_ExceptUnivVar (number 15): (univ.univ)
% Stm_Aux_7_RollbackExcptVar (number 16): (stm_builtin.rollback_exception)
% TypeCtorInfo_17 (number 17): (private_builtin.type_info)
% Stm_Aux_8_IsValid (number 18): (stm_builtin.stm_validation_result)
% V_19 (number 19): string
% V_20 (number 20): string
% Stm_Aux_9_IsValid (number 21): (stm_builtin.stm_validation_result)
% V_22 (number 22): string
% TypeCtorInfo_23 (number 23): (private_builtin.type_info)
% V_24 (number 24): string
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event

:- pred 'StmExpanded_rollback_main_0_0_0'(stm_var(int), int, int).
:- mode 'StmExpanded_rollback_main_0_0_0'(in, out, out) is cc_multi.
'StmExpanded_rollback_main_0_0_0'(TVar, X, Y) :-
  promise_pure (
    impure stm_builtin.stm_create_transaction_log(Stm_Aux_1_STM0),
    Stm_Aux_3_AtomicClosure = 'StmExpanded_wrapper_main_3_0_0'(TVar),
    exception.unsafe_try_stm(Stm_Aux_3_AtomicClosure, Stm_Aux_4_ExceptionResult, Stm_Aux_1_STM0, Stm_Aux_2_STM),
    ( % cannot_fail switch on `Stm_Aux_4_ExceptionResult'
      % Stm_Aux_4_ExceptionResult has functor exception.succeeded/1
      Stm_Aux_4_ExceptionResult = exception.succeeded(Stm_Aux_5_BoringResult),
			Stm_Aux_5_BoringResult = {X, Y}
    ;
      % Stm_Aux_4_ExceptionResult has functor exception.exception/1
      Stm_Aux_4_ExceptionResult = exception.exception(Stm_Aux_6_ExceptUnivVar),
      (if
        Stm_Aux_4_ExceptUnivVar = univ(rollback_retry)
      then
        ( % cannot_fail switch on `Stm_Aux_7_RollbackExcptVar'
          % Stm_Aux_7_RollbackExcptVar has functor stm_builtin.rollback_invalid_transaction/0
					Stm_Aux_7_RollbackExcptVar = rollback_invalid_transaction,
          impure stm_builtin.stm_discard_transaction_log(Stm_Aux_2_STM),
          'StmExpanded_rollback_main_0_0_0'(TVar, X, Y)
        ;
          % Stm_Aux_7_RollbackExcptVar has functor stm_builtin.rollback_retry/0
					Stm_Aux_7_RollbackExcptVar = rollback_retry,
          impure stm_builtin.stm_lock,
          impure stm_builtin.stm_validate(Stm_Aux_2_STM, Stm_Aux_9_IsValid),
          ( % cannot_fail switch on `Stm_Aux_9_IsValid'
            % Stm_Aux_9_IsValid has functor stm_builtin.stm_transaction_valid/0
						Stm_Aux_9_IsValid = stm_transaction_valid,
            impure stm_builtin.stm_block(Stm_Aux_2_STM)
          ;
            % Stm_Aux_9_IsValid has functor stm_builtin.stm_transaction_invalid/0
						Stm_Aux_9_IsValid = stm_transaction_invalid,
            impure stm_builtin.stm_unlock
          ),
          impure stm_builtin.stm_discard_transaction_log(Stm_Aux_2_STM),
          'StmExpanded_rollback_main_0_0_0'(TVar, X, Y)
        )
      else
        impure stm_builtin.stm_lock,
        impure stm_builtin.stm_validate(Stm_Aux_2_STM, Stm_Aux_8_IsValid),
        impure stm_builtin.stm_unlock,
        ( % cannot_fail switch on `Stm_Aux_8_IsValid'
          % Stm_Aux_8_IsValid has functor stm_builtin.stm_transaction_valid/0
					Stm_Aux_8_IsValid = stm_transaction_valid,
          exception.rethrow(Stm_Aux_4_ExceptionResult)
        ;
          % Stm_Aux_8_IsValid has functor stm_builtin.stm_transaction_invalid/0
					Stm_Aux_8_IsValid = stm_transaction_invalid,
          impure stm_builtin.stm_discard_transaction_log(Stm_Aux_2_STM),
          'StmExpanded_rollback_main_0_0_0'(TVar, X, Y)
        )
      )
    )
  ).

  % pred id 774
  % mode number 0 of predicate `StmExpanded_wrapper_main_3_0_0'/3 (det):
% variable types map (6 entries):
% STM_BLA (number 5): (stm_builtin.stm)
% stm_ResultVar (number 6): (stm_builtin.stm_dummy_output)
% Stm_Aux_0_NewUO (number 7): (stm_builtin.stm)
% Stm_Aux_1_Stm_Expand_IsValid (number 8): (stm_builtin.stm_validation_result)
% Stm_Aux_2_Stm_Expand_Rollback (number 10): (stm_builtin.rollback_exception)
% TypeCtorInfo_11 (number 11): (private_builtin.type_info)
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
:- pred 'StmExpanded_wrapper_main_3_0_0'(stm_var(int), {int, int}, stm, stm) is det.
:- mode 'StmExpanded_wrapper_main_3_0_0'(in, (builtin.out), (builtin.di), (builtin.uo)) is det.
'StmExpanded_wrapper_main_3_0_0'(TVar, Stm_ResultVar, STM_BLA, Stm_Aux_0_NewUO) :-
% 	throw(rollback_invalid_transaction),	
  read_stm_var(TVar, X, STM_BLA, STM_BLA1),
	Y = 2,
	unsafe_write_stm_var(TVar, 456, STM_BLA1, Stm_Aux_0_NewUO),
	Stm_ResultVar = {X, Y},
  promise_pure (
    impure stm_builtin.stm_lock,
    impure stm_builtin.stm_validate(Stm_Aux_0_NewUO, Stm_Aux_1_Stm_Expand_IsValid),
    ( % cannot_fail switch on `Stm_Aux_1_Stm_Expand_IsValid'
      % Stm_Aux_1_Stm_Expand_IsValid has functor stm_builtin.stm_transaction_valid/0
			Stm_Aux_1_Stm_Expand_IsValid = stm_transaction_valid,
      impure stm_builtin.stm_commit(Stm_Aux_0_NewUO),
      impure stm_builtin.stm_unlock
    ;
      % Stm_Aux_1_Stm_Expand_IsValid has functor stm_builtin.stm_transaction_invalid/0
			Stm_Aux_1_Stm_Expand_IsValid = stm_transaction_invalid,
      impure stm_builtin.stm_unlock,
      Stm_Aux_2_Stm_Expand_Rollback = stm_builtin.rollback_invalid_transaction,
      exception.throw(Stm_Aux_2_Stm_Expand_Rollback)
    )
  ).

  % pred id 775
  % mode number 0 of predicate `StmExpanded_actual_main_2_0_0'/2 (det):
% variable types map (2 entries):
% IO0 (number 3): (io.state)
% IO (number 4): (io.state)
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
:- pred 'StmExpanded_actual_main_2_0_0'(stm_var(int), int, int, io, io).
:- mode 'StmExpanded_actual_main_2_0_0'(in, out, out, (builtin.di), (builtin.uo)) is cc_multi.
'StmExpanded_actual_main_2_0_0'(TVar, X, Y, IO0, IO) :-
  'StmExpanded_rollback_main_0_0_0'(TVar, X, Y),
  IO = IO0.
