
:- module test8.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module stm_builtin.
:- import_module exception.
:- import_module univ.
:- import_module int.
:- import_module string.
%-------- Predicates --------

  % pred id 0
  % mode number 0 of predicate `test8.main'/2 (det):
% variable types map (26 entries):
% IO0 (number 3): (io.state)
% IO (number 4): (io.state)
% TVar (number 5): (stm_builtin.stm_var(int))
% IO1 (number 6): (io.state)
% IO2 (number 7): (io.state)
% B0 (number 8): (stm_builtin.stm)
% B (number 9): (stm_builtin.stm)
% X (number 10): int
% V_11 (number 11): int
% V_12 (number 12): string
% V_13 (number 13): string
% V_14 (number 14): string
% V_15 (number 15): string
% V_16 (number 16): string
% B0 (number 17): (stm_builtin.stm)
% B (number 18): (stm_builtin.stm)
% B0 (number 19): (stm_builtin.stm)
% B (number 20): (stm_builtin.stm)
% TypeCtorInfo_21 (number 21): (private_builtin.type_info)
% TypeCtorInfo_22 (number 22): (private_builtin.type_info)
% TypeCtorInfo_23 (number 23): (private_builtin.type_info)
% TypeCtorInfo_24 (number 24): (private_builtin.type_info)
% TypeCtorInfo_25 (number 25): (private_builtin.type_info)
% TypeCtorInfo_26 (number 26): (private_builtin.type_info)
% TypeCtorInfo_27 (number 27): (private_builtin.type_info)
% TypeCtorInfo_28 (number 28): (private_builtin.type_info)
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
test8.main(IO0, IO) :-
  V_11 = 123,
  stm_builtin.new_stm_var(V_11, _TVar, IO0, IO1),
  test8.'StmExpanded_toplevel_main_3_0_6'(X, IO1, IO2),
  V_13 = "X = ",
  V_15 = string.string(X),
  V_16 = "\n",
  V_14 = string.(V_15 ++ V_16),
  V_12 = string.(V_13 ++ V_14),
  io.write_string(V_12, IO2, IO).

  % pred id 1
  % mode number 0 of predicate `test8.retry_det'/1 (erroneous):
% variable types map (1 entries):
% STM (number 2): (stm_builtin.stm)
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
:- pred retry_det(stm).
:- mode retry_det((builtin.ui)) is det.
test8.retry_det(STM) :-
  stm_builtin.retry(STM).

  % pred id 604
  % mode number 0 of predicate `test8.StmExpanded_rollback_main_1_0_0'/1 (det):
% variable types map (15 entries):
% X (number 10): int
% STM0_Aux_1 (number 30): (stm_builtin.stm)
% STM_Aux_2 (number 31): (stm_builtin.stm)
% Closure_Aux_3 (number 32): pred(int, (stm_builtin.stm), (stm_builtin.stm))
% TypeCtorInfo_33 (number 33): (private_builtin.type_info)
% ExceptionResult_Aux_4 (number 34): (exception.exception_result(int))
% ExceptUnivVar_Aux_5 (number 35): (univ.univ)
% TypeCtorInfo_36 (number 36): (private_builtin.type_info)
% TypeCtorInfo_37 (number 37): (private_builtin.type_info)
% ValidResult_Aux_6 (number 38): (stm_builtin.stm_validation_result)
% ValidResult_Aux_7 (number 39): (stm_builtin.stm_validation_result)
% UnivPayload_Aux_8 (number 40): (stm_builtin.rollback_exception)
% RollbackExcpt_Aux_9 (number 41): (stm_builtin.rollback_exception)
% UnivPayload_Aux_10 (number 42): (stm_builtin.rollback_exception)
% RollbackExcpt_Aux_11 (number 43): (stm_builtin.rollback_exception)
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
:- pred 'StmExpanded_rollback_main_1_0_0'(int).
:- mode 'StmExpanded_rollback_main_1_0_0'((builtin.out)) is cc_multi.
test8.'StmExpanded_rollback_main_1_0_0'(X) :-
  promise_pure (
    impure stm_builtin.stm_create_transaction_log(STM0_Aux_1),
    Closure_Aux_3 = test8.'StmExpanded_wrapper_main_3_0_5',
    exception.unsafe_try_stm(Closure_Aux_3, ExceptionResult_Aux_4, STM0_Aux_1, STM_Aux_2),
    ( % cannot_fail switch on `ExceptionResult_Aux_4'
      % ExceptionResult_Aux_4 has functor exception.exception/1
      ExceptionResult_Aux_4 = exception.exception(ExceptUnivVar_Aux_5),
      (if
        RollbackExcpt_Aux_11 = stm_builtin.rollback_invalid_transaction,
        univ.type_to_univ(UnivPayload_Aux_10, ExceptUnivVar_Aux_5),
        UnivPayload_Aux_10 = RollbackExcpt_Aux_11
      then
        impure stm_builtin.stm_discard_transaction_log(STM_Aux_2),
        test8.'StmExpanded_rollback_main_1_0_0'(X)
      else
        (if
          RollbackExcpt_Aux_9 = stm_builtin.rollback_retry,
          univ.type_to_univ(UnivPayload_Aux_8, ExceptUnivVar_Aux_5),
          UnivPayload_Aux_8 = RollbackExcpt_Aux_9
        then
          impure stm_builtin.stm_lock,
          impure stm_builtin.stm_validate(STM_Aux_2, ValidResult_Aux_7),
          ( % cannot_fail switch on `ValidResult_Aux_7'
	    ValidResult_Aux_7 = stm_builtin.stm_transaction_valid,
            % ValidResult_Aux_7 has functor stm_builtin.stm_transaction_valid/0
            impure stm_builtin.stm_block(STM_Aux_2)
          ;
            % ValidResult_Aux_7 has functor stm_builtin.stm_transaction_invalid/0
	    ValidResult_Aux_7 = stm_builtin.stm_transaction_invalid,
            impure stm_builtin.stm_unlock
          ),
          impure stm_builtin.stm_discard_transaction_log(STM_Aux_2),
          test8.'StmExpanded_rollback_main_1_0_0'(X)
        else
          impure stm_builtin.stm_lock,
          impure stm_builtin.stm_validate(STM_Aux_2, ValidResult_Aux_6),
          impure stm_builtin.stm_unlock,
          ( % cannot_fail switch on `ValidResult_Aux_6'
            % ValidResult_Aux_6 has functor stm_builtin.stm_transaction_valid/0
	    ValidResult_Aux_6 = stm_builtin.stm_transaction_valid,
            exception.rethrow(ExceptionResult_Aux_4)
          ;
            % ValidResult_Aux_6 has functor stm_builtin.stm_transaction_invalid/0
	    ValidResult_Aux_6 = stm_builtin.stm_transaction_invalid,
            test8.'StmExpanded_rollback_main_1_0_0'(X)
          )
        )
      )
    ;
      % ExceptionResult_Aux_4 has functor exception.succeeded/1
      ExceptionResult_Aux_4 = exception.succeeded(X)
    )
  ).

  % pred id 605
  % mode number 0 of predicate `test8.StmExpanded_simple_wrapper_main_3_0_1'/3 (det):
% variable types map (6 entries):
% B0 (number 8): (stm_builtin.stm)
% B (number 9): (stm_builtin.stm)
% X (number 10): int
% stm_ResultVar (number 29): int
% V_30 (number 30): (private_builtin.type_info)
% V_31 (number 31): (private_builtin.type_info)
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
:- pred 'StmExpanded_simple_wrapper_main_3_0_1'(int, stm, stm).
:- mode 'StmExpanded_simple_wrapper_main_3_0_1'((builtin.out), (builtin.di), (builtin.uo)) is det.
test8.'StmExpanded_simple_wrapper_main_3_0_1'(Stm_ResultVar, B0, B) :-
  B0 = B,
  X = 1,
  test8.retry_det(B),
  Stm_ResultVar = X.

  % pred id 606
  % mode number 0 of predicate `test8.StmExpanded_simple_wrapper_main_3_0_2'/3 (det):
% variable types map (6 entries):
% X (number 10): int
% B0 (number 17): (stm_builtin.stm)
% B (number 18): (stm_builtin.stm)
% stm_ResultVar (number 29): int
% V_30 (number 30): (private_builtin.type_info)
% V_31 (number 31): (private_builtin.type_info)
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
:- pred 'StmExpanded_simple_wrapper_main_3_0_2'(int, stm, stm).
:- mode 'StmExpanded_simple_wrapper_main_3_0_2'((builtin.out), (builtin.di), (builtin.uo)) is det.
test8.'StmExpanded_simple_wrapper_main_3_0_2'(Stm_ResultVar, B0, B) :-
  B0 = B,
  X = 2,
  test8.retry_det(B),
  Stm_ResultVar = X.

  % pred id 607
  % mode number 0 of predicate `test8.StmExpanded_simple_wrapper_main_3_0_3'/3 (det):
% variable types map (6 entries):
% X (number 10): int
% B0 (number 19): (stm_builtin.stm)
% B (number 20): (stm_builtin.stm)
% stm_ResultVar (number 29): int
% V_30 (number 30): (private_builtin.type_info)
% V_31 (number 31): (private_builtin.type_info)
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
:- pred 'StmExpanded_simple_wrapper_main_3_0_3'(int, stm, stm).
:- mode 'StmExpanded_simple_wrapper_main_3_0_3'((builtin.out), (builtin.di), (builtin.uo)) is det.
test8.'StmExpanded_simple_wrapper_main_3_0_3'(Stm_ResultVar, B0, B) :-
  B0 = B,
  X = 3,
  test8.retry_det(B),
  Stm_ResultVar = X.

  % pred id 608
  % mode number 0 of predicate `test8.StmExpanded_or_else_main_3_0_4'/3 (cc_multi):
% variable types map (34 entries):
% X (number 10): int
% STMDI_Aux_0 (number 29): (stm_builtin.stm)
% STMUO_Aux_1 (number 30): (stm_builtin.stm)
% InnSTM_Aux_2 (number 31): (stm_builtin.stm)
% InnSTM_Aux_3 (number 32): (stm_builtin.stm)
% InnSTM_Aux_4 (number 33): (stm_builtin.stm)
% TypeCtorInfo_34 (number 34): (private_builtin.type_info)
% TypeCtorInfo_35 (number 35): (private_builtin.type_info)
% InterSTM_Aux_5 (number 36): (stm_builtin.stm)
% InterSTM_Aux_6 (number 37): (stm_builtin.stm)
% RetryCons_Aux_7 (number 38): (stm_builtin.rollback_exception)
% RollbackCons_Aux_8 (number 39): (stm_builtin.rollback_exception)
% IsValidConst_Aux_9 (number 40): (stm_builtin.stm_validation_result)
% ValidResult_Aux_10 (number 41): (stm_builtin.stm_validation_result)
% ValidResult_Aux_11 (number 42): (stm_builtin.stm_validation_result)
% ValidResult_Aux_12 (number 43): (stm_builtin.stm_validation_result)
% InnerSTM0_Aux_13 (number 44): (stm_builtin.stm)
% ExcptRes_Aux_14 (number 45): (exception.exception_result(int))
% Closure_Aux_15 (number 46): pred(int, (stm_builtin.stm), (stm_builtin.stm))
% ExceptUnivVar_Aux_16 (number 47): (univ.univ)
% UnivPayload_Aux_17 (number 48): (stm_builtin.rollback_exception)
% RollbackExcpt_Aux_18 (number 49): (stm_builtin.rollback_exception)
% InnerSTM0_Aux_19 (number 50): (stm_builtin.stm)
% ExcptRes_Aux_20 (number 51): (exception.exception_result(int))
% Closure_Aux_21 (number 52): pred(int, (stm_builtin.stm), (stm_builtin.stm))
% ExceptUnivVar_Aux_22 (number 53): (univ.univ)
% UnivPayload_Aux_23 (number 54): (stm_builtin.rollback_exception)
% RollbackExcpt_Aux_24 (number 55): (stm_builtin.rollback_exception)
% InnerSTM0_Aux_25 (number 56): (stm_builtin.stm)
% ExcptRes_Aux_26 (number 57): (exception.exception_result(int))
% Closure_Aux_27 (number 58): pred(int, (stm_builtin.stm), (stm_builtin.stm))
% ExceptUnivVar_Aux_28 (number 59): (univ.univ)
% UnivPayload_Aux_29 (number 60): (stm_builtin.rollback_exception)
% RollbackExcpt_Aux_30 (number 61): (stm_builtin.rollback_exception)
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
:- pred 'StmExpanded_or_else_main_3_0_4'(int, stm, stm).
:- mode 'StmExpanded_or_else_main_3_0_4'((builtin.out), (builtin.di), (builtin.uo)) is cc_multi.
test8.'StmExpanded_or_else_main_3_0_4'(X, STMDI_Aux_0, STMUO_Aux_1) :-
  promise_pure (
    impure stm_builtin.stm_create_nested_transaction_log(STMDI_Aux_0, InnerSTM0_Aux_25),
    Closure_Aux_27 = test8.'StmExpanded_simple_wrapper_main_3_0_1',
    exception.unsafe_try_stm(Closure_Aux_27, ExcptRes_Aux_26, InnerSTM0_Aux_25, InnSTM_Aux_2),
    ( % cannot_fail switch on `ExcptRes_Aux_26'
      % ExcptRes_Aux_26 has functor exception.exception/1
      ExcptRes_Aux_26 = exception.exception(ExceptUnivVar_Aux_28),
      (if
        RollbackExcpt_Aux_30 = stm_builtin.rollback_retry,
        univ.type_to_univ(UnivPayload_Aux_29, ExceptUnivVar_Aux_28),
        UnivPayload_Aux_29 = RollbackExcpt_Aux_30
      then
        impure stm_builtin.stm_create_nested_transaction_log(STMDI_Aux_0, InnerSTM0_Aux_19),
        Closure_Aux_21 = test8.'StmExpanded_simple_wrapper_main_3_0_2',
        exception.unsafe_try_stm(Closure_Aux_21, ExcptRes_Aux_20, InnerSTM0_Aux_19, InnSTM_Aux_3),
        ( % cannot_fail switch on `ExcptRes_Aux_20'
          % ExcptRes_Aux_20 has functor exception.exception/1
          ExcptRes_Aux_20 = exception.exception(ExceptUnivVar_Aux_22),
          (if
            RollbackExcpt_Aux_24 = stm_builtin.rollback_retry,
            univ.type_to_univ(UnivPayload_Aux_23, ExceptUnivVar_Aux_22),
            UnivPayload_Aux_23 = RollbackExcpt_Aux_24
          then
            impure stm_builtin.stm_create_nested_transaction_log(STMDI_Aux_0, InnerSTM0_Aux_13),
            Closure_Aux_15 = test8.'StmExpanded_simple_wrapper_main_3_0_3',
            exception.unsafe_try_stm(Closure_Aux_15, ExcptRes_Aux_14, InnerSTM0_Aux_13, InnSTM_Aux_4),
            ( % cannot_fail switch on `ExcptRes_Aux_14'
              % ExcptRes_Aux_14 has functor exception.exception/1
              ExcptRes_Aux_14 = exception.exception(ExceptUnivVar_Aux_16),
              (if
                RollbackExcpt_Aux_18 = stm_builtin.rollback_retry,
                univ.type_to_univ(UnivPayload_Aux_17, ExceptUnivVar_Aux_16),
                UnivPayload_Aux_17 = RollbackExcpt_Aux_18
              then
                IsValidConst_Aux_9 = stm_builtin.stm_transaction_valid,
                impure stm_builtin.stm_lock,
                impure stm_builtin.stm_validate(InnSTM_Aux_2, ValidResult_Aux_10),
                impure stm_builtin.stm_validate(InnSTM_Aux_3, ValidResult_Aux_11),
                impure stm_builtin.stm_validate(InnSTM_Aux_4, ValidResult_Aux_12),
                (if
                  ValidResult_Aux_10 = IsValidConst_Aux_9,
                  ValidResult_Aux_11 = IsValidConst_Aux_9,
                  ValidResult_Aux_12 = IsValidConst_Aux_9
                then
                  impure stm_builtin.stm_merge_nested_logs(InnSTM_Aux_2, STMDI_Aux_0, InterSTM_Aux_5),
                  impure stm_builtin.stm_merge_nested_logs(InnSTM_Aux_3, InterSTM_Aux_5, InterSTM_Aux_6),
                  impure stm_builtin.stm_merge_nested_logs(InnSTM_Aux_4, InterSTM_Aux_6, STMUO_Aux_1),
                  impure stm_builtin.stm_unlock,
                  RetryCons_Aux_7 = stm_builtin.rollback_retry,
                  exception.throw(RetryCons_Aux_7)
                else
                  impure stm_builtin.stm_unlock,
                  RollbackCons_Aux_8 = stm_builtin.rollback_invalid_transaction,
                  exception.throw(RollbackCons_Aux_8)
                )
              else
                impure stm_builtin.stm_discard_transaction_log(InnSTM_Aux_4),
                exception.rethrow(ExcptRes_Aux_14)
              )
            ;
              % ExcptRes_Aux_14 has functor exception.succeeded/1
              ExcptRes_Aux_14 = exception.succeeded(X),
              impure stm_builtin.stm_merge_nested_logs(InnSTM_Aux_4, STMDI_Aux_0, STMUO_Aux_1)
            )
          else
            impure stm_builtin.stm_discard_transaction_log(InnSTM_Aux_3),
            exception.rethrow(ExcptRes_Aux_20)
          )
        ;
          % ExcptRes_Aux_20 has functor exception.succeeded/1
          ExcptRes_Aux_20 = exception.succeeded(X),
          impure stm_builtin.stm_merge_nested_logs(InnSTM_Aux_3, STMDI_Aux_0, STMUO_Aux_1)
        )
      else
        impure stm_builtin.stm_discard_transaction_log(InnSTM_Aux_2),
        exception.rethrow(ExcptRes_Aux_26)
      )
    ;
      % ExcptRes_Aux_26 has functor exception.succeeded/1
      ExcptRes_Aux_26 = exception.succeeded(X),
      impure stm_builtin.stm_merge_nested_logs(InnSTM_Aux_2, STMDI_Aux_0, STMUO_Aux_1)
    )
  ).

  % pred id 609
  % mode number 0 of predicate `test8.StmExpanded_wrapper_main_3_0_5'/3 (det):
% variable types map (7 entries):
% B0 (number 8): (stm_builtin.stm)
% B (number 9): (stm_builtin.stm)
% X (number 10): int
% stm_ResultVar (number 29): int
% Stm_Expand_IsValid_Aux_0 (number 32): (stm_builtin.stm_validation_result)
% Stm_Expand_Rollback_Aux_1 (number 33): (stm_builtin.rollback_exception)
% TypeCtorInfo_34 (number 34): (private_builtin.type_info)
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
:- pred 'StmExpanded_wrapper_main_3_0_5'(int, stm, stm).
:- mode 'StmExpanded_wrapper_main_3_0_5'((builtin.out), (builtin.di), (builtin.uo)) is cc_multi.
test8.'StmExpanded_wrapper_main_3_0_5'(Stm_ResultVar, B0, B) :-
  test8.'StmExpanded_or_else_main_3_0_4'(X, B0, B),
  Stm_ResultVar = X,
  promise_pure (
    impure stm_builtin.stm_lock,
    impure stm_builtin.stm_validate(B, Stm_Expand_IsValid_Aux_0),
    ( % cannot_fail switch on `Stm_Expand_IsValid_Aux_0'
      % Stm_Expand_IsValid_Aux_0 has functor stm_builtin.stm_transaction_valid/0
      Stm_Expand_IsValid_Aux_0 = stm_builtin.stm_transaction_valid,
      impure stm_builtin.stm_commit(B),
      impure stm_builtin.stm_unlock
    ;
      % Stm_Expand_IsValid_Aux_0 has functor stm_builtin.stm_transaction_invalid/0
      Stm_Expand_IsValid_Aux_0 = stm_builtin.stm_transaction_invalid,
      impure stm_builtin.stm_unlock,
      Stm_Expand_Rollback_Aux_1 = stm_builtin.rollback_invalid_transaction,
      exception.throw(Stm_Expand_Rollback_Aux_1)
    )
  ).

  % pred id 610
  % mode number 0 of predicate `test8.StmExpanded_toplevel_main_3_0_6'/3 (det):
% variable types map (3 entries):
% IO1 (number 6): (io.state)
% IO2 (number 7): (io.state)
% X (number 10): int
% type_info varmap:
% typeclass_info varmap:
% rtti_var_info:
% address is not taken
% does not contain parallel conjunction
% does not contain user event
:- pred 'StmExpanded_toplevel_main_3_0_6'(int, io, io).
:- mode 'StmExpanded_toplevel_main_3_0_6'((builtin.out), (builtin.di), (builtin.uo)) is cc_multi.
test8.'StmExpanded_toplevel_main_3_0_6'(X, IO1, IO2) :-
  test8.'StmExpanded_rollback_main_1_0_0'(X),
  IO2 = IO1.

:- end_module test8.
