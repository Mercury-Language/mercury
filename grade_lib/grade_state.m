%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_state.
:- interface.

:- import_module grade_spec.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

:- type solver_var_map == map(solver_var_id, solver_var).

:- type solver_var
    --->    solver_var(
                sv_cnt_all              :: int,
                sv_cnt_possible         :: int,
                sv_values               :: list(solver_var_value)
            ).

:- type solver_var_value
    --->    solver_var_value(
                svv_id                  :: solver_var_value_id,
                svv_is_possible         :: solver_var_value_possible
            ).

:- type solver_var_value_possible
    --->    is_possible
    ;       not_possible(
                np_why                  :: not_possible_why
            ).

:- type not_possible_why
    --->    npw_config
    ;       npw_user
    ;       npw_requirement(requirement_application)
    ;       npw_labeling.

:- type requirement_application
    --->    requirement_application(
                ra_req_id               :: requirement_id,
                ra_req_desc             :: string,
                ra_dir                  :: req_dir
            ).

:- type req_dir
    --->    narrow_then_values
    ;       delete_if_value.

%---------------------------------------------------------------------------%

:- type requirement_id
    --->    requirement_id(int).

:- type requirement
    --->    requirement(
                req_id                  :: requirement_id,
                req_desc                :: string,
                req_if_solver_var       :: solver_var_id,
                req_if_value            :: solver_var_value_id,
                req_then_solver_var     :: solver_var_id,
                req_then_values         :: list(solver_var_value_id)
            ).

%---------------------------------------------------------------------------%

:- type solver_info
    --->    solver_info(
                si_reqs                 :: list(requirement),
                si_vars_priority        :: list(solver_var_id),
                si_solver_var_map       :: solver_var_map
            ).

%---------------------------------------------------------------------------%
:- end_module grade_state.
%---------------------------------------------------------------------------%
