%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This module defines the state of the constraint solver.
%
% For each solver variable, the state records what values it may ever have,
% and for each of those values, it records whether that value has been
% ruled out yet, and if yes, by *what* constraint or other means it was
% ruled out. We record such "why not" information because we can use it
% to generate useful error messages if the constraint solver finds an
% inconsistency.
%

:- module grade_lib.grade_state.
:- interface.

:- import_module grade_lib.grade_spec.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

:- type solver_var_map == map(solver_var_id, solver_var).

:- type solver_var
    --->    solver_var(
                % The number of all the values that this variable may ever
                % be bound to.
                sv_cnt_all              :: int,

                % The number of all the values that it is still possible
                % for this variable variable to take. This will be sv_cnt_all,
                % minus the number of values that have already been ruled out
                % for this variable.
                sv_cnt_possible         :: int,

                % A list of the values that this variable may ever have,
                % specifying for each of those values whether it is still
                % possible or not, and if not, why not.
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
            % The variable cannot have this value, because autoconfiguration
            % has selected another value.

    ;       npw_user
            % The variable cannot have this value, because the user
            % has requested another value for this variable.

    ;       npw_requirement(requirement_application)
            % The variable cannot have this value, due to the application
            % of the given requirement in the given manner.

    ;       npw_labeling.
            % The variable cannot have this value, because labelling has
            % selected another value for this variable.

:- type requirement_application
    --->    requirement_application(
                % The given value of the solver variable was ruled out
                % by the application of the requirement with this id
                % and description, and in this direction.
                ra_req_id               :: requirement_id,
                ra_req_desc             :: string,
                ra_dir                  :: req_dir
            ).

:- type req_dir
    --->    narrow_then_values
    ;       delete_if_value.

%---------------------------------------------------------------------------%
%
% The requirement type is an extension of the requirement_spec type
% defined in grade_spec.m. It is defined for efficient processing, not
% easy readability of the requirement specifications, and it associates
% a unique id with each requirement, for use in "why not" records
% in the solver var map.
%

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
%
% The state of the solver.
%
% The most frequently updated part is the si_solver_var_map field, which
% records which values are (still) possible for each solver variable.
%
% The si_reqs field lists all the requirements (constraints) that may
% still be useful for propagation. Initially, this field will contain
% all the requirements transliterated from init_requirement_specs,
% but when we use a requirement for propagation, we may notice that
% the requirement won't be needed ever again. If that happens, we delete
% the requirement from the solver_info, so we won't waste time trying
% to apply it again.
%
% The si_vars_priority field will initially list all the solver variables
% in order of their priority for labelling. If two variables both have
% more than one possible value, labelling will pick the one that occurs
% earlier in this list. When labelling finds that a variable has only one
% possible value, it knows it won't be interested in the variable ever again,
% so it removes such variables from the si_vars_priority field, to make
% future labelling steps faster.
%

:- type solver_info
    --->    solver_info(
                si_reqs                 :: list(requirement),
                si_vars_priority        :: list(solver_var_id),
                si_solver_var_map       :: solver_var_map
            ).

%---------------------------------------------------------------------------%
:- end_module grade_lib.grade_state.
%---------------------------------------------------------------------------%
