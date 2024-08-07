%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: dead_cell_table.m.
% Main authors: nancy.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.dead_cell_table.
:- interface.

:- import_module transform_hlds.ctgc.structure_reuse.domain.
:- import_module transform_hlds.smm_common.

:- import_module bool.
:- import_module io.
:- import_module map.

%---------------------------------------------------------------------------%

    % We use the type dead_cell_table to collect all deconstructions
    % that possibly leave garbage behind.
    %
    % A dead_cell_table maps program points onto reuse conditions.
    %
:- type dead_cell_table == map(program_point, reuse_condition).

%---------------------------------------------------------------------------%

    % Initialise a dead_cell_table.
    %
:- func dead_cell_table_init = dead_cell_table.

    % Check whether the table is empty.
    %
:- pred dead_cell_table_is_empty(dead_cell_table::in) is semidet.

    % Succeeds if the given program point is listed in the table. Return
    % the associated reuse_condition.
    %
:- pred dead_cell_table_search(program_point::in, dead_cell_table::in,
    reuse_condition::out) is semidet.

    % Add a program point and its associated reuse_condition to the table.
    %
:- pred dead_cell_table_set(program_point::in, reuse_condition::in,
    dead_cell_table::in, dead_cell_table::out) is det.

    % Remove a program point from the table.
    %
:- pred dead_cell_table_remove(program_point::in,
    dead_cell_table::in, dead_cell_table::out) is det.

    % Remove all program points from the table for which the reuse_conditions
    % are "conditional".
    %
:- pred dead_cell_table_remove_conditionals(dead_cell_table::in,
    dead_cell_table::out) is det.

    % Dump the contents of the table.
    %
:- pred dead_cell_table_maybe_dump(io.text_output_stream::in,
    bool::in, dead_cell_table::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

dead_cell_table_init = map.init.

dead_cell_table_is_empty(Table) :-
    map.is_empty(Table).

dead_cell_table_search(PP, Table, ReuseCond) :-
    map.search(Table, PP, ReuseCond).

dead_cell_table_set(PP, RC, !Table) :-
    map.set(PP, RC, !Table).

dead_cell_table_remove(PP, !Table) :-
    map.det_remove(PP, _, !Table).

dead_cell_table_remove_conditionals(!Table) :-
    map.foldl(dead_cell_table_add_unconditional, !.Table,
        dead_cell_table_init, !:Table).

:- pred dead_cell_table_add_unconditional(program_point::in,
    reuse_condition::in, dead_cell_table::in, dead_cell_table::out) is det.

dead_cell_table_add_unconditional(PP, C, !Table) :-
    ( if reuse_condition_is_conditional(C) then
        true
    else
        dead_cell_table_set(PP, C, !Table)
    ).

%---------------------------------------------------------------------------%

dead_cell_table_maybe_dump(Stream, MaybeDump, Table, !IO) :-
    (
        MaybeDump = no
    ;
        MaybeDump = yes,
        io.write_string(Stream, "\t\t|--------|\n", !IO),
        map.foldl(dead_cell_entry_dump(Stream), Table, !IO),
        io.write_string(Stream, "\t\t|--------|\n", !IO)
    ).

:- pred dead_cell_entry_dump(io.text_output_stream::in,
    program_point::in, reuse_condition::in, io::di, io::uo) is det.

dead_cell_entry_dump(Stream, PP, Cond, !IO) :-
    ( if reuse_condition_is_conditional(Cond) then
        io.write_string(Stream, "\t\t|  cond  |\t", !IO)
    else
        io.write_string(Stream, "\t\t| always |\t", !IO)
    ),
    dump_program_point(Stream, PP, !IO),
    io.write_string(Stream, "\n", !IO).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.dead_cell_table.
%---------------------------------------------------------------------------%
