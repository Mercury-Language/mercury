%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2003-2004, 2006-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: analysis.m.
% Main authors: stayl, wangp.
%
% An inter-module analysis framework, as described in
%
%   Nicholas Nethercote. The Analysis Framework of HAL,
%   Chapter 7: Inter-module Analysis, Master's Thesis,
%   University of Melbourne, September 2001, revised April 2002.
%   <http://njn.valgrind.org/pubs/masters2001.ps>.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module analysis.framework.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module term.
:- import_module unit.

%---------------------------------------------------------------------------%

    % The intention is that eventually any compiler can use this library
    % via .NET by defining an instance of this type class.
:- typeclass compiler(Compiler) where [
    func compiler_name(Compiler) = string,

        % Describe the analyses which can be performed by a compiler.
        %
    pred analyses(Compiler, analysis_name, analysis_type),
    mode analyses(in, in, out) is semidet,
    mode analyses(in, out, out) is multi,

        % module_name_to_read_file_name(Compiler, Globals, Ext,
        %   ModuleName, MaybeFileName, !IO)
        %
    pred module_name_to_read_file_name(Compiler::in, globals::in,
        ext::in, module_name::in, maybe_error(string)::out,
        io::di, io::uo) is det,

        % module_name_to_write_file_name(Compiler, Globals, Ext,
        %   ModuleName, FileName, !IO)
        %
    pred module_name_to_write_file_name(Compiler::in, globals::in,
        ext::in, module_name::in, string::out, io::di, io::uo) is det
].

:- type analysis_name == string.

:- type analysis_type
    --->    some [FuncInfo, Call, Answer]
            analysis_type(
                unit(Call),
                unit(Answer)
            ) => analysis(FuncInfo, Call, Answer).

%---------------------%

    % An analysis is defined by a type describing call patterns
    % and a type defining answer patterns. If the analysis needs to store
    % more information about the function being analysed (e.g. arity),
    % it should be stored as part of the type for call patterns.
    %
:- typeclass analysis(FuncInfo, Call, Answer)
    <= (call_pattern(FuncInfo, Call),
        answer_pattern(FuncInfo, Answer))
    where
[
    func analysis_name(Call::unused, Answer::unused) =
        (analysis_name::out) is det,

    % The version number should be changed when the Call or Answer types
    % are changed, so that results which use the old types can be discarded.
    %
    func analysis_version_number(Call::unused, Answer::unused) =
        (int::out) is det,

    func preferred_fixpoint_type(Call::unused, Answer::unused) =
        (fixpoint_type::out) is det,

    func bottom(FuncInfo::in, Call::unused) = (Answer::out) is det,
    func top(FuncInfo::in, Call::unused) = (Answer::out) is det,

    pred get_func_info(module_info::in, module_name::in, func_id::in,
        Call::unused, Answer::unused, FuncInfo::out) is det
].

:- type fixpoint_type
    --->    least_fixpoint
            % Start at `bottom'.
            % Must run to completion.

    ;       greatest_fixpoint.
            % Start at `top'.
            % Can stop at any time.

    % This will need to encode language specific details like whether
    % it is a predicate or a function, and the arity and mode number.
:- type func_id
    --->    func_id(
                fid_pf      :: pred_or_func,
                fid_name    :: string,
                fid_arity   :: pred_form_arity,
                fid_mode    :: proc_id
            ).

%---------------------%

:- typeclass call_pattern(FuncInfo, Call)
    <= (partial_order(FuncInfo, Call),
        to_term(Call))
    where [].

:- typeclass answer_pattern(FuncInfo, Answer)
    <= (partial_order(FuncInfo, Answer),
        to_term(Answer))
    where [].

%---------------------%

    % An analysis result is a call pattern paired with an answer.
    % The result has a status associated with it.
    %
:- type some_analysis_result
    --->    some [FuncInfo, Call, Answer]
            some_analysis_result(
                some_ar_call    :: Call,
                some_ar_answer  :: Answer,
                some_ar_status  :: analysis_status
            )
            => analysis(FuncInfo, Call, Answer).

:- type analysis_result(Call, Answer)
    --->    analysis_result(
                ar_call     :: Call,
                ar_answer   :: Answer,
                ar_status   :: analysis_status
            ).

    % The status of a module or a specific analysis result.
    %
    % NOTE: the lub operation below depends on the function symbols
    % being in this exact order.
    %
:- type analysis_status
    --->    invalid
    ;       suboptimal
    ;       optimal.

%---------------------%

:- type analysis_map(T)         == map(module_name, module_analysis_map(T)).
:- type module_analysis_map(T)  == map(analysis_name, func_analysis_map(T)).
:- type func_analysis_map(T)    == map(func_id, list(T)).

%---------------------%

:- typeclass partial_order(FuncInfo, T) <= (T -> FuncInfo) where
[
    pred more_precise_than(FuncInfo::in, T::in, T::in) is semidet,
    pred equivalent(FuncInfo::in, T::in, T::in) is semidet
].

%---------------------%

:- typeclass to_term(S) where [
    func to_term(S) = term,
    pred from_term(term::in, S::out) is semidet
].

%---------------------------------------------------------------------------%

    % Least upper bound of two analysis_status values.
    %
:- func lub(analysis_status, analysis_status) = analysis_status.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

lub(StatusA, StatusB) = Status :-
    compare(Cmp, StatusA, StatusB),
    (
        Cmp = (=),
        Status = StatusA
    ;
        Cmp = (<),
        Status = StatusA
    ;
        Cmp = (>),
        Status = StatusB
    ).

%---------------------------------------------------------------------------%
:- end_module analysis.framework.
%---------------------------------------------------------------------------%
