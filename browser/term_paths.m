%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: term_paths.m.
%
% This module defines operations on paths for the other modules
% in this directory.
%
%---------------------------------------------------------------------------%

:- module mdb.term_paths.
:- interface.

:- import_module mdb.browser_info.
:- import_module mdb.browser_term.
:- import_module mdb.parse.

:- import_module io.
:- import_module list.

:- type deref_result(T)
    --->    deref_result(T)
    ;       deref_error(list(down_dir), down_dir).

    % We assume a root-relative path. We assume Term is the entire term
    % passed into browse/3, not a subterm.
    %
:- pred deref_subterm(browser_term::in, list(down_dir)::in,
    deref_result(browser_term)::out) is cc_multi.

%---------------------------------------------------------------------------%

:- pred report_deref_error(debugger::in, list(down_dir)::in, down_dir::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred set_path(path::in, browser_info::in, browser_info::out) is det.

:- pred change_dir(list(down_dir)::in, path::in, list(down_dir)::out) is det.

%---------------------------------------------------------------------------%

    % True if the given string can be used to cd to the return value of a
    % function.
    %
:- pred string_is_return_value_alias(string::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module deconstruct.
:- import_module int.
:- import_module maybe.
:- import_module string.
:- import_module type_desc.
:- import_module univ.

%---------------------------------------------------------------------------%

deref_subterm(BrowserTerm, Path, Result) :-
    (
        BrowserTerm = plain_term(Univ),
        deref_subterm_2(Univ, Path, [], SubResult),
        deref_result_univ_to_browser_term(SubResult, Result)
    ;
        BrowserTerm = synthetic_term(_Functor, Args, MaybeReturn),
        (
            Path = [],
            SubBrowserTerm = BrowserTerm,
            Result = deref_result(SubBrowserTerm)
        ;
            Path = [Step | PathTail],
            ( if
                (
                    Step = down_child_num(N),
                    ( if
                        N = list.length(Args) + 1,
                        MaybeReturn = yes(ReturnValue)
                    then
                        ArgUniv = ReturnValue
                    else
                        % The first argument of a non-array
                        % is numbered argument 1.
                        list.index1(Args, N, ArgUniv)
                    )
                ;
                    Step = down_child_name(Name),
                    string_is_return_value_alias(Name),
                    MaybeReturn = yes(ArgUniv)
                )
            then
                deref_subterm_2(ArgUniv, PathTail, [Step], SubResult),
                deref_result_univ_to_browser_term(SubResult, Result)
            else
                Result = deref_error([], Step)
            )
        )
    ).

:- pred deref_subterm_2(univ::in, list(down_dir)::in, list(down_dir)::in,
    deref_result(univ)::out) is cc_multi.

deref_subterm_2(Univ, Path, RevPath0, Result) :-
    (
        Path = [],
        Result = deref_result(Univ)
    ;
        Path = [Dir | Dirs],
        (
            Dir = down_child_num(N),
            ( if
                TypeCtor = type_ctor(univ_type(Univ)),
                type_ctor_name(TypeCtor) = "array",
                type_ctor_module_name(TypeCtor) = "array"
            then
                % The first element of an array is at index zero.
                arg_cc(univ_value(Univ), N, MaybeValue)
            else
                % The first argument of a non-array is numbered argument 1
                % by the user but argument 0 by deconstruct.argument.
                arg_cc(univ_value(Univ), N - 1, MaybeValue)
            )
        ;
            Dir = down_child_name(Name),
            named_arg_cc(univ_value(Univ), Name, MaybeValue)
        ),
        (
            MaybeValue = arg(Value),
            ArgN = univ(Value),
            deref_subterm_2(ArgN, Dirs, [Dir | RevPath0], Result)
        ;
            MaybeValue = no_arg,
            Result = deref_error(list.reverse(RevPath0), Dir)
        )
    ).

:- pred deref_result_univ_to_browser_term(deref_result(univ)::in,
    deref_result(browser_term)::out) is det.

deref_result_univ_to_browser_term(SubResult, Result) :-
    (
        SubResult = deref_result(SubUniv),
        SubBrowserTerm = plain_term(SubUniv),
        Result = deref_result(SubBrowserTerm)
    ;
        SubResult = deref_error(OKPath, ErrorDir),
        Result = deref_error(OKPath, ErrorDir)
    ).

report_deref_error(Debugger, OKPath, ErrorDir, !IO) :-
    write_string_debugger(Debugger, "error: ", !IO),
    (
        OKPath = [_ | _],
        Context = "in subdir " ++ down_dirs_to_string(OKPath) ++ ": ",
        write_string_debugger(Debugger, Context, !IO)
    ;
        OKPath = []
    ),
    Msg = "there is no subterm " ++ down_dir_to_string(ErrorDir) ++ "\n",
    write_string_debugger(Debugger, Msg, !IO).

:- func down_dir_to_string(down_dir) = string.

down_dir_to_string(down_child_num(Num)) = int_to_string(Num).
down_dir_to_string(down_child_name(Name)) = Name.

:- func down_dirs_to_string(list(down_dir)) = string.

down_dirs_to_string([]) = "".
down_dirs_to_string([Dir | Dirs]) = DirStr :-
    (
        Dirs = [],
        DirStr = down_dir_to_string(Dir)
    ;
        Dirs = [_ | _],
        DirStr = down_dir_to_string(Dir) ++ "/" ++ down_dirs_to_string(Dirs)
    ).

%---------------------%

set_path(NewPath, !Info) :-
    Dirs0 = !.Info ^ bri_dirs,
    change_dir(Dirs0, NewPath, Dirs),
    !Info ^ bri_dirs := Dirs.

change_dir(PwdDirs, Path, RootRelDirs) :-
    (
        Path = root_rel(Dirs),
        NewDirs = Dirs
    ;
        Path = dot_rel(Dirs),
        NewDirs = down_to_up_down_dirs(PwdDirs) ++ Dirs
    ),
    simplify_dirs(NewDirs, RootRelDirs).

    % Remove "/dir/../" sequences from a list of directories to yield
    % a form that lacks ".." entries.
    % If there are more ".." entries than normal entries, we return
    % the empty list.
    %
:- pred simplify_dirs(list(up_down_dir)::in, list(down_dir)::out) is det.

simplify_dirs(Dirs, SimpleDirs) :-
    list.reverse(Dirs, RevDirs),
    simplify_rev_dirs(RevDirs, 0, [], SimpleDirs).

    % simplify_rev_dirs(RevUpDownDirs, ToDelete, !DownDirs):
    %
    % Assumes a reverse list of directories and removes redundant `..'
    % entries by scanning from the bottom most directory to the top,
    % counting how many `..' occurred (!.ToDelete) and removing entries
    % accordingly. !DownDirs accumulates the simplified dirs processed so far
    % so we can be tail recursive.
    %
:- pred simplify_rev_dirs(list(up_down_dir)::in, int::in,
    list(down_dir)::in, list(down_dir)::out) is det.

simplify_rev_dirs([], _, !DownDirs).
simplify_rev_dirs([RevUpDownDir | RevUpDownDirs], !.ToDelete, !DownDirs) :-
    (
        RevUpDownDir = updown_parent,
        !:ToDelete = !.ToDelete + 1
    ;
        (
            RevUpDownDir = updown_child_num(ChildNum),
            DownDir = down_child_num(ChildNum)
        ;
            RevUpDownDir = updown_child_name(ChildName),
            DownDir = down_child_name(ChildName)
        ),
        ( if !.ToDelete > 0 then
            !:ToDelete = !.ToDelete - 1
        else
            !:DownDirs = [DownDir | !.DownDirs]
        )
    ),
    simplify_rev_dirs(RevUpDownDirs, !.ToDelete, !DownDirs).

%---------------------------------------------------------------------------%

string_is_return_value_alias("r").
string_is_return_value_alias("res").
string_is_return_value_alias("rv").
string_is_return_value_alias("result").
string_is_return_value_alias("return").
string_is_return_value_alias("ret").

%---------------------------------------------------------------------------%
:- end_module mdb.term_paths.
%---------------------------------------------------------------------------%
