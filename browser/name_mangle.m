%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000,2002, 2005-2006 The University of Melbourne.
% Copyright (C) 2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: name_mangle.m.
% Purpose: name mangling support.
% Main author: fjh.
% Stability: low.
%
% This module provides the proc_name_mangle/1 function, which takes a value of
% type `mercury_proc' and returns a string which is the symbol name for
% specified procedure, and which is suitable for use in a call to dl.sym.
%
% The details of name mangling are implementation-dependent, so unfortunately
% the values stored in the `mercury_proc' type might be subject to change
% in different Mercury implementations. Any code which creates or examines
% values of that type should be carefully isolated so that it can be
% easily changed if the representation of `mercury_proc' changes.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.name_mangle.
:- interface.

    % Given a mercury_proc specifying the module name,
    % predicate or function indicator, predicate name, arity,
    % and mode number of a Mercury procedure,
    % return the label name of that procedure.
    % The label name returned is suitable for passing to dl.sym.
    %
:- func proc_name_mangle(mercury_proc) = string.

:- type mercury_proc
    --->    mercury_proc(
                is_pred_or_func,
                module_name,
                pred_name,
                arity,
                mode_num
            ).

:- type is_pred_or_func
    --->    predicate
    ;       function.

:- type module_name == sym_name.

:- type sym_name
    --->    qualified(sym_name, string)
    ;       unqualified(string).

:- type pred_name == string.

    % Note that for functions, that arity here does *not* include
    % the function result, e.g. int:'*' has arity 2, not 3.
:- type arity == int.

    % Mode numbers start from zero.
:- type mode_num == int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

proc_name_mangle(MercuryProc) =
    ( if high_level_code then
        mlds_proc_name_mangle(MercuryProc)
    else
        llds_proc_name_mangle(MercuryProc)
    ).

% NOTE:  most of the code below is very similar to the code in
% compiler/llds_out.m. Any changes there may require changes here and vice
% versa.

%---------------------------------------------------------------------------%

:- func llds_proc_name_mangle(mercury_proc) = string.

llds_proc_name_mangle(MercuryProc) = LabelName :-
    MercuryProc = mercury_proc(PredOrFunc, Module, Name0, Arity, ModeNum),
    sym_name_mangle(Module, ModuleName),
    ( if
        (
            Module = unqualified("builtin")
        ;
            Name0 = "main",
            Arity = 2
        )
        % The conditions above define which labels are printed without
        % module qualification.
    then
        LabelName0 = Name0
    else
        qualify_name(ModuleName, Name0, LabelName0)
    ),
    name_mangle(LabelName0, LabelName1),
    string.int_to_string(Arity, ArityString),
    string.int_to_string(ModeNum, ModeNumString),
    string.append_list([LabelName1, "_", ArityString, "_", ModeNumString],
        LabelName2),
    (
        PredOrFunc = function,
        string.append("fn__", LabelName2, LabelName3)
    ;
        PredOrFunc = predicate,
        LabelName3 = LabelName2
    ),
    string.append("mercury__", LabelName3, LabelName4),
    ( if use_asm_labels then
        % On OS X dlsym will insert the leading underscore for us.
        % XXX this has been the behaviour of dlsym on OS X since at least
        % version 10.6, but according to the man page some older versions
        % didn't do that.
        EntryPrefix = ( if system_is_osx then "entry_" else "_entry_" ),
        string.append(EntryPrefix, LabelName4, LabelName)
    else
        LabelName = LabelName4
    ).

%---------------------------------------------------------------------------%

% NOTE: the following code needs to be kept in sync with the predicates
% mlds_pred_label_to_string and mlds_output_pred_label in compiler/mlds_to_c.m.

:- func mlds_proc_name_mangle(mercury_proc) = string.

mlds_proc_name_mangle(MercuryProc) = LabelName :-
    MercuryProc = mercury_proc(PredOrFunc, Module, Name0, Arity, ModeNum),
    sym_name_mangle(Module, ModuleName),
    ( if
        PredOrFunc = predicate,
        Name0 = "main",
        Arity = 2
        % The conditions above define which labels are printed without
        % module qualification.
    then
        LabelName0 = Name0
    else
        qualify_name(ModuleName, Name0, LabelName0)
    ),
    name_mangle(LabelName0, LabelName1),
    (
        PredOrFunc = predicate,
        PredOrFuncString = "p"
    ;
        PredOrFunc = function,
        PredOrFuncString = "f"
    ),
    string.int_to_string(Arity, ArityString),
    string.int_to_string(ModeNum, ModeNumString),
    string.append_list([LabelName1, "_", ArityString, "_",
        PredOrFuncString, "_", ModeNumString],
        LabelName).

%---------------------------------------------------------------------------%

:- pred sym_name_mangle(sym_name::in, string::out) is det.

sym_name_mangle(unqualified(Name), MangledName) :-
    name_mangle(Name, MangledName).
sym_name_mangle(qualified(ModuleName, PlainName), MangledName) :-
    sym_name_mangle(ModuleName, MangledModuleName),
    name_mangle(PlainName, MangledPlainName),
    qualify_name(MangledModuleName, MangledPlainName, MangledName).

    % Convert a Mercury predicate name into something that can form
    % part of a C identifier. This predicate is necessary because
    % quoted names such as 'name with embedded spaces' are valid
    % predicate names in Mercury.
    %
:- pred name_mangle(string::in, string::out) is det.

name_mangle(Name, MangledName) :-
    ( if string.is_all_alnum_or_underscore(Name) then
        % Any names that start with `f_' are changed so that they start with
        % `f__' instead, so that we can use names starting with `f_'
        % (followed by anything except an underscore) without fear
        % of name collisions.
        ( if string.append("f_", Suffix, Name) then
            string.append("f__", Suffix, MangledName)
        else
            MangledName = Name
        )
    else
        convert_to_valid_c_identifier(Name, MangledName)
    ).

:- pred convert_to_valid_c_identifier(string::in, string::out) is det.

convert_to_valid_c_identifier(String, Name) :-
    ( if name_conversion_table(String, Name0) then
        Name = Name0
    else
        convert_to_valid_c_identifier_2(String, Name0),
        string.append("f", Name0, Name)
    ).

:- pred qualify_name(string::in, string::in, string::out) is det.

qualify_name(Module0, Name0, Name) :-
    string.append_list([Module0, "__", Name0], Name).

    % A table used to convert Mercury functors into C identifiers.
    % Feel free to add any new translations you want.
    % The C identifiers should start with "f_",
    % to avoid introducing name clashes.
    % If the functor name is not found in the table, then
    % we use a fall-back method which produces ugly names.
    %
:- pred name_conversion_table(string::in, string::out) is semidet.

name_conversion_table("\\=", "f_not_equal").
name_conversion_table(">=", "f_greater_or_equal").
name_conversion_table("=<", "f_less_or_equal").
name_conversion_table("=", "f_equal").
name_conversion_table("<", "f_less_than").
name_conversion_table(">", "f_greater_than").
name_conversion_table("-", "f_minus").
name_conversion_table("+", "f_plus").
name_conversion_table("*", "f_times").
name_conversion_table("/", "f_slash").
name_conversion_table(",", "f_comma").
name_conversion_table(";", "f_semicolon").
name_conversion_table("!", "f_cut").

    % This is the fall-back method.
    %
    % Given a string, produce a C identifier for that string by concatenating
    % the decimal expansions of the character codes in the string,
    % separated by underlines. The C identifier will start with "f_";
    % this predicate constructs everything except the initial "f".
    %
    % For example, given the input "\n\t" we return "_10_8".
    %
:- pred convert_to_valid_c_identifier_2(string::in, string::out) is det.

convert_to_valid_c_identifier_2(String, Name) :-
    ( if string.first_char(String, Char, Rest) then
        char.to_int(Char, Code),
        string.int_to_string(Code, CodeString),
        string.append("_", CodeString, ThisCharString),
        convert_to_valid_c_identifier_2(Rest, Name0),
        string.append(ThisCharString, Name0, Name)
    else
        % String is the empty string
        Name = String
    ).

%---------------------------------------------------------------------------%

:- pred use_asm_labels is semidet.

:- pragma foreign_proc("C",
    use_asm_labels,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_USE_ASM_LABELS
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").
use_asm_labels :-
    private_builtin.sorry("use_asm_labels").

:- pred system_is_osx is semidet.

:- pragma foreign_proc("C",
    system_is_osx,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#if defined(MR_MAC_OSX)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

:- pred high_level_code is semidet.

:- pragma foreign_proc("C",
    high_level_code,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_HIGHLEVEL_CODE
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").
high_level_code :-
    private_builtin.sorry("high_level_code").

%---------------------------------------------------------------------------%
