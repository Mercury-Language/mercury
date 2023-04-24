%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2007, 2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: declarative_oracle.m
% Author: Mark Brown
%
% This module implements the oracle for a Mercury declarative debugger.
% It is called by the front end of the declarative debugger to provide
% information about the intended interpretation of the program being debugged.
%
% The module has a knowledge base as a sub-component. This is a cache for
% all the assumptions that the oracle is currently making. When the oracle
% is queried, it first checks the KB to see if an answer is available there.
%
% If no answer is available in the KB, then the oracle uses the UI (in
% browser/declarative_user.m) to get the required answer from the user.
% If any new knowledge is obtained, it is added to the KB so the user
% will not be asked the same question twice.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.declarative_oracle.
:- interface.

:- import_module mdb.browser_info.
:- import_module mdb.declarative_debugger.
:- import_module mdb.declarative_user.
:- import_module mdb.help.
:- import_module mdbcomp.
:- import_module mdbcomp.rtti_access.
:- import_module mdbcomp.sym_name.

:- import_module bool.
:- import_module io.

%---------------------------------------------------------------------------%

    % A response that the oracle gives to a query about the truth
    % of an EDT node.
    %
:- type oracle_response(T)
    --->    oracle_response_answer(decl_answer(T))
    ;       oracle_response_show_info(io.text_output_stream)
    ;       oracle_response_change_search(user_search_mode)
    ;       oracle_response_undo

            % Ask the diagnoser to revert to the
            % last question it asked.

    ;       oracle_response_exit_diagnosis(T)
    ;       oracle_response_abort_diagnosis.

:- pred oracle_response_undoable(oracle_response(T)::in) is semidet.

%---------------------------------------------------------------------------%

    % The oracle state. This is threaded around the declarative debugger.
    %
:- type oracle_state.

    % Produce a new oracle state.
    %
:- pred oracle_state_init(io.text_input_stream::in, io.text_output_stream::in,
    browser_info.browser_persistent_state::in, help_system::in,
    oracle_state::out) is det.

    % Trust all the modules in the Mercury standard library.
    %
:- pred trust_standard_library(oracle_state::in, oracle_state::out) is det.

    % Add a module to the set of modules trusted by the oracle.
    %
:- pred add_trusted_module(module_name::in,
    oracle_state::in, oracle_state::out) is det.

    % Add a predicate/function to the set of predicates/functions trusted
    % by the oracle.
    %
:- pred add_trusted_pred_or_func(proc_layout::in,
    oracle_state::in, oracle_state::out) is det.

    % remove_trusted(Id, !Oracle):
    %
    % Removes the trusted object with the given Id from the set of trusted
    % objects.
    %
:- pred remove_trusted(int::in, oracle_state::in, oracle_state::out)
    is semidet.

    % get_trusted_list(Oracle, MDBCommandFormat, String):
    %
    % Return a string listing the trusted objects.
    %
    % If MDBCommandFormat is true, then return the list in the form of
    % a series of mdb `trust' commands, which can be executed.
    %
    % If MDBCommandFormat is false, then return the list in a format
    % that is suitable for display to the user.
    %
:- pred get_trusted_list(oracle_state::in, bool::in, string::out) is det.

    % query_oracle(Question, Response, AnswerFromUser, !Oracle, !IO):
    %
    % Query the oracle about the program being debugged. Question is a node
    % in the evaluation tree, Response is the oracle's response. The oracle
    % state is threaded through so its contents can be updated after user
    % responses. If the response came directly from the user, then
    % AnswerFromUser will be yes, otherwise it will be no.
    %
:- pred query_oracle(decl_question(T)::in, oracle_response(T)::out, bool::out,
    oracle_state::in, oracle_state::out, io::di, io::uo) is cc_multi.

    % True if the answer to the question is in the knowledge base, or
    % the predicate is trusted.
    %
:- pred answer_known(oracle_state::in, decl_question(T)::in,
    decl_answer(T)::out(known_answer)) is semidet.

    % Confirm that the node found is indeed an e_bug or an i_bug. If the bug
    % is overruled, force the oracle to forget everything it knows about
    % the evidence that led to that bug.
    %
:- pred oracle_confirm_bug(decl_bug::in, decl_evidence(T)::in,
    decl_confirmation::out, oracle_state::in, oracle_state::out,
    io::di, io::uo) is cc_multi.

    % Revise a question in the oracle's knowledge base so that the oracle
    % will get an answer to the question from the user.
    %
:- pred revise_oracle(decl_question(T)::in,
    oracle_state::in, oracle_state::out) is det.

    % update_revised_knowledge_base(Oracle1, Oracle2, Oracle3):
    %
    % Update the revised knowledge base in Oracle1 with the current knowledge
    % base from Oracle2 and return the resulting oracle in Oracle3.
    %
:- pred update_revised_knowledge_base(oracle_state::in, oracle_state::in,
    oracle_state::out) is det.

    % Returns the state of the term browser.
    %
:- func get_oracle_browser_state(oracle_state)
    = browser_info.browser_persistent_state.

    % Return the input stream used for interacting with the user.
    %
:- func get_oracle_user_input_stream(oracle_state) = io.text_input_stream.

    % Return the output stream used for interacting with the user.
    %
:- func get_oracle_user_output_stream(oracle_state) = io.text_output_stream.

    % Sets the state of the term browser.
    %
:- pred set_oracle_browser_state(browser_info.browser_persistent_state::in,
    oracle_state::in, oracle_state::out) is det.

    % Set the testing flag of the user_state in the given oracle.
    %
:- pred set_oracle_testing_flag(are_we_testing::in,
    oracle_state::in, oracle_state::out) is det.

    % Reset the oracle's knowledge base.
    %
:- pred reset_oracle_knowledge_base(oracle_state::in, oracle_state::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.declarative_execution.
:- import_module mdbcomp.prim_data.

:- import_module bimap.
:- import_module counter.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

oracle_response_undoable(oracle_response_answer(_)).
oracle_response_undoable(oracle_response_change_search(_)).

%---------------------------------------------------------------------------%

:- type oracle_state
    --->    oracle(
                % Current information about the intended interpretation.
                % These answers have been given, but have not since been
                % revised.
                kb_current          :: oracle_kb,

                % Old information about the intended interpretation.
                % These answers were given and subsequently revised,
                % but new answers to the questions have not yet been given.
                kb_revised          :: oracle_kb,

                % User interface.
                user_state          :: user_state,

                % The set of trusted "objects" (the Mercury standard library,
                % modules, predicates, and/or functions) trusted by the oracle.
                % We map each to an integer as an id, and vice versa, in order
                % to allow the user to untrust an object by giving its id,
                % instead of having to give e.g. a module name again.
                %
                % This field is only used to implement the user interface,
                % i.e. the command to add, list and delete trusted objects.
                % The actual test whether a procedure is trusted is done
                % using the module_trust_map field below, which is optimized
                % for that task.
                trusted_object_map  :: bimap(trusted_object, int),

                % Counter to allocate ids to trusted objects.
                trusted_id_counter  :: counter,

                % Maps each module to an indication whether
                %
                % - the entirety of the module is trusted, and if so,
                %   whether this trust came about
                %
                %   - because this module is part of the Mercury standard
                %     library, and the user trusted the standard library, or
                %   - because the user trusted this module explicitly, and
                %
                % - which predicates and functions in the module were
                %   trusted individually by the user.
                %
                % This field must be kept in sync with the trusted object map.
                module_trust_map    :: module_trust_map
            ).

:- type trusted_object
    --->    trusted_standard_library

    ;       trusted_module(module_name)
            % Trust all predicates/functions in the named module.

    ;       trusted_predicate(
                module_name,
                string,     % pred name
                int         % arity
            )

    ;       trusted_function(
                module_name,
                string,     % function name
                int         % arity including return value
            ).

:- type module_trust_map == map(module_name, module_trust_info).

:- type is_trusted
    --->    is_not_trusted
    ;       is_trusted.

:- type module_trust_info
    --->    module_trust_info(
                is_trusted_stdlib   :: is_trusted,
                is_trusted_module   :: is_trusted,
                trusted_preds       :: map(string, set(int)),
                trusted_funcs       :: map(string, set(int))
            ).

%---------------------------------------------------------------------------%

oracle_state_init(InStr, OutStr, Browser, HelpSystem, Oracle) :-
    oracle_kb_init(Current),
    oracle_kb_init(Old),
    user_state_init(InStr, OutStr, Browser, HelpSystem, User),
    counter.init(0, Counter0),
    Oracle0 = oracle(Current, Old, User, bimap.init, Counter0, map.init),
    % Trust the standard library by default.
    trust_standard_library(Oracle0, Oracle).

%---------------------------------------------------------------------------%

trust_standard_library(!Oracle) :-
    Counter0 = !.Oracle ^ trusted_id_counter,
    counter.allocate(Id, Counter0, Counter),
    Trusted0 = !.Oracle ^ trusted_object_map,
    ( if bimap.insert(trusted_standard_library, Id, Trusted0, Trusted) then
        !Oracle ^ trusted_object_map := Trusted,
        !Oracle ^ trusted_id_counter := Counter,

        ModuleTrustMap0 = !.Oracle ^ module_trust_map,
        list.foldl(add_to_module_trust_map(trust_module_as_stdlib),
            stdlib_modules, ModuleTrustMap0, ModuleTrustMap),
        !Oracle ^ module_trust_map := ModuleTrustMap
    else
        true
    ).

add_trusted_module(ModuleName, !Oracle) :-
    Counter0 = !.Oracle ^ trusted_id_counter,
    counter.allocate(Id, Counter0, Counter),
    Trusted0 = !.Oracle ^ trusted_object_map,
    ( if bimap.insert(trusted_module(ModuleName), Id, Trusted0, Trusted) then
        !Oracle ^ trusted_object_map := Trusted,
        !Oracle ^ trusted_id_counter := Counter,

        ModuleTrustMap0 = !.Oracle ^ module_trust_map,
        add_to_module_trust_map(trust_module_on_its_own, ModuleName,
            ModuleTrustMap0, ModuleTrustMap),
        !Oracle ^ module_trust_map := ModuleTrustMap
    else
        true
    ).

add_trusted_pred_or_func(ProcLayout, !Oracle) :-
    Counter0 = !.Oracle ^ trusted_id_counter,
    counter.allocate(Id, Counter0, Counter),
    ProcLabel = get_proc_label_from_layout(ProcLayout),
    (
        ProcLabel = ordinary_proc_label(ModuleName, PredOrFunc, _,
            Name, Arity, _)
    ;
        ProcLabel = special_proc_label(ModuleName, _, _, Name, Arity, _),
        PredOrFunc = pf_predicate
    ),
    Trusted0 = !.Oracle ^ trusted_object_map,
    ( if
        (
            PredOrFunc = pf_predicate,
            TrustedObject = trusted_predicate(ModuleName, Name, Arity)
        ;
            PredOrFunc = pf_function,
            TrustedObject = trusted_function(ModuleName, Name, Arity)
        ),
        bimap.insert(TrustedObject, Id, Trusted0, Trusted)
    then
        !Oracle ^ trusted_object_map := Trusted,
        !Oracle ^ trusted_id_counter := Counter,

        ModuleTrustMap0 = !.Oracle ^ module_trust_map,
        ( if map.search(ModuleTrustMap0, ModuleName, ModuleTrustInfo0) then
            ModuleTrustInfo1 = ModuleTrustInfo0
        else
            ModuleTrustInfo1 = module_trust_info(is_not_trusted, is_not_trusted,
                map.init, map.init)
        ),
        (
            PredOrFunc = pf_predicate,
            PredMap1 = ModuleTrustInfo1 ^ trusted_preds,
            add_trusted_arity(Name, Arity, PredMap1, PredMap),
            ModuleTrustInfo = ModuleTrustInfo1 ^ trusted_preds := PredMap
        ;
            PredOrFunc = pf_function,
            FuncMap1 = ModuleTrustInfo1 ^ trusted_funcs,
            add_trusted_arity(Name, Arity, FuncMap1, FuncMap),
            ModuleTrustInfo = ModuleTrustInfo1 ^ trusted_funcs := FuncMap
        ),
        map.set(ModuleName, ModuleTrustInfo,
            ModuleTrustMap0, ModuleTrustMap),
        !Oracle ^ module_trust_map := ModuleTrustMap
    else
        true
    ).

remove_trusted(Id, !Oracle) :-
    Trusted0 = !.Oracle ^ trusted_object_map,
    bimap.search(Trusted0, OldTrustedObject, Id),
    bimap.delete_value(Id, Trusted0, Trusted),
    !Oracle ^ trusted_object_map := Trusted,

    ModuleTrustMap0 = !.Oracle ^ module_trust_map,
    (
        OldTrustedObject = trusted_standard_library,
        list.foldl(remove_from_module_trust_map(trust_module_as_stdlib),
            stdlib_modules, ModuleTrustMap0, ModuleTrustMap)
    ;
        OldTrustedObject = trusted_module(ModuleName),
        remove_from_module_trust_map(trust_module_on_its_own, ModuleName,
            ModuleTrustMap0, ModuleTrustMap)
    ;
        ( OldTrustedObject = trusted_predicate(ModuleName, Name, Arity)
        ; OldTrustedObject = trusted_function(ModuleName, Name, Arity)
        ),
        map.lookup(ModuleTrustMap0, ModuleName, ModuleTrustInfo0),
        ModuleTrustInfo0 = module_trust_info(TrustedStdLib0, TrustedOnItsOwn0,
            PredMap0, FuncMap0),
        (
            OldTrustedObject = trusted_predicate(_, _, _),
            remove_trusted_arity(Name, Arity, PredMap0, PredMap),
            FuncMap = FuncMap0
        ;
            OldTrustedObject = trusted_function(_, _, _),
            remove_trusted_arity(Name, Arity, FuncMap0, FuncMap),
            PredMap = PredMap0
        ),
        ModuleTrustInfo = module_trust_info(TrustedStdLib0, TrustedOnItsOwn0,
            PredMap, FuncMap),
        ( if
            TrustedStdLib0 = is_not_trusted,
            TrustedOnItsOwn0 = is_not_trusted,
            map.is_empty(PredMap),
            map.is_empty(FuncMap)
        then
            map.delete(ModuleName, ModuleTrustMap0, ModuleTrustMap)
        else
            map.det_update(ModuleName, ModuleTrustInfo,
                ModuleTrustMap0, ModuleTrustMap)
        )
    ),
    !Oracle ^ module_trust_map := ModuleTrustMap.

%---------------------------------------------------------------------------%

:- type trust_module_as
    --->    trust_module_as_stdlib
    ;       trust_module_on_its_own.

:- pred add_to_module_trust_map(trust_module_as::in, module_name::in,
    module_trust_map::in, module_trust_map::out) is det.

add_to_module_trust_map(TrustModuleAs, ModuleName, !ModuleTrustMap) :-
    ( if map.search(!.ModuleTrustMap, ModuleName, ModuleTrustInfo0) then
        (
            TrustModuleAs = trust_module_as_stdlib,
            ModuleTrustInfo = ModuleTrustInfo0 ^ is_trusted_stdlib :=
                is_trusted
        ;
            TrustModuleAs = trust_module_on_its_own,
            ModuleTrustInfo = ModuleTrustInfo0 ^ is_trusted_module :=
                is_trusted
        ),
        map.det_update(ModuleName, ModuleTrustInfo, !ModuleTrustMap)
    else
        (
            TrustModuleAs = trust_module_as_stdlib,
            TrustStdLib = is_trusted,
            TrustOnItsOwn = is_not_trusted
        ;
            TrustModuleAs = trust_module_on_its_own,
            TrustStdLib = is_not_trusted,
            TrustOnItsOwn = is_trusted
        ),
        ModuleTrustInfo = module_trust_info(TrustStdLib, TrustOnItsOwn,
            map.init, map.init),
        map.det_insert(ModuleName, ModuleTrustInfo, !ModuleTrustMap)
    ).

:- pred remove_from_module_trust_map(trust_module_as::in, module_name::in,
    module_trust_map::in, module_trust_map::out) is det.

remove_from_module_trust_map(TrustModuleAs, ModuleName, !ModuleTrustMap) :-
    map.lookup(!.ModuleTrustMap, ModuleName, ModuleTrustInfo0),
    (
        TrustModuleAs = trust_module_as_stdlib,
        ModuleTrustInfo = ModuleTrustInfo0 ^ is_trusted_stdlib :=
            is_not_trusted
    ;
        TrustModuleAs = trust_module_on_its_own,
        ModuleTrustInfo = ModuleTrustInfo0 ^ is_trusted_module :=
            is_not_trusted
    ),
    ModuleTrustInfo = module_trust_info(TrustedStdLib, TrustedOnItsOwn,
        PredMap, FuncMap),
    ( if
        TrustedStdLib = is_not_trusted,
        TrustedOnItsOwn = is_not_trusted,
        map.is_empty(PredMap),
        map.is_empty(FuncMap)
    then
        map.delete(ModuleName, !ModuleTrustMap)
    else
        map.det_update(ModuleName, ModuleTrustInfo, !ModuleTrustMap)
    ).

:- pred add_trusted_arity(string::in, int::in,
    map(string, set(int))::in, map(string, set(int))::out) is det.

add_trusted_arity(Name, Arity, !TrustedArityMap) :-
    ( if map.search(!.TrustedArityMap, Name, TrustedArities0) then
        set.insert(Arity, TrustedArities0, TrustedArities),
        map.det_update(Name, TrustedArities, !TrustedArityMap)
    else
        set.singleton_set(Arity, TrustedArities),
        map.det_insert(Name, TrustedArities, !TrustedArityMap)
    ).

:- pred remove_trusted_arity(string::in, int::in,
    map(string, set(int))::in, map(string, set(int))::out) is det.

remove_trusted_arity(Name, Arity, !TrustedArityMap) :-
    map.lookup(!.TrustedArityMap, Name, TrustedArities0),
    set.det_remove(Arity, TrustedArities0, TrustedArities),
    ( if set.is_empty(TrustedArities) then
        map.delete(Name, !TrustedArityMap)
    else
        map.det_update(Name, TrustedArities, !TrustedArityMap)
    ).

:- func stdlib_modules = list(module_name).

stdlib_modules = StdLibModuleNames :-
    GetStdLibModuleNames =
        ( pred(ModuleName::out) is multi :-
            stdlib_module_doc_undoc(ModuleNameStr, _),
            ModuleName = string_to_sym_name(ModuleNameStr)
        ),
    solutions.solutions(GetStdLibModuleNames, StdLibModuleNames).

%---------------------------------------------------------------------------%

get_trusted_list(Oracle, MDBCommandFormat, Str) :-
    IdToObjectMap = bimap.reverse_map(Oracle ^ trusted_object_map),
    IdObjectPairs = map.to_assoc_list(IdToObjectMap),
    (
        MDBCommandFormat = yes,
        TrustCmdStrs = list.map(format_trust_command, IdObjectPairs),
        Str = string.append_list(TrustCmdStrs)
    ;
        MDBCommandFormat = no,
        DisplayStrs = list.map(format_trust_display, IdObjectPairs),
        (
            DisplayStrs = [],
            Str = "There are no trusted modules, predicates or functions.\n"
        ;
            DisplayStrs = [_ | _],
            Str = string.append_list(["Trusted objects:\n" | DisplayStrs])
        )
    ).

:- func format_trust_command(pair(int, trusted_object)) = string.

format_trust_command(_Id - TrustedObject) = CmdStr :-
    (
        TrustedObject = trusted_standard_library,
        CmdStr = "trust std lib\n"
    ;
        TrustedObject = trusted_module(ModuleName),
        ModuleNameStr = sym_name_to_string(ModuleName),
        string.format("trust %s\n", [s(ModuleNameStr)], CmdStr)
    ;
        TrustedObject = trusted_predicate(ModuleName, Name, Arity),
        ModuleNameStr = sym_name_to_string(ModuleName),
        string.format("trust pred*%s.%s/%d\n",
            [s(ModuleNameStr), s(Name), i(Arity)], CmdStr)
    ;
        TrustedObject = trusted_function(ModuleName, Name, Arity),
        ModuleNameStr = sym_name_to_string(ModuleName),
        string.format("trust func*%s.%s/%d\n",
            [s(ModuleNameStr), s(Name), i(Arity - 1)], CmdStr)
    ).

:- func format_trust_display(pair(int, trusted_object)) = string.

format_trust_display(Id - TrustedObject) = Display :-
    (
        TrustedObject = trusted_standard_library,
        CmdDisplay = "the Mercury standard library"
    ;
        TrustedObject = trusted_module(ModuleName),
        ModuleNameStr = sym_name_to_string(ModuleName),
        CmdDisplay = "module " ++ ModuleNameStr
    ;
        TrustedObject = trusted_predicate(ModuleName, Name, Arity),
        ModuleNameStr = sym_name_to_string(ModuleName),
        CmdDisplay = "predicate " ++ ModuleNameStr ++ "."
            ++ Name ++ "/" ++ int_to_string(Arity)
    ;
        TrustedObject = trusted_function(ModuleName, Name, Arity),
        ModuleNameStr = sym_name_to_string(ModuleName),
        CmdDisplay = "function " ++ ModuleNameStr ++ "."
            ++ Name ++ "/" ++ int_to_string(Arity - 1)
    ),
    string.format("%d: %s\n", [i(Id), s(CmdDisplay)], Display).

%---------------------------------------------------------------------------%

query_oracle(Question, Response, FromUser, !Oracle, !IO) :-
    ( if answer_known(!.Oracle, Question, Answer) then
        Response = oracle_response_answer(Answer),
        FromUser = no
    else
        make_user_question(!.Oracle ^ kb_revised, Question, UserQuestion),
        query_oracle_user(UserQuestion, Response, !Oracle, !IO),
        FromUser = yes
    ).

:- pred make_user_question(oracle_kb::in, decl_question(T)::in,
    user_question(T)::out) is det.

make_user_question(Revised, DeclQuestion, UserQuestion) :-
    ( if
        query_oracle_kb(Revised, DeclQuestion, Answer),
        Answer = truth_value(_, DeclTruth)
    then
        UserQuestion = question_with_default(DeclQuestion, DeclTruth)
    else
        UserQuestion = plain_question(DeclQuestion)
    ).

:- pred query_oracle_user(user_question(T)::in, oracle_response(T)::out,
    oracle_state::in, oracle_state::out, io::di, io::uo) is cc_multi.

query_oracle_user(UserQuestion, OracleResponse, !Oracle, !IO) :-
    User0 = !.Oracle ^ user_state,
    query_user(UserQuestion, UserResponse, User0, User, !IO),
    (
        UserResponse = user_response_answer(Question, Answer),
        OracleResponse = oracle_response_answer(Answer),
        Current0 = !.Oracle ^ kb_current,
        Revised0 = !.Oracle ^ kb_revised,
        retract_oracle_kb(Question, Revised0, Revised),
        assert_oracle_kb(Question, Answer, Current0, Current),
        !Oracle ^ kb_current := Current,
        !Oracle ^ kb_revised := Revised
    ;
        UserResponse = user_response_trust_predicate(Question),
        Atom = get_decl_question_atom(Question),
        add_trusted_pred_or_func(Atom ^ proc_layout, !Oracle),
        OracleResponse = oracle_response_answer(
            ignore(get_decl_question_node(Question)))
    ;
        UserResponse = user_response_trust_module(Question),
        Atom = get_decl_question_atom(Question),
        ProcLabel = get_proc_label_from_layout(Atom ^ proc_layout),
        get_pred_attributes(ProcLabel, Module, _, _, _),
        add_trusted_module(Module, !Oracle),
        OracleResponse = oracle_response_answer(
            ignore(get_decl_question_node(Question)))
    ;
        UserResponse = user_response_show_info(OutStream),
        OracleResponse = oracle_response_show_info(OutStream)
    ;
        UserResponse = user_response_change_search(Mode),
        OracleResponse = oracle_response_change_search(Mode)
    ;
        UserResponse = user_response_exit_diagnosis(Node),
        OracleResponse = oracle_response_exit_diagnosis(Node)
    ;
        UserResponse = user_response_abort_diagnosis,
        OracleResponse = oracle_response_abort_diagnosis
    ;
        UserResponse = user_response_undo,
        OracleResponse = oracle_response_undo
    ),
    !Oracle ^ user_state := User.

answer_known(Oracle, Question, Answer) :-
    Atom = get_decl_question_atom(Question),
    ( if proc_is_trusted(Oracle, Atom ^ proc_layout) then
        % We tell the analyser that this node does not contain a bug,
        % however its children may still contain bugs, since trusted procs
        % may call untrusted procs. One example is when an untrusted closure
        % is passed to a trusted predicate.
        Answer = ignore(get_decl_question_node(Question))
    else
        query_oracle_kb(Oracle ^ kb_current, Question, Answer)
    ).

:- pred proc_is_trusted(oracle_state::in, proc_layout::in) is semidet.

proc_is_trusted(Oracle, ProcLayout) :-
    ModuleTrustMap = Oracle ^ module_trust_map,
    ProcLabel = get_proc_label_from_layout(ProcLayout),
    (
        ProcLabel = ordinary_proc_label(Module, PredOrFunc, _, Name, Arity, _),
        % If the search fails, the procedure is not trusted.
        map.search(ModuleTrustMap, Module, ModuleTrustInfo),
        ModuleTrustInfo = module_trust_info(TrustedAsStdLib, TrustedAsModule,
            PredMap, FuncMap),
        (
            TrustedAsStdLib = is_trusted
        ;
            TrustedAsModule = is_trusted
        ;
            (
                PredOrFunc = pf_predicate,
                map.search(PredMap, Name, TrustedArities)
            ;
                PredOrFunc = pf_function,
                map.search(FuncMap, Name, TrustedArities)
            ),
            set.contains(TrustedArities, Arity)
        )
    ;
        ProcLabel = special_proc_label(_, _, _, _, _, _)
    ).

oracle_confirm_bug(Bug, Evidence, Confirmation, Oracle0, Oracle, !IO) :-
    User0 = Oracle0 ^ user_state,
    user_confirm_bug(Bug, Confirmation, User0, User, !IO),
    Oracle1 = Oracle0 ^ user_state := User,
    (
        Confirmation = overrule_bug,
        list.foldl(revise_oracle, Evidence, Oracle1, Oracle)
    ;
        ( Confirmation = confirm_bug
        ; Confirmation = abort_diagnosis
        ),
        Oracle = Oracle1
    ).

revise_oracle(Question, !Oracle) :-
    Current0 = !.Oracle ^ kb_current,
    ( if query_oracle_kb(Current0, Question, Answer) then
        retract_oracle_kb(Question, Current0, Current),
        Revised0 = !.Oracle ^ kb_revised,
        assert_oracle_kb(Question, Answer, Revised0, Revised),
        !Oracle ^ kb_revised := Revised,
        !Oracle ^ kb_current := Current
    else
        true
    ).

update_revised_knowledge_base(Oracle1, Oracle2, Oracle3) :-
    Oracle3 = Oracle1 ^ kb_revised := Oracle2 ^ kb_current.

%---------------------------------------------------------------------------%
%
% This section implements the oracle knowledge base, which stores anything
% that the debugger knows about the intended interpretation. This can be used
% to check the correctness of an EDT node.
%

    % The type of the knowledge base. Other fields may be added in
    % the future, such as for assertions made on-the-fly by the user,
    % or assertions in the program text.
    %
:- type oracle_kb
    ---> oracle_kb(
            % For ground atoms, the knowledge is represented directly with
            % a map. This is used, for example, in the common case that
            % the user supplies a truth value for a "wrong answer" node.
            kb_ground_map :: map(final_decl_atom, decl_truth),

            % This map stores knowledge about the completeness of the set
            % of solutions generated by calling the given initial atom.
            % This is used, for example, in the common case that the user
            % supplies a truth value for a "missing answer" node.
            kb_complete_map :: map(init_decl_atom, decl_truth),

            % Mapping from call atoms to information about which
            % exceptions are possible or impossible.
            kb_exceptions_map :: map(init_decl_atom, known_exceptions)
        ).

:- type known_exceptions
    --->    known_excp(
                % Possible exceptions.
                possible        :: set(decl_exception),

                % Impossible exceptions.
                impossible      :: set(decl_exception),

                % Exceptions from inadmissible calls.
                inadmissible    :: set(decl_exception)
            ).

:- pred oracle_kb_init(oracle_kb::out) is det.

oracle_kb_init(oracle_kb(G, C, X)) :-
    map.init(G),
    map.init(C),
    map.init(X).

:- pred get_kb_ground_map(oracle_kb::in,
    map(final_decl_atom, decl_truth)::out) is det.
:- pred get_kb_complete_map(oracle_kb::in,
    map(init_decl_atom, decl_truth)::out) is det.
:- pred get_kb_exceptions_map(oracle_kb::in,
    map(init_decl_atom, known_exceptions)::out) is det.

get_kb_ground_map(KB, X) :-
    X = KB ^ kb_ground_map.
get_kb_complete_map(KB, X) :-
    X = KB ^ kb_complete_map.
get_kb_exceptions_map(KB, X) :-
    X = KB ^ kb_exceptions_map.

:- pred set_kb_ground_map(map(final_decl_atom, decl_truth)::in,
    oracle_kb::in, oracle_kb::out) is det.
:- pred set_kb_complete_map(map(init_decl_atom, decl_truth)::in,
    oracle_kb::in, oracle_kb::out) is det.
:- pred set_kb_exceptions_map(map(init_decl_atom, known_exceptions)::in,
    oracle_kb::in, oracle_kb::out) is det.

set_kb_ground_map(M, !KB) :-
    !KB ^ kb_ground_map := M.
set_kb_complete_map(M, !KB) :-
    !KB ^ kb_complete_map := M.
set_kb_exceptions_map(M, !KB) :-
    !KB ^ kb_exceptions_map := M.

%---------------------------------------------------------------------------%

:- pred query_oracle_kb(oracle_kb::in, decl_question(T)::in,
    decl_answer(T)::out(known_answer)) is semidet.

query_oracle_kb(KB, Question, Answer) :-
    (
        Question = wrong_answer(Node, _, Atom),
        get_kb_ground_map(KB, Map),
        map.search(Map, Atom, Truth),
        Answer = truth_value(Node, Truth)
    ;
        Question = missing_answer(Node, Call, _Solns),
        get_kb_complete_map(KB, CMap),
        map.search(CMap, Call, Truth),
        Answer = truth_value(Node, Truth)
    ;
        Question = unexpected_exception(Node, Call, Exception),
        get_kb_exceptions_map(KB, XMap),
        map.search(XMap, Call, X),
        X = known_excp(Possible, Impossible, Inadmissible),
        ( if set.member(Exception, Possible) then
            Answer = truth_value(Node, truth_correct)
        else if set.member(Exception, Impossible) then
            Answer = truth_value(Node, truth_erroneous)
        else
            set.member(Exception, Inadmissible),
            Answer = truth_value(Node, truth_inadmissible)
        )
    ).

    % assert_oracle_kb/3 assumes that the asserted fact is consistent
    % with the current knowledge base. This will generally be the case,
    % since the user will never be asked questions which the knowledge base
    % knows anything about.
    %
:- pred assert_oracle_kb(decl_question(T)::in, decl_answer(T)::in,
    oracle_kb::in, oracle_kb::out) is det.

assert_oracle_kb(Question, Answer, !KB) :-
    (
        ( Answer = suspicious_subterm(_, _, _, _, _)
        ; Answer = ignore(_)
        ; Answer = skip(_)
        )
    ;
        Answer = truth_value(_, Truth),
        (
            Question = wrong_answer(_, _, Atom),
            get_kb_ground_map(!.KB, Map0),
            ProcLayout = Atom ^ final_atom ^ proc_layout,

            % Insert all modes for the atom if the atom is correct,
            % and just the one mode if it is not correct. In general,
            % we cannot insert all modes for erroneous or inadmissible atoms,
            % since the atom might be erroneous with respect to one mode,
            % but inadmissible with respect to another mode.

            (
                Truth = truth_correct,
                foldl(add_atom_to_ground_map(Truth, Atom),
                    get_all_modes_for_layout(ProcLayout), Map0, Map)
            ;
                ( Truth = truth_erroneous
                ; Truth = truth_inadmissible
                ),
                add_atom_to_ground_map(Truth, Atom, ProcLayout, Map0, Map)
            ),
            set_kb_ground_map(Map, !KB)
        ;
            Question = missing_answer(_, Call, _),
            get_kb_complete_map(!.KB, Map0),
            map.set(Call, Truth, Map0, Map),
            set_kb_complete_map(Map, !KB)
        ;
            Question = unexpected_exception(_, Call, Exception),
            get_kb_exceptions_map(!.KB, Map0),
            ( if map.search(Map0, Call, OldKnownExceptions) then
                KnownExceptions0 = OldKnownExceptions
            else
                set.init(EmptyPossible0),
                set.init(EmptyImpossible0),
                set.init(EmptyInadmissible0),
                KnownExceptions0 = known_excp(EmptyPossible0, EmptyImpossible0,
                    EmptyInadmissible0)
            ),
            (
                Truth = truth_correct,
                Possible0 = KnownExceptions0 ^ possible,
                set.insert(Exception, Possible0, Possible),
                KnownExceptions = KnownExceptions0 ^ possible := Possible
            ;
                Truth = truth_erroneous,
                Impossible0 = KnownExceptions0 ^ impossible,
                set.insert(Exception, Impossible0, Impossible),
                KnownExceptions = KnownExceptions0 ^ impossible := Impossible
            ;
                Truth = truth_inadmissible,
                Inadmissible0 = KnownExceptions0 ^ inadmissible,
                set.insert(Exception, Inadmissible0, Inadmissible),
                KnownExceptions =
                    KnownExceptions0 ^ inadmissible := Inadmissible
            ),
            map.set(Call, KnownExceptions, Map0, Map),
            set_kb_exceptions_map(Map, !KB)
        )
    ).

:- pred retract_oracle_kb(decl_question(T)::in, oracle_kb::in, oracle_kb::out)
    is det.

retract_oracle_kb(Question, !KB) :-
    (
        Question = wrong_answer(_, _, Atom),
        GroundMap0 = !.KB ^ kb_ground_map,
        % Delete all modes of the predicate/function.
        list.foldl(remove_atom_from_ground_map(Atom),
            get_all_modes_for_layout(Atom ^ final_atom ^ proc_layout),
            GroundMap0, GroundMap),
        !KB ^ kb_ground_map := GroundMap
    ;
        Question = missing_answer(_, InitAtom, _),
        CompleteMap0 = !.KB ^ kb_complete_map,
        map.delete(InitAtom, CompleteMap0, CompleteMap),
        !KB ^ kb_complete_map := CompleteMap
    ;
        Question = unexpected_exception(_, InitAtom, Exception),
        ExceptionsMap0 = !.KB ^ kb_exceptions_map,
        ( if
            map.search(ExceptionsMap0, InitAtom, KnownExceptions0),
            KnownExceptions0 =
                known_excp(Possible0, Impossible0, Inadmissible0)
        then
            set.delete(Exception, Possible0, Possible),
            set.delete(Exception, Impossible0, Impossible),
            set.delete(Exception, Inadmissible0, Inadmissible),
            KnownExceptions = known_excp(Possible, Impossible, Inadmissible),
            map.set(InitAtom, KnownExceptions, ExceptionsMap0, ExceptionsMap)
        else
            ExceptionsMap = ExceptionsMap0
        ),
        !KB ^ kb_exceptions_map := ExceptionsMap
    ).

:- pred add_atom_to_ground_map(decl_truth::in, final_decl_atom::in,
    proc_layout::in,
    map(final_decl_atom, decl_truth)::in,
    map(final_decl_atom, decl_truth)::out) is det.

add_atom_to_ground_map(Truth, FinalAtom, ProcLayout, !Map) :-
    map.set(final_decl_atom(
        atom(ProcLayout, FinalAtom ^ final_atom ^ atom_args),
        FinalAtom ^ final_io_actions), Truth, !Map).

:- pred remove_atom_from_ground_map(final_decl_atom::in, proc_layout::in,
    map(final_decl_atom, decl_truth)::in,
    map(final_decl_atom, decl_truth)::out) is det.

remove_atom_from_ground_map(FinalAtom, ProcLayout, !Map) :-
    FinalDeclAtom = final_decl_atom(
        atom(ProcLayout, FinalAtom ^ final_atom ^ atom_args),
        FinalAtom ^ final_io_actions),
    map.delete(FinalDeclAtom, !Map).

%---------------------------------------------------------------------------%

get_oracle_browser_state(Oracle) =
    mdb.declarative_user.get_user_browser_state(Oracle ^ user_state).

get_oracle_user_input_stream(Oracle) =
    declarative_user.get_user_input_stream(Oracle ^ user_state).

get_oracle_user_output_stream(Oracle) =
    declarative_user.get_user_output_stream(Oracle ^ user_state).

set_oracle_browser_state(Browser, !Oracle) :-
    User0 = !.Oracle ^ user_state,
    declarative_user.set_user_browser_state(Browser, User0, User),
    !Oracle ^ user_state := User.

set_oracle_testing_flag(Testing, !Oracle) :-
    User0 = !.Oracle ^ user_state,
    set_user_testing_flag(Testing, User0, User),
    !Oracle ^ user_state := User.

%---------------------------------------------------------------------------%

reset_oracle_knowledge_base(!Oracle) :-
    oracle_kb_init(EmptyKB),
    !Oracle ^ kb_revised := !.Oracle ^ kb_current,
    !Oracle ^ kb_current := EmptyKB.

%---------------------------------------------------------------------------%
:- end_module mdb.declarative_oracle.
%---------------------------------------------------------------------------%
