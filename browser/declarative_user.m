%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: declarative_user.m.
% Author: Mark Brown.

% Purpose:
%   This module performs all the user interaction of the front
% end of the declarative debugger.  It is responsible for displaying
% questions and bugs in a human-readable format, and for getting
% responses to debugger queries from the user.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mdb.declarative_user.
:- interface.

:- import_module mdb.browser_info.
:- import_module mdb.declarative_debugger.
:- import_module mdb.help.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type user_question(T)
    --->    plain_question(decl_question(T))
    ;       question_with_default(decl_question(T), decl_truth).

:- type user_response(T)
    --->    user_answer(decl_question(T), decl_answer(T))
    ;       trust_predicate(decl_question(T))
    ;       trust_module(decl_question(T))

    ;       show_info(io.output_stream)
            % Request that the analyser display some information
            % about the state of the search and the current
            % question to the given output stream.

    ;       change_search(user_search_mode)
            % Request that a new search strategy be used.

    ;       undo
            % The user wants to undo the last answer they gave.

    ;       exit_diagnosis(T)
    ;       abort_diagnosis.

:- type user_search_mode
    --->    top_down
    ;       divide_and_query
    ;       suspicion_divide_and_query
    ;       binary.

:- type user_state.

:- pred user_state_init(io.input_stream::in, io.output_stream::in,
    browser_info.browser_persistent_state::in, help.system::in,
    user_state::out) is det.

    % This predicate handles the interactive part of the declarative
    % debugging process.  The user is presented with a question,
    % possibly with a default answer, and is asked to respond about the
    % truth of it in the intended interpretation.
    %
:- pred query_user(user_question(T)::in, user_response(T)::out,
    user_state::in, user_state::out, io::di, io::uo) is cc_multi.

    % Confirm that the node found is indeed an e_bug or an i_bug.
    %
:- pred user_confirm_bug(decl_bug::in, decl_confirmation::out,
    user_state::in, user_state::out, io::di, io::uo) is cc_multi.

    % Returns the state of the term browser.
    %
:- func get_browser_state(user_state) = browser_info.browser_persistent_state.

    % Sets the state of the term browser.
    %
:- pred set_browser_state(browser_info.browser_persistent_state::in,
    user_state::in, user_state::out) is det.

    % Return the output stream used for interacting with the user.
    %
:- func get_user_output_stream(user_state) = io.output_stream.

    % Set the testing flag of the user_state.
    %
:- pred set_user_testing_flag(bool::in, user_state::in, user_state::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.browse.
:- import_module mdb.browser_term.
:- import_module mdb.declarative_execution.
:- import_module mdb.declarative_tree.
:- import_module mdb.io_action.
:- import_module mdb.parse.
:- import_module mdb.term_rep.
:- import_module mdb.util.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.rtti_access.

:- import_module char.
:- import_module deconstruct.
:- import_module exception.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type user_command
    --->    yes
            % The node is correct.

    ;       no
            % The node is erroneous.

    ;       inadmissible
            % The node is inadmissible.

    ;       skip
            % The user has no answer.

    ;       browse_arg(maybe(int))
            % Browse the nth argument before answering.  Or browse
            % the whole predicate/function if the maybe is no.

    ;       browse_xml_arg(maybe(int))
            % Browse the argument using an XML browser.

    ;       browse_io(int)
            % Browse the nth IO action before answering.

    ;       print_arg(int, int)
            % Print the nth to the mth arguments before answering.

    ;       print_io(int, int)
            % Print the nth to the mth IO actions before answering.

    ;       pd
            % Commence procedural debugging from this point.

    ;       param_command(param_cmd)

    ;       trust_predicate
            % Trust the predicate being asked about.

    ;       trust_module
            % Trust the module being asked about.

    ;       info
            % Print some information about the current question.

    ;       undo
            % Undo the user's last answer.

    ;       ask
            % The user wants the current question re-asked.

    ;       change_search(user_search_mode)
            % Change the current search strategy.

    ;       quit
            % Abort this diagnosis session.

    ;       help(maybe(string))
            % Request help before answering.  If the maybe argument
            % is no then a general help message is displayed,
            % otherwise help on the given command is displayed.

    ;       empty_command
            % User just pressed return.

    ;       illegal_command.
            % None of the above.

:- type user_state
    --->    user(
                instr               :: io.input_stream,
                outstr              :: io.output_stream,
                browser             :: browser_persistent_state,

                % Yes if the question should be displayed when querying
                % the user. This is used to supress the displaying of the
                % question after the user issues a command which does not
                % answer the question (such as an `info' command).
                display_question    :: bool,

                help_system         :: help.system,

                % If this following flag is set to yes then user responses
                % will be simulated and will always be `no', except when
                % confirming a bug in which case the response will be `yes'.
                testing             :: bool
            ).

user_state_init(InStr, OutStr, Browser, HelpSystem,
    user(InStr, OutStr, Browser, yes, HelpSystem, no)).

%-----------------------------------------------------------------------------%

query_user(UserQuestion, Response, !User, !IO) :-
    Question = get_decl_question(UserQuestion),
    (
        !.User ^ testing = yes,
        Node = get_decl_question_node(Question),
        Response = user_answer(Question, truth_value(Node, erroneous))
    ;
        !.User ^ testing = no,
        (
            !.User ^ display_question = yes,
            write_decl_question(Question, !.User, !IO),
            user_question_prompt(UserQuestion, Prompt),
            !:User = !.User ^ display_question := no
        ;
            !.User ^ display_question = no,
            Prompt = "dd> "
        ),
        get_command(Prompt, Command, !User, !IO),
        handle_command(Command, UserQuestion, Response, !User, !IO),
        ( Response \= show_info(_) ->
            !:User = !.User ^ display_question := yes
        ;
            true
        )
    ).

:- pred handle_command(user_command::in, user_question(T)::in,
    user_response(T)::out, user_state::in, user_state::out,
    io::di, io::uo) is cc_multi.

handle_command(yes, UserQuestion, Response, !User, !IO) :-
    Question = get_decl_question(UserQuestion),
    Node = get_decl_question_node(Question),
    Response = user_answer(Question, truth_value(Node, correct)).

handle_command(no, UserQuestion, Response, !User, !IO) :-
    Question = get_decl_question(UserQuestion),
    Node = get_decl_question_node(Question),
    Response = user_answer(Question, truth_value(Node, erroneous)).

handle_command(inadmissible, UserQuestion, Response, !User, !IO) :-
    Question = get_decl_question(UserQuestion),
    Node = get_decl_question_node(Question),
    Response = user_answer(Question, truth_value(Node, inadmissible)).

handle_command(skip, UserQuestion, Response, !User, !IO) :-
    Question = get_decl_question(UserQuestion),
    Node = get_decl_question_node(Question),
    Response = user_answer(Question, skip(Node)).

handle_command(browse_arg(MaybeArgNum), UserQuestion, Response, !User, !IO) :-
    Question = get_decl_question(UserQuestion),
    edt_node_trace_atoms(Question, InitAtom, FinalAtom),
    (
        MaybeArgNum = yes(ArgNum),
        browse_atom_argument(InitAtom, FinalAtom, ArgNum, MaybeTrack,
            !User, !IO),
        (
            MaybeTrack = no_track,
            query_user(UserQuestion, Response, !User, !IO)
        ;
            MaybeTrack = track(HowTrack, ShouldAssertInvalid, TermPath),
            ArgPos = arg_num_to_arg_pos(ArgNum),
            Node = get_decl_question_node(Question),
            Answer = suspicious_subterm(Node, ArgPos, TermPath, HowTrack,
                ShouldAssertInvalid),
            Response = user_answer(Question, Answer)
        )
    ;
        MaybeArgNum = no,
        browse_atom(InitAtom, FinalAtom, MaybeTrack, !User, !IO),
        (
            MaybeTrack = no_track,
            query_user(UserQuestion, Response, !User, !IO)
        ;
            MaybeTrack = track(HowTrack, ShouldAssertInvalid,
                [ArgNum | TermPath]),
            ArgPos = arg_num_to_arg_pos(ArgNum),
            Node = get_decl_question_node(Question),
            Answer = suspicious_subterm(Node, ArgPos, TermPath,
                HowTrack, ShouldAssertInvalid),
            Response = user_answer(Question, Answer)
        ;
            %
            % Tracking the entire atom doesn't make sense.
            %
            MaybeTrack = track(_, _, []),
            io.write_string(!.User ^ outstr,
                "Cannot track the entire atom. " ++
                "Please select a subterm to track.\n", !IO),
            query_user(UserQuestion, Response, !User, !IO)
        )
    ).

handle_command(browse_xml_arg(MaybeArgNum), UserQuestion, Response,
        !User, !IO) :-
    Question = get_decl_question(UserQuestion),
    edt_node_trace_atoms(Question, _, FinalAtom),
    (
        MaybeArgNum = yes(ArgNum),
        browse_xml_atom_argument(FinalAtom, ArgNum, !.User, !IO)
    ;
        MaybeArgNum = no,
        browse_xml_atom(FinalAtom, !.User, !IO)
    ),
    query_user(UserQuestion, Response, !User, !IO).

handle_command(print_arg(From, To), UserQuestion, Response,
        !User, !IO) :-
    Question = get_decl_question(UserQuestion),
    edt_node_trace_atoms(Question, _, TraceAtom),
    print_atom_arguments(TraceAtom, From, To, !.User, !IO),
    query_user(UserQuestion, Response, !User, !IO).

handle_command(param_command(ParamCommand), UserQuestion, Response,
        !User, !IO) :-
    Browser0 = !.User ^ browser,
    DummyTerm = synthetic_term("", [], no),
    Info0 = browser_info(DummyTerm, [], browse, no, Browser0, no_track, no),
    run_param_command(internal, ParamCommand, no, Info0, Info, !IO),
    Info = browser_info(_, _, _, _, Browser, _, _),
    !:User = !.User ^ browser := Browser,
    query_user(UserQuestion, Response, !User, !IO).

handle_command(trust_predicate, UserQuestion, trust_predicate(Question),
        !User, !IO) :-
    Question = get_decl_question(UserQuestion).

handle_command(trust_module, UserQuestion, trust_module(Question),
        !User, !IO) :-
    Question = get_decl_question(UserQuestion).

handle_command(info, _, show_info(!.User ^ outstr), !User, !IO).

handle_command(undo, _, undo, !User, !IO).

handle_command(browse_io(ActionNum), UserQuestion, Response,
        !User, !IO) :-
    Question = get_decl_question(UserQuestion),
    edt_node_io_actions(Question, MaybeIoActions),
    % We don't have code yet to trace a marked I/O action.
    browse_chosen_io_action(MaybeIoActions, ActionNum, _MaybeTrack,
        !User, !IO),
    query_user(UserQuestion, Response, !User, !IO).

handle_command(print_io(From, To), UserQuestion, Response, !User, !IO) :-
    Question = get_decl_question(UserQuestion),
    edt_node_io_actions(Question, MaybeIoActions),
    print_chosen_io_actions(MaybeIoActions, From, To, !.User, !IO),
    query_user(UserQuestion, Response, !User, !IO).

handle_command(change_search(Mode), _, change_search(Mode), !User, !IO).

handle_command(ask, UserQuestion, Response, !User, !IO) :-
    !:User = !.User ^ display_question := yes,
    query_user(UserQuestion, Response, !User, !IO).

handle_command(pd, UserQuestion, Response, !User, !IO) :-
    Question = get_decl_question(UserQuestion),
    Node = get_decl_question_node(Question),
    Response = exit_diagnosis(Node).

handle_command(quit, _, Response, !User, !IO) :-
    Response = abort_diagnosis.

handle_command(help(MaybeCmd), UserQuestion, Response, !User, !IO) :-
    (
        MaybeCmd = yes(Cmd),
        Path = ["decl", Cmd]
    ;
        MaybeCmd = no,
        Path = ["concepts", "decl_debug"]
    ),
    help.path(!.User ^ help_system, Path, !.User ^ outstr, Res, !IO),
    (
        Res = help_ok
    ;
        Res = help_error(Message),
        io.write_strings([Message, "\n"], !IO)
    ),
    query_user(UserQuestion, Response, !User, !IO).

handle_command(empty_command, UserQuestion, Response, !User, !IO) :-
    (
        UserQuestion = plain_question(_),
        Command = skip
    ;
        UserQuestion = question_with_default(_, Truth),
        (
            Truth = correct,
            Command = yes
        ;
            Truth = erroneous,
            Command = no
        ;
            Truth = inadmissible,
            Command = inadmissible
        )
    ),
    handle_command(Command, UserQuestion, Response, !User, !IO).

handle_command(illegal_command, UserQuestion, Response, !User, !IO) :-
    io.write_string(!.User ^ outstr, "Unknown command, 'h' for help.\n", !IO),
    query_user(UserQuestion, Response, !User, !IO).

:- func arg_num_to_arg_pos(int) = arg_pos.

arg_num_to_arg_pos(ArgNum) = ArgPos :-
    Which = chosen_head_vars_presentation,
    (
        Which = only_user_headvars,
        ArgPos = user_head_var(ArgNum)
    ;
        Which = all_headvars,
        ArgPos = any_head_var(ArgNum)
    ).

:- func get_decl_question(user_question(T)) = decl_question(T).

get_decl_question(plain_question(Q)) = Q.
get_decl_question(question_with_default(Q, _)) = Q.

:- pred user_question_prompt(user_question(T)::in, string::out) is det.

user_question_prompt(plain_question(Question), Prompt) :-
    decl_question_prompt(Question, Prompt).

user_question_prompt(question_with_default(Question, DefaultTruth), Prompt) :-
    decl_question_prompt(Question, QuestionPrompt),
    default_prompt(DefaultTruth, DefaultPrompt),
    string.append(QuestionPrompt, DefaultPrompt, Prompt).

:- pred decl_question_prompt(decl_question(T)::in, string::out) is det.

decl_question_prompt(wrong_answer(_, _, _), "Valid? ").
decl_question_prompt(missing_answer(_, _, [_ | _]), "Complete? ").
decl_question_prompt(missing_answer(_, _, []), "Unsatisfiable? ").
decl_question_prompt(unexpected_exception(_, _, _), "Expected? ").

:- pred default_prompt(decl_truth::in, string::out) is det.

default_prompt(correct, "[yes] ").
default_prompt(erroneous, "[no] ").
default_prompt(inadmissible, "[inadmissible] ").

    % Find the initial and final atoms for a question.  For all
    % questions besides wrong answer questions the initial and
    % final atoms will be the same.
    %
:- pred edt_node_trace_atoms(decl_question(T)::in, trace_atom::out,
    trace_atom::out) is det.

edt_node_trace_atoms(wrong_answer(_, InitDeclAtom, FinalDeclAtom),
    InitDeclAtom ^ init_atom, FinalDeclAtom ^ final_atom).
edt_node_trace_atoms(missing_answer(_, InitDeclAtom, _),
    InitDeclAtom ^ init_atom, InitDeclAtom ^ init_atom).
edt_node_trace_atoms(unexpected_exception(_, InitDeclAtom, _),
    InitDeclAtom ^ init_atom, InitDeclAtom ^ init_atom).

:- pred edt_node_io_actions(decl_question(T)::in, maybe(io_action_range)::out)
    is det.

edt_node_io_actions(wrong_answer(_, _, FinalDeclAtom),
    FinalDeclAtom ^ final_io_actions).
edt_node_io_actions(missing_answer(_, _, _), no).
edt_node_io_actions(unexpected_exception(_, _, _), no).

:- pred decl_bug_trace_atom(decl_bug::in, trace_atom::out, trace_atom::out)
    is det.

decl_bug_trace_atom(e_bug(incorrect_contour(InitDeclAtom, FinalDeclAtom, _,
    _)), InitDeclAtom ^ init_atom, FinalDeclAtom ^ final_atom).
decl_bug_trace_atom(e_bug(partially_uncovered_atom(InitDeclAtom, _)),
    InitDeclAtom ^ init_atom, InitDeclAtom ^ init_atom).
decl_bug_trace_atom(e_bug(unhandled_exception(InitDeclAtom, _, _)),
    InitDeclAtom ^ init_atom, InitDeclAtom ^ init_atom).
decl_bug_trace_atom(i_bug(inadmissible_call(_, _, InitDeclAtom, _)),
    InitDeclAtom ^ init_atom, InitDeclAtom ^ init_atom).

:- pred decl_bug_io_actions(decl_bug::in, maybe(io_action_range)::out) is det.

decl_bug_io_actions(e_bug(incorrect_contour(_, FinalDeclAtom, _, _)),
    FinalDeclAtom ^ final_io_actions).
decl_bug_io_actions(e_bug(partially_uncovered_atom(_, _)), no).
decl_bug_io_actions(e_bug(unhandled_exception(_, _, _)), no).
decl_bug_io_actions(i_bug(inadmissible_call(_, _, _, _)), no).

:- pred browse_chosen_io_action(maybe(io_action_range)::in, int::in,
    maybe_track_subterm(term_path)::out, user_state::in, user_state::out,
    io::di, io::uo) is cc_multi.

browse_chosen_io_action(MaybeIoActions, ActionNum, MaybeTrack, !User, !IO) :-
    (
        MaybeIoActions = yes(IoActions),
        find_tabled_io_action(IoActions, ActionNum, MaybeIoAction, !IO),
        (
            MaybeIoAction = yes(IoAction),
            browse_io_action(IoAction, MaybeTrack, !User, !IO)
        ;
            MaybeIoAction = no,
            MaybeTrack = no_track
        )
    ;
        MaybeIoActions = no,
        io.write_string("No such IO action.\n", !IO),
        MaybeTrack = no_track
    ).

:- pred find_tabled_io_action(io_action_range::in, int::in,
    maybe(io_action)::out, io::di, io::uo) is det.

find_tabled_io_action(io_action_range(Cur, End), TabledActionNum,
        MaybeIoAction, !IO) :-
    ( Cur = End ->
        MaybeIoAction = no
    ;
        get_maybe_io_action(Cur, MaybeTabledIoAction, !IO),
        (
            MaybeTabledIoAction = tabled(IoAction),
            ( TabledActionNum = 1 ->
                MaybeIoAction = yes(IoAction)
            ;
                find_tabled_io_action(io_action_range(Cur + 1, End),
                    TabledActionNum - 1, MaybeIoAction, !IO)
            )
        ;
            MaybeTabledIoAction = untabled,
            find_tabled_io_action(io_action_range(Cur + 1, End),
                TabledActionNum, MaybeIoAction, !IO)
        )
    ).

:- pred print_chosen_io_actions(maybe(io_action_range)::in, int::in, int::in,
    user_state::in, io::di, io::uo) is cc_multi.

print_chosen_io_actions(MaybeIoActions, From, To, User0, !IO) :-
    print_chosen_io_action(MaybeIoActions, From, User0, OK, !IO),
    (
        OK = yes,
        From + 1 =< To
    ->
        print_chosen_io_actions(MaybeIoActions, From + 1, To, User0, !IO)
    ;
        true
    ).

:- pred print_chosen_io_action(maybe(io_action_range)::in, int::in,
    user_state::in, bool::out, io::di, io::uo) is cc_multi.

print_chosen_io_action(MaybeIoActions, ActionNum, User0, OK, !IO) :-
    (
        MaybeIoActions = yes(IoActions),
        find_tabled_io_action(IoActions, ActionNum, MaybeIoAction, !IO),
        (
            MaybeIoAction = yes(IoAction),
            print_tabled_io_action(User0, tabled(IoAction), !IO),
            OK = yes
        ;
            MaybeIoAction = no,
            io.write_string("No such IO action.\n", !IO),
            OK = no
        )
    ;
        MaybeIoActions = no,
        io.write_string("No such IO action.\n", !IO),
        OK = no
    ).

:- pred browse_io_action(io_action::in, maybe_track_subterm(term_path)::out,
    user_state::in, user_state::out, io::di, io::uo) is cc_multi.

browse_io_action(IoAction, no_track, !User, !IO) :-
    Term = io_action_to_browser_term(IoAction),
    browse_browser_term(Term, !.User ^ instr, !.User ^ outstr, no,
        MaybeTrackDirs, !.User ^ browser, Browser, !IO),
    (
        MaybeTrackDirs = track(_, _, _),
        io.write_string(!.User ^ outstr,
            "Sorry, tracking of I/O actions is not yet supported.\n", !IO),
        browse_io_action(IoAction, _, !User, !IO)
    ;
        MaybeTrackDirs = no_track
    ),
    !:User = !.User ^ browser := Browser.

:- pred browse_decl_bug(decl_bug::in, maybe(int)::in, user_state::in,
    user_state::out, io::di, io::uo) is cc_multi.

browse_decl_bug(Bug, MaybeArgNum, !User, !IO) :-
    decl_bug_trace_atom(Bug, InitAtom, FinalAtom),
    (
        MaybeArgNum = yes(ArgNum),
        browse_atom_argument(InitAtom, FinalAtom, ArgNum, _, !User, !IO)
    ;
        MaybeArgNum = no,
        browse_atom(InitAtom, FinalAtom, _, !User, !IO)
    ).

:- pred browse_xml_decl_bug(decl_bug::in, maybe(int)::in, user_state::in,
    io::di, io::uo) is cc_multi.

browse_xml_decl_bug(Bug, MaybeArgNum, User, !IO) :-
    decl_bug_trace_atom(Bug, _, FinalAtom),
    (
        MaybeArgNum = yes(ArgNum),
        browse_xml_atom_argument(FinalAtom, ArgNum, User, !IO)
    ;
        MaybeArgNum = no,
        browse_xml_atom(FinalAtom, User, !IO)
    ).

:- pred browse_atom_argument(trace_atom::in, trace_atom::in, int::in,
    maybe_track_subterm(term_path)::out, user_state::in, user_state::out,
    io::di, io::uo) is cc_multi.

browse_atom_argument(InitAtom, FinalAtom, ArgNum, MaybeTrack, !User, !IO) :-
    FinalAtom = atom(_, Args0),
    maybe_filter_headvars(chosen_head_vars_presentation, Args0, Args),
    (
        list.index1(Args, ArgNum, ArgInfo),
        ArgInfo = arg_info(_, _, MaybeArg),
        MaybeArg = yes(ArgRep),
        term_rep.rep_to_univ(ArgRep, Arg)
    ->
        browse_browser_term(univ_to_browser_term(Arg),
            !.User ^ instr, !.User ^ outstr,
            yes(get_subterm_mode_from_atoms_for_arg(ArgNum,
                InitAtom, FinalAtom)),
            MaybeTrackDirs, !.User ^ browser, Browser, !IO),
        convert_maybe_track_dirs_to_term_path_from_arg(ArgRep,
            MaybeTrackDirs, MaybeTrack),
        !:User = !.User ^ browser := Browser
    ;
        io.write_string(!.User ^ outstr, "Invalid argument number\n", !IO),
        MaybeTrack = no_track
    ).

:- pred browse_xml_atom_argument(trace_atom::in, int::in, user_state::in,
    io::di, io::uo) is cc_multi.

browse_xml_atom_argument(Atom, ArgNum, User, !IO) :-
    Atom = atom(_, Args0),
    maybe_filter_headvars(chosen_head_vars_presentation, Args0, Args),
    (
        list.index1(Args, ArgNum, ArgInfo),
        ArgInfo = arg_info(_, _, MaybeArg),
        MaybeArg = yes(ArgRep),
        term_rep.rep_to_univ(ArgRep, Arg)
    ->
        save_and_browse_browser_term_xml(univ_to_browser_term(Arg),
            User ^ outstr, User ^ outstr, User ^ browser, !IO)
    ;
        io.write_string(User ^ outstr, "Invalid argument number\n", !IO)
    ).

:- pred browse_atom(trace_atom::in, trace_atom::in,
    maybe_track_subterm(term_path)::out,
    user_state::in, user_state::out, io::di, io::uo) is cc_multi.

browse_atom(InitAtom, FinalAtom, MaybeTrack, !User, !IO) :-
    FinalAtom = atom(ProcLayout, Args),
    ProcLabel = get_proc_label_from_layout(ProcLayout),
    get_user_arg_values(Args, ArgValues),
    get_pred_attributes(ProcLabel, Module, Name, _, PredOrFunc),
    IsFunction = pred_to_bool(unify(PredOrFunc, function)),
    sym_name_to_string(Module, ".", ModuleStr),
    BrowserTerm = synthetic_term_to_browser_term(ModuleStr ++ "." ++ Name,
        ArgValues, IsFunction),
    browse_browser_term(BrowserTerm, !.User ^ instr, !.User ^ outstr,
        yes(get_subterm_mode_from_atoms(InitAtom, FinalAtom)),
        MaybeTrackDirs, !.User ^ browser, Browser, !IO),
    convert_maybe_track_dirs_to_term_path_from_atom(FinalAtom,
        MaybeTrackDirs, MaybeTrack),
    !:User = !.User ^ browser := Browser.

:- pred browse_xml_atom(trace_atom::in, user_state::in, io::di, io::uo)
    is cc_multi.

browse_xml_atom(Atom, User, !IO) :-
    Atom = atom(ProcLayout, Args),
    ProcLabel = get_proc_label_from_layout(ProcLayout),
    get_user_arg_values(Args, ArgValues),
    get_pred_attributes(ProcLabel, Module, Name, _, PredOrFunc),
    IsFunction = pred_to_bool(unify(PredOrFunc, function)),
    sym_name_to_string(Module, ".", ModuleStr),
    BrowserTerm = synthetic_term_to_browser_term(ModuleStr ++ "." ++ Name,
        ArgValues, IsFunction),
    save_and_browse_browser_term_xml(BrowserTerm, User ^ outstr,
        User ^ outstr, User ^ browser, !IO).

:- func get_subterm_mode_from_atoms(trace_atom::in, trace_atom::in,
    list(dir)::in(simplified_dirs)) = (browser_term_mode::out) is det.

get_subterm_mode_from_atoms(InitAtom, FinalAtom, Dirs) = Mode :-
    convert_dirs_to_term_path_from_atom(FinalAtom, Dirs, Path),
    (
        Path = [ArgNum | TermPath],
        ArgPos = arg_num_to_arg_pos(ArgNum),
        Mode = get_subterm_mode_from_atoms_and_term_path(InitAtom,
            FinalAtom, ArgPos, TermPath)
    ;
        Path = [],
        Mode = not_applicable
    ).

:- func get_subterm_mode_from_atoms_and_term_path(trace_atom, trace_atom,
    arg_pos, term_path) = browser_term_mode.

get_subterm_mode_from_atoms_and_term_path(InitAtom, FinalAtom, ArgPos,
        TermPath) = Mode :-
    ( trace_atom_subterm_is_ground(InitAtom, ArgPos, TermPath) ->
        Mode = input
    ; trace_atom_subterm_is_ground(FinalAtom, ArgPos, TermPath) ->
        Mode = output
    ;
        Mode = unbound
    ).

:- func get_subterm_mode_from_atoms_for_arg(int::in, trace_atom::in,
    trace_atom::in, list(dir)::in(simplified_dirs)) =
    (browser_term_mode::out) is det.

get_subterm_mode_from_atoms_for_arg(ArgNum, InitAtom, FinalAtom, Dirs)
        = Mode :-
    convert_dirs_to_term_path_from_atom(FinalAtom, Dirs, TermPath),
    ArgPos = arg_num_to_arg_pos(ArgNum),
    Mode = get_subterm_mode_from_atoms_and_term_path(InitAtom, FinalAtom,
        ArgPos, TermPath).

:- pred get_user_arg_values(list(trace_atom_arg)::in, list(univ)::out) is det.

get_user_arg_values([], []).
get_user_arg_values([arg_info(UserVisible, _, MaybeValue) | Args], Values) :-
    get_user_arg_values(Args, Values0),
    (
        UserVisible = yes,
        (
            MaybeValue = yes(ValueRep),
            term_rep.rep_to_univ(ValueRep, Value)
        ;
            MaybeValue = no,
            Value = univ('_'`with_type`unbound)
        ),
        Values = [Value | Values0]
    ;
        UserVisible = no,
        Values = Values0
    ).

:- pred print_atom_arguments(trace_atom::in, int::in, int::in, user_state::in,
    io::di, io::uo) is cc_multi.

print_atom_arguments(Atom, From, To, User, !IO) :-
    print_atom_argument(Atom, From, User, OK, !IO),
    (
        OK = yes,
        From + 1 =< To
    ->
        print_atom_arguments(Atom, From + 1, To, User, !IO)
    ;
        true
    ).

:- pred print_atom_argument(trace_atom::in, int::in, user_state::in, bool::out,
    io::di, io::uo) is cc_multi.

print_atom_argument(Atom, ArgNum, User, OK, !IO) :-
    Atom = atom(_, Args0),
    maybe_filter_headvars(chosen_head_vars_presentation, Args0, Args),
    (
        list.index1(Args, ArgNum, ArgInfo),
        ArgInfo = arg_info(_, _, MaybeArg),
        MaybeArg = yes(ArgRep),
        term_rep.rep_to_univ(ArgRep, Arg)
    ->
        print_browser_term(univ_to_browser_term(Arg), User ^ outstr,
            decl_caller_type, User ^ browser, !IO),
        OK = yes
    ;
        io.write_string(User ^ outstr, "Invalid argument number\n", !IO),
        OK = no
    ).

:- pred convert_maybe_track_dirs_to_term_path_from_atom(
    trace_atom::in,
    maybe_track_subterm(list(dir))::in,
    maybe_track_subterm(term_path)::out) is det.

convert_maybe_track_dirs_to_term_path_from_atom(_, no_track, no_track).
convert_maybe_track_dirs_to_term_path_from_atom(Atom,
        track(HowTrack, ShouldAssertInvalid, Dirs),
        track(HowTrack, ShouldAssertInvalid, TermPath)) :-
    simplify_dirs(Dirs, SimplifiedDirs),
    convert_dirs_to_term_path_from_atom(Atom, SimplifiedDirs, TermPath).

:- pred convert_maybe_track_dirs_to_term_path_from_arg(
    term_rep::in,
    maybe_track_subterm(list(dir))::in,
    maybe_track_subterm(term_path)::out) is det.

convert_maybe_track_dirs_to_term_path_from_arg(_, no_track, no_track).
convert_maybe_track_dirs_to_term_path_from_arg(Term,
        track(HowTrack, ShouldAssertInvalid, Dirs),
        track(HowTrack, ShouldAssertInvalid, TermPath)) :-
    simplify_dirs(Dirs, SimplifiedDirs),
    convert_dirs_to_term_path(Term, SimplifiedDirs, TermPath).

    % Reverse the first argument and append the second to it.
    %
:- pred reverse_and_append(list(T)::in, list(T)::in, list(T)::out) is det.

reverse_and_append([], Bs, Bs).
reverse_and_append([A | As], Bs, Cs) :-
    reverse_and_append(As, [A | Bs], Cs).

%-----------------------------------------------------------------------------%

:- pred user_confirm_bug_help(user_state::in, io::di, io::uo) is det.

user_confirm_bug_help(User, !IO) :-
    io.write_strings(User ^ outstr, [
        "Answer one of:\n",
        "\ty\tyes\t\tconfirm that the suspect is a bug\n",
        "\tn\tno\t\tdo not accept that the suspect is a bug\n",
        "\tb\tbrowse\t\tbrowse the suspect\n",
        "\tq\tquit\t\t",
            "abort this diagnosis session and return to mdb\n",
        "\th, ?\thelp\t\tthis help message\n"
    ], !IO).

:- pred get_command(string::in, user_command::out,
    user_state::in, user_state::out, io::di, io::uo) is det.

get_command(Prompt, Command, User, User, !IO) :-
    util.trace_getline(Prompt, Result, User ^ instr, User ^ outstr, !IO),
    (
        Result = ok(String),
        Words = string.words(char.is_whitespace, String),
        (
            Words = [CmdWord | CmdArgs],
            (
                cmd_handler(CmdWord, CmdHandler),
                CommandPrime = CmdHandler(CmdArgs)
            ->
                Command = CommandPrime
            ;
                Command = illegal_command
            )
        ;
            Words = [],
            Command = empty_command
        )
    ;
        Result = error(Error),
        io.error_message(Error, Msg),
        io.write_string(User ^ outstr, Msg, !IO),
        io.nl(User ^ outstr, !IO),
        Command = quit
    ;
        Result = eof,
        Command = quit
    ).

:- pred cmd_handler(string::in,
    (func(list(string)) = user_command)::out(func(in) = out is semidet))
    is semidet.

cmd_handler("y",        one_word_cmd(yes)).
cmd_handler("yes",      one_word_cmd(yes)).
cmd_handler("n",        one_word_cmd(no)).
cmd_handler("no",       one_word_cmd(no)).
cmd_handler("i",        one_word_cmd(inadmissible)).
cmd_handler("inadmissible",     one_word_cmd(inadmissible)).
cmd_handler("s",        one_word_cmd(skip)).
cmd_handler("skip",     one_word_cmd(skip)).
cmd_handler("pd",       one_word_cmd(pd)).
% `abort' is a synonym for `quit' and is just here for backwards compatibility.
cmd_handler("a",        one_word_cmd(quit)).
cmd_handler("abort",        one_word_cmd(quit)).
cmd_handler("q",        one_word_cmd(quit)).
cmd_handler("quit",     one_word_cmd(quit)).
cmd_handler("?",        help_cmd).
cmd_handler("h",        help_cmd).
cmd_handler("help",     help_cmd).
cmd_handler("info",     one_word_cmd(info)).
cmd_handler("b",        browse_arg_cmd).
cmd_handler("browse",       browse_arg_cmd).
cmd_handler("p",        print_arg_cmd).
cmd_handler("print",        print_arg_cmd).
cmd_handler("format",   format_arg_cmd).
cmd_handler("depth",    format_param_arg_cmd("depth")).
cmd_handler("size",     format_param_arg_cmd("size")).
cmd_handler("width",    format_param_arg_cmd("width")).
cmd_handler("lines",    format_param_arg_cmd("lines")).
cmd_handler("num_io_actions",  num_io_actions_cmd).
% cmd_handler("xml_browser_cmd", set_xml_browser_cmd_cmd).
% cmd_handler("xml_tmp_filename", set_xml_tmp_filename_cmd).
cmd_handler("t",        trust_arg_cmd).
cmd_handler("trust",        trust_arg_cmd).
cmd_handler("mode",     search_mode_cmd).
cmd_handler("m",        search_mode_cmd).
cmd_handler("undo",     one_word_cmd(undo)).

:- func one_word_cmd(user_command::in, list(string)::in) = (user_command::out)
    is semidet.

one_word_cmd(Cmd, []) = Cmd.

:- func browse_arg_cmd(list(string)::in) = (user_command::out) is semidet.

browse_arg_cmd([]) = browse_arg(no).
browse_arg_cmd([Arg]) = BrowseCmd :-
    ( string.to_int(Arg, ArgNum) ->
        BrowseCmd = browse_arg(yes(ArgNum))
    ;
        ( Arg = "-x" ; Arg = "--xml" ),
        BrowseCmd = browse_xml_arg(no)
    ).
browse_arg_cmd(["-x", Arg]) = browse_xml_arg(yes(ArgNum)) :-
    string.to_int(Arg, ArgNum).
browse_arg_cmd(["--xml", Arg]) = browse_xml_arg(yes(ArgNum)) :-
    string.to_int(Arg, ArgNum).
browse_arg_cmd(["io", Arg]) = browse_io(ArgNum) :-
    string.to_int(Arg, ArgNum).

:- func print_arg_cmd(list(string)::in) = (user_command::out) is semidet.

print_arg_cmd([]) = ask.
print_arg_cmd([Arg]) = print_arg(From, To) :-
    string_to_range(Arg, From, To).
print_arg_cmd(["io", Arg]) = print_io(From, To) :-
    string_to_range(Arg, From, To).

:- func format_arg_cmd(list(string)::in) = (user_command::out) is semidet.

format_arg_cmd(ArgWords) = param_command(format(MaybeOptionTable, Setting)) :-
    ArgWords \= [],
    parse.parse(["format" | ArgWords],
        param_command(format(MaybeOptionTable, Setting))).

:- func format_param_arg_cmd(string::in, list(string)::in)
    = (user_command::out) is semidet.

format_param_arg_cmd(Cmd, ArgWords)
        = param_command(format_param(MaybeOptionTable, Setting)) :-
    ArgWords \= [],
    parse.parse([Cmd | ArgWords],
        param_command(format_param(MaybeOptionTable, Setting))).

:- func num_io_actions_cmd(list(string)::in) = (user_command::out) is semidet.

num_io_actions_cmd([Arg]) = param_command(num_io_actions(N)) :-
    string.to_int(Arg, N).

% :- func set_xml_browser_cmd_cmd(list(string)::in) = (user_command::out)
%     is semidet.
%
% set_xml_browser_cmd_cmd([Arg]) = param_command(xml_browser_cmd(Arg)).
%
% :- func set_xml_tmp_filename_cmd(list(string)::in) = (user_command::out)
%     is semidet.
%
% set_xml_tmp_filename_cmd([Arg]) = param_command(xml_tmp_filename(Arg)).

:- func trust_arg_cmd(list(string)::in) = (user_command::out) is semidet.

trust_arg_cmd([]) = trust_predicate.
trust_arg_cmd(["module"]) = trust_module.

:- func search_mode_cmd(list(string)::in) = (user_command::out) is semidet.

search_mode_cmd(["top-down"])       = change_search(top_down).
search_mode_cmd(["top_down"])       = change_search(top_down).
search_mode_cmd(["td"])         = change_search(top_down).
search_mode_cmd(["divide-and-query"])   = change_search(divide_and_query).
search_mode_cmd(["divide_and_query"])   = change_search(divide_and_query).
search_mode_cmd(["dq"])         = change_search(divide_and_query).
search_mode_cmd(["binary"])     = change_search(binary).
search_mode_cmd(["b"])          = change_search(binary).
search_mode_cmd(["suspicion-divide-and-query"]) =
    change_search(suspicion_divide_and_query).
search_mode_cmd(["suspicion_divide_and_query"]) =
    change_search(suspicion_divide_and_query).
search_mode_cmd(["sdq"]) =
    change_search(suspicion_divide_and_query).

:- func help_cmd(list(string)::in) = (user_command::out) is semidet.

help_cmd([]) = help(no).
help_cmd([Cmd]) = help(yes(Cmd)).

:- pred string_to_range(string::in, int::out, int::out) is semidet.

string_to_range(Arg, From, To) :-
    ( string.to_int(Arg, Num) ->
        From = Num,
        To = Num
    ;
        [FirstStr, SecondStr] = string.words(is_dash, Arg),
        string.to_int(FirstStr, First),
        string.to_int(SecondStr, Second),
        ( First =< Second ->
            From = First,
            To = Second
        ;
            From = Second,
            To = First
        )
    ).

:- pred is_dash(char::in) is semidet.

is_dash('-').

%-----------------------------------------------------------------------------%

user_confirm_bug(Bug, Response, !User, !IO) :-
    (
        !.User ^ testing = yes,
        Response = confirm_bug
    ;
        !.User ^ testing = no,
        write_decl_bug(Bug, !.User, !IO),
        get_command("Is this a bug? ", Command, !User, !IO),
        ( Command = yes ->
            Response = confirm_bug
        ; Command = no ->
            Response = overrule_bug
        ; Command = quit ->
            Response = abort_diagnosis
        ; Command = browse_arg(MaybeArgNum) ->
            browse_decl_bug(Bug, MaybeArgNum, !User, !IO),
            user_confirm_bug(Bug, Response, !User, !IO)
        ; Command = browse_xml_arg(MaybeArgNum) ->
            browse_xml_decl_bug(Bug, MaybeArgNum, !.User, !IO),
            user_confirm_bug(Bug, Response, !User, !IO)
        ; Command = browse_io(ActionNum) ->
            decl_bug_io_actions(Bug, MaybeIoActions),
            browse_chosen_io_action(MaybeIoActions, ActionNum, _MaybeTrack,
                !User, !IO),
            user_confirm_bug(Bug, Response, !User, !IO)
        ;
            user_confirm_bug_help(!.User, !IO),
            user_confirm_bug(Bug, Response, !User, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

    % Returns the caller type we want to use throughout the
    % declarative debugger.
    %
:- func decl_caller_type = browse_caller_type.

decl_caller_type = print.

    % Display the node in user readable form on the current output stream.
    %
:- pred write_decl_question(decl_question(T)::in, user_state::in,
    io::di, io::uo) is cc_multi.

write_decl_question(wrong_answer(_, _, Atom), User, !IO) :-
    write_decl_final_atom(User, "", decl_caller_type, Atom, !IO).

write_decl_question(missing_answer(_, Call, Solns), User, !IO) :-
    write_decl_init_atom(User, "Call ", decl_caller_type, Call, !IO),
    (
        Solns = []
    ;
        Solns = [_ | _],
        io.write_string(User ^ outstr, "Solutions:\n", !IO),
        list.foldl(write_decl_final_atom(User, "\t", print_all), Solns, !IO)
    ).

write_decl_question(unexpected_exception(_, Call, ExceptionRep), User, !IO) :-
    write_decl_init_atom(User, "Call ", decl_caller_type, Call, !IO),
    io.write_string(User ^ outstr, "Throws ", !IO),
    term_rep.rep_to_univ(ExceptionRep, Exception),
    io.write(User ^ outstr, include_details_cc, univ_value(Exception), !IO),
    io.nl(User ^ outstr, !IO).

:- pred write_decl_bug(decl_bug::in, user_state::in, io::di, io::uo)
    is cc_multi.

write_decl_bug(e_bug(EBug), User, !IO) :-
    (
        EBug = incorrect_contour(_, Atom, Contour, _),
        io.write_string(User ^ outstr, "Found incorrect contour:\n", !IO),
        io.write_list(Contour, "",
            write_decl_final_atom(User, "", decl_caller_type), !IO),
        write_decl_final_atom(User, "", decl_caller_type, Atom, !IO)
    ;
        EBug = partially_uncovered_atom(Atom, _),
        io.write_string(User ^ outstr,
            "Found partially uncovered atom:\n", !IO),
        write_decl_init_atom(User, "", decl_caller_type, Atom, !IO)
    ;
        EBug = unhandled_exception(Atom, ExceptionRep, _),
        io.write_string(User ^ outstr,
            "Found unhandled or incorrect exception:\n", !IO),
        write_decl_init_atom(User, "", decl_caller_type, Atom, !IO),
        term_rep.rep_to_univ(ExceptionRep, Exception),
        io.write(User ^ outstr, include_details_cc,
            univ_value(Exception), !IO),
        io.nl(User ^ outstr, !IO)
    ).

write_decl_bug(i_bug(IBug), User, !IO) :-
    IBug = inadmissible_call(Parent, _, Call, _),
    io.write_string(User ^ outstr, "Found inadmissible call:\n", !IO),
    write_decl_atom(User, "Parent ", decl_caller_type, init(Parent), !IO),
    write_decl_atom(User, "Call ", decl_caller_type, init(Call), !IO).

:- pred write_decl_init_atom(user_state::in, string::in,
    browse_caller_type::in, init_decl_atom::in,
    io::di, io::uo) is cc_multi.

write_decl_init_atom(User, Indent, CallerType, InitAtom, !IO) :-
    write_decl_atom(User, Indent, CallerType, init(InitAtom), !IO).

:- pred write_decl_final_atom(user_state::in, string::in,
    browse_caller_type::in, final_decl_atom::in,
    io::di, io::uo) is cc_multi.

write_decl_final_atom(User, Indent, CallerType, FinalAtom, !IO) :-
    write_decl_atom(User, Indent, CallerType, final(FinalAtom), !IO).

:- pred write_decl_atom(user_state::in, string::in, browse_caller_type::in,
    some_decl_atom::in, io::di, io::uo) is cc_multi.

write_decl_atom(User, Indent, CallerType, DeclAtom, !IO) :-
    io.write_string(User ^ outstr, Indent, !IO),
    unravel_decl_atom(DeclAtom, TraceAtom, MaybeIoActions),
    TraceAtom = atom(ProcLayout, Args0),
    ProcLabel = get_proc_label_from_layout(ProcLayout),
    get_pred_attributes(ProcLabel, _, Functor, _, PredOrFunc),
    Which = chosen_head_vars_presentation,
    maybe_filter_headvars(Which, Args0, Args1),
    list.map(trace_atom_arg_to_univ, Args1, Args),
    % Call the term browser to print the atom (or part of it up to
    % a size limit) as a goal.
    BrowserTerm = synthetic_term_to_browser_term(Functor, Args,
        is_function(PredOrFunc)),
    browse.print_browser_term(BrowserTerm, User ^ outstr, CallerType,
        User ^ browser, !IO),
    write_maybe_tabled_io_actions(User, MaybeIoActions, !IO).

:- pred write_maybe_tabled_io_actions(user_state::in,
    maybe(io_action_range)::in, io::di, io::uo) is cc_multi.

write_maybe_tabled_io_actions(User, MaybeIoActions, !IO) :-
    (
        MaybeIoActions = yes(IoActions),
        count_tabled_io_actions(IoActions, NumTabled, NumUntabled, !IO),
        write_io_actions(User, NumTabled, IoActions, !IO),
        ( NumUntabled > 0 ->
            io.write_string(User ^ outstr, "Warning: some IO " ++
                "actions for this atom are not tabled.\n", !IO)
        ;
            true
        )
    ;
        MaybeIoActions = no
    ).

:- pred count_tabled_io_actions(io_action_range::in, int::out, int::out,
    io::di, io::uo) is det.

count_tabled_io_actions(io_action_range(Start, End), NumTabled,
        NumUntabled, !IO) :-
    count_tabled_io_actions_2(Start, End, 0, NumTabled, 0,
        NumUntabled, !IO).

:- pred count_tabled_io_actions_2(io_seq_num::in,
    io_seq_num::in, int::in, int::out, int::in, int::out, io::di, io::uo)
    is det.

count_tabled_io_actions_2(Cur, End, PrevTabled, Tabled,
        PrevUntabled, Untabled, !IO) :-
    ( Cur = End ->
        Untabled = PrevUntabled,
        Tabled = PrevTabled
    ;
        get_maybe_io_action(Cur, MaybeIoAction, !IO),
        (
            MaybeIoAction = tabled(_),
            NewPrevUntabled = PrevUntabled,
            NewPrevTabled = PrevTabled + 1
        ;
            MaybeIoAction = untabled,
            NewPrevUntabled = PrevUntabled + 1,
            NewPrevTabled = PrevTabled
        ),
        count_tabled_io_actions_2(Cur + 1, End,
            NewPrevTabled, Tabled, NewPrevUntabled, Untabled, !IO)
    ).

:- pred trace_atom_arg_to_univ(trace_atom_arg::in, univ::out) is det.

trace_atom_arg_to_univ(TraceAtomArg, Univ) :-
    MaybeUniv = TraceAtomArg ^ arg_value,
    (
        MaybeUniv = yes(Rep),
        term_rep.rep_to_univ(Rep, Univ)
    ;
        MaybeUniv = no,
        Univ = univ('_' `with_type` unbound)
    ).

:- pred write_io_actions(user_state::in, int::in, io_action_range::in,
    io::di, io::uo) is cc_multi.

write_io_actions(User, NumTabled, IoActions, !IO) :-
    ( NumTabled = 0 ->
        true
    ;
        ( NumTabled = 1 ->
            io.write_string(User ^ outstr, "1 tabled IO action:", !IO)
        ;
            io.write_int(User ^ outstr, NumTabled, !IO),
            io.write_string(User ^ outstr, " tabled IO actions:", !IO)
        ),
        NumPrinted = get_num_printed_io_actions(User ^ browser),
        ( NumTabled =< NumPrinted ->
            io.nl(User ^ outstr, !IO),
            print_tabled_io_actions(User, IoActions, !IO)
        ;
            io.write_string(User ^ outstr, " too many to show", !IO),
            io.nl(User ^ outstr, !IO)
        )
    ).

:- pred print_tabled_io_actions(user_state::in, io_action_range::in,
    io::di, io::uo) is cc_multi.

print_tabled_io_actions(User, IoActions, !IO) :-
    IoActions = io_action_range(Start, End),
    print_tabled_io_actions_2(User, Start, End, !IO).

:- pred print_tabled_io_actions_2(user_state::in,
    io_seq_num::in, io_seq_num::in, io::di, io::uo) is cc_multi.

print_tabled_io_actions_2(User, Cur, End, !IO) :-
    ( Cur = End ->
        true
    ;
        get_maybe_io_action(Cur, MaybeIoAction, !IO),
        print_tabled_io_action(User, MaybeIoAction, !IO),
        print_tabled_io_actions_2(User, Cur + 1, End, !IO)
    ).

:- pred print_tabled_io_action(user_state::in, maybe_tabled_io_action::in,
    io::di, io::uo) is cc_multi.

print_tabled_io_action(_, untabled, !IO).
print_tabled_io_action(User, tabled(IoAction), !IO) :-
    Term = io_action_to_browser_term(IoAction),
    browse.print_browser_term(Term, User ^ outstr, print_all,
        User ^ browser, !IO).

%-----------------------------------------------------------------------------%

get_browser_state(User) = User ^ browser.

set_browser_state(Browser, !User) :-
    !:User = !.User ^ browser := Browser.

get_user_output_stream(User) = User ^ outstr.

set_user_testing_flag(Testing, User, User ^ testing := Testing).

%-----------------------------------------------------------------------------%

:- pred convert_dirs_to_term_path_from_atom(trace_atom::in,
    list(dir)::in(simplified_dirs), term_path::out) is det.

convert_dirs_to_term_path_from_atom(_, [], []).
convert_dirs_to_term_path_from_atom(atom(_, Args), [Dir | Dirs], TermPath) :-
    (
        Dir = child_num(Pos),
        Arg = list.det_index1(Args, Pos),
        Arg = arg_info(_, _, MaybeValue)
    ;
        Dir = child_name(Name),
        ( string_is_return_value_alias(Name) ->
            ( list.last(Args, LastArg) ->
                LastArg = arg_info(_, _, MaybeValue),
                Pos = list.length(Args)
            ;
                throw(internal_error("convert_dirs_to_term_path_from_atom",
                    "argument list empty"))
            )
        ;
            throw(internal_error("convert_dirs_to_term_path_from_atom",
                "argument of atom cannot be named"))
        )
    ),
    (
        MaybeValue = yes(TermRep),
        convert_dirs_to_term_path(TermRep, Dirs, TermPath0),
        TermPath = [Pos | TermPath0]
    ;
        MaybeValue = no,
        (
            % The user can cd to an unbound argument, but they
            % can't cd into subterms of an unbound argument.
            (
                Dirs = [],
                TermPath = [Pos]
            ;
                Dirs = [_ | _],
                throw(internal_error("convert_dirs_to_term_path_from_atom",
                    "no value for first position in path"))
            )
        )
    ).

%-----------------------------------------------------------------------------%
