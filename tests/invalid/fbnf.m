%-----------------------------------------------------------------------------%
%
% File: fbnf.m
% Main author: Sean Charles
% Date: Sat Oct  1 19:48:26 2022
%
% FORTH but not FORTH
%
%-----------------------------------------------------------------------------%

:- module fbnf.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module stack.
:- import_module string.

%----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    Arg = string.join_list(" ", Args),
    io.format("ARG: %s\n", [s(Arg)], !IO),

    ( if string.is_empty(Arg) then
        repl(initial_state, _, !IO)
    else
        interpret(string.words(Arg), initial_state, State1, !IO),
        repl(State1, _, !IO)
    ).

%----------------------------------------------------------------------------%

:- type lstr == list(string).

    % This is the WORD handler definition
    %
% :- type word_handler == (pred(fstate, fstate, io, io, lstr, lstr)).
% :- inst word_handler == (pred(in, out, di, uo, in, out)is det).

:- type word_handler
    --->    word_handler(
                pred(
                    fstate::in, fstate::out,
                    io::di, io::uo,
                    lstr::in, lstr::out
                ) is det
            ).

:- type word_map == map(string, word_handler).
:- type stack_entry == stack(int).

   % FBNF operating state
   %
:- type fstate
    --->    fstate(
                fs_compiling    :: bool,
                fs_terminate    :: bool,
                fs_dict         :: word_map,
                fs_stack        :: stack_entry,
                fs_error        :: maybe(string)
            ).

   % Create the initial state of the machine.
   %
:- func initial_state = (fstate::out) is det.

initial_state =
    fstate(
        no,                         % interpret mode
        no,                         % terminate requested ?
        initial_dictionary,         % core words
        stack.init : stack_entry,   % initial empty stack
        no                          % no error message
    ).

   % Create default system WORDS dictionary.
   % Here we pre-load all the stock words that the user can build on
   % during the course of the session.
   %
:- func initial_dictionary = (word_map::out) is det.

initial_dictionary = Map :-
    some [!Words] (
        !:Words = map.init,

        map.set(":",     word_handler(define_word), !Words),
        map.set("bye",   word_handler(session_end), !Words),
        map.set("words", word_handler(list_words),  !Words),

        Map = !.Words
    ).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%
%               System WORD Implementations
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

    % ":" - define a new word.
    %
:- pred define_word(fstate::in, fstate::out, io::di, io::uo,
    lstr::in, lstr::out) is det.

define_word(!State, !IO) -->
    {
        io.format("DEFINE WORD\n", [], !IO)
    }.

    % "BYE" - terminate the current session.
    %
:- pred session_end(fstate::in, fstate::out, io::di, io::uo,
    lstr::in, lstr::out) is det.

session_end(!State, !IO) -->
    {
        io.format("BYE\n", [], !IO)
    }.

    % "WORDS" - list all defined words.
    %
:- pred list_words(fstate::in, fstate::out, io::di, io::uo,
    lstr::in, lstr::out) is det.

list_words(!State, !IO) -->
    {
        io.format("WORDS\n", [], !IO)
    }.

%----------------------------------------------------------------------------%

    % Read-Eval-Print-Loop
    %
:- pred repl(fstate::in, fstate::out, io::di, io::uo) is det.

repl(!State, !IO) :-
    io.format("fbnf> ", [], !IO),
    io.flush_output(!IO),
    io.read_line_as_string(Res, !IO),
    (
        Res = ok(Str),
        interpret(string.words(Str), !State, !IO),
        repl(!State, !IO)
    ;
        Res = error(Err),
        io.format("error: %s\n", [s(io.error_message(Err))], !IO),
        repl(!State, !IO)
    ;
        Res = eof,
        io.format("Bye!\n", [], !IO)
    ).

%----------------------------------------------------------------------------%

% from "Starting FORTH" ...
%
% This will activate a word called INTERPRET, also known as the “text
% interpreter.” The text interpreter scans the input stream, looking for strings
% of characters separated by spaces. When a string is found, it is looked up in
% the dictionary.
%
% If the word is in the dictionary, it is pointed out to a word called EXECUTE.
% EXECUTE executes the definition (in this case an asterisk is printed).
% Finally, the interpreter says everything’s “ok.”

    % 'INTERPRET'
    % Process the current input stream of user tokens.
    % If an error is detected then we abort the loop and abandon the input.
    % Anything processed up to the error remains intact though this may or may
    % not lead to a safe state (TBD).
    %
:- pred interpret(list(string)::in, fstate::in, fstate::out,
    io::di, io::uo) is det.

interpret(Words, !State, !IO) :-
    compiling(no, !State),
    interp1(Words, !State, !IO).

:- pred interp1(list(string)::in, fstate::in, fstate::out,
    io::di, io::uo) is det.

interp1([], !State, !IO).
interp1([W | Ws], !State, !IO) :-
    io.format("interp1: %s\n", [s(W)], !IO),
    execute(W, Ws, Rest, !State, !IO),
    flush_error(HadError, !State, !IO),
    ( if HadError = yes then
        true % abandon ship.
    else
        interp1(Rest, !State, !IO)
    ).

    % 'EXECUTE'
    % Find the word in the dictionary so we can invoke it, if it's not
    % found we want to record the error and terminate processing.
    %
:- pred execute(string::in, list(string)::in, list(string)::out,
    fstate::in, fstate::out, io::di, io::uo) is det.

execute(Word, Words, Rest, !State, !IO) :-
    Dict =  fs_dict(!.State),
    ( if map.search(Dict, Word, word_handler(Entry)) then
        Entry(!State, !IO)
    else
        set_error(string.format("word not found: %s\n", [s(Word)]), !State)
    ),
    Rest = Words.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%
% STATE change operations
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

    % Set 'compiling' mode as indicated.
    %
:- pred compiling(bool::in, fstate::in, fstate::out) is det.

compiling(Mode, !State) :-
    !:State = !.State ^ fs_compiling := Mode.

:- func is_compiling(fstate::in) = (bool::out) is det.

is_compiling(S) = S ^ fs_compiling.

    % Set an error message into the state.
    %
:- pred set_error(string::in, fstate::in, fstate::out) is det.

set_error(Message, !State) :-
    !:State = !.State ^ fs_error := yes(Message).

    % Flush (by writing) the current error message.
    % Flushed will be 'yes' if there was an error in the state record
    % and this will cause the current processing to abort.
    %
:- pred flush_error(bool::out, fstate::in, fstate::out, io::di, io::uo) is det.

flush_error(Flushed, !State, !IO) :-
    ( if yes(Error) = !.State ^ fs_error then
        io.format("error: %s\n", [s(Error)], !IO),
        Flushed = yes
    else
        Flushed = no
    ).

%----------------------------------------------------------------------------%
:- end_module fbnf.
%----------------------------------------------------------------------------%

