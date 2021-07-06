%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: eliza.m.
% Main author: bromage.
%
% This source file is hereby placed in the public domain.  -bromage.
%
% Eliza, the famous psychotherapist.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module eliza.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module require.

%-----------------------------------------------------------------------------%

    % Print the opening banner, initialise the response state, run the main
    % loop.
    %
main(!IO) :-
    io.write_string("\nHi!  I'm Eliza.  Please tell me your problem.\n", !IO),
    initial_state(State),
    main_loop([], State, !IO),
    io.write_string("\nGoodbye.\n", !IO).

:- pred main_loop(list(string)::in, eliza_state::in, io::di, io::uo) is det.

main_loop(Prev, StateIn, !IO) :-
    eliza.read_line(MaybeLine, !IO),
    (
        MaybeLine = yes(Line0),
        parse(Line0, Line1),
        (
            Line1 = [],
            main_loop(Prev, StateIn, !IO)
        ;
            Line1 = [_ | _],
            ( if Line1 = Prev then
                generate_repeat(StateIn, StateOut, !IO)
            else
                generate_response(Line1, StateIn, StateOut, !IO)
            ),
            main_loop(Line1, StateOut, !IO)
        )
    ;
        MaybeLine = no
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Response state processing functions

    % The response state consists of two parts: a set of
    % (type - messages) pairs and a list of repeat messages.
    % The response messages are used in response to a context,
    % and the repeat messages are used in response to a repeated
    % input line.

:- type response_state == assoc_list(message_type, list(message)).
:- type repeat_state   == list(string).

:- type eliza_state
    --->    state(response_state, repeat_state).

    % Initialise the state by reading in the initial message
    % database.
    %
:- pred initial_state(eliza_state::out) is det.

initial_state(state(ResMsg,RepMsg)) :-
    repeat_messages(RepMsg),
    response_messages(ResMsg).

    % Get a repeat message, and then cycle the list so that
    % a new one will come up next time.
    %
:- pred get_repeat(string::out, eliza_state::in, eliza_state::out) is det.

get_repeat(MsgOut, state(Res, RepIn), state(Res, RepOut)) :-
    (
        RepIn = [Msg | Rest],
        MsgOut = Msg,
        list.append(Rest, [Msg], RepOut)
    ;
        RepIn = [],
        error("Error: No repeat messages.\n")
    ).

    % Get a response message, and then cycle the list so that
    % a new one will come up next time.
    %
:- pred get_response(message_type::in, message::out,
    eliza_state::in, eliza_state::out) is det.

get_response(Type, MsgOut, state(ResIn, Rep), state(ResOut, Rep)) :-
    get_response2(Type, MsgOut, ResIn, ResOut).

:- pred get_response2(message_type::in, message::out,
    response_state::in, response_state::out) is det.

get_response2(_Type, _MsgOut, [], []) :-
    error("Error: Cannot match message type.\n").
get_response2(Type, MsgOut,
        [Type2 - Msgs2 | RestIn],
        [Type2 - Msgs3 | RestOut]) :-
    ( if Type = Type2 then
        ( if Msgs2 = [MsgOut1 | MsgOutRest] then
            MsgOut = MsgOut1,
            RestOut = RestIn,
            list.append(MsgOutRest, [MsgOut], Msgs3)
        else
            error("Error: Empty response list.\n")
        )
    else
        Msgs2 = Msgs3,
        get_response2(Type, MsgOut, RestIn, RestOut)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Write a prompt, then read a line.
    %
:- pred read_line(maybe(list(char))::out, io::di, io::uo) is det.

read_line(MaybeLine, !IO) :-
    io.write_string("\n> ", !IO),
    io.flush_output(!IO),
    io.input_stream(Stdin, !IO),
    io.read_line(Stdin, Result, !IO),
    io.write_string("\n", !IO),
    (
        Result = ok(Line1),
        MaybeLine = yes(Line1)
    ;
        ( Result = eof
        ; Result = error(_)
        ),
        MaybeLine = no
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % These are the characters that we must strip from a line during parsing.
    %
:- pred is_punct(char::in) is semidet.

is_punct('.').
is_punct(',').
is_punct('!').
is_punct('?').

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Parse the input string into words.  This is
    % achieved in three phases:
    %
    %     - Strip leading whitespace and intermediate
    %       punctuation symbols.
    %
    %     - Turn the resulting line into a list of words (each
    %       word stored as a list of characters).
    %
    %     - Turn each word into a string.
    %
:- pred parse(list(char)::in, list(string)::out) is det.

parse -->
    strip,
    form_words,
    words_to_strings.

:- pred strip(list(char)::in, list(char)::out) is det.

strip([], []).
strip([X | Xs], Ys) :-
    ( if char.is_whitespace(X) then
        strip(Xs, Ys)
    else
        strip2([X | Xs], Ys)
    ).

:- pred strip2(list(char)::in, list(char)::out) is det.

strip2([], []).
strip2([X | Xs], Ys) :-
    ( if is_punct(X) then
        strip2([' ' | Xs], Ys)
    else
        strip2(Xs, Ys1),
        ( if char.is_whitespace(X), Ys1 = [] then
            Ys = []
        else
            Ys = [X | Ys1]
        )
    ).

:- pred form_words(list(char)::in, list(list(char))::out) is det.

form_words([], []).
form_words([X | Xs], Ys) :-
    ( if char.is_whitespace(X) then
        form_words(Xs, Ys)
    else
        form_word(Xs, [X], Word, Rest),
        form_words(Rest, Words),
        Ys = [Word | Words]
    ).

:- pred form_word(list(char)::in, list(char)::in,
    list(char)::out, list(char)::out) is det.

form_word([], Word1, Word2, []) :-
    list.reverse(Word1, Word2).
form_word([X | Xs], WordIn, WordOut, Rest) :-
    ( if char.is_whitespace(X) then
        list.reverse(WordIn, WordOut), Rest = Xs
    else
        form_word(Xs, [X | WordIn], WordOut, Rest)
    ).

:- pred words_to_strings(list(list(char))::in, list(string)::out) is det.

words_to_strings([], []).
words_to_strings([X | Xs], [Y | Ys]) :-
    string.from_char_list(X, Y),
    words_to_strings(Xs, Ys).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Generate and display a repeat message
    %
:- pred generate_repeat(eliza_state::in, eliza_state::out,
    io::di, io::uo) is det.

generate_repeat(!State, !IO) :-
    get_repeat(Msg, !State),
    io.write_string(Msg, !IO),
    io.write_string("\n", !IO).

    % Generate and display a repeat message
    %
:- pred generate_response(list(string)::in,
    eliza_state::in, eliza_state::out, io::di, io::uo) is det.

generate_response(Words, !State, !IO) :-
    % Find out what sort of message we are dealing with.
    find_handle(Words, MsgType, Rest),
    get_response(MsgType, Maybe - String, !State),
    io.write_string(String, !IO),

    % If we must parrot back part of the original message,
    % resolve conjugates, write that string and then add
    % a trailing punctuation mark.
    (
        Maybe = yes(C),
        perform_conjugate(Rest, Postfix),
        eliza_write_strings(Postfix, !IO),
        io.write_char(C, !IO)
    ;
        Maybe = no
    ),
    io.write_string("\n", !IO).

    % Write a list of strings to the output stream with
    % words thrown in between.
    %
:- pred eliza_write_strings(list(string)::in, io::di, io::uo) is det.

eliza_write_strings([], !IO).
eliza_write_strings([X | Xs], !IO) :-
    io.write_char(' ', !IO),
    io.write_string(X, !IO),
    eliza_write_strings(Xs, !IO).

:- pred match_prefix(list(string)::in,
    list(string)::in, list(string)::out) is semidet.

match_prefix([], Ys, Ys).
match_prefix([X | Xs], [Y | Ys], Zs) :-
    string.to_upper(Y, X),
    match_prefix(Xs,Ys,Zs).

:- pred find_handle(list(string)::in, message_type::out,
    list(string)::out) is det.

find_handle(In, MsgType, Out) :-
    response_handles(Handles),
    find_handle2(In, MsgType, Out, Handles).

:- pred find_handle2(list(string)::in, message_type::out,
    list(string)::out, assoc_list(list(string), message_type)::in) is det.

find_handle2(In, no_key_message, In, []).
find_handle2(In, Type, Out, [Prefix - Type2 | Handles]) :-
    ( if find_handle3(In, Prefix, Rest) then
        Out = Rest, Type = Type2
    else
        find_handle2(In, Type, Out, Handles)
    ).

:- pred find_handle3(list(string)::in, list(string)::in, list(string)::out)
    is semidet.

find_handle3([X | Xs], Prefix, Rest) :-
    ( if match_prefix(Prefix, [X | Xs], Rest2) then
        Rest = Rest2
    else
        find_handle3(Xs, Prefix, Rest)
    ).

:- pred perform_conjugate(list(string)::in, list(string)::out) is det.

perform_conjugate([], []).
perform_conjugate([X | Xs], [Y | Ys]) :-
    ( if ( X = "I", Xs = [] ) then
        Y = "me", Ys = []
    else
        conjugate_map(Map),
        string.to_upper(X, Xupp),
        ( if map.search(Map, Xupp, Result) then
            Y = Result
        else
            Y = X
        ),
        perform_conjugate(Xs, Ys)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type message == pair(maybe(char), string).

:- type message_type
    --->    can_you
    ;       can_i
    ;       you_are
    ;       i_dont
    ;       i_feel
    ;       why_dont
    ;       why_cant
    ;       are_you
    ;       i_cant
    ;       i_am
    ;       you
    ;       yes
    ;       no
    ;       computer
    ;       i_want
    ;       question
    ;       name
    ;       because
    ;       sorry
    ;       dream
    ;       hello
    ;       maybe
    ;       your
    ;       always
    ;       think
    ;       alike
    ;       friend
    ;       no_key_message.

:- pred conjugate_map(map(string, string)::out) is det.

conjugate_map(MapOut) :-
    one_way_conjugates(AL1),
    two_way_conjugates(AL2),
    assoc_list.reverse_members(AL2, AL3),
    list.append(AL1, AL2, AL12),
    list.append(AL12, AL3, AL123),
    prepare_conj(AL123, ALFinal),
    map.from_assoc_list(ALFinal, MapOut).

:- pred prepare_conj(assoc_list(string, string)::in,
    assoc_list(string, string)::out) is det.

prepare_conj([], []).
prepare_conj([X - V | Xs], [Y - V | Ys]) :-
    string.to_upper(X,Y),
    prepare_conj(Xs, Ys).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred response_handles(assoc_list(list(string), message_type)::out) is det.

response_handles([
        ["CAN","YOU"]           - can_you,
        ["CAN","I"]             - can_i,
        ["YOU","ARE"]           - you_are,
        ["YOU'RE"]              - you_are,
        ["I","DON'T"]           - i_dont,
        ["I","FEEL"]            - i_feel,
        ["WHY","DON'T","YOU"]   - why_dont,
        ["WHY","CAN'T","I"]     - why_cant,
        ["ARE","YOU"]           - are_you,
        ["I","CAN'T"]           - i_cant,
        ["I","AM"]              - i_am,
        ["I'M"]                 - i_am,
        ["YES"]                 - yes,
        ["NO"]                  - no,
        ["COMPUTER"]            - computer,
        ["COMPUTERS"]           - computer,
        ["I","WANT"]            - i_want,
        ["WHAT"]                - question,
        ["HOW"]                 - question,
        ["WHO"]                 - question,
        ["WHERE"]               - question,
        ["WHEN"]                - question,
        ["WHY"]                 - question,
        ["NAME"]                - name,
        ["BECAUSE"]             - because,
        ["CAUSE"]               - because,
        ["SORRY"]               - sorry,
        ["DREAM"]               - dream,
        ["DREAMS"]              - dream,
        ["HI"]                  - hello,
        ["HELLO"]               - hello,
        ["MAYBE"]               - maybe,
        ["YOUR"]                - your,
        ["ALWAYS"]              - always,
        ["THINK"]               - think,
        ["ALIKE"]               - alike,
        ["FRIEND"]              - friend,
        ["FRIENDS"]             - friend,
        ["YOU"]                 - you
    ]).

:- pred one_way_conjugates(assoc_list(string, string)::out) is det.

one_way_conjugates([
    "me" - "you"
    ]).

:- pred two_way_conjugates(assoc_list(string, string)::out) is det.

two_way_conjugates([
    "are" - "am",
    "were" - "was",
    "you" - "I",
    "your" - "my",
    "I've" - "you've",
    "I'm" - "you're"
    ]).

:- pred repeat_messages(repeat_state::out) is det.

repeat_messages([
    "Why did you repeat yourself?",
    "Do you expect a different answer by repeating yourself?",
    "Come, come, elucidate your thoughts.",
    "Please don't repeat yourself!"
    ]).

:- pred response_messages(response_state::out) is det.

response_messages(
    [
    can_you - [
        yes('?') - "Don't you believe that I can",
        yes('?') - "Perhaps you would like to be able to",
        yes('?') - "You want me to be able to"
        ],

    can_i - [
        yes('?') - "Perhaps you don't want to",
        yes('?') - "Do you want to be able to"
        ],

    you_are - [
        yes('?') - "What makes you think I am",
        yes('?') - "Does it please you to believe I am",
        yes('?') - "Perhaps you would like to be",
        yes('?') - "Do you sometimes wish you were"
        ],

    i_dont - [
        yes('?') - "Don't you really",
        yes('?') - "Why don't you",
        yes('?') - "Do you wish to be able to",
        no       - "Does that trouble you?"
        ],

    i_feel - [
        no       - "Tell me more about such feelings.",
        yes('?') - "Do you often feel",
        yes('?') - "Do you enjoy feeling"
        ],

    why_dont - [
        yes('?') - "Do you really believe I don't",
        yes('.') - "Perhaps in good time I will",
        yes('?') - "Do you want me to"
        ],

    why_cant - [
        yes('?') - "Do you think you should be able to",
        yes('?') - "Why can't you"
        ],

    are_you - [
        yes('?') - "Why are you interested in whether or not I am",
        yes('?') - "Would you prefer if I were not",
        yes('?') - "Perhaps in your fantasies I am"
        ],

    i_cant - [
        yes('?') - "How do you know you can't",
        no       - "Have you tried?",
        yes('?') - "Perhaps you can now"
        ],

    i_am - [
        yes('?') - "Did you come to me because you are",
        yes('?') - "How long have you been",
        yes('?') - "Do you believe it is normal to be",
        yes('?') - "Do you enjoy being"
        ],

    you - [
        no       - "We were discussing you --not me.",
        yes('?') - "Oh,",
        no       - "You're not really talking about me, are you?"
        ],

    yes - [
        no       - "You seem quite positive.",
        no       - "Are you sure?",
        no       - "I see.",
        no       - "I understand."
        ],

    no - [
        no       - "Are you saying no just to be negative?",
        no       - "You are being a bit negative.",
        no       - "Why not?",
        no       - "Are you sure?",
        no       - "Why no?"
        ],

    computer - [
        no       - "Do computers worry you?",
        no       - "Are you talking about me in particular?",
        no       - "Are you frightened by machines?",
        no       - "Why do you mention computers?",
        no       - "What do you think machines have to do with your problems?",
        no       - "Don't you think computers can help people?",
        no       - "What is it about machines that worries you?"
        ],

    i_want - [
        yes('?') - "Why do you want",
        yes('?') - "What would it mean to you if you got",
        yes('?') - "Suppose you got",
        yes('?') - "What if you never got",
        yes('.') - "I sometimes also want"
        ],

    question - [
        no       - "Why do you ask?",
        no       - "Does that question interest you?",
        no       - "What answer would please you the most?",
        no       - "What do you think?",
        no       - "Are such questions on your mind often?",
        no       - "What is it that you really want to know?",
        no       - "Have you asked anyone else?",
        no       - "Have you asked such questions before?",
        no       - "What else comes to mind when you ask that?"
        ],

    name - [
        no       - "Names don't interest me.",
        no       - "I don't care about names --please go on."
        ],

    because - [
        no       - "Is that the real reason?",
        no       - "Don't any other reasons come to mind?",
        no       - "Does that reason explain anything else?",
        no       - "What other reasons might there be?"
        ],

    sorry - [
        no       - "Please don't apologise!",
        no       - "Apologies are not necessary.",
        no       - "What feelings do you have when you apologise?",
        no       - "Don't be so defensive!"
        ],

    dream - [
        no       - "What does that dream suggest to you?",
        no       - "Do you dream often?",
        no       - "What persons appear in your dreams?",
        no       - "Are you disturbed by your dreams?"
        ],

    hello - [
        no       - "How do you...please state your problem."
        ],

    maybe - [
        no       - "You don't seem quite certain.",
        no       - "Why the uncertain tone?",
        no       - "Can't you be more positive?",
        no       - "You aren't sure?",
        no       - "Don't you know?"
        ],

    your - [
        yes('?') - "Why are you concerned about my",
        yes('?') - "What about your own"
        ],

    always - [
        no       - "Can you think of a specific example?",
        no       - "When?",
        no       - "What are you thinking of?",
        no       - "Really, always?"
        ],

    think - [
        no       - "Do you really think so?",
        yes('?') - "But you are not sure you",
        yes('?') - "Do you doubt you"
        ],

    alike - [
        no       - "In what way?",
        no       - "What resemblance do you see?",
        no       - "What does the similarity suggest to you?",
        no       - "What other connections do you see?",
        no       - "Could there really be some connection?",
        no       - "How?"
        ],

    friend - [
        no       - "Why do you bring up the topic of friends?",
        no       - "Do your friends worry you?",
        no       - "Do your friends pick on you?",
        no       - "Are you sure you have any friends?",
        no       - "Do you impose on your friends?",
        no       - "Perhaps your love for friends worries you."
        ],

    no_key_message - [
        no       - "I'm not sure I understand you fully.",
        no       - "What does that suggest to you?",
        no       - "I see.",
        no       - "Can you elaborate on that?",
        no       - "Say, do you have any psychological problems?"
        ]
    ]).

%-----------------------------------------------------------------------------%
:- end_module eliza.
%-----------------------------------------------------------------------------%
