%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: eliza.m.
% Main author: bromage.

% This source file is hereby placed in the public domain.  -bromage.

% Eliza, the famous psychotherapist.

%-----------------------------------------------------------------------------%

:- module eliza.
:- interface.
:- import_module io.

:- pred main(io__state :: di, io__state :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool, char, list, string, std_util, map, assoc_list, require.

%-----------------------------------------------------------------------------%

	% Print the opening banner, initialise the response state,
	% run the main loop.

main -->
	io__write_string("\nHi!  I'm Eliza.  Please tell me your problem.\n"),
	{ eliza__initial_state(State) },
	eliza__main_loop([], State),
	io__write_string("\nGoodbye.\n").

:- pred eliza__main_loop(list(string), eliza__state, io__state, io__state).
:- mode eliza__main_loop(in, in, di, uo) is det.
eliza__main_loop(Prev, StateIn) -->
	eliza__read_line(Line0, Ok),
	( { Ok = yes } ->
	    { eliza__parse(Line0, Line1) },
	    ( { Line1 = [] } ->
		eliza__main_loop(Prev, StateIn)
	    ;
		( { Line1 = Prev } ->
		    eliza__generate_repeat(StateIn, StateOut)
		;
		    eliza__generate_response(Line1, StateIn, StateOut) 
		),
	        eliza__main_loop(Line1, StateOut) 
	    )
	;
	    { true }
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

:- type eliza__state ---> state(response_state, repeat_state).

	% Initialise the state by reading in the initial message
	% database.

:- pred eliza__initial_state(eliza__state :: out) is det.
eliza__initial_state(state(ResMsg,RepMsg)) :-
	repeat_messages(RepMsg),
	response_messages(ResMsg).

	% Get a repeat message, and then cycle the list so that
	% a new one will come up next time.

:- pred eliza__get_repeat(string, eliza__state, eliza__state).
:- mode eliza__get_repeat(out, in, out) is det.
eliza__get_repeat(MsgOut, state(Res, RepIn), state(Res, RepOut)) :-
	( RepIn = [Msg | Rest] ->
	    MsgOut = Msg,
	    list__append(Rest, [Msg], RepOut)
	;
	    error("Error: No repeat messages.\n")
	).

	% Get a response message, and then cycle the list so that
	% a new one will come up next time.

:- pred eliza__get_response(message_type, message, eliza__state, eliza__state).
:- mode eliza__get_response(in, out, in, out) is det.
eliza__get_response(Type, MsgOut, state(ResIn, Rep), state(ResOut, Rep)) :-
	eliza__get_response2(Type, MsgOut, ResIn, ResOut).

:- pred eliza__get_response2(message_type, message,
		response_state, response_state).
:- mode eliza__get_response2(in, out, in, out) is det.
eliza__get_response2(_Type, _MsgOut, [], []) :-
	error("Error: Cannot match message type.\n").
eliza__get_response2(Type, MsgOut,
		[Type2 - Msgs2 | RestIn], 
		[Type2 - Msgs3 | RestOut]) :-
	( Type = Type2 ->
	    ( Msgs2 = [MsgOut1 | MsgOutRest] ->
		MsgOut = MsgOut1,
		RestOut = RestIn,
		list__append(MsgOutRest, [MsgOut], Msgs3)
	    ;
		error("Error: Empty response list.\n") 
	    )
	;
	    Msgs2 = Msgs3,
	    eliza__get_response2(Type, MsgOut, RestIn, RestOut) 
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Write a prompt, then read a line.

:- pred eliza__read_line(list(char), bool, io__state, io__state).
:- mode eliza__read_line(out, out, di, uo) is det.
eliza__read_line(Line, Ok) -->
	io__write_string("\n> "),
	io__flush_output,
	io__input_stream(Stdin),
	io__read_line(Stdin, Result),
	io__write_string("\n"),
	( { Result = ok(Line1) } ->
	   { Ok = yes, Line = Line1 }
	;
	   { Ok = no, Line = [] }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% These are the characters that we must strip from a
	% line during parsing.

:- pred eliza__is_punct(char).
:- mode eliza__is_punct(in) is semidet.
eliza__is_punct('.').
eliza__is_punct(',').
eliza__is_punct('!').
eliza__is_punct('?').

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

:- pred eliza__parse(list(char) :: in, list(string) :: out) is det.
eliza__parse -->
	eliza__strip,
	eliza__form_words,
	eliza__words_to_strings.

:- pred eliza__strip(list(char) :: in, list(char) :: out) is det.
eliza__strip([], []).
eliza__strip([X | Xs], Ys) :-
	( char__is_whitespace(X) ->
	    eliza__strip(Xs, Ys)
	;
	    eliza__strip2([X | Xs], Ys)
	).

:- pred eliza__strip2(list(char) :: in, list(char) :: out) is det.
eliza__strip2([], []).
eliza__strip2([X | Xs], Ys) :-
	( eliza__is_punct(X) ->
	    eliza__strip2([' ' | Xs], Ys)
	;
	     eliza__strip2(Xs, Ys1),
	    ( char__is_whitespace(X), Ys1 = [] ->
		Ys = []
	    ;
		Ys = [X | Ys1] 
	    ) 
	).

:- pred eliza__form_words(list(char), list(list(char))).
:- mode eliza__form_words(in, out) is det.
eliza__form_words([], []).
eliza__form_words([X | Xs], Ys) :-
	( char__is_whitespace(X) ->
	    eliza__form_words(Xs, Ys)
	;
	    eliza__form_word(Xs, [X], Word, Rest),
	    eliza__form_words(Rest, Words),
	    Ys = [Word | Words] 
	).

:- pred eliza__form_word(list(char), list(char), list(char), list(char)).
:- mode eliza__form_word(in, in, out, out) is det.
eliza__form_word([], Word1, Word2, []) :-
	list__reverse(Word1, Word2).
eliza__form_word([X | Xs], WordIn, WordOut, Rest) :-
	( char__is_whitespace(X) ->
	    list__reverse(WordIn, WordOut), Rest = Xs
	;
	    eliza__form_word(Xs, [X | WordIn], WordOut, Rest) 
	).

:- pred eliza__words_to_strings(list(list(char)), list(string)).
:- mode eliza__words_to_strings(in, out) is det.
eliza__words_to_strings([], []).
eliza__words_to_strings([X | Xs], [Y | Ys]) :-
	string__from_char_list(X, Y),
	eliza__words_to_strings(Xs, Ys).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Generate and display a repeat message

:- pred eliza__generate_repeat(eliza__state, eliza__state,
		io__state, io__state).
:- mode eliza__generate_repeat(in, out, di, uo) is det.
eliza__generate_repeat(StateIn, StateOut) -->
	{ eliza__get_repeat(Msg, StateIn, StateOut) },
	io__write_string(Msg),
	io__write_string("\n").

	% Generate and display a repeat message

:- pred eliza__generate_response(list(string),
		eliza__state, eliza__state,
		io__state, io__state).
:- mode eliza__generate_response(in, in, out, di, uo) is det.
eliza__generate_response(Words, StateIn, StateOut) -->

	% Find out what sort of message we are dealing with.

	{ eliza__find_handle(Words, MsgType, Rest) },
	{ eliza__get_response(MsgType, Maybe - String, StateIn, StateOut) },
	io__write_string(String),

	% If we must parrot back part of the original message,
	% resolve conjugates, write that string and then add
	% a trailing punctuation mark.

	( { Maybe = yes(C) } ->
	    { eliza__perform_conjugate(Rest, Postfix) },
	    eliza__write_strings(Postfix),
	    io__write_char(C)
	;
	    { true } 
	),
	io__write_string("\n").

	% Write a list of strings to the output stream with
	% words thrown in between.

:- pred eliza__write_strings(list(string), io__state, io__state).
:- mode eliza__write_strings(in, di, uo) is det.
eliza__write_strings([]) --> { true }.
eliza__write_strings([X | Xs]) -->
	io__write_char(' '),
	io__write_string(X),
	eliza__write_strings(Xs).

:- pred eliza__match_prefix(list(string), list(string), list(string)).
:- mode eliza__match_prefix(in, in, out) is semidet.
eliza__match_prefix([], Ys, Ys).
eliza__match_prefix([X | Xs], [Y | Ys], Zs) :-
	string__to_upper(Y, X),
	eliza__match_prefix(Xs,Ys,Zs).
   
:- pred eliza__find_handle(list(string), message_type, list(string)).
:- mode eliza__find_handle(in, out, out) is det.
eliza__find_handle(In, MsgType, Out) :-
	response_handles(Handles),
	eliza__find_handle2(In, MsgType, Out, Handles).

:- pred eliza__find_handle2(list(string), message_type, list(string),
		assoc_list(list(string), message_type)).
:- mode eliza__find_handle2(in, out, out, in) is det.
eliza__find_handle2(In, no_key_message, In, []).
eliza__find_handle2(In, Type, Out, [Prefix - Type2 | Handles]) :-
	( eliza__find_handle3(In, Prefix, Rest) ->
	    Out = Rest, Type = Type2
	;
	    eliza__find_handle2(In, Type, Out, Handles) 
	).

:- pred eliza__find_handle3(list(string), list(string), list(string)).
:- mode eliza__find_handle3(in, in, out) is semidet.
eliza__find_handle3([X | Xs], Prefix, Rest) :-
	( eliza__match_prefix(Prefix, [X | Xs], Rest2) ->
	    Rest = Rest2
	;
	    eliza__find_handle3(Xs, Prefix, Rest) 
	).

:- pred eliza__perform_conjugate(list(string), list(string)).
:- mode eliza__perform_conjugate(in, out) is det.
eliza__perform_conjugate([], []).
eliza__perform_conjugate([X | Xs], [Y | Ys]) :-
	( ( X = "I", Xs = [] ) ->
	    Y = "me", Ys = []
	;
	    eliza__conjugate_map(Map),
	    string__to_upper(X, Xupp),
	    ( map__search(Map, Xupp, Result) ->
		Y = Result
	    ;
		Y = X 
	    ),
	    eliza__perform_conjugate(Xs, Ys)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type message == pair(maybe(char), string).

:- type message_type --->
	can_you ; can_i ; you_are ; i_dont ; i_feel ; why_dont ;
	why_cant ; are_you ; i_cant ; i_am ; you ; yes ; no ;
	computer ; i_want ; question ; name ; because ; sorry ; dream ;
	hello ; maybe ; your ; always ; think ; alike ; friend ;
	no_key_message.

:- pred eliza__conjugate_map(map(string, string) :: out) is det.
eliza__conjugate_map(MapOut) :-
	one_way_conjugates(AL1),
	two_way_conjugates(AL2),
	assoc_list__reverse_members(AL2, AL3),
	list__append(AL1, AL2, AL12),
	list__append(AL12, AL3, AL123),
	prepare_conj(AL123, ALFinal),
	map__from_assoc_list(ALFinal, MapOut).

:- pred prepare_conj(assoc_list(string, string), assoc_list(string, string)).
:- mode prepare_conj(in, out) is det.
prepare_conj([], []).
prepare_conj([X - V | Xs], [Y - V | Ys]) :-
	string__to_upper(X,Y),
	prepare_conj(Xs, Ys).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred response_handles(assoc_list(list(string), message_type) :: out) is det.
response_handles([
 	["CAN","YOU"]		- can_you,
        ["CAN","I"]             - can_i,
        ["YOU","ARE"] 		- you_are,
        ["YOU'RE"] 		- you_are,
        ["I","DON'T"]           - i_dont,
        ["I","FEEL"]		- i_feel,
	["WHY","DON'T","YOU"]	- why_dont,
	["WHY","CAN'T","I"]	- why_cant,
        ["ARE","YOU"]		- are_you,
        ["I","CAN'T"]          	- i_cant,
        ["I","AM"]             	- i_am,
        ["I'M"]                	- i_am,
        ["YES"]                	- yes,
        ["NO"]                 	- no,
        ["COMPUTER"]           	- computer,
        ["COMPUTERS"]          	- computer,
        ["I","WANT"]           	- i_want,
        ["WHAT"]               	- question,
        ["HOW"]                	- question,
        ["WHO"]                	- question,
        ["WHERE"]              	- question,
        ["WHEN"]               	- question,
        ["WHY"]                	- question,
        ["NAME"]               	- name,
        ["BECAUSE"]            	- because,
        ["CAUSE"]              	- because,
        ["SORRY"]              	- sorry,
        ["DREAM"]              	- dream,
        ["DREAMS"]             	- dream,
        ["HI"]                 	- hello,
        ["HELLO"]              	- hello,
        ["MAYBE"]              	- maybe,
        ["YOUR"]               	- your,
        ["ALWAYS"]             	- always,
        ["THINK"]              	- think,
        ["ALIKE"]              	- alike,
        ["FRIEND"]             	- friend,
        ["FRIENDS"]            	- friend,
        ["YOU"]                	- you
	]).

:- pred one_way_conjugates(assoc_list(string, string) :: out) is det.
one_way_conjugates([
	"me" - "you"
	]).

:- pred two_way_conjugates(assoc_list(string, string) :: out) is det.
two_way_conjugates([
	"are" - "am",
	"were" - "was",
	"you" - "I",
	"your" - "my",
	"I've" - "you've",
	"I'm" - "you're"
	]).

:- pred repeat_messages(repeat_state :: out) is det.
repeat_messages([
 	"Why did you repeat yourself?",
	"Do you expect a different answer by repeating yourself?",
	"Come, come, elucidate your thoughts.",
	"Please don't repeat yourself!" 
	]).

:- pred response_messages(response_state :: out) is det.
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
		no       - "What resemblence do you see?",
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

% eliza.m %
