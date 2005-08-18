%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% error_util.m
% Main author: zs.
%
% This module contains code that can be helpful in the formatting of
% error messages.
%
% Given a context, a starting indentation level and a list of words,
% print an error message that looks like this:
%
% module.m:10: first line of error message blah blah blah
% module.m:10:   second line of error message blah blah blah
% module.m:10:   third line of error message blah blah blah
%
% The words will be packed into lines as tightly as possible,
% with spaces between each pair of words, subject to the constraints
% that every line starts with a context, followed by Indent+1 spaces
% on the first line and Indent+3 spaces on later lines, and that every
% line contains at most 79 characters (unless a long single word
% forces the line over this limit).
%
% The caller supplies the list of words to be printed in the form
% of a list of error message components. Each component may specify
% a string to printed exactly as it is, or it may specify a string
% containing a list of words, which may be broken at white space.
%
%-----------------------------------------------------------------------------%

:- module parse_tree__error_util.

:- interface.

:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module char.
:- import_module io.
:- import_module list.
:- import_module std_util.

:- type format_component
    --->    fixed(string)   % This string should appear in the output
                            % in one piece, as it is.

    ;       prefix(string)  % This string should appear in the output
                            % in one piece, as it is, inserted directly
                            % before the next format_component, without
                            % any intervening space.

    ;       suffix(string)  % This string should appear in the output
                            % in one piece, as it is, appended directly
                            % after the previous format_component, without
                            % any intervening space.

    ;       words(string)   % This string contains words separated by
                            % white space. The words should appear in
                            % the output in the given order, but the
                            % white space may be rearranged and line
                            % breaks may be inserted.

    ;       sym_name(sym_name)
                            % The output should contain the string form of
                            % the sym_name, surrounded by `' quotes.

    ;       sym_name_and_arity(sym_name_and_arity)
                            % The output should contain the string form of
                            % the sym_name, followed by '/' and the arity,
                            % all surrounded by `' quotes.

    ;       pred_or_func(pred_or_func)
                            % Output the string "predicate" or "function"
                            % as appropriate.

    ;       simple_call_id(simple_call_id)
                            % Output the identity of the given call.

    ;       nl              % Insert a line break if there has been text
                            % output since the last line break.

    ;       nl_indent_delta(int).
                            % Act as nl, but also add the given integer
                            % (which should be a small positive or negative
                            % integer) to the current indent level.

:- type format_components == list(format_component).

    % Convert a list of strings into a list of format_components
    % separated by commas, with the last two elements separated by `and'.
    %
:- func list_to_pieces(list(string)) = list(format_component).

    % Convert a list of lists of format_components into a list of
    % format_components separated by commas, with the last two elements
    % separated by `and'.
    %
:- func component_lists_to_pieces(list(list(format_component))) =
    list(format_component).

    % Convert a list of format_components into a list of format_components
    % separated by commas, with the last two elements separated by `and'.
    %
:- func component_list_to_pieces(list(format_component)) =
    list(format_component).

    % choose_number(List, Singular, Plural) = Form
    %
    % Choose between a singular version and a plural version of something,
    % based on the length of a list.  Chooses the plural if the list is empty.
    %
:- func choose_number(list(T), U, U) = U.

    % Display the given error message, without a context and with standard
    % indentation.
    %
:- pred write_error_pieces_plain(list(format_component)::in,
    io::di, io::uo) is det.

    % write_error_plain_with_progname(ProgName, Msg):
    %
    % Display Msg as the error string, with ProgName as a context
    % and with standard indentation.
    %
:- pred write_error_plain_with_progname(string::in, string::in,
    io::di, io::uo) is det.

    % write_error_pieces(Context, Indent, Components):
    %
    % Display `Components' as the error message, with
    % `Context' as a context and indent by `Indent'.
    %
:- pred write_error_pieces(prog_context::in, int::in,
    list(format_component)::in, io::di, io::uo) is det.

    % Display the given error message, but indent the first line.
    % This is useful when adding extra lines to an already displayed message.
    %
:- pred write_error_pieces_not_first_line(prog_context::in, int::in,
    list(format_component)::in, io::di, io::uo) is det.

    % Display the given error message. The bool is true iff
    % this is the first line.
    %
:- pred write_error_pieces_maybe_first_line(bool::in, prog_context::in,
    int::in, list(format_component)::in, io::di, io::uo) is det.

:- pred write_error_pieces_maybe_with_context(maybe(prog_context)::in, int::in,
    list(format_component)::in, io::di, io::uo) is det.

:- func error_pieces_to_string(list(format_component)) = string.

:- func describe_sym_name(sym_name) = string.

:- func describe_sym_name_and_arity(sym_name_and_arity) = string.

:- func pred_or_func_to_string(pred_or_func) = string.

    % Append a punctuation character to a message, avoiding unwanted
    % line splitting between the message and the punctuation.
    %
:- func append_punctuation(list(format_component), char) =
    list(format_component).

    % Put `' quotes around the given string.
    %
:- func add_quotes(string) = string.

    % report_error_num_args(MaybePredOrFunc, Arity, CorrectArities).
    %
    % Write "wrong number of arguments (<Arity>; should be <CorrectArities>)",
    % adjusting `Arity' and `CorrectArities' if `MaybePredOrFunc' is
    % `yes(function)'.
    %
:- pred report_error_num_args(maybe(pred_or_func)::in, int::in, list(int)::in,
    io::di, io::uo) is det.

    % sorry(ModuleName, Message)
    % Call error/1 with a "Sorry, not implemented" message.
    %
    % Use this for features that should be implemented (or at
    % least could be implemented).
    %
:- func sorry(string, string) = _ is erroneous.
:- pred sorry(string::in, string::in) is erroneous.

    % unexpected(ModuleName, Message)
    % Call error/1 with an "Unexpected" message.
    %
    % Use this to handle cases which are not expected to arise (i.e.
    % bugs).
    %
:- func unexpected(string, string) = _ is erroneous.
:- pred unexpected(string::in, string::in) is erroneous.

    % Record the fact that a warning has been issued; set the exit status
    % to error if the --halt-at-warn option is set.
    %
:- pred record_warning(io::di, io::uo) is det.

    % Report a warning, and set the exit status to error if the
    % --halt-at-warn option is set.
    %
:- pred report_warning(string::in, io::di, io::uo) is det.

    % Report a warning to the specified stream, and set the exit status
    % to error if the --halt-at-warn option is set.
    %
:- pred report_warning(io__output_stream::in, string::in, io::di, io::uo)
    is det.

    % Report a warning, and set the exit status to error if the
    % --halt-at-warn option is set.
    %
:- pred report_warning(prog_context::in, int::in, list(format_component)::in,
    io::di, io::uo) is det.

:- implementation.

:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module libs__globals.
:- import_module libs__options.

:- import_module char.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module term.

list_to_pieces([]) = [].
list_to_pieces([Elem]) = [words(Elem)].
list_to_pieces([Elem1, Elem2]) = [fixed(Elem1), words("and"), fixed(Elem2)].
list_to_pieces([Elem1, Elem2, Elem3 | Elems]) =
    [fixed(Elem1 ++ ",") | list_to_pieces([Elem2, Elem3 | Elems])].

component_lists_to_pieces([]) = [].
component_lists_to_pieces([Components]) = Components.
component_lists_to_pieces([Components1, Components2]) =
        list__condense([Components1, [words("and")], Components2]).
component_lists_to_pieces(
        [Components1, Components2, Components3 | Components]) =
    list__append(append_punctuation(Components1, ','),
        component_lists_to_pieces([Components2, Components3 | Components])).

component_list_to_pieces([]) = [].
component_list_to_pieces([Component]) = [Component].
component_list_to_pieces([Component1, Component2]) =
        [Component1, words("and"), Component2].
component_list_to_pieces(
        [Component1, Component2, Component3 | Components]) =
    list__append(append_punctuation([Component1], ','),
        component_list_to_pieces([Component2, Component3 | Components])).

choose_number([], _Singular, Plural) = Plural.
choose_number([_], Singular, _Plural) = Singular.
choose_number([_, _ | _], _Singular, Plural) = Plural.

write_error_pieces_plain(Components, !IO) :-
    write_error_pieces_maybe_with_context(yes, no, 0, Components, !IO).

write_error_plain_with_progname(ProgName, Msg, !IO) :-
    write_error_pieces_plain([fixed(ProgName ++ ":"), words(Msg)], !IO).

write_error_pieces(Context, Indent, Components, !IO) :-
    write_error_pieces_maybe_with_context(yes, yes(Context),
        Indent, Components, !IO).

write_error_pieces_not_first_line(Context, Indent, Components, !IO) :-
    write_error_pieces_maybe_with_context(no, yes(Context),
        Indent, Components, !IO).

write_error_pieces_maybe_first_line(IsFirst, Context, Indent, Components,
        !IO) :-
    (
        IsFirst = yes,
        write_error_pieces(Context, Indent, Components, !IO)
    ;
        IsFirst = no,
        write_error_pieces_not_first_line(Context, Indent, Components, !IO)
    ).

write_error_pieces_maybe_with_context(MaybeContext, Indent, Components, !IO) :-
    write_error_pieces_maybe_with_context(yes, MaybeContext,
        Indent, Components, !IO).

:- pred write_error_pieces_maybe_with_context(bool::in,
    maybe(prog_context)::in, int::in, list(format_component)::in,
    io::di, io::uo) is det.

write_error_pieces_maybe_with_context(IsFirst, MaybeContext,
        FixedIndent, Components, !IO) :-
    (
            % The fixed characters at the start of the line are:
            % filename
            % :
            % line number (min 3 chars)
            % :
            % space
            % indent
        (
            MaybeContext = yes(Context),
            term__context_file(Context, FileName),
            term__context_line(Context, LineNumber),
            string__length(FileName, FileNameLength),
            string__int_to_string(LineNumber, LineNumberStr),
            string__length(LineNumberStr, LineNumberStrLength0),
            ( LineNumberStrLength0 < 3 ->
                LineNumberStrLength = 3
            ;
                LineNumberStrLength = LineNumberStrLength0
            ),
            ContextLength = FileNameLength + 1 + LineNumberStrLength + 2
        ;
            MaybeContext = no,
            ContextLength = 0
        ),
        convert_components_to_paragraphs(Components, Paragraphs),
        FirstIndent = (IsFirst = yes -> 0 ; 1),
        Remain = 79 - (ContextLength + FixedIndent),
        group_words(IsFirst, FirstIndent, Paragraphs, Remain, Lines)
    ),
    write_lines(Lines, MaybeContext, FixedIndent, !IO).

:- func indent_increment = int.

indent_increment = 2.

:- pred write_lines(list(line)::in, maybe(prog_context)::in, int::in,
    io::di, io::uo) is det.

write_lines([], _, _, !IO).
write_lines([Line | Lines], MaybeContext, FixedIndent, !IO) :-
    (
        MaybeContext = yes(Context),
        prog_out__write_context(Context, !IO)
    ;
        MaybeContext = no
    ),
    Line = line(LineIndent, LineWords),
    Indent = FixedIndent + LineIndent * indent_increment,
    string__pad_left("", ' ', Indent, IndentStr),
    io__write_string(IndentStr, !IO),
    write_line(LineWords, !IO),
    write_lines(Lines, MaybeContext, FixedIndent, !IO).

:- pred write_line(list(string)::in, io::di, io::uo) is det.

write_line([], !IO).
write_line([Word | Words], !IO) :-
    io__write_string(Word, !IO),
    write_line_rest(Words, !IO),
    io__write_char('\n', !IO).

:- pred write_line_rest(list(string)::in, io::di, io::uo) is det.

write_line_rest([], !IO).
write_line_rest([Word | Words], !IO) :-
    io__write_char(' ', !IO),
    io__write_string(Word, !IO),
    write_line_rest(Words, !IO).

error_pieces_to_string([]) = "".
error_pieces_to_string([Component | Components]) = Str :-
    TailStr = error_pieces_to_string(Components),
    (
        Component = fixed(Word),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = prefix(Word),
        Str = Word ++ TailStr
    ;
        Component = suffix(Word),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = words(Words),
        Str = join_string_and_tail(Words, Components, TailStr)
    ;
        Component = sym_name(SymName),
        Word = sym_name_to_word(SymName),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = sym_name_and_arity(SymNameAndArity),
        Word = sym_name_and_arity_to_word(SymNameAndArity),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = pred_or_func(PredOrFunc),
        Word = pred_or_func_to_string(PredOrFunc),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = simple_call_id(SimpleCallId),
        Word = simple_call_id_to_string(SimpleCallId),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = nl,
        Str = "\n" ++ TailStr
    ;
        Component = nl_indent_delta(_),
        % There is nothing we can do about the indent delta.
        Str = "\n" ++ TailStr
    ).

:- func join_string_and_tail(string, list(format_component), string) = string.

join_string_and_tail(Word, Components, TailStr) = Str :-
    ( TailStr = "" ->
        Str = Word
    ; Components = [suffix(_) | _] ->
        Str = Word ++ TailStr
    ;
        Str = Word ++ " " ++ TailStr
    ).

%----------------------------------------------------------------------------%

:- type paragraph
    --->    paragraph(
                list(string),   % The list of words to print in the paragraph.
                                % It should not be empty.
                int             % The indent delta to apply for the next
                                % paragraph.
            ).

:- pred convert_components_to_paragraphs(list(format_component)::in,
    list(paragraph)::out) is det.

convert_components_to_paragraphs(Components, Paras) :-
    convert_components_to_paragraphs_acc(Components, [], [], Paras).

:- type word
    --->    word(string)
    ;       prefix_word(string)
    ;       suffix_word(string).

:- pred convert_components_to_paragraphs_acc(list(format_component)::in,
    list(word)::in, list(paragraph)::in, list(paragraph)::out) is det.

convert_components_to_paragraphs_acc([], RevWords0, !Paras) :-
    Strings = rev_words_to_strings(RevWords0),
    list__reverse([paragraph(Strings, 0) | !.Paras], !:Paras).
convert_components_to_paragraphs_acc([Component | Components], RevWords0,
        !Paras) :-
    (
        Component = fixed(Word),
        RevWords1 = [word(Word) | RevWords0]
    ;
        Component = prefix(Word),
        RevWords1 = [prefix_word(Word) | RevWords0]
    ;
        Component = suffix(Word),
        RevWords1 = [suffix_word(Word) | RevWords0]
    ;
        Component = words(WordsStr),
        break_into_words(WordsStr, RevWords0, RevWords1)
    ;
        Component = sym_name(SymName),
        RevWords1 = [word(sym_name_to_word(SymName)) | RevWords0]
    ;
        Component = sym_name_and_arity(SymNameAndArity),
        Word = sym_name_and_arity_to_word(SymNameAndArity),
        RevWords1 = [word(Word) | RevWords0]
    ;
        Component = pred_or_func(PredOrFunc),
        Word = pred_or_func_to_string(PredOrFunc),
        RevWords1 = [word(Word) | RevWords0]
    ;
        Component = simple_call_id(SimpleCallId),
        WordsStr = simple_call_id_to_string(SimpleCallId),
        break_into_words(WordsStr, RevWords0, RevWords1)
    ;
        Component = nl,
        Strings = rev_words_to_strings(RevWords0),
        list.cons(paragraph(Strings, 0), !Paras),
        RevWords1 = []
    ;
        Component = nl_indent_delta(IndentDelta),
        Strings = rev_words_to_strings(RevWords0),
        list.cons(paragraph(Strings, IndentDelta), !Paras),
        RevWords1 = []
    ),
    convert_components_to_paragraphs_acc(Components, RevWords1, !Paras).

:- type plain_or_prefix
    --->    plain(string)
    ;       prefix(string).

:- func rev_words_to_strings(list(word)) = list(string).

rev_words_to_strings(RevWords) = Strings :-
    PorPs = list__reverse(rev_words_to_rev_plain_or_prefix(RevWords)),
    Strings = join_prefixes(PorPs).

:- func rev_words_to_rev_plain_or_prefix(list(word)) = list(plain_or_prefix).

rev_words_to_rev_plain_or_prefix([]) = [].
rev_words_to_rev_plain_or_prefix([Word | Words]) = PorPs :-
    (
        Word = word(String),
        PorPs = [plain(String) | rev_words_to_rev_plain_or_prefix(Words)]
    ;
        Word = prefix_word(Prefix),
        PorPs = [prefix(Prefix) | rev_words_to_rev_plain_or_prefix(Words)]
    ;
        Word = suffix_word(Suffix),
        (
            Words = [],
            PorPs = [plain(Suffix)]
        ;
            Words = [word(String) | Tail],
            PorPs = [plain(String ++ Suffix)
                | rev_words_to_rev_plain_or_prefix(Tail)]
        ;
            Words = [prefix_word(Prefix) | Tail],
            % Convert the prefix/suffix combination into a plain word.
            % We could convert it into a prefix, but since prefix/suffix
            % combinations shouldn't come up at all, what we do here probably
            % doesn't matter.
            PorPs = [plain(Prefix ++ Suffix)
                | rev_words_to_rev_plain_or_prefix(Tail)]
        ;
            Words = [suffix_word(MoreSuffix) | Tail],
            PorPs = rev_words_to_rev_plain_or_prefix(
                [suffix_word(MoreSuffix ++ Suffix) | Tail])
        )
    ).

:- func join_prefixes(list(plain_or_prefix)) = list(string).

join_prefixes([]) = [].
join_prefixes([Head | Tail]) = Strings :-
    TailStrings = join_prefixes(Tail),
    (
        Head = plain(String),
        Strings = [String | TailStrings]
    ;
        Head = prefix(Prefix),
        (
            TailStrings = [First | Later],
            Strings = [Prefix ++ First | Later]
        ;
            TailStrings = [],
            Strings = [Prefix | TailStrings]
        )
    ).

:- func sym_name_to_word(sym_name) = string.

sym_name_to_word(SymName) = "`" ++ SymStr ++ "'" :-
    sym_name_to_string(SymName, SymStr).

:- func sym_name_and_arity_to_word(sym_name_and_arity) = string.

sym_name_and_arity_to_word(SymName / Arity) =
        "`" ++ SymStr ++ "'/" ++ ArityStr :-
    sym_name_to_string(SymName, SymStr),
    string__int_to_string(Arity, ArityStr).

:- pred break_into_words(string::in, list(word)::in, list(word)::out) is det.

break_into_words(String, Words0, Words) :-
    break_into_words_from(String, 0, Words0, Words).

:- pred break_into_words_from(string::in, int::in, list(word)::in,
    list(word)::out) is det.

break_into_words_from(String, Cur, Words0, Words) :-
    ( find_word_start(String, Cur, Start) ->
        find_word_end(String, Start, End),
        Length = End - Start + 1,
        string__substring(String, Start, Length, WordStr),
        Next = End + 1,
        break_into_words_from(String, Next, [word(WordStr) | Words0], Words)
    ;
        Words = Words0
    ).

:- pred find_word_start(string::in, int::in, int::out) is semidet.

find_word_start(String, Cur, WordStart) :-
    string__index(String, Cur, Char),
    ( char__is_whitespace(Char) ->
        Next = Cur + 1,
        find_word_start(String, Next, WordStart)
    ;
        WordStart = Cur
    ).

:- pred find_word_end(string::in, int::in, int::out) is det.

find_word_end(String, Cur, WordEnd) :-
    Next = Cur + 1,
    ( string__index(String, Next, Char) ->
        ( char__is_whitespace(Char) ->
            WordEnd = Cur
        ;
            find_word_end(String, Next, WordEnd)
        )
    ;
        WordEnd = Cur
    ).

%----------------------------------------------------------------------------%

:- type line
    --->    line(
                int,            % Indent level; multiply by indent_increment
                                % to get number of spaces of indentation.
                list(string)    % The words on the line.
            ).

    % Groups the given words into lines. The first line can have up to Max
    % characters on it; the later lines (if any) up to Max-2 characters.
    % The given list of paragraphs must be nonempty, since we always return
    % at least one line.
    %
:- pred group_words(bool::in, int::in, list(paragraph)::in, int::in,
    list(line)::out) is det.

group_words(IsFirst, CurIndent, Paras, Max, Lines) :-
    (
        Paras = [],
        Lines = []
    ;
        Paras = [FirstPara | LaterParas],
        FirstPara = paragraph(FirstParaWords, FirstIndentDelta),
        (
            IsFirst = yes,
            RestIndent = CurIndent + 1
        ;
            IsFirst = no,
            RestIndent = CurIndent
        ),
        NextIndent = RestIndent + FirstIndentDelta,
        (
            FirstParaWords = [],
            group_words(IsFirst, NextIndent, LaterParas, Max, Lines)
        ;
            FirstParaWords = [FirstWord | LaterWords],
            get_line_of_words(FirstWord, LaterWords, CurIndent, Max,
                LineWords, RestWords),
            CurLine = line(CurIndent, LineWords),

            group_nonfirst_line_words(RestWords, RestIndent, Max,
                ParaRestLines),
            ParaLines = [CurLine | ParaRestLines],

            group_words(no, NextIndent, LaterParas, Max, RestLines),
            Lines = ParaLines ++ RestLines
        )
    ).

:- pred group_nonfirst_line_words(list(string)::in, int::in, int::in,
    list(line)::out) is det.

group_nonfirst_line_words(Words, Indent, Max, Lines) :-
    (
        Words = [],
        Lines = []
    ;
        Words = [FirstWord | LaterWords],
        get_line_of_words(FirstWord, LaterWords, Indent, Max,
            LineWords, RestWords),
        Line = line(Indent, LineWords),
        group_nonfirst_line_words(RestWords, Indent, Max, RestLines),
        Lines = [Line | RestLines]
    ).

:- pred get_line_of_words(string::in, list(string)::in, int::in, int::in,
    list(string)::out, list(string)::out) is det.

get_line_of_words(FirstWord, LaterWords, Indent, Max, Line, RestWords) :-
    string__length(FirstWord, FirstWordLen),
    Avail = Max - Indent * indent_increment,
    get_later_words(LaterWords, FirstWordLen, Avail, [FirstWord],
        Line, RestWords).

:- pred get_later_words(list(string)::in, int::in, int::in,
    list(string)::in, list(string)::out, list(string)::out) is det.

get_later_words([], _, _, Line, Line, []).
get_later_words([Word | Words], OldLen, Avail, Line0, Line, RestWords) :-
    string__length(Word, WordLen),
    NewLen = OldLen + 1 + WordLen,
    ( NewLen =< Avail ->
        list__append(Line0, [Word], Line1),
        get_later_words(Words, NewLen, Avail, Line1, Line, RestWords)
    ;
        Line = Line0,
        RestWords = [Word | Words]
    ).

%-----------------------------------------------------------------------------%

describe_sym_name_and_arity(SymName / Arity) =
        string__append_list(["`", SymNameString, "/",
            string__int_to_string(Arity), "'"]) :-
    sym_name_to_string(SymName, SymNameString).

describe_sym_name(SymName) = string__append_list(["`", SymNameString, "'"]) :-
    sym_name_to_string(SymName, SymNameString).

pred_or_func_to_string(predicate) = "predicate".
pred_or_func_to_string(function) = "function".

append_punctuation([], _) = _ :-
    error("append_punctuation: appending punctuation after nothing").
append_punctuation([Piece0], Punc) = [Piece] :-
    % Avoid unwanted line splitting between the message and the punctuation.
    (
        Piece0 = words(String),
        Piece = words(string__append(String, char_to_string(Punc)))
    ;
        Piece0 = fixed(String),
        Piece = fixed(string__append(String, char_to_string(Punc)))
    ;
        Piece0 = prefix(Prefix),
        Piece = prefix(string__append(Prefix, char_to_string(Punc)))
    ;
        Piece0 = suffix(Suffix),
        Piece = suffix(string__append(Suffix, char_to_string(Punc)))
    ;
        Piece0 = sym_name(SymName),
        String = sym_name_to_word(SymName),
        Piece = fixed(string__append(String, char_to_string(Punc)))
    ;
        Piece0 = sym_name_and_arity(SymNameAndArity),
        String = sym_name_and_arity_to_word(SymNameAndArity),
        Piece = fixed(string__append(String, char_to_string(Punc)))
    ;
        Piece0 = pred_or_func(PredOrFunc),
        String = pred_or_func_to_string(PredOrFunc),
        Piece = fixed(string__append(String, char_to_string(Punc)))
    ;
        Piece0 = simple_call_id(SimpleCallId),
        String = simple_call_id_to_string(SimpleCallId),
        Piece = words(string__append(String, char_to_string(Punc)))
    ;
        Piece0 = nl,
        unexpected(this_file,
            "append_punctutation: appending punctuation after nl")
    ;
        Piece0 = nl_indent_delta(_),
        unexpected(this_file,
            "append_punctutation: appending punctuation after nl_indent_delta")
    ).
append_punctuation([Piece1, Piece2 | Pieces], Punc) =
    [Piece1 | append_punctuation([Piece2 | Pieces], Punc)].

add_quotes(Str) = "`" ++ Str ++ "'".

%-----------------------------------------------------------------------------%

report_error_num_args(MaybePredOrFunc, Arity0, Arities0, !IO) :-
    % Adjust arities for functions.
    ( MaybePredOrFunc = yes(function) ->
        adjust_func_arity(function, Arity, Arity0),
        list__map(
            (pred(OtherArity0::in, OtherArity::out) is det :-
                adjust_func_arity(function, OtherArity, OtherArity0)
            ),
            Arities0, Arities)
    ;
        Arity = Arity0,
        Arities = Arities0
    ),
    io__write_string("wrong number of arguments (", !IO),
    io__write_int(Arity, !IO),
    io__write_string("; should be ", !IO),
    report_error_right_num_args(Arities, !IO),
    io__write_string(")", !IO).

:- pred report_error_right_num_args(list(int)::in, io::di, io::uo) is det.

report_error_right_num_args([], !IO).
report_error_right_num_args([Arity | Arities], !IO) :-
    io__write_int(Arity, !IO),
    (
        Arities = [],
        true
    ;
        Arities = [_],
        io__write_string(" or ", !IO)
    ;
        Arities = [_, _ | _],
        io__write_string(", ", !IO)
    ),
    report_error_right_num_args(Arities, !IO).

    % Call error/1 with a "Sorry, not implemented" message.
    %
sorry(Module, What) = _ :- sorry(Module, What).
sorry(Module, What) :-
    string__format("%s: Sorry, not implemented: %s",
        [s(Module), s(What)], ErrorMessage),
    error(ErrorMessage).

unexpected(Module, What) = _ :- unexpected(Module, What).
unexpected(Module, What) :-
    string__format("%s: Unexpected: %s", [s(Module), s(What)], ErrorMessage),
    error(ErrorMessage).

record_warning(!IO) :-
    globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn, !IO),
    (
        HaltAtWarn = yes,
        io__set_exit_status(1, !IO)
    ;
        HaltAtWarn = no
    ).

report_warning(Message, !IO) :-
    record_warning(!IO),
    io__write_string(Message, !IO).

report_warning(Stream, Message, !IO) :-
    record_warning(!IO),
    io__write_string(Stream, Message, !IO).

report_warning(Context, Indent, Components, !IO) :-
    record_warning(!IO),
    write_error_pieces(Context, Indent, Components, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "error_util.m".

%-----------------------------------------------------------------------------%
