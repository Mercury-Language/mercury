:- module split_line.
% break up a list(char) into a list(string) with each string containing a single
% word from the list(char).  Words are delimited by whitespace.

:- interface.

:- import_module list, char, string.

% put list of chars into a list of strings using whitespace as separator
:- pred split_line(list(char), list(string)).
:- mode split_line(in, out) is det.

:- implementation.

split_line(Line, Params) :-
    split_line0(Line, [], Params0),
    list__reverse(Params0, Params).

:- pred split_line0(list(char), list(string), list(string)).
:- mode split_line0(in, in, out) is det.

split_line0(List, Acc, Params) :-
    (
        List = [],
        Params = Acc
    ;
        List = [_|_],
        get_string(List, Param, List1),
        split_line0(List1, [Param | Acc], Params)
    ).

% get string from first non-blank characters in list
:- pred get_string(list(char), string, list(char)).
:- mode get_string(in, out, out) is det.

get_string(Lin, String, Lout) :-
    remove_leading_spaces(Lin, List1),
    get_list(List1, ListStr, Lout),
    string__from_char_list(ListStr, String).

% assume leading blanks have been removed
:- pred get_list(list(char), list(char), list(char)).
:- mode get_list(in, out, out) is det.

get_list(Lin, ListStr, Lout) :-
    (
        Lin = [],
        ListStr = [],
        Lout = []
    ;
        Lin = [C | List1],
            (
                char__is_whitespace(C)
            ->
                ListStr = [],
                Lout = List1    % don't put C back onto the list
            ;
                get_list(List1, ListStr1, Lout),
                ListStr = [C | ListStr1]
            )
    ).

% remove leading blanks from list
:- pred remove_leading_spaces(list(char), list(char)).
:- mode remove_leading_spaces(in, out) is det.

remove_leading_spaces(Lin, Lout) :-
    (
        Lin = [],
        Lout = []
    ;
        Lin = [C | L1],
            (
                char__is_whitespace(C)
            ->
                remove_leading_spaces(L1, Lout)
            ;
                Lout = [C | L1]
            )
    ).
