%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: char.m.
% Main author: fjh.
% Stability: high.

% This module defines some predicates that manipulate characters.

% Originally we used `character' rather than `char' for the type name
% because `char' was used by NU-Prolog to mean something different.
% But now we use `char' and the use of `character' is discouraged.
%
% NU-Prolog atoms can only include 7-bit ASCII chars, so the current
% implementation does not support 8-bit characters.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module char.
:- interface.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type char == character.

:- pred char__to_int(char, int).
:- mode char__to_int(in, out) is det.
:- mode char__to_int(in, in) is semidet.	% implied
:- mode char__to_int(out, in) is semidet.
	% Convert a character to it's corresponding numerical code.

:- pred char__to_upper(char, char).
:- mode char__to_upper(in, out) is det.
	% Convert a character to uppercase.

:- pred char__to_lower(char, char).
:- mode char__to_lower(in, out) is det.
	% Convert a character to lowercase.

:- pred char__lower_upper(char, char).
:- mode char__lower_upper(in, out) is semidet.
:- mode char__lower_upper(out, in) is semidet.
	% char__lower_upper(Lower, Upper) is true iff
	% Lower is a lower-case letter and Upper is the corresponding
	% upper-case letter.

:- pred char__is_whitespace(char).
:- mode char__is_whitespace(in) is semidet.
	% True iff the character is whitespace, i.e. a space, tab,
	% newline, carriage return, form-feed, or vertical tab.

:- pred char__is_upper(char).
:- mode char__is_upper(in) is semidet.
	% True iff the character is an uppercase letter.

:- pred char__is_lower(char).
:- mode char__is_lower(in) is semidet.
	% True iff the character is a lowercase letter.

:- pred char__is_alpha(char).
:- mode char__is_alpha(in) is semidet.
	% True iff the character is a letter.

:- pred char__is_alnum(char).
:- mode char__is_alnum(in) is semidet.
	% True iff the character is a letter or digit.

:- pred char__is_alpha_or_underscore(char).
:- mode char__is_alpha_or_underscore(in) is semidet.
	% True iff the character is a letter or an underscore.

:- pred char__is_alnum_or_underscore(char).
:- mode char__is_alnum_or_underscore(in) is semidet.
	% True iff the character is a letter, a digit or an underscore.

:- pred char__is_digit(char).
:- mode char__is_digit(in) is semidet.
	% True iff the character is a decimal digit (0-9).

:- pred char__is_binary_digit(char).
:- mode char__is_binary_digit(in) is semidet.
	% True iff the character is a binary digit (0 or 1).

:- pred char__is_octal_digit(char).
:- mode char__is_octal_digit(in) is semidet.
	% True iff the character is a octal digit (0-7).

:- pred char__is_hex_digit(char).
:- mode char__is_hex_digit(in) is semidet.
	% True iff the character is a hexadecimal digit (0-9, a-f, A-F).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

char__is_whitespace(' ').
char__is_whitespace('\t').
char__is_whitespace('\n').
char__is_whitespace('\r').
char__is_whitespace('\f').
char__is_whitespace('\v').

char__is_alpha(Char) :-
	( char__is_lower(Char) ->
		true
	; char__is_upper(Char) ->
		true
	;
		fail
	).

char__is_alnum(Char) :-
	( char__is_alpha(Char) ->
		true
	; char__is_digit(Char) ->
		true
	;
		fail
	).

char__is_alpha_or_underscore(Char) :-
	( Char = '_' ->
		true
	;	
		char__is_alpha(Char)
	).

char__is_alnum_or_underscore(Char) :-
	( char__is_digit(Char) ->
		true
	;	
		char__is_alpha_or_underscore(Char)
	).

char__is_lower(Lower) :-
	char__lower_upper(Lower, _).

char__is_upper(Upper) :-
	(
		char__lower_upper(_, Upper)
	->
		true
	;
		fail
	).

char__to_lower(Char, Lower) :-
	(
		char__lower_upper(LowerChar, Char)
	->
		Lower = LowerChar
	;
		Lower = Char
	).

char__to_upper(Char, Upper) :-
	(
		char__lower_upper(Char, UpperChar)
	->
		Upper = UpperChar
	;
		Upper = Char
	).

%-----------------------------------------------------------------------------%

% Lots of big tables.
%
% It's conceivable that there are more efficient implementations,
% but these versions are very portable.

%-----------------------------------------------------------------------------%

char__is_binary_digit('0').
char__is_binary_digit('1').

char__is_octal_digit('0').
char__is_octal_digit('1').
char__is_octal_digit('2').
char__is_octal_digit('3').
char__is_octal_digit('4').
char__is_octal_digit('5').
char__is_octal_digit('6').
char__is_octal_digit('7').

char__is_digit('0').
char__is_digit('1').
char__is_digit('2').
char__is_digit('3').
char__is_digit('4').
char__is_digit('5').
char__is_digit('6').
char__is_digit('7').
char__is_digit('8').
char__is_digit('9').

char__is_hex_digit('0').
char__is_hex_digit('1').
char__is_hex_digit('2').
char__is_hex_digit('3').
char__is_hex_digit('4').
char__is_hex_digit('5').
char__is_hex_digit('6').
char__is_hex_digit('7').
char__is_hex_digit('8').
char__is_hex_digit('9').
char__is_hex_digit('a').
char__is_hex_digit('b').
char__is_hex_digit('c').
char__is_hex_digit('d').
char__is_hex_digit('e').
char__is_hex_digit('f').
char__is_hex_digit('A').
char__is_hex_digit('B').
char__is_hex_digit('C').
char__is_hex_digit('D').
char__is_hex_digit('E').
char__is_hex_digit('F').

%%% char__to_int('\000', 0).	% not supported by NU-Prolog
char__to_int('\001', 1).
char__to_int('\002', 2).
char__to_int('\003', 3).
char__to_int('\004', 4).
char__to_int('\005', 5).
char__to_int('\006', 6).
char__to_int('\007', 7).
char__to_int('\010', 8).
char__to_int('\011', 9).
char__to_int('\012', 10).
char__to_int('\013', 11).
char__to_int('\014', 12).
char__to_int('\015', 13).
char__to_int('\016', 14).
char__to_int('\017', 15).
char__to_int('\020', 16).
char__to_int('\021', 17).
char__to_int('\022', 18).
char__to_int('\023', 19).
char__to_int('\024', 20).
char__to_int('\025', 21).
char__to_int('\026', 22).
char__to_int('\027', 23).
char__to_int('\030', 24).
char__to_int('\031', 25).
char__to_int('\032', 26).
char__to_int('\033', 27).
char__to_int('\034', 28).
char__to_int('\035', 29).
char__to_int('\036', 30).
char__to_int('\037', 31).
char__to_int('\040', 32).
char__to_int('\041', 33).
char__to_int('\042', 34).
char__to_int('\043', 35).
char__to_int('\044', 36).
char__to_int('\045', 37).
char__to_int('\046', 38).
char__to_int('\047', 39).
char__to_int('\050', 40).
char__to_int('\051', 41).
char__to_int('\052', 42).
char__to_int('\053', 43).
char__to_int('\054', 44).
char__to_int('\055', 45).
char__to_int('\056', 46).
char__to_int('\057', 47).
char__to_int('\060', 48).
char__to_int('\061', 49).
char__to_int('\062', 50).
char__to_int('\063', 51).
char__to_int('\064', 52).
char__to_int('\065', 53).
char__to_int('\066', 54).
char__to_int('\067', 55).
char__to_int('\070', 56).
char__to_int('\071', 57).
char__to_int('\072', 58).
char__to_int('\073', 59).
char__to_int('\074', 60).
char__to_int('\075', 61).
char__to_int('\076', 62).
char__to_int('\077', 63).
char__to_int('\100', 64).
char__to_int('\101', 65).
char__to_int('\102', 66).
char__to_int('\103', 67).
char__to_int('\104', 68).
char__to_int('\105', 69).
char__to_int('\106', 70).
char__to_int('\107', 71).
char__to_int('\110', 72).
char__to_int('\111', 73).
char__to_int('\112', 74).
char__to_int('\113', 75).
char__to_int('\114', 76).
char__to_int('\115', 77).
char__to_int('\116', 78).
char__to_int('\117', 79).
char__to_int('\120', 80).
char__to_int('\121', 81).
char__to_int('\122', 82).
char__to_int('\123', 83).
char__to_int('\124', 84).
char__to_int('\125', 85).
char__to_int('\126', 86).
char__to_int('\127', 87).
char__to_int('\130', 88).
char__to_int('\131', 89).
char__to_int('\132', 90).
char__to_int('\133', 91).
char__to_int('\134', 92).
char__to_int('\135', 93).
char__to_int('\136', 94).
char__to_int('\137', 95).
char__to_int('\140', 96).
char__to_int('\141', 97).
char__to_int('\142', 98).
char__to_int('\143', 99).
char__to_int('\144', 100).
char__to_int('\145', 101).
char__to_int('\146', 102).
char__to_int('\147', 103).
char__to_int('\150', 104).
char__to_int('\151', 105).
char__to_int('\152', 106).
char__to_int('\153', 107).
char__to_int('\154', 108).
char__to_int('\155', 109).
char__to_int('\156', 110).
char__to_int('\157', 111).
char__to_int('\160', 112).
char__to_int('\161', 113).
char__to_int('\162', 114).
char__to_int('\163', 115).
char__to_int('\164', 116).
char__to_int('\165', 117).
char__to_int('\166', 118).
char__to_int('\167', 119).
char__to_int('\170', 120).
char__to_int('\171', 121).
char__to_int('\172', 122).
char__to_int('\173', 123).
char__to_int('\174', 124).
char__to_int('\175', 125).
char__to_int('\176', 126).
char__to_int('\177', 127).

% XXX
% NU-Prolog atoms can only include 7-bit ASCII chars.

/***********
char__to_int('\200', 128).
char__to_int('\201', 129).
char__to_int('\202', 130).
char__to_int('\203', 131).
char__to_int('\204', 132).
char__to_int('\205', 133).
char__to_int('\206', 134).
char__to_int('\207', 135).
char__to_int('\210', 136).
char__to_int('\211', 137).
char__to_int('\212', 138).
char__to_int('\213', 139).
char__to_int('\214', 140).
char__to_int('\215', 141).
char__to_int('\216', 142).
char__to_int('\217', 143).
char__to_int('\220', 144).
char__to_int('\221', 145).
char__to_int('\222', 146).
char__to_int('\223', 147).
char__to_int('\224', 148).
char__to_int('\225', 149).
char__to_int('\226', 150).
char__to_int('\227', 151).
char__to_int('\230', 152).
char__to_int('\231', 153).
char__to_int('\232', 154).
char__to_int('\233', 155).
char__to_int('\234', 156).
char__to_int('\235', 157).
char__to_int('\236', 158).
char__to_int('\237', 159).
char__to_int('\240', 160).
char__to_int('\241', 161).
char__to_int('\242', 162).
char__to_int('\243', 163).
char__to_int('\244', 164).
char__to_int('\245', 165).
char__to_int('\246', 166).
char__to_int('\247', 167).
char__to_int('\250', 168).
char__to_int('\251', 169).
char__to_int('\252', 170).
char__to_int('\253', 171).
char__to_int('\254', 172).
char__to_int('\255', 173).
char__to_int('\256', 174).
char__to_int('\257', 175).
char__to_int('\260', 176).
char__to_int('\261', 177).
char__to_int('\262', 178).
char__to_int('\263', 179).
char__to_int('\264', 180).
char__to_int('\265', 181).
char__to_int('\266', 182).
char__to_int('\267', 183).
char__to_int('\270', 184).
char__to_int('\271', 185).
char__to_int('\272', 186).
char__to_int('\273', 187).
char__to_int('\274', 188).
char__to_int('\275', 189).
char__to_int('\276', 190).
char__to_int('\277', 191).
char__to_int('\300', 192).
char__to_int('\301', 193).
char__to_int('\302', 194).
char__to_int('\303', 195).
char__to_int('\304', 196).
char__to_int('\305', 197).
char__to_int('\306', 198).
char__to_int('\307', 199).
char__to_int('\310', 200).
char__to_int('\311', 201).
char__to_int('\312', 202).
char__to_int('\313', 203).
char__to_int('\314', 204).
char__to_int('\315', 205).
char__to_int('\316', 206).
char__to_int('\317', 207).
char__to_int('\320', 208).
char__to_int('\321', 209).
char__to_int('\322', 210).
char__to_int('\323', 211).
char__to_int('\324', 212).
char__to_int('\325', 213).
char__to_int('\326', 214).
char__to_int('\327', 215).
char__to_int('\330', 216).
char__to_int('\331', 217).
char__to_int('\332', 218).
char__to_int('\333', 219).
char__to_int('\334', 220).
char__to_int('\335', 221).
char__to_int('\336', 222).
char__to_int('\337', 223).
char__to_int('\340', 224).
char__to_int('\341', 225).
char__to_int('\342', 226).
char__to_int('\343', 227).
char__to_int('\344', 228).
char__to_int('\345', 229).
char__to_int('\346', 230).
char__to_int('\347', 231).
char__to_int('\350', 232).
char__to_int('\351', 233).
char__to_int('\352', 234).
char__to_int('\353', 235).
char__to_int('\354', 236).
char__to_int('\355', 237).
char__to_int('\356', 238).
char__to_int('\357', 239).
char__to_int('\360', 240).
char__to_int('\361', 241).
char__to_int('\362', 242).
char__to_int('\363', 243).
char__to_int('\364', 244).
char__to_int('\365', 245).
char__to_int('\366', 246).
char__to_int('\367', 247).
char__to_int('\370', 248).
char__to_int('\371', 249).
char__to_int('\372', 250).
char__to_int('\373', 251).
char__to_int('\374', 252).
char__to_int('\375', 253).
char__to_int('\376', 254).
char__to_int('\377', 255).
*********/

%-----------------------------------------------------------------------------%

:- char__lower_upper(X, Y) when X or Y.

char__lower_upper('a', 'A').
char__lower_upper('b', 'B').
char__lower_upper('c', 'C').
char__lower_upper('d', 'D').
char__lower_upper('e', 'E').
char__lower_upper('f', 'F').
char__lower_upper('g', 'G').
char__lower_upper('h', 'H').
char__lower_upper('i', 'I').
char__lower_upper('j', 'J').
char__lower_upper('k', 'K').
char__lower_upper('l', 'L').
char__lower_upper('m', 'M').
char__lower_upper('n', 'N').
char__lower_upper('o', 'O').
char__lower_upper('p', 'P').
char__lower_upper('q', 'Q').
char__lower_upper('r', 'R').
char__lower_upper('s', 'S').
char__lower_upper('t', 'T').
char__lower_upper('u', 'U').
char__lower_upper('v', 'V').
char__lower_upper('w', 'W').
char__lower_upper('x', 'X').
char__lower_upper('y', 'Y').
char__lower_upper('z', 'Z').

%-----------------------------------------------------------------------------%
