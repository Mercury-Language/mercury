%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: char.nl.
% Main author: fjh.

% This module defines some predicates that manipulate characters.

% At the moment we are using 'character' rather than 'char'
% because 'char' is already used by NU-Prolog to mean something
% different.
%
% NU-Prolog atoms can only include 7-bit ASCII chars, so the current
% implementation does not support 8-bit characters.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module char.
:- interface.

%-----------------------------------------------------------------------------%

:- pred char_to_int(character, int).
:- mode char_to_int(in, out).
:- mode char_to_int(out, in).
	% Convert a character to it's corresponding numerical code.

:- pred to_upper(character, character).
:- mode to_upper(in, out).
	% Convert a character to uppercase.

:- pred to_lower(character, character).
:- mode to_lower(in, out).
	% Convert a character to lowercase.

:- pred is_upper(character).
:- mode is_upper(in).
	% True iff the character is an uppercase letter.

:- pred is_alpha(character).
:- mode is_alpha(in).
	% True iff the character is a letter.

:- pred is_digit(character).
:- mode is_digit(in).
	% True iff the character is a decimal digit.

:- pred is_lower(character).
:- mode is_lower(in).
	% True iff the character is a lowercase letter.

:- pred upper_lower(character, character).
:- mode upper_lower(in, out).
:- mode upper_lower(out, in).
	% upper_lower(Upper, Lower) is true iff
	% Upper is an upper-case letter and Lower is the corresponding
	% lower-case letter.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

is_alpha(Char) :-
	some [] (
		is_lower(Char) ;
		is_upper(Char)
	).

is_digit('0').
is_digit('1').
is_digit('2').
is_digit('3').
is_digit('4').
is_digit('5').
is_digit('6').
is_digit('7').
is_digit('8').
is_digit('9').

is_lower(Lower) :-
	some [Upper] upper_lower(Lower, Upper).

is_upper(Upper) :-
	some [Lower] upper_lower(Lower, Upper).

to_lower(Char, Lower) :-
	(if some [LowerChar]
		upper_lower(LowerChar, Char)
	then
		Lower = LowerChar
	else
		Lower = Char
	).

to_upper(Char, Upper) :-
	(if some [UpperChar]
		upper_lower(Char, UpperChar)
	then
		Upper = UpperChar
	else
		Upper = Char
	).

%-----------------------------------------------------------------------------%

% Lots of big tables.
%
% It's conceivable that there are more efficient implementations,
% but these versions are very portable.

%-----------------------------------------------------------------------------%

%%% char_to_int('\000', 0).	% not supported by NU-Prolog
char_to_int('\001', 1).
char_to_int('\002', 2).
char_to_int('\003', 3).
char_to_int('\004', 4).
char_to_int('\005', 5).
char_to_int('\006', 6).
char_to_int('\007', 7).
char_to_int('\010', 8).
char_to_int('\011', 9).
char_to_int('\012', 10).
char_to_int('\013', 11).
char_to_int('\014', 12).
char_to_int('\015', 13).
char_to_int('\016', 14).
char_to_int('\017', 15).
char_to_int('\020', 16).
char_to_int('\021', 17).
char_to_int('\022', 18).
char_to_int('\023', 19).
char_to_int('\024', 20).
char_to_int('\025', 21).
char_to_int('\026', 22).
char_to_int('\027', 23).
char_to_int('\030', 24).
char_to_int('\031', 25).
char_to_int('\032', 26).
char_to_int('\033', 27).
char_to_int('\034', 28).
char_to_int('\035', 29).
char_to_int('\036', 30).
char_to_int('\037', 31).
char_to_int('\040', 32).
char_to_int('\041', 33).
char_to_int('\042', 34).
char_to_int('\043', 35).
char_to_int('\044', 36).
char_to_int('\045', 37).
char_to_int('\046', 38).
char_to_int('\047', 39).
char_to_int('\050', 40).
char_to_int('\051', 41).
char_to_int('\052', 42).
char_to_int('\053', 43).
char_to_int('\054', 44).
char_to_int('\055', 45).
char_to_int('\056', 46).
char_to_int('\057', 47).
char_to_int('\060', 48).
char_to_int('\061', 49).
char_to_int('\062', 50).
char_to_int('\063', 51).
char_to_int('\064', 52).
char_to_int('\065', 53).
char_to_int('\066', 54).
char_to_int('\067', 55).
char_to_int('\070', 56).
char_to_int('\071', 57).
char_to_int('\072', 58).
char_to_int('\073', 59).
char_to_int('\074', 60).
char_to_int('\075', 61).
char_to_int('\076', 62).
char_to_int('\077', 63).
char_to_int('\100', 64).
char_to_int('\101', 65).
char_to_int('\102', 66).
char_to_int('\103', 67).
char_to_int('\104', 68).
char_to_int('\105', 69).
char_to_int('\106', 70).
char_to_int('\107', 71).
char_to_int('\110', 72).
char_to_int('\111', 73).
char_to_int('\112', 74).
char_to_int('\113', 75).
char_to_int('\114', 76).
char_to_int('\115', 77).
char_to_int('\116', 78).
char_to_int('\117', 79).
char_to_int('\120', 80).
char_to_int('\121', 81).
char_to_int('\122', 82).
char_to_int('\123', 83).
char_to_int('\124', 84).
char_to_int('\125', 85).
char_to_int('\126', 86).
char_to_int('\127', 87).
char_to_int('\130', 88).
char_to_int('\131', 89).
char_to_int('\132', 90).
char_to_int('\133', 91).
char_to_int('\134', 92).
char_to_int('\135', 93).
char_to_int('\136', 94).
char_to_int('\137', 95).
char_to_int('\140', 96).
char_to_int('\141', 97).
char_to_int('\142', 98).
char_to_int('\143', 99).
char_to_int('\144', 100).
char_to_int('\145', 101).
char_to_int('\146', 102).
char_to_int('\147', 103).
char_to_int('\150', 104).
char_to_int('\151', 105).
char_to_int('\152', 106).
char_to_int('\153', 107).
char_to_int('\154', 108).
char_to_int('\155', 109).
char_to_int('\156', 110).
char_to_int('\157', 111).
char_to_int('\160', 112).
char_to_int('\161', 113).
char_to_int('\162', 114).
char_to_int('\163', 115).
char_to_int('\164', 116).
char_to_int('\165', 117).
char_to_int('\166', 118).
char_to_int('\167', 119).
char_to_int('\170', 120).
char_to_int('\171', 121).
char_to_int('\172', 122).
char_to_int('\173', 123).
char_to_int('\174', 124).
char_to_int('\175', 125).
char_to_int('\176', 126).
char_to_int('\177', 127).

% XXX
% NU-Prolog atoms can only include 7-bit ASCII chars.

/***********
char_to_int('\200', 128).
char_to_int('\201', 129).
char_to_int('\202', 130).
char_to_int('\203', 131).
char_to_int('\204', 132).
char_to_int('\205', 133).
char_to_int('\206', 134).
char_to_int('\207', 135).
char_to_int('\210', 136).
char_to_int('\211', 137).
char_to_int('\212', 138).
char_to_int('\213', 139).
char_to_int('\214', 140).
char_to_int('\215', 141).
char_to_int('\216', 142).
char_to_int('\217', 143).
char_to_int('\220', 144).
char_to_int('\221', 145).
char_to_int('\222', 146).
char_to_int('\223', 147).
char_to_int('\224', 148).
char_to_int('\225', 149).
char_to_int('\226', 150).
char_to_int('\227', 151).
char_to_int('\230', 152).
char_to_int('\231', 153).
char_to_int('\232', 154).
char_to_int('\233', 155).
char_to_int('\234', 156).
char_to_int('\235', 157).
char_to_int('\236', 158).
char_to_int('\237', 159).
char_to_int('\240', 160).
char_to_int('\241', 161).
char_to_int('\242', 162).
char_to_int('\243', 163).
char_to_int('\244', 164).
char_to_int('\245', 165).
char_to_int('\246', 166).
char_to_int('\247', 167).
char_to_int('\250', 168).
char_to_int('\251', 169).
char_to_int('\252', 170).
char_to_int('\253', 171).
char_to_int('\254', 172).
char_to_int('\255', 173).
char_to_int('\256', 174).
char_to_int('\257', 175).
char_to_int('\260', 176).
char_to_int('\261', 177).
char_to_int('\262', 178).
char_to_int('\263', 179).
char_to_int('\264', 180).
char_to_int('\265', 181).
char_to_int('\266', 182).
char_to_int('\267', 183).
char_to_int('\270', 184).
char_to_int('\271', 185).
char_to_int('\272', 186).
char_to_int('\273', 187).
char_to_int('\274', 188).
char_to_int('\275', 189).
char_to_int('\276', 190).
char_to_int('\277', 191).
char_to_int('\300', 192).
char_to_int('\301', 193).
char_to_int('\302', 194).
char_to_int('\303', 195).
char_to_int('\304', 196).
char_to_int('\305', 197).
char_to_int('\306', 198).
char_to_int('\307', 199).
char_to_int('\310', 200).
char_to_int('\311', 201).
char_to_int('\312', 202).
char_to_int('\313', 203).
char_to_int('\314', 204).
char_to_int('\315', 205).
char_to_int('\316', 206).
char_to_int('\317', 207).
char_to_int('\320', 208).
char_to_int('\321', 209).
char_to_int('\322', 210).
char_to_int('\323', 211).
char_to_int('\324', 212).
char_to_int('\325', 213).
char_to_int('\326', 214).
char_to_int('\327', 215).
char_to_int('\330', 216).
char_to_int('\331', 217).
char_to_int('\332', 218).
char_to_int('\333', 219).
char_to_int('\334', 220).
char_to_int('\335', 221).
char_to_int('\336', 222).
char_to_int('\337', 223).
char_to_int('\340', 224).
char_to_int('\341', 225).
char_to_int('\342', 226).
char_to_int('\343', 227).
char_to_int('\344', 228).
char_to_int('\345', 229).
char_to_int('\346', 230).
char_to_int('\347', 231).
char_to_int('\350', 232).
char_to_int('\351', 233).
char_to_int('\352', 234).
char_to_int('\353', 235).
char_to_int('\354', 236).
char_to_int('\355', 237).
char_to_int('\356', 238).
char_to_int('\357', 239).
char_to_int('\360', 240).
char_to_int('\361', 241).
char_to_int('\362', 242).
char_to_int('\363', 243).
char_to_int('\364', 244).
char_to_int('\365', 245).
char_to_int('\366', 246).
char_to_int('\367', 247).
char_to_int('\370', 248).
char_to_int('\371', 249).
char_to_int('\372', 250).
char_to_int('\373', 251).
char_to_int('\374', 252).
char_to_int('\375', 253).
char_to_int('\376', 254).
char_to_int('\377', 255).
*********/

%-----------------------------------------------------------------------------%

upper_lower('a', 'A').
upper_lower('b', 'B').
upper_lower('c', 'C').
upper_lower('d', 'D').
upper_lower('e', 'E').
upper_lower('f', 'F').
upper_lower('g', 'G').
upper_lower('h', 'H').
upper_lower('i', 'I').
upper_lower('j', 'J').
upper_lower('k', 'K').
upper_lower('l', 'L').
upper_lower('m', 'M').
upper_lower('n', 'N').
upper_lower('o', 'O').
upper_lower('p', 'P').
upper_lower('q', 'Q').
upper_lower('r', 'R').
upper_lower('s', 'S').
upper_lower('t', 'T').
upper_lower('u', 'U').
upper_lower('v', 'V').
upper_lower('w', 'W').
upper_lower('x', 'X').
upper_lower('y', 'Y').
upper_lower('z', 'Z').

%-----------------------------------------------------------------------------%
