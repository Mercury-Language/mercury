%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2004 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: conway@cs.mu.oz.au.
%
%---------------------------------------------------------------------------%

:- module unicode.

:- interface.

:- type unicode == int.

:- func ('!') = unicode.
:- func ('"') = unicode.
:- func ('#') = unicode.
:- func ('$') = unicode.
:- func ('%') = unicode.
:- func ('&') = unicode.
:- func ('''') = unicode.
:- func ('(') = unicode.
:- func (')') = unicode.
:- func ('*') = unicode.
:- func ('+') = unicode.
:- func (',') = unicode.
:- func ('-') = unicode.
:- func ('.') = unicode.
:- func ('/') = unicode.
:- func ('0') = unicode.
:- func ('1') = unicode.
:- func ('2') = unicode.
:- func ('3') = unicode.
:- func ('4') = unicode.
:- func ('5') = unicode.
:- func ('6') = unicode.
:- func ('7') = unicode.
:- func ('8') = unicode.
:- func ('9') = unicode.
:- func (':') = unicode.
:- func (';') = unicode.
:- func ('<') = unicode.
:- func ('=') = unicode.
:- func ('>') = unicode.
:- func ('?') = unicode.
:- func ('@') = unicode.
:- func ('A') = unicode.
:- func ('B') = unicode.
:- func ('C') = unicode.
:- func ('D') = unicode.
:- func ('E') = unicode.
:- func ('F') = unicode.
:- func ('G') = unicode.
:- func ('H') = unicode.
:- func ('I') = unicode.
:- func ('J') = unicode.
:- func ('K') = unicode.
:- func ('L') = unicode.
:- func ('M') = unicode.
:- func ('N') = unicode.
:- func ('O') = unicode.
:- func ('P') = unicode.
:- func ('Q') = unicode.
:- func ('R') = unicode.
:- func ('S') = unicode.
:- func ('T') = unicode.
:- func ('U') = unicode.
:- func ('V') = unicode.
:- func ('W') = unicode.
:- func ('X') = unicode.
:- func ('Y') = unicode.
:- func ('Z') = unicode.
:- func ('[') = unicode.
:- func ('\\') = unicode.
:- func (']') = unicode.
:- func ('^') = unicode.
:- func ('_') = unicode.
:- func ('`') = unicode.
:- func ('a') = unicode.
:- func ('b') = unicode.
:- func ('c') = unicode.
:- func ('d') = unicode.
:- func ('e') = unicode.
:- func ('f') = unicode.
:- func ('g') = unicode.
:- func ('h') = unicode.
:- func ('i') = unicode.
:- func ('j') = unicode.
:- func ('k') = unicode.
:- func ('l') = unicode.
:- func ('m') = unicode.
:- func ('n') = unicode.
:- func ('o') = unicode.
:- func ('p') = unicode.
:- func ('q') = unicode.
:- func ('r') = unicode.
:- func ('s') = unicode.
:- func ('t') = unicode.
:- func ('u') = unicode.
:- func ('v') = unicode.
:- func ('w') = unicode.
:- func ('x') = unicode.
:- func ('y') = unicode.
:- func ('z') = unicode.
:- func ('{') = unicode.
:- func ('|') = unicode.
:- func ('}') = unicode.
:- func ('~') = unicode.

:- implementation.

('!') = 0x21.
('"') = 0x22.
('#') = 0x23.
('$') = 0x24.
('%') = 0x25.
('&') = 0x26.
('''') = 0x27.
('(') = 0x28.
(')') = 0x29.
('*') = 0x2A.
('+') = 0x2B.
(',') = 0x2C.
('-') = 0x2D.
('.') = 0x2E.
('/') = 0x2F.
('0') = 0x30.
('1') = 0x31.
('2') = 0x32.
('3') = 0x33.
('4') = 0x34.
('5') = 0x35.
('6') = 0x36.
('7') = 0x37.
('8') = 0x38.
('9') = 0x39.
(':') = 0x3A.
(';') = 0x3B.
('<') = 0x3C.
('=') = 0x3D.
('>') = 0x3E.
('?') = 0x3F.
('@') = 0x40.
('A') = 0x41.
('B') = 0x42.
('C') = 0x43.
('D') = 0x44.
('E') = 0x45.
('F') = 0x46.
('G') = 0x47.
('H') = 0x48.
('I') = 0x49.
('J') = 0x4A.
('K') = 0x4B.
('L') = 0x4C.
('M') = 0x4D.
('N') = 0x4E.
('O') = 0x4F.
('P') = 0x50.
('Q') = 0x51.
('R') = 0x52.
('S') = 0x53.
('T') = 0x54.
('U') = 0x55.
('V') = 0x56.
('W') = 0x57.
('X') = 0x58.
('Y') = 0x59.
('Z') = 0x5A.
('[') = 0x5B.
('\\') = 0x5C.
(']') = 0x5D.
('^') = 0x5E.
('_') = 0x5F.
('`') = 0x60.
('a') = 0x61.
('b') = 0x62.
('c') = 0x63.
('d') = 0x64.
('e') = 0x65.
('f') = 0x66.
('g') = 0x67.
('h') = 0x68.
('i') = 0x69.
('j') = 0x6A.
('k') = 0x6B.
('l') = 0x6C.
('m') = 0x6D.
('n') = 0x6E.
('o') = 0x6F.
('p') = 0x70.
('q') = 0x71.
('r') = 0x72.
('s') = 0x73.
('t') = 0x74.
('u') = 0x75.
('v') = 0x76.
('w') = 0x77.
('x') = 0x78.
('y') = 0x79.
('z') = 0x7A.
('{') = 0x7B.
('|') = 0x7C.
('}') = 0x7D.
('~') = 0x7E.
