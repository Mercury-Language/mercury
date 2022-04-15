%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: conway@cs.mu.oz.au.
%
%---------------------------------------------------------------------------%

:- module xml.parse.chars.
:- interface.

:- import_module parsing.
:- import_module unicode.

:- pred letter(pstate(_), pstate(unicode)).
:- mode letter(in, out) is det.

:- pred baseChar(pstate(T1), pstate(unicode)).
:- mode baseChar(pdi, puo) is det.

:- pred ideographic(pstate(T1), pstate(unicode)).
:- mode ideographic(pdi, puo) is det.

:- pred combiningChar(pstate(T1), pstate(unicode)).
:- mode combiningChar(pdi, puo) is det.

:- pred digit(pstate(_), pstate(unicode)).
:- mode digit(in, out) is det.

:- pred extender(pstate(T1), pstate(unicode)).
:- mode extender(pdi, puo) is det.

:- implementation.

%B. Character Classes
%
%   Following the characteristics defined in the Unicode standard,
%   characters are classed as base characters (among others, these contain
%   the alphabetic characters of the Latin alphabet, without diacritics),
%   ideographic characters, and combining characters (among others, this
%   class contains most diacritics); these classes combine to form the
%   class of letters. Digits and extenders are also distinguished.
%
%   Characters
%   [84]  Letter ::= BaseChar | Ideographic

letter -->
    baseChar or ideographic.

%   [85]  BaseChar ::= [#x0041-#x005A] | [#x0061-#x007A] | [#x00C0-#x00D6]
%   | [#x00D8-#x00F6] | [#x00F8-#x00FF] | [#x0100-#x0131]
%   | [#x0134-#x013E] | [#x0141-#x0148] | [#x014A-#x017E]
%   | [#x0180-#x01C3] | [#x01CD-#x01F0] | [#x01F4-#x01F5]
%   | [#x01FA-#x0217] | [#x0250-#x02A8] | [#x02BB-#x02C1] | #x0386
%   | [#x0388-#x038A] | #x038C | [#x038E-#x03A1] | [#x03A3-#x03CE]
%   | [#x03D0-#x03D6] | #x03DA | #x03DC | #x03DE | #x03E0
%   | [#x03E2-#x03F3] | [#x0401-#x040C] | [#x040E-#x044F]
%   | [#x0451-#x045C] | [#x045E-#x0481] | [#x0490-#x04C4]
%   | [#x04C7-#x04C8] | [#x04CB-#x04CC] | [#x04D0-#x04EB]
%   | [#x04EE-#x04F5] | [#x04F8-#x04F9] | [#x0531-#x0556] | #x0559
%   | [#x0561-#x0586] | [#x05D0-#x05EA] | [#x05F0-#x05F2]
%   | [#x0621-#x063A] | [#x0641-#x064A] | [#x0671-#x06B7]
%   | [#x06BA-#x06BE] | [#x06C0-#x06CE] | [#x06D0-#x06D3] | #x06D5
%   | [#x06E5-#x06E6] | [#x0905-#x0939] | #x093D | [#x0958-#x0961]
%   | [#x0985-#x098C] | [#x098F-#x0990] | [#x0993-#x09A8]
%   | [#x09AA-#x09B0] | #x09B2 | [#x09B6-#x09B9] | [#x09DC-#x09DD]
%   | [#x09DF-#x09E1] | [#x09F0-#x09F1] | [#x0A05-#x0A0A]
%   | [#x0A0F-#x0A10] | [#x0A13-#x0A28] | [#x0A2A-#x0A30]
%   | [#x0A32-#x0A33] | [#x0A35-#x0A36] | [#x0A38-#x0A39]
%   | [#x0A59-#x0A5C] | #x0A5E | [#x0A72-#x0A74] | [#x0A85-#x0A8B]
%   | #x0A8D | [#x0A8F-#x0A91] | [#x0A93-#x0AA8] | [#x0AAA-#x0AB0]
%   | [#x0AB2-#x0AB3] | [#x0AB5-#x0AB9] | #x0ABD | #x0AE0
%   | [#x0B05-#x0B0C] | [#x0B0F-#x0B10] | [#x0B13-#x0B28]
%   | [#x0B2A-#x0B30] | [#x0B32-#x0B33] | [#x0B36-#x0B39] | #x0B3D
%   | [#x0B5C-#x0B5D] | [#x0B5F-#x0B61] | [#x0B85-#x0B8A]
%   | [#x0B8E-#x0B90] | [#x0B92-#x0B95] | [#x0B99-#x0B9A] | #x0B9C
%   | [#x0B9E-#x0B9F] | [#x0BA3-#x0BA4] | [#x0BA8-#x0BAA]
%   | [#x0BAE-#x0BB5] | [#x0BB7-#x0BB9] | [#x0C05-#x0C0C]
%   | [#x0C0E-#x0C10] | [#x0C12-#x0C28] | [#x0C2A-#x0C33]
%   | [#x0C35-#x0C39] | [#x0C60-#x0C61] | [#x0C85-#x0C8C]
%   | [#x0C8E-#x0C90] | [#x0C92-#x0CA8] | [#x0CAA-#x0CB3]
%   | [#x0CB5-#x0CB9] | #x0CDE | [#x0CE0-#x0CE1] | [#x0D05-#x0D0C]
%   | [#x0D0E-#x0D10] | [#x0D12-#x0D28] | [#x0D2A-#x0D39]
%   | [#x0D60-#x0D61] | [#x0E01-#x0E2E] | #x0E30 | [#x0E32-#x0E33]
%   | [#x0E40-#x0E45] | [#x0E81-#x0E82] | #x0E84 | [#x0E87-#x0E88]
%   | #x0E8A | #x0E8D | [#x0E94-#x0E97] | [#x0E99-#x0E9F]
%   | [#x0EA1-#x0EA3] | #x0EA5 | #x0EA7 | [#x0EAA-#x0EAB]
%   | [#x0EAD-#x0EAE] | #x0EB0 | [#x0EB2-#x0EB3] | #x0EBD
%   | [#x0EC0-#x0EC4] | [#x0F40-#x0F47] | [#x0F49-#x0F69]
%   | [#x10A0-#x10C5] | [#x10D0-#x10F6] | #x1100 | [#x1102-#x1103]
%   | [#x1105-#x1107] | #x1109 | [#x110B-#x110C] | [#x110E-#x1112]
%   | #x113C | #x113E | #x1140 | #x114C | #x114E | #x1150
%   | [#x1154-#x1155] | #x1159 | [#x115F-#x1161] | #x1163 | #x1165
%   | #x1167 | #x1169 | [#x116D-#x116E] | [#x1172-#x1173] | #x1175
%   | #x119E | #x11A8 | #x11AB | [#x11AE-#x11AF] | [#x11B7-#x11B8]
%   | #x11BA | [#x11BC-#x11C2] | #x11EB | #x11F0 | #x11F9
%   | [#x1E00-#x1E9B] | [#x1EA0-#x1EF9] | [#x1F00-#x1F15]
%   | [#x1F18-#x1F1D] | [#x1F20-#x1F45] | [#x1F48-#x1F4D]
%   | [#x1F50-#x1F57] | #x1F59 | #x1F5B | #x1F5D | [#x1F5F-#x1F7D]
%   | [#x1F80-#x1FB4] | [#x1FB6-#x1FBC] | #x1FBE | [#x1FC2-#x1FC4]
%   | [#x1FC6-#x1FCC] | [#x1FD0-#x1FD3] | [#x1FD6-#x1FDB]
%   | [#x1FE0-#x1FEC] | [#x1FF2-#x1FF4] | [#x1FF6-#x1FFC] | #x2126
%   | [#x212A-#x212B] | #x212E | [#x2180-#x2182] | [#x3041-#x3094]
%   | [#x30A1-#x30FA] | [#x3105-#x312C] | [#xAC00-#xD7A3]

:- type nil ---> nil.

baseChar -->
    return(nil),
    (0x0041-0x005A) or (0x0061-0x007A) or (0x00C0-0x00D6)
    or (0x00D8-0x00F6) or (0x00F8-0x00FF) or (0x0100-0x0131)
    or (0x0134-0x013E) or (0x0141-0x0148) or (0x014A-0x017E)
    or (0x0180-0x01C3) or (0x01CD-0x01F0) or (0x01F4-0x01F5)
    or (0x01FA-0x0217) or (0x0250-0x02A8) or (0x02BB-0x02C1) or lit1(0x0386)
    or (0x0388-0x038A) or lit1(0x038C) or (0x038E-0x03A1) or (0x03A3-0x03CE)
    or (0x03D0-0x03D6) or lit1(0x03DA) or lit1(0x03DC) or lit1(0x03DE)
    or lit1(0x03E0)
    or (0x03E2-0x03F3) or (0x0401-0x040C) or (0x040E-0x044F)
    or (0x0451-0x045C) or (0x045E-0x0481) or (0x0490-0x04C4)
    or (0x04C7-0x04C8) or (0x04CB-0x04CC) or (0x04D0-0x04EB)
    or (0x04EE-0x04F5) or (0x04F8-0x04F9) or (0x0531-0x0556) or lit1(0x0559)
    or (0x0561-0x0586) or (0x05D0-0x05EA) or (0x05F0-0x05F2)
    or (0x0621-0x063A) or (0x0641-0x064A) or (0x0671-0x06B7)
    or (0x06BA-0x06BE) or (0x06C0-0x06CE) or (0x06D0-0x06D3) or lit1(0x06D5)
    or (0x06E5-0x06E6) or (0x0905-0x0939) or lit1(0x093D) or (0x0958-0x0961)
    or (0x0985-0x098C) or (0x098F-0x0990) or (0x0993-0x09A8)
    or (0x09AA-0x09B0) or lit1(0x09B2) or (0x09B6-0x09B9) or (0x09DC-0x09DD)
    or (0x09DF-0x09E1) or (0x09F0-0x09F1) or (0x0A05-0x0A0A)
    or (0x0A0F-0x0A10) or (0x0A13-0x0A28) or (0x0A2A-0x0A30)
    or (0x0A32-0x0A33) or (0x0A35-0x0A36) or (0x0A38-0x0A39)
    or (0x0A59-0x0A5C) or lit1(0x0A5E) or (0x0A72-0x0A74) or (0x0A85-0x0A8B)
    or lit1(0x0A8D) or (0x0A8F-0x0A91) or (0x0A93-0x0AA8) or (0x0AAA-0x0AB0)
    or (0x0AB2-0x0AB3) or (0x0AB5-0x0AB9) or lit1(0x0ABD) or lit1(0x0AE0)
    or (0x0B05-0x0B0C) or (0x0B0F-0x0B10) or (0x0B13-0x0B28)
    or (0x0B2A-0x0B30) or (0x0B32-0x0B33) or (0x0B36-0x0B39) or lit1(0x0B3D)
    or (0x0B5C-0x0B5D) or (0x0B5F-0x0B61) or (0x0B85-0x0B8A)
    or (0x0B8E-0x0B90) or (0x0B92-0x0B95) or (0x0B99-0x0B9A) or lit1(0x0B9C)
    or (0x0B9E-0x0B9F) or (0x0BA3-0x0BA4) or (0x0BA8-0x0BAA)
    or (0x0BAE-0x0BB5) or (0x0BB7-0x0BB9) or (0x0C05-0x0C0C)
    or (0x0C0E-0x0C10) or (0x0C12-0x0C28) or (0x0C2A-0x0C33)
    or (0x0C35-0x0C39) or (0x0C60-0x0C61) or (0x0C85-0x0C8C)
    or (0x0C8E-0x0C90) or (0x0C92-0x0CA8) or (0x0CAA-0x0CB3)
    or (0x0CB5-0x0CB9) or lit1(0x0CDE) or (0x0CE0-0x0CE1) or (0x0D05-0x0D0C)
    or (0x0D0E-0x0D10) or (0x0D12-0x0D28) or (0x0D2A-0x0D39)
    or (0x0D60-0x0D61) or (0x0E01-0x0E2E) or lit1(0x0E30) or (0x0E32-0x0E33)
    or (0x0E40-0x0E45) or (0x0E81-0x0E82) or lit1(0x0E84) or (0x0E87-0x0E88)
    or lit1(0x0E8A) or lit1(0x0E8D) or (0x0E94-0x0E97) or (0x0E99-0x0E9F)
    or (0x0EA1-0x0EA3) or lit1(0x0EA5) or lit1(0x0EA7) or (0x0EAA-0x0EAB)
    or (0x0EAD-0x0EAE) or lit1(0x0EB0) or (0x0EB2-0x0EB3) or lit1(0x0EBD)
    or (0x0EC0-0x0EC4) or (0x0F40-0x0F47) or (0x0F49-0x0F69)
    or (0x10A0-0x10C5) or (0x10D0-0x10F6) or lit1(0x1100) or (0x1102-0x1103)
    or (0x1105-0x1107) or lit1(0x1109) or (0x110B-0x110C) or (0x110E-0x1112)
    or lit1(0x113C) or lit1(0x113E) or lit1(0x1140) or lit1(0x114C)
    or lit1(0x114E) or lit1(0x1150)
    or (0x1154-0x1155) or lit1(0x1159) or (0x115F-0x1161) or lit1(0x1163)
    or lit1(0x1165)
    or lit1(0x1167) or lit1(0x1169) or (0x116D-0x116E) or (0x1172-0x1173)
    or lit1(0x1175)
    or lit1(0x119E) or lit1(0x11A8) or lit1(0x11AB) or (0x11AE-0x11AF)
    or (0x11B7-0x11B8)
    or lit1(0x11BA) or (0x11BC-0x11C2) or lit1(0x11EB) or lit1(0x11F0)
    or lit1(0x11F9)
    or (0x1E00-0x1E9B) or (0x1EA0-0x1EF9) or (0x1F00-0x1F15)
    or (0x1F18-0x1F1D) or (0x1F20-0x1F45) or (0x1F48-0x1F4D)
    or (0x1F50-0x1F57) or lit1(0x1F59) or lit1(0x1F5B) or lit1(0x1F5D)
    or (0x1F5F-0x1F7D)
    or (0x1F80-0x1FB4) or (0x1FB6-0x1FBC) or lit1(0x1FBE) or (0x1FC2-0x1FC4)
    or (0x1FC6-0x1FCC) or (0x1FD0-0x1FD3) or (0x1FD6-0x1FDB)
    or (0x1FE0-0x1FEC) or (0x1FF2-0x1FF4) or (0x1FF6-0x1FFC) or lit1(0x2126)
    or (0x212A-0x212B) or lit1(0x212E) or (0x2180-0x2182) or (0x3041-0x3094)
    or (0x30A1-0x30FA) or (0x3105-0x312C) or (0xAC00-0xD7A3).

%   [86]  Ideographic ::= [#x4E00-#x9FA5] | #x3007 | [#x3021-#x3029]

ideographic -->
    return(nil),
    (0x4E00-0x9FA5) or lit1(0x3007) or (0x3021-0x3029).

%   [87]  CombiningChar ::= [#x0300-#x0345] | [#x0360-#x0361]
%   | [#x0483-#x0486] | [#x0591-#x05A1] | [#x05A3-#x05B9]
%   | [#x05BB-#x05BD] | #x05BF | [#x05C1-#x05C2] | #x05C4
%   | [#x064B-#x0652] | #x0670 | [#x06D6-#x06DC] | [#x06DD-#x06DF]
%   | [#x06E0-#x06E4] | [#x06E7-#x06E8] | [#x06EA-#x06ED]
%   | [#x0901-#x0903] | #x093C | [#x093E-#x094C] | #x094D
%   | [#x0951-#x0954] | [#x0962-#x0963] | [#x0981-#x0983] | #x09BC
%   | #x09BE | #x09BF | [#x09C0-#x09C4] | [#x09C7-#x09C8]
%   | [#x09CB-#x09CD] | #x09D7 | [#x09E2-#x09E3] | #x0A02 | #x0A3C
%   | #x0A3E | #x0A3F | [#x0A40-#x0A42] | [#x0A47-#x0A48]
%   | [#x0A4B-#x0A4D] | [#x0A70-#x0A71] | [#x0A81-#x0A83] | #x0ABC
%   | [#x0ABE-#x0AC5] | [#x0AC7-#x0AC9] | [#x0ACB-#x0ACD]
%   | [#x0B01-#x0B03] | #x0B3C | [#x0B3E-#x0B43] | [#x0B47-#x0B48]
%   | [#x0B4B-#x0B4D] | [#x0B56-#x0B57] | [#x0B82-#x0B83]
%   | [#x0BBE-#x0BC2] | [#x0BC6-#x0BC8] | [#x0BCA-#x0BCD] | #x0BD7
%   | [#x0C01-#x0C03] | [#x0C3E-#x0C44] | [#x0C46-#x0C48]
%   | [#x0C4A-#x0C4D] | [#x0C55-#x0C56] | [#x0C82-#x0C83]
%   | [#x0CBE-#x0CC4] | [#x0CC6-#x0CC8] | [#x0CCA-#x0CCD]
%   | [#x0CD5-#x0CD6] | [#x0D02-#x0D03] | [#x0D3E-#x0D43]
%   | [#x0D46-#x0D48] | [#x0D4A-#x0D4D] | #x0D57 | #x0E31
%   | [#x0E34-#x0E3A] | [#x0E47-#x0E4E] | #x0EB1 | [#x0EB4-#x0EB9]
%   | [#x0EBB-#x0EBC] | [#x0EC8-#x0ECD] | [#x0F18-#x0F19] | #x0F35
%   | #x0F37 | #x0F39 | #x0F3E | #x0F3F | [#x0F71-#x0F84]
%   | [#x0F86-#x0F8B] | [#x0F90-#x0F95] | #x0F97 | [#x0F99-#x0FAD]
%   | [#x0FB1-#x0FB7] | #x0FB9 | [#x20D0-#x20DC] | #x20E1
%   | [#x302A-#x302F] | #x3099 | #x309A

combiningChar -->
    return(nil),
    (0x0300-0x0345) or (0x0360-0x0361)
    or (0x0483-0x0486) or (0x0591-0x05A1) or (0x05A3-0x05B9)
    or (0x05BB-0x05BD) or lit1(0x05BF) or (0x05C1-0x05C2) or lit1(0x05C4)
    or (0x064B-0x0652) or lit1(0x0670) or (0x06D6-0x06DC) or (0x06DD-0x06DF)
    or (0x06E0-0x06E4) or (0x06E7-0x06E8) or (0x06EA-0x06ED)
    or (0x0901-0x0903) or lit1(0x093C) or (0x093E-0x094C) or lit1(0x094D)
    or (0x0951-0x0954) or (0x0962-0x0963) or (0x0981-0x0983) or lit1(0x09BC)
    or lit1(0x09BE) or lit1(0x09BF) or (0x09C0-0x09C4) or (0x09C7-0x09C8)
    or (0x09CB-0x09CD) or lit1(0x09D7) or (0x09E2-0x09E3) or lit1(0x0A02)
    or lit1(0x0A3C)
    or lit1(0x0A3E) or lit1(0x0A3F) or (0x0A40-0x0A42) or (0x0A47-0x0A48)
    or (0x0A4B-0x0A4D) or (0x0A70-0x0A71) or (0x0A81-0x0A83) or lit1(0x0ABC)
    or (0x0ABE-0x0AC5) or (0x0AC7-0x0AC9) or (0x0ACB-0x0ACD)
    or (0x0B01-0x0B03) or lit1(0x0B3C) or (0x0B3E-0x0B43) or (0x0B47-0x0B48)
    or (0x0B4B-0x0B4D) or (0x0B56-0x0B57) or (0x0B82-0x0B83)
    or (0x0BBE-0x0BC2) or (0x0BC6-0x0BC8) or (0x0BCA-0x0BCD) or lit1(0x0BD7)
    or (0x0C01-0x0C03) or (0x0C3E-0x0C44) or (0x0C46-0x0C48)
    or (0x0C4A-0x0C4D) or (0x0C55-0x0C56) or (0x0C82-0x0C83)
    or (0x0CBE-0x0CC4) or (0x0CC6-0x0CC8) or (0x0CCA-0x0CCD)
    or (0x0CD5-0x0CD6) or (0x0D02-0x0D03) or (0x0D3E-0x0D43)
    or (0x0D46-0x0D48) or (0x0D4A-0x0D4D) or lit1(0x0D57) or lit1(0x0E31)
    or (0x0E34-0x0E3A) or (0x0E47-0x0E4E) or lit1(0x0EB1) or (0x0EB4-0x0EB9)
    or (0x0EBB-0x0EBC) or (0x0EC8-0x0ECD) or (0x0F18-0x0F19) or lit1(0x0F35)
    or lit1(0x0F37) or lit1(0x0F39) or lit1(0x0F3E) or lit1(0x0F3F)
    or (0x0F71-0x0F84)
    or (0x0F86-0x0F8B) or (0x0F90-0x0F95) or lit1(0x0F97) or (0x0F99-0x0FAD)
    or (0x0FB1-0x0FB7) or lit1(0x0FB9) or (0x20D0-0x20DC) or lit1(0x20E1)
    or (0x302A-0x302F) or lit1(0x3099) or lit1(0x309A).

%   [88]  Digit ::= [#x0030-#x0039] | [#x0660-#x0669] | [#x06F0-#x06F9]
%   | [#x0966-#x096F] | [#x09E6-#x09EF] | [#x0A66-#x0A6F]
%   | [#x0AE6-#x0AEF] | [#x0B66-#x0B6F] | [#x0BE7-#x0BEF]
%   | [#x0C66-#x0C6F] | [#x0CE6-#x0CEF] | [#x0D66-#x0D6F]
%   | [#x0E50-#x0E59] | [#x0ED0-#x0ED9] | [#x0F20-#x0F29]

digit -->
    return(nil),
    (0x0030-0x0039) or (0x0660-0x0669) or (0x06f0-0x06f9)
    or (0x0966-0x096f) or (0x09e6-0x09ef) or (0x0a66-0x0a6f)
    or (0x0ae6-0x0aef) or (0x0b66-0x0b6f) or (0x0be7-0x0bef)
    or (0x0c66-0x0c6f) or (0x0ce6-0x0cef) or (0x0d66-0x0d6f)
    or (0x0e50-0x0e59) or (0x0ed0-0x0ed9) or (0x0f20-0x0f29).

%   [89]  extender ::= #x00b7 | #x02d0 | #x02d1 | #x0387 | #x0640 | #x0e46
%   | #x0ec6 | #x3005 | [#x3031-#x3035] | [#x309d-#x309e]
%   | [#x30fc-#x30fe]

extender -->
    return(nil),
    lit1(0x00b7) or lit1(0x02d0) or lit1(0x02d1) or lit1(0x0387)
    or lit1(0x0640) or lit1(0x0e46)
    or lit1(0x0ec6) or lit1(0x3005) or (0x3031-0x3035) or (0x309d-0x309e)
    or (0x30fc-0x30fe).

