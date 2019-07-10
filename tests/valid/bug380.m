%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The bug was that mercury_string.h was #included in the generated C file
% only if the module being compiled imported the string module, but
% despite this, the compiler could generate references to macros
% defined in that file. 
%
%---------------------------------------------------------------------------%

:- module bug380.
:- interface.

:- pred word(string).
:- mode word(in) is semidet.
:- mode word(out) is multi.

:- implementation.

word("aback").
word("abaft").
word("abandon").
word("abandoned").
word("abandoning").
word("abandonment").
word("abandons").
word("abase").
word("abased").
word("abasement").
word("abasements").
word("abases").
word("abash").
word("abashed").
word("abashes").
word("abashing").
word("abasing").
word("abate").
word("abated").
word("abatement").
word("abatements").
word("abater").
word("abates").
word("abating").
word("abbe").
word("abbey").
