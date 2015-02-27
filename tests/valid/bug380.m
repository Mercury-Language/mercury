%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug380.
:- interface.

:- pred word(string).
:- mode word(in) is semidet.
:- mode word(out) is multi.

:- implementation.

% :- import_module string.

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
