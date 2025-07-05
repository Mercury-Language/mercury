%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Compiling this program with rotd-2025-07-04 results in
% Uncaught Mercury exception:
% Software Error: function `check_hlds.pre_typecheck.arg_num_pieces'/2:
%     Unexpected: ArgNum is not numbered correctly for return value
%---------------------------------------------------------------------------%

:- module bug576.
:- interface.

:- type icu ---> icu.

:- type normalization_form
    --->    nfd
    ;       nfc
    ;       nfkd
    ;       nfkc.

:- func icu_normalize(icu, normalization_form, string) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

icu_normalize(_, Form, !.S) = !:S :-
    (
        Form = nfd
    ;
        Form = nfc
    ;
        Form = nfkd
    ;
        Form = nfkc
    ).

%---------------------------------------------------------------------------%
:- end_module bug576.
%---------------------------------------------------------------------------%
