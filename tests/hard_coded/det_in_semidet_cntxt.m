%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
% The Mercury compiler dated Wed Jul 9th 1997
% got an internal error for this test case.

:- module det_in_semidet_cntxt.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

:- type name    ==  string.
:- type sympton ==  string.
:- type vaccine ==  string.

:- type disease
    --->    d(name, list(sympton), list(vaccine)).

main -->
    read_diseases(_Diseases).

%---------------------------------------------------------------------------%

    % read_diseases(D)
    %
    % D is the list of diseases.
    %
:- pred read_diseases(list(disease), io, io).
:- mode read_diseases(out, di, uo) is det.

read_diseases(D) -->
    read_disease(MaybeDisease),
    (
        { MaybeDisease = yes(Disease) },
        read_diseases(Diseases),
        { D = [Disease | Diseases] }
    ;
        { MaybeDisease = no },
        { D = [] }
    ).

:- pred read_disease(maybe(disease), io, io).
:- mode read_disease(out, di, uo) is det.

read_disease(MaybeDisease) -->
    read_name(MaybeName),
    (
        { MaybeName = yes(Name) },
        read_symptons(Symptons),
        read_vaccines(Vaccines),
        { MaybeDisease = yes(d(Name, Symptons, Vaccines)) }
    ;
        { MaybeName = no },
        { MaybeDisease = no }
    ).

:- pred read_name(maybe(string)::out, io::di, io::uo) is det.

read_name(MaybeName) -->
    io.read_word(Result),
    (
        { Result = ok(Name0) },
        { string.from_char_list(Name0, Name) },
        { MaybeName = yes(Name) }
    ;
        { Result = eof },
        { MaybeName = no }
    ;
        { Result = error(Err) },
        { io.error_message(Err, ErrStr) },
        { error(ErrStr) }
    ).

:- pred read_symptons(list(string)::out, io::di, io::uo) is det.
read_symptons([]) --> [].

:- pred read_vaccines(list(string)::out, io::di, io::uo) is det.
read_vaccines([]) --> [].

%---------------------------------------------------------------------------%
