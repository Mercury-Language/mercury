% Test conversion of if-then-elses to disjunctions.
% The Mercury compiler of 4/8/1999 did not wrap a commit around
% the negated condition in the else branch, causing an abort.
:- module ite_to_disj.

:- interface.

:- import_module aditi.

:- pred footy_current_name(aditi__state, string, string).
:- mode footy_current_name(aditi__aditi_mui, in, out) is multi.
:- pragma aditi(footy_current_name/3).

:- implementation.

footy_current_name(DB, Name, CurrentName) :-
        (
            footy_alias(DB, Name, NewName)
        ->
            CurrentName = NewName
        ;
            CurrentName = Name
        ).

% footy_alias(DB, OldTeamName, NewTeamName)
:- pred footy_alias(aditi__state, string, string).
:- mode footy_alias(aditi__aditi_mui, out, out) is nondet.
:- pragma base_relation(footy_alias/3).
:- pragma aditi_index(footy_alias/3,unique_B_tree,[2]).
% relation name: $USER/footy/footy_alias__3

