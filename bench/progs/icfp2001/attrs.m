%------------------------------------------------------------------------------%
% attrs.m
% Ralph Becket <rbeck@microsoft.com>
% Thu Jul 26 16:29:28 BST 2001
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
% Attribute records and processing.
%
%------------------------------------------------------------------------------%

:- module attrs.

:- interface.

:- import_module bool, int.



    % XXX Should we have a special inst or type constraining the
    % range for `u' and `size'?
    %
    % XXX Should we override equality for this or stick with the
    % equivalent/2 relation below?
    %
:- type attrs
    --->    attrs(
                b       :: bool,
                em      :: bool,
                i       :: bool,
                s       :: bool,
                tt      :: bool,
                u       :: int,
                size    :: int,
                colour  :: colour
            ).

    % NOTE that root_colour is there to denote the root context colour.
    % It is not accessible from the tags.
    %
:- type colour ---> r ; g ; b ; c ; m ; y ; k ; w ; root_colour.

    % NOTE root_size is a magic constant to denote the root context
    % size.  It is not accessible from the tags.
    %
:- func root_size = int.

    % The maximum level of underlining.
    %
:- func max_u = int.

    % The maximum size.
    %
:- func max_size = int.

    % Decide whether two attribute records are equivalent for
    % printing characters.
    %
:- pred equivalent(attrs, attrs).
:- mode equivalent(in, in) is semidet.

    % Decide whether two attribute records are equivalent where
    % whitespace is concerned.
    %
:- pred equivalent_for_space(attrs, attrs).
:- mode equivalent_for_space(in, in) is semidet.

    % Convert an attribute record for interpretation with whitespace.
    %
:- func adjust_for_space(attrs) = attrs.

    % Default value for an attribute record.
    %
:- func root_attrs = attrs.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.



root_size = -1.

max_u = 3.

max_size = 9.

%------------------------------------------------------------------------------%

equivalent(A, B) :-
    A ^ b               =               B ^ b,
    A ^ i               =               B ^ i,
    A ^ s               =               B ^ s,
    ( A ^ s = no => A ^ em = B ^ em ),
    A ^ tt              =               B ^ tt,
    A ^ size            =               B ^ size,
    min(max_u, A ^ u)   = min(max_u, B ^ u) `with_type` int,
    A ^ colour          =               B ^ colour.

%------------------------------------------------------------------------------%

    % Identical attrs in a TT context are considered different.
    %
equivalent_for_space(A, B) :-
    A ^ tt              =               no,
    B ^ tt              =               no,
    A ^ size            =               B ^ size,
    min(max_u, A ^ u)   = min(max_u, B ^ u) `with_type` int,
    % not ( A ^ u \= 0, A ^ colour = B ^ colour ).
    ( A ^ u > 0 => A ^ colour = B ^ colour ).

%------------------------------------------------------------------------------%

adjust_for_space(Attrs) =
    ((((( Attrs
                ^ s      := no )
                ^ em     := no )
                ^ i      := no )
                ^ b      := no )
                ^ colour := ( if Attrs ^ u = 0 then w else Attrs ^ colour ) ).

%------------------------------------------------------------------------------%

root_attrs =
    attrs(
        /* b       = */ no,
        /* em      = */ no,
        /* i       = */ no,
        /* s       = */ no,
        /* tt      = */ no,
        /* u       = */ 0,
        /* size    = */ root_size,
        /* colour  = */ root_colour
    ).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
