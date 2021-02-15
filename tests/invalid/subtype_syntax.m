%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_syntax.
:- interface.

:- type s =< t.             % =< is reserved
:- type S =< T.             % =< is reserved

:- type s =< 123            % supertype must be type
    --->    s.

:- type s =< T              % supertype cannot be var
    --->    s.

:- type s =< t(123)         % supertype args must be types
    --->    s.

:- type 123 =< t            % subtype must be symbol
    --->    1               % constructors must be symbols
    ;       Two.

:- type T =< t              % subtype must be symbol
    --->    s.

:- type s(int) =< t         % subtype args must be variables
    --->    s.

:- type s(T, T) =< t        % subtype vars must be distinct
    --->    s.

:- type s(T) =< t(U, _V)    % vars in supertype must not be free
    --->    s.

:- type s =< t
    --->    s(U).           % vars in body must not be free

%---------------------------------------------------------------------------%

:- type dummy_export
    --->    dummy_export.
