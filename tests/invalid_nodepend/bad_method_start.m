%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Test the error message for when ':-' occurs at the beginning
% of a type class method declaration.
%---------------------------------------------------------------------------%

:- module bad_method_start.
:- interface.

:- type suppress_warnings ---> suppress_warnings.

:- typeclass provider(Provider) where [
    :- func get_provider_name(Provider) = string
].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
