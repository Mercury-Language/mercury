%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury Team

:- module bug414.

:- interface.

:- import_module string.

:- type loader.

:- func prefix_locator(loader) = string.

:- implementation.

:- type loader
    --->    loader(
                loader_prefix           :: string,
                loader_replay_prefix    :: string,
                loader_replay_options   :: replay_options
            ).

:- type replay_options
    --->    replay_none
    ;       replay_save(string)
    ;       replay_load(string).

prefix_locator(Loader) = Prefix :-
    % Mercury is unable to properly detect the switch on Replay.
    % It works if the a single deconstruction of Loader is made outside
    % the switch and unfication-asignments inside the switch.
    Replay = Loader ^ loader_replay_options,
    (
        ( Replay = replay_none
        ; Replay = replay_save(_)
        ),
        Prefix = Loader ^ loader_prefix
    ;
        Replay = replay_load(_),
        Prefix = Loader ^ loader_replay_prefix
    ).
