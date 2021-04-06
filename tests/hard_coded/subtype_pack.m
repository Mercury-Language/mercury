%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that subtype arguments are packed the same as their base types.
%
% To view the type representations are decided, run:
%
%   mmc --make-short-int subtype_pack_2.m
%   mmc --make-int subtype_pack_2.m
%   mmc -C --show-local-type-repns subtype_pack.m
%   cat subtype_pack.type_repns
%
%---------------------------------------------------------------------------%

:- module subtype_pack.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module construct.
:- import_module deconstruct.
:- import_module list.
:- import_module string.
:- import_module type_desc.
:- import_module univ.

:- import_module subtype_pack_2.

:- type enum
    --->    apple
    ;       orange
    ;       lemon.

:- type sub_enum =< enum
    --->    orange
    ;       lemon.

:- type dummy(T)
    --->    dummy.

:- type sub_dummy =< dummy(sub_enum)
    --->    dummy.

:- type notag
    --->    notag(notag0(enum)).

:- type sub_notag =< notag
    --->    notag(sub_notag0(sub_enum)).

:- type notag0(T)
    --->    notag0(T).

:- type sub_notag0(T) =< notag0(T)
    --->    notag0(T).

:- type tagged
    --->    none
    ;       tagged(int).

:- type sub_tagged =< tagged
    --->    tagged(int).

:- type struct(T, U)
    --->    const0
    ;       const1
    ;       struct(
                f0_1    :: enum,            % 2 bits (packed)
                f0_2    :: enum,            % 2 bits (packed)
                f1_0    :: dummy(enum),     % 0 bits
                f1      :: notag,           % word
                f2      :: sub_notag,       % word
                f3      :: tagged,          % word
                f4      :: sub_tagged,      % word
                f5      :: T,               % word
                f6      :: U,               % word
                f7_1    :: sub_abs_enum,    % 3 bits (packed)
                f7_2    :: sub_abs_enum,    % 3 bits (packed)
                f8_0    :: sub_abs_dummy,   % 0 bits
                f8      :: sub_abs_notag    % word
            ).

:- type substruct =< struct(sub_enum, float)
    --->    struct(
                s_f0_1  :: sub_enum,        % differs from base
                s_f0_2  :: sub_enum,        % differs from base
                s_f1_0  :: sub_dummy,       % differs from base
                s_f1    :: sub_notag,       % differs from base
                s_f2    :: sub_notag,
                s_f3    :: sub_tagged,      % differs from base
                s_f4    :: sub_tagged,
                s_f5    :: sub_enum,        % differs from base
                s_f6    :: float,           % differs from base
                s_f7_1  :: sub_abs_enum,
                s_f7_2  :: sub_abs_enum,
                s_f8_0  :: sub_abs_dummy,
                s_f8    :: sub_abs_notag
            ).

main(!IO) :-
    Sub = struct(
            orange,
            lemon,
            dummy,
            notag(notag0(orange)),
            notag(notag0(lemon)),
            tagged(42),
            tagged(4242),
            lemon,
            3.14159,
            make_sub_abs_enum,
            make_sub_abs_enum,
            make_sub_abs_dummy,
            make_sub_abs_notag
        ),
    io.print_line(Sub : substruct, !IO),
    Base = coerce(Sub) : struct(_, _),
    io.print_line(Base, !IO).
