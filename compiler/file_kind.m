%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015-2011 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: file_kind.m.
% Main author: zs.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.file_kind.
:- interface.

%-----------------------------------------------------------------------------%
%
% The different kinds of files that the frontend of the Mercury compiler
% deals with:
%
% - source files,
% - automatically generated interface files, and
% - automatically generated optimization files.
%

:- type file_kind
    --->    fk_src
    ;       fk_int(int_file_kind)
    ;       fk_opt(opt_file_kind).

:- type int_or_opt_file_kind
    --->    iofk_int(int_file_kind)
    ;       iofk_opt(opt_file_kind).

:- type src_file_kind
    --->    sfk_src.

:- type int_file_kind
    --->    ifk_int0
    ;       ifk_int1
    ;       ifk_int2
    ;       ifk_int3.

:- type opt_file_kind
    --->    ofk_opt
    ;       ofk_trans_opt.

:- func file_kind_to_extension(file_kind) = string.
:- func int_file_kind_to_extension(int_file_kind) = string.
:- func opt_file_kind_to_extension(opt_file_kind) = string.

:- pred extension_to_file_kind(string::in, file_kind::out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

file_kind_to_extension(fk_src) = ".m".
file_kind_to_extension(fk_int(IntFileKind)) =
    int_file_kind_to_extension(IntFileKind).
file_kind_to_extension(fk_opt(OptFileKind)) =
    opt_file_kind_to_extension(OptFileKind).

int_file_kind_to_extension(ifk_int0) = ".int0".
int_file_kind_to_extension(ifk_int2) = ".int2".
int_file_kind_to_extension(ifk_int3) = ".int3".
int_file_kind_to_extension(ifk_int1) = ".int".

opt_file_kind_to_extension(ofk_opt) = ".opt".
opt_file_kind_to_extension(ofk_trans_opt) = ".trans_opt".

extension_to_file_kind(Extension, FileKind) :-
    (
        Extension = ".m",
        FileKind = fk_src
    ;
        Extension = ".int0",
        FileKind = fk_int(ifk_int0)
    ;
        Extension = ".int3",
        FileKind = fk_int(ifk_int3)
    ;
        Extension = ".int2",
        FileKind = fk_int(ifk_int2)
    ;
        Extension = ".int",
        FileKind = fk_int(ifk_int1)
    ;
        Extension = ".opt",
        FileKind = fk_opt(ofk_opt)
    ;
        Extension = ".trans_opt",
        FileKind = fk_opt(ofk_trans_opt)
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.file_kind.
%-----------------------------------------------------------------------------%
