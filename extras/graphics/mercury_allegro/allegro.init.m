%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.init.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.init.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- pred install_allegro(system_driver::in, bool::out, io::di, io::uo) is det.
:- pred allegro_init(bool::out, io::di, io::uo) is det.
:- pred allegro_exit(io::di, io::uo) is det.

:- func allegro_id = string.
:- pred allegro_error(string::out, io::di, io::uo) is det.
:- pred allegro_version(int::out, int::out, int::out) is det.
:- func allegro_version_string = string.
:- func allegro_date_string = string.
:- pred allegro_date(int::out, int::out, int::out) is det.
:- func al_id(int, int, int, int) = int.
:- func make_version(int, int, int) = int.

% :- pred os(os_type::out, int::out, int::out, io::di, io::uo) is det.
% :- pred os_multitasking(bool::out, io::di, io::uo) is det.
:- pred allegro_message(string::in, io::di, io::uo) is det.
:- pred set_window_title(string::in, io::di, io::uo) is det.
% :- pred set_close_button_callback((pred (io, io))::(pred (di, uo) is det), io::di, io::uo) is det.
:- pred desktop_color_depth(maybe(int)::out, io::di, io::uo) is det.
:- pred get_desktop_resolution(maybe({int, int})::out, io::di, io::uo) is det.
% :- pred check_cpu(io::di, io::uo) is det.
% :- pred cpu_vendor(string::out, io::di, io::uo) is det.
% :- pred cpu(cpu_family::out, cpu_model::out, io::di, io::uo) is det.
% :- pred cpu_capabilities(list(cpu_capability)::out, io::di, io::uo) is det.

:- type system_driver
    --->    system_autodetect
    ;       system_none.

:- type os_type
    --->    ostype_unknown
    ;       ostype_win3
    ;       ostype_win95
    ;       ostype_win98
    ;       ostype_winme
    ;       ostype_winnt
    ;       ostype_win2000
    ;       ostype_winxp
    ;       ostype_os2
    ;       ostype_warp
    ;       ostype_dosemu
    ;       ostype_opendos
    ;       ostype_linux
    ;       ostype_sunos
    ;       ostype_freebsd
    ;       ostype_netbsd
    ;       ostype_irix
    ;       ostype_darwin
    ;       ostype_qnx
    ;       ostype_unix
    ;       ostype_beos
    ;       ostype_macos
    ;       ostype_macosx.

:- type cpu_family
    --->    unknown
    ;       i386
    ;       i486(cpu_model_i486)
    ;       i586(cpu_model_i586)
    ;       i686(cpu_model_i686)
    ;       itanium
    ;       powerpc(cpu_model_powerpc)
    ;       extended(cpu_model_extended).

:- type cpu_model_i486
    --->    i486dx
    ;       i486dx50
    ;       i486sx
    ;       i487sx
    ;       i486sl
    ;       i486sx2
    ;       i486dx2
    ;       i486dx4.

:- type cpu_model_i586
    --->    pentium
    ;       pentiump54c
    ;       pentiumoverdrive
    ;       pentiumoverdrivedx4
    ;       cyrix
    ;       unknown
    ;       k5
    ;       k6.

:- type cpu_model_i686
    --->    pentiumproa
    ;       pentiumpro
    ;       pentiumiiklamath
    ;       pentiumii
    ;       celeron
    ;       pentiumiiikatmai
    ;       pentiumiiicoppermine
    ;       pentiumiiimobile
    ;       athlon
    ;       duron.

:- type cpu_model_extended
    --->    pentiumiv
    ;       xeon
    ;       athlon64
    ;       opteron.

:- type cpu_model_powerpc
    --->    ppc_601
    ;       ppc_602
    ;       ppc_603
    ;       ppc_603e
    ;       ppc_603ev
    ;       ppc_604
    ;       ppc_604e
    ;       ppc_620
    ;       ppc_750
    ;       ppc_7400
    ;       ppc_7450.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

:- pragma foreign_import_module("C", allegro.util).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    install_allegro(System::in, Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Success = (0 == install_allegro(System, &errno, atexit)) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    allegro_init(Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Success = (0 == allegro_init()) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    allegro_exit(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    allegro_exit();
    IO = IO0;
").

:- pragma foreign_proc("C",
    allegro_id = (ID::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ID = allegro_id;
").

:- pragma foreign_proc("C",
    allegro_error(Error::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Error = allegro_error;
    IO = IO0;
").

:- pragma foreign_proc("C",
    allegro_version(Version::out, Sub::out, WIP::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Version = ALLEGRO_VERSION;
    Sub = ALLEGRO_SUB_VERSION;
    WIP = ALLEGRO_WIP_VERSION;
").

:- pragma foreign_proc("C",
    allegro_version_string = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = (char *)ALLEGRO_VERSION_STR;
").

:- pragma foreign_proc("C",
    allegro_date_string = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = (char *)ALLEGRO_DATE_STR;
").

:- pragma foreign_proc("C",
    allegro_date(Year::out, Month::out, Day::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Year    = ALLEGRO_DATE / 10000;
    Month   = (ALLEGRO_DATE / 100) % 100;
    Day     = ALLEGRO_DATE % 100;
").

:- pragma foreign_proc("C",
    al_id(A::in, B::in, C::in, D::in) = (ID::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ID = AL_ID(A, B, C, D);
").

:- pragma foreign_proc("C",
    make_version(A::in, B::in, C::in) = (Ver::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ver = MAKE_VERSION(A, B, C);
").

:- pragma foreign_proc("C",
    allegro_message(Message::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    allegro_message(""%s"", Message);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_window_title(Title::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_window_title(Title);
    IO = IO0;
").

:- pragma foreign_proc("C",
    desktop_color_depth(MaybeDepth::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    int Depth = desktop_color_depth();
    if (Depth == 0) {
        MaybeDepth = _mal_make_no_int();
    } else {
        MaybeDepth = _mal_make_yes_int(Depth);
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_desktop_resolution(MaybeRes::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    int Width;
    int Height;
    if (get_desktop_resolution(&Width, &Height) == 0) {
        MaybeRes = _mal_make_yes_int_int(Width, Height);
    } else {
        MaybeRes = _mal_make_no_int_int();
    }
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_enum("C", system_driver/0, [
    system_autodetect   - "SYSTEM_AUTODETECT",
    system_none         - "SYSTEM_NONE"
]).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
