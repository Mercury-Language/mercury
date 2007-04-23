%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.timer.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.timer.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type ticker.

:- pred install_timer(bool::out, io::di, io::uo) is det.
:- pred remove_timer(io::di, io::uo) is det.
:- pred install_int(int::in, maybe(ticker)::out, io::di, io::uo) is det.
:- pred install_int_ex(int::in, maybe(ticker)::out, io::di, io::uo) is det.
:- pred remove_int(ticker::in, io::di, io::uo) is det.
:- pred retrace_count(int::out, io::di, io::uo) is det.
:- pred rest(int::in, io::di, io::uo) is det.

:- func secs_to_timer(int) = int.
:- func msec_to_timer(int) = int.
:- func bps_to_timer(int) = int.
:- func bpm_to_timer(int) = int.

:- pred get_ticker(ticker::in, int::out, io::di, io::uo) is det.
:- pred set_ticker(ticker::in, int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #include <allegro/internal/aintern.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    install_timer(Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Success = (0 == install_timer()) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    remove_timer(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    remove_timer();
    _mal_reset_tickers();
    IO = IO0;
").

install_int(Speed, Result, !IO) :-
    install_int_ex(msec_to_timer(Speed), Result, !IO).

install_int_ex(Speed, Result, !IO) :-
    install_ticker(Speed, Index, !IO),
    (if Index = -1 then
        Result = no
    else
        Result = yes(ticker(Index))
    ).

remove_int(ticker(Index), !IO) :-
    remove_ticker(Index, !IO).

:- pragma foreign_proc("C",
    retrace_count(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Get = retrace_count;
    IO = IO0;
").

:- pragma foreign_proc("C",
    rest(Time::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    rest(Time);
    IO = IO0;
").

:- pragma foreign_proc("C",
    secs_to_timer(Secs::in) = (Timer::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Timer = SECS_TO_TIMER(Secs);
").

:- pragma foreign_proc("C",
    msec_to_timer(Msec::in) = (Timer::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Timer = MSEC_TO_TIMER(Msec);
").

:- pragma foreign_proc("C",
    bps_to_timer(BPS::in) = (Timer::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Timer = BPS_TO_TIMER(BPS);
").

:- pragma foreign_proc("C",
    bpm_to_timer(BPM::in) = (Timer::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Timer = BPM_TO_TIMER(BPM);
").

%-----------------------------------------------------------------------------%

:- type ticker ---> ticker(int).

:- pragma foreign_decl("C",
"
    #define NUM_TICKERS    8

    typedef void (*inc_ticker_t)(void);

    /* globals */
    static int ticker_used[NUM_TICKERS];
    static volatile int ticker_count[NUM_TICKERS];
    static const inc_ticker_t inc_ticker_table[NUM_TICKERS];

    /* forward declarations */
    extern void _mal_reset_tickers(void);
").

:- pragma foreign_code("C",
"
    static void inc_ticker_0(void) { ticker_count[0]++; }
    static void inc_ticker_1(void) { ticker_count[1]++; }
    static void inc_ticker_2(void) { ticker_count[2]++; }
    static void inc_ticker_3(void) { ticker_count[3]++; }
    static void inc_ticker_4(void) { ticker_count[4]++; }
    static void inc_ticker_5(void) { ticker_count[5]++; }
    static void inc_ticker_6(void) { ticker_count[6]++; }
    static void inc_ticker_7(void) { ticker_count[7]++; }

    static const inc_ticker_t inc_ticker_table[NUM_TICKERS] = {
        inc_ticker_0,
        inc_ticker_1,
        inc_ticker_2,
        inc_ticker_3,
        inc_ticker_4,
        inc_ticker_5,
        inc_ticker_6,
        inc_ticker_7
    };

    void _mal_reset_tickers(void)
    {
        int i;
        for (i = 0; i < NUM_TICKERS; i++) {
            ticker_used[i] = FALSE;
            ticker_count[i] = 0;
        }
        _remove_exit_func(_mal_reset_tickers);
    }
").

:- pred install_ticker(int::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    install_ticker(Speed::in, Ticker::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    int i;

    Ticker = -1;
    IO = IO0;

    for (i = 0; i < NUM_TICKERS; i++) {
        if (!ticker_used[i]) {
            ticker_count[i] = 0;
            if (0 == install_int_ex(inc_ticker_table[i], Speed)) {
                _add_exit_func(_mal_reset_tickers, ""reset_tickers"");
                ticker_used[i] = TRUE;
                Ticker = i;
                break;
            }
        }
    }
").

:- pred remove_ticker(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    remove_ticker(Index::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    ticker_used[Index] = FALSE;
    remove_int(inc_ticker_table[Index]);
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_ticker(Index::in, Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Get = ticker_count[Index];
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_ticker(Index::in, Set::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    ticker_count[Index] = Set;
    IO = IO0;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
