%------------------------------------------------------------------------------%
% tfer_stm.m
% <lmika@csse.unimelb.edu.au>
% Wed Oct 24 09:51:46 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%------------------------------------------------------------------------------%

:- module tfer_semi.

:- interface.

:- import_module io.
:- import_module list.


:- type account_desc
    --->    account_desc(int, int).

:- type transfer_desc
    --->    transfer_desc(int, int, int).

:- type tfer_desc
    --->    tfer_desc(list(account_desc), list(transfer_desc)).

:- pred main(io::di, io::uo) is cc_multi.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module int.
:- import_module thread.
:- import_module thread.mvar.
:- import_module thread.semaphore.
:- import_module pair.
:- import_module assoc_list.

main(!IO) :-
    io.read(ReadRes, !IO),
    ( 
        ReadRes = ok(TferSpec `with_type` tfer_desc),
        run_it(TferSpec, !IO)
    ;
        ReadRes = error(E, N),
        io.write_string("Read failed: ", !IO),
        io.write_string(E ++ ", " ++ string(N) ++ "\n", !IO)
    ;
        ReadRes = eof
    ).


:- pred make_accounts(account_desc::in, pair(int, mvar(account))::out, 
    io::di, io::uo) is det.

make_accounts(account_desc(ID, Amount), AccountPair, !IO) :-
    new_account(ID, Amount, Account, !IO),
    AccountPair = ID - Account.


:- pred run_it(tfer_desc::in, io::di, io::uo) is cc_multi.

run_it(tfer_desc(AccountDesc, TransferDesc), !IO) :-
    list.map_foldl(make_accounts, AccountDesc, Accounts, !IO),
    spawn_threads(Accounts, TransferDesc, !IO).
    
%-----------------------------------------------------------------------------%

:- pred spawn_threads(list(pair(int, mvar(account)))::in, 
    list(transfer_desc)::in, io::di, io::uo) is cc_multi.

spawn_threads(_, [], !IO).
spawn_threads(Accounts, [Transfer | Transfers], !IO) :-
    Transfer = transfer_desc(From, To, Amount),
    FromAccount = Accounts ^ det_elem(From),
    ToAccount = Accounts ^ det_elem(To),
    thread.spawn(transfer(Amount, From, To, FromAccount, ToAccount), !IO),
    spawn_threads(Accounts, Transfers, !IO).

%------------------------------------------------------------------------------%

:- type account
    --->    account(int, int, semaphore).


:- pred transfer(int::in, int::in, int::in, mvar(account)::in, 
    mvar(account)::in, io::di, io::uo) is cc_multi.

transfer(Amount, IDA, IDB, AccountA, AccountB, !IO) :-
    ( IDA > IDB ->
        lock_account(AccountA, !IO),
        lock_account(AccountB, !IO)
    ;
        lock_account(AccountB, !IO),
        lock_account(AccountA, !IO)
    ),
    withdraw(Amount, AccountA, !IO),
    deposit(Amount, AccountB, !IO),
    ( IDA > IDB ->
        unlock_account(AccountB, !IO),
        unlock_account(AccountA, !IO)
    ;
        unlock_account(AccountA, !IO),
        unlock_account(AccountB, !IO)
    ).


:- pred new_account(int::in, int::in, mvar(account)::out, io::di, 
    io::uo) is det.

new_account(ID, Balance, AccountTVar, !IO) :-
    semaphore.new(Semaphore, !IO),
    semaphore.signal(Semaphore, !IO),
    Account = account(ID, Balance, Semaphore),
    mvar.init(AccountTVar, !IO),
    mvar.put(AccountTVar, Account, !IO).
 
:- pred lock_account(mvar(account)::in, io::di, io::uo) is det.

lock_account(AccountTVar, !IO) :-
    mvar.take(AccountTVar, Account, !IO),
    Account = account(_, _, Semaphore),
    mvar.put(AccountTVar, Account, !IO),
    semaphore.wait(Semaphore, !IO).


:- pred unlock_account(mvar(account)::in, io::di, io::uo) is det.

unlock_account(AccountTVar, !IO) :-
    mvar.take(AccountTVar, Account, !IO),
    Account = account(_, _, Semaphore),
    mvar.put(AccountTVar, Account, !IO),
    semaphore.signal(Semaphore, !IO).

:- pred deposit(int::in, mvar(account)::in, io::di, io::uo) is det.

deposit(Amount, AccountTVar, !Stm) :-
    mvar.take(AccountTVar, Account0, !Stm),
    Account0 = account(ID, Balance0, Semaphore),
    Balance = Balance0 + Amount,
    Account = account(ID, Balance, Semaphore),
    mvar.put(AccountTVar, Account, !Stm).


:- pred withdraw(int::in, mvar(account)::in, io::di, io::uo) is det.

withdraw(Amount, AccountTVar, !Stm) :-
    Amount1 = -Amount,
    deposit(Amount1, AccountTVar, !Stm).


%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
