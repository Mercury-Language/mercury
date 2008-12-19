%------------------------------------------------------------------------------%
% tfer_stm.m
% <lmika@csse.unimelb.edu.au>
% Wed Oct 24 09:51:46 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%------------------------------------------------------------------------------%

:- module tfer_stm.

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

:- import_module stm_builtin.
:- import_module exception.
:- import_module univ.
:- import_module string.
:- import_module int.
:- import_module thread.
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


:- pred make_accounts(account_desc::in, pair(int, stm_var(account))::out, 
    io::di, io::uo) is det.

make_accounts(account_desc(ID, Amount), AccountPair, !IO) :-
    new_account(ID, Amount, Account, !IO),
    AccountPair = ID - Account.


:- pred run_it(tfer_desc::in, io::di, io::uo) is cc_multi.

run_it(tfer_desc(AccountDesc, TransferDesc), !IO) :-
    list.map_foldl(make_accounts, AccountDesc, Accounts, !IO),
    spawn_threads(Accounts, TransferDesc, !IO).
    
%-----------------------------------------------------------------------------%

:- pred spawn_threads(list(pair(int, stm_var(account)))::in, 
    list(transfer_desc)::in, io::di, io::uo) is cc_multi.

spawn_threads(_, [], !IO).
spawn_threads(Accounts, [Transfer | Transfers], !IO) :-
    Transfer = transfer_desc(From, To, Amount),
    FromAccount = Accounts ^ det_elem(From),
    ToAccount = Accounts ^ det_elem(To),
    thread.spawn(transfer(Amount, FromAccount, ToAccount), !IO),
    spawn_threads(Accounts, Transfers, !IO).

%------------------------------------------------------------------------------%

:- type account
    --->    account(int, int).


:- pred transfer(int::in, stm_var(account)::in, stm_var(account)::in,
    io::di, io::uo) is cc_multi.

transfer(Amount, AccountA, AccountB, IO0, IO) :-
    atomic [outer(IO0, IO), inner(STM0, STM)] (
        withdraw(Amount, AccountA, STM0, STM1),
        deposit(Amount, AccountB, STM1, STM)
    ).


:- pred display_amount(stm_var(account)::in, io::di, io::uo) is cc_multi.

display_amount(Account, IO0, IO) :-
    atomic [outer(IO0, IO1), inner(STM0, STM)] (
        get_balance(Account, ID, Balance, STM0, STM)
    ),
    io.write_string(string(ID) ++ ": " ++ string(Balance) ++ "\n", IO1, IO).


:- pred new_account(int::in, int::in, stm_var(account)::out, io::di, 
    io::uo) is det.

new_account(ID, Balance, AccountTVar, !IO) :-
    Account = account(ID, Balance),
    new_stm_var(Account, AccountTVar, !IO).
    
:- pred deposit(int::in, stm_var(account)::in, stm::di, stm::uo) is det.

deposit(Amount, AccountTVar, !Stm) :-
    read_stm_var(AccountTVar, Account0, !Stm),
    Account0 = account(ID, Balance0),
    Balance = Balance0 + Amount,
    Account = account(ID, Balance),
    write_stm_var(AccountTVar, Account, !Stm).


:- pred withdraw(int::in, stm_var(account)::in, stm::di, stm::uo) is det.

withdraw(Amount, AccountTVar, !Stm) :-
    Amount1 = -Amount,
    deposit(Amount1, AccountTVar, !Stm).


:- pred get_balance(stm_var(account)::in, int::out, int::out, 
    stm::di, stm::uo) is det.

get_balance(AccountTVar, ID, Balance, !Stm) :-
    read_stm_var(AccountTVar, Account, !Stm),
    Account = account(ID, Balance).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
