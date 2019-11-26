//in development, not completed
module BankAccount

type BankAccount = {
    Open: bool
    mutable Balance: decimal
    }

let mkBankAccount() = { Open = false; Balance = 0.0m }

let openAccount account = { Open = true; Balance = 0.0m }

let closeAccount account = { account with Open = false}

let getBalance (account: BankAccount) = 
    match account.Open with
    | true -> Some account.Balance
    | false -> None

let updateBalance (change: decimal) (account: BankAccount) = 
    lock account (fun () -> account.Balance <- account.Balance + change)
    account