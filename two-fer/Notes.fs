module File1
open System

let twoFer (input: string option): string = 
    let x = Option.defaultValue "you" input
    //sprintf "One for %s, one for me." x
    let strTwoFer = "One for {0}, one for me." 
    String.Format(strTwoFer, x)
