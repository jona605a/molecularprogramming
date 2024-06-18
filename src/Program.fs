// For more information see https://aka.ms/fsharp-console-apps

module Main

open CRNParser
open FParsec


[<EntryPoint>]
let main args =


    match run pmodule "sqrt[hej,  
      farvel]" with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    match run pmodule "add[a,  
      b,   c  ]" with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    match run prxn "rxn[ A + B + C, B + C, 1.34]" with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    0