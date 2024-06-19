// For more information see https://aka.ms/fsharp-console-apps

module Main

open CRNParser
open CRNInterpreter
open FParsec


let rmws (s: string) =
    s.Replace(" ", "").Replace("\t", "").Replace("\n", "").Replace("\r", "")


[<EntryPoint>]
let main args =


    match run pmodule (rmws "sqrt[hej,  farvel]") with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    match
        run
            pmodule
            (rmws
                "add[a,  
      b,   c  ]")
    with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    match run prxn (rmws "rxn[ A + B + C, B + C, 1.34]") with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


    match run pconditionals (rmws "ifGT[{ sub[atmp,btmp,a] }]") with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg



    let teststep =
        (rmws
            "step[{
 ifGT[{ sub[atmp,btmp,a] }],
 ifLT[{ sub[btmp,atmp,b] }]}]")

    match run pstep teststep with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg



    let testprog =
        rmws
            "crn = {
 conc[a,32 ],
 conc[b,12 ],
 step[{
 ld [a, atmp],
 ld [b, btmp],
 cmp[a,b]
 }],
 step[{
 ifGT[{ sub[atmp,btmp,a] }],
 ifLT[{ sub[btmp,atmp,b] }]
 }]
 };"

    match run pprogram testprog with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    let ast = match run pprogram testprog with
                | Success(result, _, _) -> result
                | Failure(errorMsg, _, _) -> failwith "program not parsed"

    let interpretedAst = interpretProgram ast

    printfn "%A" (Seq.item 5 interpretedAst)

    0
