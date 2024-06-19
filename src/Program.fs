// For more information see https://aka.ms/fsharp-console-apps

module Main

open CRNParser
open CRNInterpreter
open Treecode
open FParsec


let rmws (s: string) =
    s.Replace(" ", "").Replace("\t", "").Replace("\n", "").Replace("\r", "")


[<EntryPoint>]
let main args =

    let gcdprog =
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


    let ast = match run pprogram gcdprog with
                | Success(result, _, _) -> result
                | Failure(errorMsg, _, _) -> failwith "program not parsed"

    let interpretedAst = interpretProgram ast

    //printfn "%A" (Seq.item 5 interpretedAst)

    let d = design (astToTree ast)
    printfn "%s" (designtostring d)

    

    0
