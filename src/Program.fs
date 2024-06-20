// For more information see https://aka.ms/fsharp-console-apps

module Main

open CRNParser
open CRNInterpreter
open Treecode
open FParsec


let rmws (s: string) =
    s.Replace(" ", "").Replace("\t", "").Replace("\n", "").Replace("\r", "")


let printStateRange (ss: State seq) i j =
    // Seq.skip i ss |> Seq.take j |> Seq.toList |> printfn "%A"
    let l = Seq.skip i ss |> Seq.take j |> Seq.toList
    let rec printStateAsList = function
        | [] -> printfn "NEXTSTEP"
        | (spec, conc) :: xs -> 
            printfn "%s %A" spec conc
            printStateAsList xs
    let rec printrec = function
        | [] -> ()
        | st :: sts -> 
            printStateAsList (Map.toList st)
            printrec sts
    printrec l



let printAst ast =
    design (astToTree ast) |> printfn "%A"


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

    let test =
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

    // printfn "%A" (Seq.item 10 interpretedAst)
    // printfn "%A" (interpretedAst |> Seq.take 10 |> Seq.toList)
    printStateRange interpretedAst 0 10
    
    // printAst ast
    

    0
