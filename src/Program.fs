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

    let rec printStateAsList =
        function
        | [] -> printfn "NEXTSTEP"
        | (spec, conc) :: xs ->
            printfn "%s %A" spec conc
            printStateAsList xs

    let rec printrec =
        function
        | [] -> ()
        | st :: sts ->
            printStateAsList (Map.toList st)
            printrec sts

    printrec l

let printAst ast = design (astToTree ast) |> printfn "%A"


[<EntryPoint>]
let main args =

    let readFile filePath = System.IO.File.ReadAllText(filePath)

    if args.Length > 0 then
        let inputProgram = readFile args[0] |> rmws
        printfn "%s" inputProgram

        let ast =
            match run pprogram inputProgram with
            | Success(res, _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "program not parsed"

        let states = interpretProgram ast
        printStateRange states 0 100


    0
