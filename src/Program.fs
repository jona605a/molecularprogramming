// For more information see https://aka.ms/fsharp-console-apps

module Main

open CRNParser
open CRNInterpreter
open Reactions
open ReactionsParser
open Treecode
open FParsec


let rmws (s: string) =
    s.Replace(" ", "").Replace("\t", "").Replace("\n", "").Replace("\r", "")

let rmwsreaction (s : string) =
    s.Replace(" ", "").Replace("\t", "").Replace("\r", "")


let printStateRange (ss: State seq) i j printSpecies=
    // Seq.skip i ss |> Seq.take j |> Seq.toList |> printfn "%A"
    let printSpecies s = printSpecies = [] || List.contains s printSpecies
    let l = Seq.skip i ss |> Seq.take j |> Seq.toList

    let rec printStateAsList =
        function
        | [] -> printfn "NEXTSTEP"
        | (spec, conc) :: xs when printSpecies spec-> 
        | (spec, conc) :: xs ->
            printfn "%s %A" spec conc
            printStateAsList xs
        | (spec,conc) :: xs -> printStateAsList xs
    let rec printrec = function

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
        printStateRange states 0 100 []


 
    let reactions = [Rxn(Map.ofList [("A",1);("B",1)],Map.ofList [("A",1);("B",1);("C",1)],1.0 );Rxn(Map.ofList [("C",1)],Map.ofList [("Ø",1)],1.0)]
    let initState = (Map.ofList [("A",6.0);("B",2.0)])
    let simulation = simulateReactions initState reactions 0.01

    printStateRange simulation 0 1000 ["A";"B";"C"]



    0
