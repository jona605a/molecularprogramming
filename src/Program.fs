﻿// For more information see https://aka.ms/fsharp-console-apps

module Main

open CRNpp
open CRNParser
open CRNInterpreter
open CRNCompiler
open Reactions
open ReactionsParser
open Treecode
open FParsec





let printStateRange (ss: State seq) i j printSpecies =
    // Seq.skip i ss |> Seq.take j |> Seq.toList |> printfn "%A"
    let printSpecies s =
        printSpecies = [] || List.contains s printSpecies

    let l = Seq.skip i ss |> Seq.take j |> Seq.toList

    let rec printStateAsList =
        function
        | [] -> printfn "NEXTSTEP"
        | (spec, conc) :: xs when printSpecies spec ->
            printfn "%s %A" spec conc
            printStateAsList xs
        | (spec, conc) :: xs -> printStateAsList xs

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
        //printfn "%s" inputProgram

        let ast =
            match run pprogram inputProgram with
            | Success(res, _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "program not parsed"

        let states = interpretProgram ast
        //printStateRange states 0 100 []




        let initState, reactions = compileCRN ast
        
        let subIsolated = commandToReactions (Sub("a","b","c")) 0 0

        let simulation = simulateReactions initState reactions 0.01

        printStateRange simulation 0 40000 []//["T1";"T2";"T3";"T4";"T5";"T6";"c";"cnext";"Yegtx"]
        //printf "%A" reactions
    else
        printfn "Program requires an argument giving a path to a .crn file"


    0
