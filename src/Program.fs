﻿// Jonathan Højlev, 26/6 // Rani Ey. í Bø 26/06

module Main

open CRNParser
open CRNInterpreter
open CRNCompiler
open Reactions
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
        | _ :: xs -> printStateAsList xs

    let rec printrec =
        function
        | [] -> ()
        | st :: sts ->
            printStateAsList (Map.toList st)
            printrec sts

    printrec l

let printAst ast = design (astToTree ast) |> designtostring |> printfn "%s"


[<EntryPoint>]
let main args =

    let readFile filePath = System.IO.File.ReadAllText(filePath)

    if args.Length = 1 then
        let inputProgram = readFile args[0] |> rmws
        
        let ast =
            match run pprogram inputProgram with
            | Success(res, _, _) -> res
            | Failure(_, _, _) -> failwith "program not parsed"

        let initState, reactions = compileCRN ast
        
        let simulation = simulateReactionsMatrix initState reactions 0.01
        //let simulation = interpretProgram ast initState

        printStateRange simulation 0 40000 ["e"]

    else if args.Length = 2 then
        let inputProgram = readFile args[0] |> rmws
        let ast =
            match run pprogram inputProgram with
            | Success(res, _, _) -> res
            | Failure(_, _, _) -> failwith "program not parsed"
        printAst ast
    else
        printfn "Program requires an argument giving a path to a .crn file"


    0
