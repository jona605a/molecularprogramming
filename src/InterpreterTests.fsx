#r "nuget: FsCheck"
#load "Treecode.fs"
#load "CRNpp.fs"
#load "CRNTypecheck.fs"
#load "CRNInterpreter.fs"


open FsCheck
open Treecode
open CRNpp
open CRNTypecheck
open CRNInterpreter


let shuffleList list =
    let rng = System.Random()
    list |> List.sortBy (fun _ -> rng.Next())

let rec shuffleSteps (R(conc, steps)) =
    R(conc, List.map (fun (S(c: List<Command>)) -> S(shuffleList c)) steps)


let stepOrderDoesNotMatter (ast: CRNpp.Root) (startIdx: int) (endIdx: int)=
    let shufSteps = shuffleSteps ast
    (isTyped ast = isTyped shufSteps) && (not (isTyped ast) || (isTyped ast && (interpretProgram ast |> Seq.skip startIdx |> Seq.take endIdx = (interpretProgram shufSteps |> Seq.skip startIdx |> Seq.take endIdx))))


// FSCheck tests ////////////////////////

let config = { Config.Quick with MaxTest = 1000 }

Check.One(config, stepOrderDoesNotMatter)
