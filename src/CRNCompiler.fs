module CRNCompiler

open CRNpp
open Reactions
open ReactionsParser

let listsToReaction l1 l2 =
    Rxn(Map.ofList (List.map (fun x -> (x, 1.0)) l1), Map.ofList (List.map (fun x -> (x, 1.0)) l2), 1)


let commandToReactions cmd subcnt =
    match cmd with
    | Ld(x, y) -> [ listsToReaction [ x ] [ x; y ]; listsToReaction [ y ] [ "Ø" ] ]
    | Add(x, y, z) -> []
    | Sub(_, _, _) -> failwith "Not Implemented"
    | Mul(_, _, _) -> failwith "Not Implemented"
    | Div(_, _, _) -> failwith "Not Implemented"
    | Sqrt(_, _) -> failwith "Not Implemented"
    | Cmp(_, _) -> failwith "Not Implemented"
    | Rx(_, _, _) -> failwith "Not Implemented"
    | IfGT(_) -> failwith "Not Implemented"
    | IfGE(_) -> failwith "Not Implemented"
    | IfEQ(_) -> failwith "Not Implemented"
    | IfLT(_) -> failwith "Not Implemented"
    | IfLE(_) -> failwith "Not Implemented"
