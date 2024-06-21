module CRNCompiler

open CRNpp
open Reactions
open ReactionsParser

let listsToReaction l1 l2 =
    let f l =
        Map.ofList (List.map (fun x -> (x, 1.0)) l)

    Rxn(f l1, f l2, 1)


let commandToReactions cmd subcnt =
    match cmd with
    | Ld(a, b) -> [ listsToReaction [ a ] [ a; b ]; listsToReaction [ b ] [ "Ø" ] ]
    | Add(a, b, c) ->
        [ listsToReaction [ a ] [ a; c ]
          listsToReaction [ b ] [ b; c ]
          listsToReaction [ c ] [ "Ø" ] ]
    | Sub(a, b, c) ->
        [ listsToReaction [ a ] [ a; c ]
          listsToReaction [ b ] [ b; sprintf "H%A" subcnt ]
          listsToReaction [ c ] [ "Ø" ]
          listsToReaction [ c; sprintf "H%A" subcnt ] [ "Ø" ] ]
    | Mul(a, b, c) -> [ listsToReaction [ a; b ] [ a; b; c ]; listsToReaction [ c ] [ "Ø" ] ]
    | Div(a, b, c) -> [ listsToReaction [ a ] [ a; c ]; listsToReaction [ b; c ] [ b ] ]
    | Sqrt(a, b) ->
        [ listsToReaction [ a ] [ a; b ]
          Rxn(Map.ofList [ (b, 1); (b, 1) ], Map.ofList [ ("Ø", 1.0) ], 0.5) ]
    | Rx(l1, l2, k) ->
        let f l =
            Map.ofList (List.map (fun x -> (x, 1.0)) l)

        [ Rxn(f l1, f l2, k) ]
    | Cmp(x, y) ->
        [ listsToReaction [ "Xgty"; y ] [ "Xlty"; y ]
          listsToReaction [ "Xlty"; x ] [ "Xgty"; x ] // CRN 7
          // Need to be seperated
          listsToReaction [ "Xgyt"; "Xlty" ] [ "Xlty"; "CMP" ]
          listsToReaction [ "Xgyt"; "Xlty" ] [ "Xgty"; "CMP" ]
          listsToReaction [ "CMP"; "Xlty" ] [ "Xlty"; "Xlty" ]
          listsToReaction [ "CMP"; "Xgty" ] [ "Xgty"; "Xgty" ] ] // CRN 8
    | IfGT(_) -> failwith "Not Implemented"
    | IfGE(_) -> failwith "Not Implemented"
    | IfEQ(_) -> failwith "Not Implemented"
    | IfLT(_) -> failwith "Not Implemented"
    | IfLE(_) -> failwith "Not Implemented"
