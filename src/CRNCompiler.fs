module CRNCompiler

open CRNpp
open CRNInterpreter
open Reactions
open ReactionsParser

let listsToReaction l1 l2 =
    let f l =
        Map.ofList (List.map (fun x -> (x, 1.0)) l)

    Rxn(f l1, f l2, 1)




let rec commandToReactions cmd subcnt = 
    let ifReactionHelpVar =
        [ listsToReaction [ "Xgty"; "Xlty" ] [ "Xlty"; "CMP1" ]
          listsToReaction [ "Xgty"; "Xlty" ] [ "Xgty"; "CMP1" ]
          listsToReaction [ "CMP1"; "Xlty" ] [ "Xlty"; "Xlty" ]
          listsToReaction [ "CMP1"; "Xgty" ] [ "Xgty"; "Xgty" ]
          listsToReaction [ "Xegty"; "Xelty" ] [ "Xelty"; "CMP2" ]
          listsToReaction [ "Xegty"; "Xelty" ] [ "Xegty"; "CMP2" ]
          listsToReaction [ "CMP2"; "Xelty" ] [ "Xelty"; "Xelty" ]
          listsToReaction [ "CMP2"; "Xegty" ] [ "Xegty"; "Xegty" ]
          listsToReaction [ "Yegtx"; "Yeltx" ] [ "Yeltx"; "CMP3" ]
          listsToReaction [ "Yegtx"; "Yeltx" ] [ "Yegtx"; "CMP3" ]
          listsToReaction [ "CMP3"; "Yeltx" ] [ "Yeltx"; "Yeltx" ]
          listsToReaction [ "CMP3"; "Yegtx" ] [ "Yegtx"; "Yegtx" ] ]

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
        [ listsToReaction [ "Xgty"; y ] [ "Xlty"; y ] // normal cmp
          listsToReaction [ "Xlty"; x ] [ "Xgty"; x ]

          listsToReaction [ "Xegty"; y ] [ "Xelty"; y ] // x + eps
          listsToReaction [ "Xelty"; "Xeps" ] [ "Xegty"; "Xeps" ]

          listsToReaction [ "Yegtx"; x ] [ "Yeltx"; x ] // y + eps
          listsToReaction [ "Yeltx"; "Yeps" ] [ "Yegtx"; "Yeps" ]

          // Need to be seperated
          ]
        @ commandToReactions (Add(x, "eps", "Xeps")) subcnt
        @ commandToReactions (Add(y, "eps", "Yeps")) subcnt

    | IfGT(cl) ->
        let tempReactions =
            fst (
                List.fold
                    (fun (st, sc) cmd ->
                        match cmd with
                        | Sub(_) -> (st @ commandToReactions cmd sc, sc + 1)
                        | cmd -> (st @ commandToReactions cmd sc, sc))
                    ([], subcnt)
                    cl
            )

        ifReactionHelpVar
        @ List.map (fun (Rxn(p, r, c)) -> Rxn(Map.add "Xgty" 1 p, Map.add "Xgty" 1 r, c)) tempReactions

    | IfGE(cl) ->
        let tempReactions =
            fst (
                List.fold
                    (fun (st, sc) cmd ->
                        match cmd with
                        | Sub(_) -> (st @ commandToReactions cmd sc, sc + 1)
                        | cmd -> (st @ commandToReactions cmd sc, sc))
                    ([], subcnt)
                    cl
            )

        ifReactionHelpVar
        @ List.map (fun (Rxn(p, r, c)) -> Rxn(Map.add "Xegty" 1 p, Map.add "Xegty" 1 r, c)) tempReactions
    | IfEQ(cl) ->
        let tempReactions =
            fst (
                List.fold
                    (fun (st, sc) cmd ->
                        match cmd with
                        | Sub(_) -> (st @ commandToReactions cmd sc, sc + 1)
                        | cmd -> (st @ commandToReactions cmd sc, sc))
                    ([], subcnt)
                    cl
            )

        ifReactionHelpVar
        @ List.map
            (fun (Rxn(p, r, c)) ->
                Rxn(Map.add "Yegtex" 1 (Map.add "Xegty" 1 p), Map.add "Yegtex" 1 (Map.add "Xegty" 1 r), c))
            tempReactions
    | IfLT(cl) ->
        let tempReactions =
            fst (
                List.fold
                    (fun (st, sc) cmd ->
                        match cmd with
                        | Sub(_) -> (st @ commandToReactions cmd sc, sc + 1)
                        | cmd -> (st @ commandToReactions cmd sc, sc))
                    ([], subcnt)
                    cl
            )

        ifReactionHelpVar
        @ List.map (fun (Rxn(p, r, c)) -> Rxn(Map.add "Xlty" 1 p, Map.add "Xlty" 1 r, c)) tempReactions
    | IfLE(cl) ->
        let tempReactions =
            fst (
                List.fold
                    (fun (st, sc) cmd ->
                        match cmd with
                        | Sub(_) -> (st @ commandToReactions cmd sc, sc + 1)
                        | cmd -> (st @ commandToReactions cmd sc, sc))
                    ([], subcnt)
                    cl
            )

        ifReactionHelpVar
        @ List.map (fun (Rxn(p, r, c)) -> Rxn(Map.add "Yegtx" 1 p, Map.add "Yegtx" 1 r, c)) tempReactions



let stepToReactions (S(cl)) subcnt =
    List.fold
        (fun (st, sc) cmd ->
            match cmd with
            | Sub(_) -> (st @ commandToReactions cmd sc, sc + 1)
            | cmd -> (st @ commandToReactions cmd sc, sc))
        ([], subcnt)
        cl


let compileSteps steplist =
    let numOfSteps = List.length steplist

    let reactions, _, _ =
        List.fold
            (fun (st, sc, stepcnt) step ->
                let rxnlst, sc' = stepToReactions step sc
                let timeVar = sprintf "T%d" (3 * stepcnt)
                (st
                 @ List.map
                     (fun (Rxn(r, p, c)) -> Rxn(Map.add timeVar 1.0 r, Map.add timeVar 1.0 p, c))
                     rxnlst,
                 sc',
                 stepcnt + 1))
            ([], 0, 1)
            steplist

    let temp = List.init (numOfSteps * 3 - 1) (fun x -> x + 1)

    Rxn(Map.ofList [ (sprintf "T%d" (numOfSteps * 3), 1.0); ("T1", 1.0) ], Map.ofList [ ("T1", 2.0) ], 1)
    :: List.fold
        (fun st i ->
            Rxn(
                Map.ofList [ (sprintf "T%d" i, 1.0); (sprintf "T%d" (i + 1), 1.0) ],
                Map.ofList [ (sprintf "T%d" (i + 1), 2.0) ],
                1
            )
            :: st)
        reactions
        temp



let compileCRN (R(conclist, steplist)) =
    let initState = getInitialState conclist
    let reactions = compileSteps steplist
    let numOfSteps = List.length steplist

    let compareVars = List.fold (fun st key -> Map.add key 0.5 st) initState [ "eps"; "Xgty"; "Xlty"; "Xegty"; "Xelty"; "Yegtx"; "Yeltx" ]
    let temp = List.init (numOfSteps * 3) (fun x -> x + 1)
    let timeVarConc i = if i = 1 || i = 2 then 0.999 else 0.002 / float (numOfSteps * 3 - 2)
    let timeVars = List.fold (fun st i -> Map.add (sprintf "T%d" i) (timeVarConc i)  st) compareVars temp
    timeVars, reactions
