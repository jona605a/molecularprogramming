module CRNCompiler

open CRNpp
open CRNInterpreter
open Reactions
open ReactionsParser

let listsToReaction l1 l2 =
    let f l =
        List.fold
            (fun map s ->
                if Map.containsKey s map then
                    Map.add s ((Map.find s map) + 1.0) map
                else
                    Map.add s 1.0 map)
            Map.empty
            l

    Rxn(f l1, f l2, 1)


let addTimeVars i (Rxn(r, p, c)) =
    let timeVar = sprintf "T%d" (3 * i)
    Rxn(Map.add timeVar 1.0 r, Map.add timeVar 1.0 p, c)




let rec commandToReactions cmd subcnt stepcnt =
    let ifReactionHelpVar =
        [
          //     listsToReaction [ "Xgty"; "Xlty" ] [ "Xlty"; "CMP1" ]
          //   listsToReaction [ "Xgty"; "Xlty" ] [ "Xgty"; "CMP1" ]
          //   listsToReaction [ "CMP1"; "Xlty" ] [ "Xlty"; "Xlty" ]
          //   listsToReaction [ "CMP1"; "Xgty" ] [ "Xgty"; "Xgty" ]
          listsToReaction [ "Xegty"; "Xelty" ] [ "Xelty"; "CMP1" ]
          listsToReaction [ "Xegty"; "Xelty" ] [ "Xegty"; "CMP1" ]
          listsToReaction [ "CMP1"; "Xelty" ] [ "Xelty"; "Xelty" ]
          listsToReaction [ "CMP1"; "Xegty" ] [ "Xegty"; "Xegty" ]
          listsToReaction [ "Yegtx"; "Yeltx" ] [ "Yeltx"; "CMP2" ]
          listsToReaction [ "Yegtx"; "Yeltx" ] [ "Yegtx"; "CMP2" ]
          listsToReaction [ "CMP2"; "Yeltx" ] [ "Yeltx"; "Yeltx" ]
          listsToReaction [ "CMP2"; "Yegtx" ] [ "Yegtx"; "Yegtx" ] ]

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
        List.map
            (addTimeVars stepcnt)
            ([
               //     listsToReaction [ "Xgty"; y ] [ "Xlty"; y ] // normal cmp
               //   listsToReaction [ "Xlty"; x ] [ "Xgty"; x ]

               listsToReaction [ "Xegty"; y ] [ "Xelty"; y ] // x + eps
               listsToReaction [ "Xelty"; "Xeps" ] [ "Xegty"; "Xeps" ]

               listsToReaction [ "Yegtx"; x ] [ "Yeltx"; x ] // y + eps
               listsToReaction [ "Yeltx"; "Yeps" ] [ "Yegtx"; "Yeps" ]


               ]
             @ commandToReactions (Add(x, "eps", "Xeps")) subcnt stepcnt
             @ commandToReactions (Add(y, "eps", "Yeps")) subcnt stepcnt)
        @ List.map (addTimeVars (stepcnt + 1)) ifReactionHelpVar

    | IfGT(cl) ->
        let tempReactions =
            fst (
                List.fold
                    (fun (st, sc) cmd ->
                        match cmd with
                        | Sub(_) -> (st @ commandToReactions cmd sc stepcnt, sc + 1)
                        | cmd -> (st @ commandToReactions cmd sc stepcnt, sc))
                    ([], subcnt)
                    cl
            )

        List.map (fun (Rxn(p, r, c)) -> Rxn(Map.add "Yeltx" 1 p, Map.add "Yeltx" 1 r, c)) tempReactions

    | IfGE(cl) ->
        let tempReactions =
            fst (
                List.fold
                    (fun (st, sc) cmd ->
                        match cmd with
                        | Sub(_) -> (st @ commandToReactions cmd sc stepcnt, sc + 1)
                        | cmd -> (st @ commandToReactions cmd sc stepcnt, sc))
                    ([], subcnt)
                    cl
            )

        List.map (fun (Rxn(p, r, c)) -> Rxn(Map.add "Xegty" 1 p, Map.add "Xegty" 1 r, c)) tempReactions

    | IfEQ(cl) ->
        let tempReactions =
            fst (
                List.fold
                    (fun (st, sc) cmd ->
                        match cmd with
                        | Sub(_) -> (st @ commandToReactions cmd sc stepcnt, sc + 1)
                        | cmd -> (st @ commandToReactions cmd sc stepcnt, sc))
                    ([], subcnt)
                    cl
            )

        List.map
            (fun (Rxn(p, r, c)) ->
                Rxn(Map.add "Yegtx" 1 (Map.add "Xegty" 1 p), Map.add "Yegtx" 1 (Map.add "Xegty" 1 r), c))
            tempReactions

    | IfLT(cl) ->
        let tempReactions =
            fst (
                List.fold
                    (fun (st, sc) cmd ->
                        match cmd with
                        | Sub(_) -> (st @ commandToReactions cmd sc stepcnt, sc + 1)
                        | cmd -> (st @ commandToReactions cmd sc stepcnt, sc))
                    ([], subcnt)
                    cl
            )

        List.map (fun (Rxn(p, r, c)) -> Rxn(Map.add "Xelty" 1 p, Map.add "Xelty" 1 r, c)) tempReactions

    | IfLE(cl) ->
        let tempReactions =
            fst (
                List.fold
                    (fun (st, sc) cmd ->
                        match cmd with
                        | Sub(_) -> (st @ commandToReactions cmd sc stepcnt, sc + 1)
                        | cmd -> (st @ commandToReactions cmd sc stepcnt, sc))
                    ([], subcnt)
                    cl
            )

        List.map (fun (Rxn(p, r, c)) -> Rxn(Map.add "Yegtx" 1 p, Map.add "Yegtx" 1 r, c)) tempReactions


let stepToReactions (S(cl)) subcnt stepcnt =
    List.fold
        (fun (st, sc, stc) cmd ->
            match cmd with
            | Sub(_) -> (List.map (addTimeVars stepcnt) (st @ commandToReactions cmd sc stc), sc + 1, stc)
            | Cmp(_) -> (st @ commandToReactions cmd sc stc, sc, stc + 1)
            | cmd -> (List.map (addTimeVars stepcnt) (st @ commandToReactions cmd sc stc), sc, stc))
        ([], subcnt, stepcnt)
        cl


let compileSteps steplist =

    let reactions, _, stepsTaken =
        List.fold
            (fun (st, sc, stc) step ->
                let rxnlst, sc', stc' = stepToReactions step sc stc
                (st @ rxnlst, sc', stc' + 1))
            ([], 0, 1)
            steplist

    let numOfSteps = stepsTaken - 1

    let temp = List.init (numOfSteps * 3 - 1) (fun x -> x + 1)

    (Rxn(Map.ofList [ (sprintf "T%d" (numOfSteps * 3), 1.0); ("T1", 1.0) ], Map.ofList [ ("T1", 2.0) ], 1)
     :: List.fold
         (fun st i ->
             Rxn(
                 Map.ofList [ (sprintf "T%d" i, 1.0); (sprintf "T%d" (i + 1), 1.0) ],
                 Map.ofList [ (sprintf "T%d" (i + 1), 2.0) ],
                 1
             )
             :: st)
         reactions
         temp),
    numOfSteps


let compileCRN (R(conclist, steplist)) =
    let initState = getInitialState conclist
    let reactions, numOfSteps = compileSteps steplist

    let compareVars =
        List.fold
            (fun st key -> Map.add key 0.5 st)
            initState
            [ "eps"; "Xgty"; "Xlty"; "Xegty"; "Xelty"; "Yegtx"; "Yeltx" ]

    let temp = List.init (numOfSteps * 3) (fun x -> x + 1)

    let timeVarConc i =
        if i = 1 || i = 2 then
            0.99999
        else
            0.00002 / float (numOfSteps * 3 - 2)

    let timeVars =
        List.fold (fun st i -> Map.add (sprintf "T%d" i) (timeVarConc i) st) compareVars temp

    timeVars, reactions
