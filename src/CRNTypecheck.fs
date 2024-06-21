module CRNTypecheck

open CRNpp

let mapAppend map key value =
    Map.change
        key
        (function
        | None -> Some([ value ])
        | Some(l) -> Some(value :: l))
        map

let isIf (com: Command) =
    match com with
    | IfGT(_) -> true
    | IfGE(_) -> true
    | IfEQ(_) -> true
    | IfLT(_) -> true
    | IfLE(_) -> true
    | _ -> false


let addOneToMap (map: Map<species, int>) (species: species) =
    Map.change
        species
        (function
        | None -> Some(1)
        | Some(i) -> Some(i + 1))
        map


let subOneOfMap (map: Map<species, int>) (species: species) =
    Map.change
        species
        (function
        | None -> Some(0)
        | Some(i) -> Some(i - 1))
        map


let isAsyclic (adj: Map<species, List<species>>) =
    let keys = (adj.Keys |> Seq.cast |> Set.ofSeq)
    let values = (List.concat adj.Values |> Set.ofList)
    let allSpecies = Set.union keys values
    let noOnePointsTo = Set.difference allSpecies values |> Set.toList

    let inDegrees =
        List.fold (fun M s -> addOneToMap M s) Map.empty (List.concat adj.Values)

    let rec isDAG q inDegs =
        // printfn "%A\n%A" q inDegs

        match q with
        | [] -> Map.isEmpty inDegs
        | e :: es ->
            match Map.tryFind e adj with
            | None -> isDAG es inDegs
            | Some(neigh) ->
                let (D, Q) =
                    List.fold
                        (fun (M, q') y ->
                            let x = (Map.find y M)
                            let M' = if x <= 1 then Map.remove y M else subOneOfMap M y
                            let Q = if x <= 1 then y :: q' else q'
                            (M', Q))
                        (inDegs, es)
                        neigh

                isDAG Q D

    isDAG noOnePointsTo inDegrees



let isTyped (R(concs, steps): Root) : bool =
    let concsPass =
        List.fold
            (fun (b, M) (C(sp, num)) -> ((b && not (Set.contains sp M)) && num >= 0, Set.add sp M))
            (true, Set.empty)
            concs
        |> fst

    let rec stepCheck
        (cmds: Command list)
        (adj: Map<species, List<species>>)
        (b1, b2, b3, b4, b5)
        (writtenTo: Set<species>)
        =
        match cmds with
        | Ld(r, p) :: cs ->
            let adj' = mapAppend adj r p

            not (writtenTo.Contains p)
            && stepCheck cs adj' (b1, b2, b3, b4, b5) (writtenTo.Add p)
        | Add(r1, r2, p) :: cs ->
            let adj' = mapAppend adj r1 p
            let adj'' = mapAppend adj' r2 p

            not (writtenTo.Contains p)
            && stepCheck cs adj'' (b1, b2, b3, b4, b5) (writtenTo.Add p)
        | Sub(r1, r2, p) :: cs ->
            let adj' = mapAppend adj r1 p
            let adj'' = mapAppend adj' r2 p

            not (writtenTo.Contains p)
            && stepCheck cs adj'' (b1, b2, b3, b4, b5) (writtenTo.Add p)
        | Mul(r1, r2, p) :: cs ->
            let adj' = mapAppend adj r1 p
            let adj'' = mapAppend adj' r2 p

            not (writtenTo.Contains p)
            && stepCheck cs adj'' (b1, b2, b3, b4, b5) (writtenTo.Add p)
        | Div(r1, r2, p) :: cs ->
            let adj' = mapAppend adj r1 p
            let adj'' = mapAppend adj' r2 p

            not (writtenTo.Contains p)
            && stepCheck cs adj'' (b1, b2, b3, b4, b5) (writtenTo.Add p)
        | Sqrt(r, p) :: cs ->
            let adj' = mapAppend adj r p

            not (writtenTo.Contains p)
            && stepCheck cs adj' (b1, b2, b3, b4, b5) (writtenTo.Add p)
        | Cmp(_, _) :: cs ->
            not (writtenTo.Contains "cmp")
            && stepCheck cs adj (b1, b2, b3, b4, b5) (writtenTo.Add "cmp")
        | Rx(reac, prodreacs, num) :: cs ->
            let prod = Set.difference (Set.ofList prodreacs) (Set.ofList reac)

            let adj' =
                List.fold (fun m r -> Set.fold (fun m' p -> mapAppend m' r p) m prod) adj reac

            let productsAreNew = Set.intersect prod writtenTo |> Set.isEmpty
            let writtenTo' = Set.union writtenTo prod
            num >= 0 && productsAreNew && stepCheck cs adj' (b1, b2, b3, b4, b5) writtenTo'
        | IfGT(com) :: cs when com <> [] ->
            if List.forall (fun c -> isIf c) cs then
                not b1
                && not b2
                && stepCheck com adj (true, true, true, true, true) writtenTo
                && stepCheck cs adj (true, b2, b3, b4, b5) writtenTo
            else
                stepCheck (cs @ [ IfGT(com) ]) adj (b1, b2, b3, b4, b5) (writtenTo)
        | IfGE(com) :: cs when com <> [] ->
            if List.forall (fun c -> isIf c) cs then
                not b1
                && not b2
                && not b3
                && not b5
                && stepCheck com adj (true, true, true, true, true) writtenTo
                && stepCheck cs adj (b1, true, b3, b4, b5) writtenTo
            else
                stepCheck (cs @ [ IfGE(com) ]) adj (b1, b2, b3, b4, b5) (writtenTo)
        | IfEQ(com) :: cs when com <> [] ->
            if List.forall (fun c -> isIf c) cs then
                not b2
                && not b3
                && not b5
                && stepCheck com adj (true, true, true, true, true) writtenTo
                && stepCheck cs adj (b1, b2, true, b4, b5) writtenTo
            else
                stepCheck (cs @ [ IfEQ(com) ]) adj (b1, b2, b3, b4, b5) (writtenTo)
        | IfLT(com) :: cs when com <> [] ->
            if List.forall (fun c -> isIf c) cs then
                not b4
                && not b5
                && stepCheck com adj (true, true, true, true, true) writtenTo
                && stepCheck cs adj (b1, b2, b3, true, b5) writtenTo
            else
                stepCheck (cs @ [ IfLT(com) ]) adj (b1, b2, b3, b4, b5) (writtenTo)
        | IfLE(com) :: cs when com <> [] ->
            if List.forall (fun c -> isIf c) cs then
                not b2
                && not b3
                && not b4
                && not b5
                && stepCheck com adj (true, true, true, true, true) writtenTo
                && stepCheck cs adj (b1, b2, b3, b4, true) writtenTo
            else
                stepCheck (cs @ [ IfLE(com) ]) adj (b1, b2, b3, b4, b5) (writtenTo)
        | [] -> isAsyclic adj
        | _ -> false



    let stepsPass =
        steps <> []
        && List.fold
            (fun b (S(cmds)) ->
                b
                && not (cmds = [])
                && stepCheck cmds (Map.empty) (false, false, false, false, false) Set.empty)
            true
            steps

    concsPass && stepsPass
