module CRNInterpreter

open CRNpp
open Treecode

type State = Map<species,float>
// type TypecheckState = Map<species, species> * (bool * bool * bool * bool * bool) * Set<species> // Adj list, bool for each if case, Written-to species
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


let addOneToMap (map:Map<species, int>) (species:species) = 
    Map.change
        species
        (function
        | None -> Some(1)
        | Some(i) -> Some(i+1))
        map
    

let subOneOfMap (map:Map<species, int>) (species:species) = 
    Map.change
        species
        (function
        | None -> Some(0)
        | Some(i) -> Some(i-1))
        map


let rec isCycleFree (heapIsh: Map<int,species Set>) (inArrows: Map<species,int>) = 
    if heapIsh.IsEmpty then
        true
    else
        if heapIsh.ContainsKey(0) && not (Map.find 0 heapIsh |> List.isEmpty) then
            let headSpecies = Map.find 0 heapIsh |> fst
            let inArrows' = List.fold (fun h s -> subOneOfMap h s) heapIsh (Map.find headSpecies inArrows)
            //isCycleFree heapish' inArrows
        else
            false


let isAsyclic (adj: Map<species, List<species>>) = 
    let keys = (adj.Keys |> Seq.cast |> Set.ofSeq)
    let values = (List.concat adj.Values |> Set.ofList)
    let allSpecies = Set.union keys values
    let noOnePointsTo = Set.
    isCycleFree (Map.fold (fun m k v-> Map.add k (v |> Seq.cast |> Set.ofSeq) m) Map.empty heapIsh) inArrows


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
            let adj' = List.fold (fun m r -> Set.fold (fun m' p -> mapAppend m' r p) m prod) adj reac
            let productsAreNew = Set.intersect prod writtenTo |> Set.isEmpty
            let writtenTo' = Set.union writtenTo prod
            num >= 0 && productsAreNew && stepCheck cs adj' (b1, b2, b3, b4, b5) writtenTo'
        | IfGT(com) :: cs -> 
            if List.forall (fun c -> isIf c) cs then
                not b1 && not b2 && stepCheck com adj (true, true, true, true, true) writtenTo && stepCheck cs adj (true, b2, b3, b4, b5) writtenTo
            else stepCheck (cs @ [IfGT(com)]) adj (b1, b2, b3, b4, b5) (writtenTo)
        | IfGE(com) :: cs -> 
            if List.forall (fun c -> isIf c) cs then
                not b1 && not b2 && not b3 && not b5 && stepCheck com adj (true, true, true, true, true) writtenTo && stepCheck cs adj (b1, true, b3, b4, b5) writtenTo
            else stepCheck (cs @ [IfGE(com)]) adj (b1, b2, b3, b4, b5) (writtenTo)
        | IfEQ(com) :: cs -> 
            if List.forall (fun c -> isIf c) cs then
                not b2 && not b3 && not b5 && stepCheck com adj (true, true, true, true, true) writtenTo && stepCheck cs adj (b1, b2, true, b4, b5) writtenTo
            else stepCheck (cs @ [IfEQ(com)]) adj (b1, b2, b3, b4, b5) (writtenTo)
        | IfLT(com) :: cs -> 
            if List.forall (fun c -> isIf c) cs then
                not b4 && not b5 && stepCheck com adj (true, true, true, true, true) writtenTo && stepCheck cs adj (b1, b2, b3, true, b5) writtenTo
            else stepCheck (cs @ [IfLT(com)]) adj (b1, b2, b3, b4, b5) (writtenTo)
        | IfLE(com) :: cs -> 
            if List.forall (fun c -> isIf c) cs then
                not b2 && not b3 && not b4 && not b5 && stepCheck com adj (true, true, true, true, true) writtenTo && stepCheck cs adj (b1, b2, b3, b4, true) writtenTo
            else stepCheck (cs @ [IfLE(com)]) adj (b1, b2, b3, b4, b5) (writtenTo)
        | [] -> isAsyclic adj



    // | Ld of species * species
    // | Add of species * species * species
    // | Sub of species * species * species
    // | Mul of species * species * species
    // | Div of species * species * species
    // | Sqrt of species * species
    // | Cmp of species * species
    // | Rx of species List * species List * float
    // | IfGT of Command List
    // | IfGE of Command List
    // | IfEQ of Command List
    // | IfLT of Command List
    // | IfLE of Command List

    let stepsPass =
        List.fold
            (fun b (S(cmds)) -> b && stepCheck cmds (Map.empty) (false, false, false, false, false) Set.empty)
            true
            steps

    concsPass && stepsPass




let getInitialState (concl : Conc List) : State = List.fold (fun state (C(s,c)) -> Map.add s c state) Map.empty concl

let getValue (s : species) (state : State) = if Map.containsKey s state then Map.find s state else 0

let eqCheck x y = abs (x-y) <= 0.5

let rec doCommandList (cl : Command List) (state : State) = 
    let getState s = getValue s state
    match cl with
    | [] -> state
    | Ld(x,y)::clt -> doCommandList clt (Map.add y (getState x) state) 
    | Add(x,y,z)::clt -> doCommandList clt (Map.add z ((getState x) + (getState y)) state)
    | Sub(x,y,z)::clt -> doCommandList clt (Map.add z ((getState x) - (getState y)) state)
    | Mul(x,y,z)::clt -> doCommandList clt (Map.add z ((getState x) * (getState y)) state)
    | Div(x,y,z)::clt -> doCommandList clt (Map.add z ((getState x) / (getState y)) state)
    | Sqrt(x,y)::clt -> doCommandList clt (Map.add y (sqrt (getState x)) state)
    | Cmp(x,y)::clt ->  let xysum = (getState x) + (getState y)
                        let Xgt,Xlt = (getState x) / xysum, (getState y) / xysum
                        let XltState = Map.add "Xlt" Xlt state
                        let XgtState = Map.add "Xgt" Xgt XltState
                        doCommandList clt XgtState
    | Rx(p,r,c)::clt -> failwith "Cannot interpret custom reactions!"
    | IfGT(cmdl)::clt -> if getState "Xgt" > getState "Xlt" then doCommandList clt (doCommandList cmdl state) else doCommandList clt state
    | IfGE(cmdl)::clt -> if getState "Xgt" > getState "Xlt" || eqCheck (getState "Xgt") (getState "Xlt") then doCommandList clt (doCommandList cmdl state) else doCommandList clt state                    
    | IfEQ(cmdl)::clt -> if eqCheck (getState "Xgt") (getState "Xlt") then doCommandList clt (doCommandList cmdl state) else doCommandList clt state                    
    | IfLT(cmdl)::clt -> if getState "Xgt" < getState "Xlt" then doCommandList clt (doCommandList cmdl state) else doCommandList clt state
    | IfLE(cmdl)::clt -> if getState "Xgt" < getState "Xlt" || eqCheck (getState "Xgt") (getState "Xlt") then doCommandList clt (doCommandList cmdl state) else doCommandList clt state                    


let doStep (S(cl): Step) (state: State) : State = doCommandList cl state


let interpretProgram (R(concl,stepl)) =
    let initialState = getInitialState concl
    Seq.unfold (fun (state, i) -> 
                    let nextState = doStep (List.item (i % List.length stepl) stepl) state
                    Some(nextState,(nextState,i+1))) (initialState,0)


let rec commandToTree cmd = match cmd with
                            | Ld(x,y) -> Node("Ld(" + x + "," + y + ")",[])
                            | Add(x,y,z) -> Node("Add(" + x + "," + y + "," + z + ")",[])
                            | Sub(x,y,z) -> Node("Sub(" + x + "," + y + "," + z + ")",[])
                            | Mul(x,y,z) -> Node("Mul(" + x + "," + y + "," + z + ")",[])
                            | Div(x,y,z) -> Node("Div(" + x + "," + y + "," + z + ")",[])
                            | Sqrt(x,y) -> Node("Sqrt(" + x + "," + y + ")",[])
                            | Cmp(x,y) -> Node("Cmp(" + x + "," + y + ")",[])
                            | Rx(p,r,c) -> Node("Rx(" + List.fold (fun s x -> s + "+" + x) "" p + "," + List.fold (fun s x -> s + "+" + x) "" r + "," + (string c) + ")" ,[])
                            | IfGT(cl) -> Node("IfGT",List.map commandToTree cl)
                            | IfGE(cl) -> Node("IfGE",List.map commandToTree cl)
                            | IfEQ(cl) -> Node("IfEQ",List.map commandToTree cl)
                            | IfLT(cl) -> Node("IfLT",List.map commandToTree cl)
                            | IfLE(cl) -> Node("IfLE",List.map commandToTree cl)


let concToTree (C(s,c)) = Node("C(" + s + "," + (string c) + ")",[])

let stepToTree (S(cl)) = Node("S",List.map commandToTree cl)





let astToTree (R(concl,stepl)) = Node("R",List.map concToTree concl @ List.map stepToTree stepl)
