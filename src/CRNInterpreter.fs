module CRNInterpreter

open CRNpp

type State = (species * float) List
// type TypecheckState = Map<species, species> * (bool * bool * bool * bool * bool) * Set<species> // Adj list, bool for each if case, Written-to species
let mapAppend map key value =
    Map.change
        key
        (function
        | None -> Some([ value ])
        | Some(l) -> Some(value :: l))
        map


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
            // num >= 0 && 
            // Set.forall (fun p -> ) prod
            true
        | IfGT(_) :: cs -> failwith "Not Implemented"
        | IfGE(_) :: cs -> failwith "Not Implemented"
        | IfEQ(_) :: cs -> failwith "Not Implemented"
        | IfLT(_) :: cs -> failwith "Not Implemented"
        | IfLE(_) :: cs -> failwith "Not Implemented"
        | [] -> true // Check DAG



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




let getInitialState (r: Root) : State = []


let doStep (step: Step) (state: State) : State = []
