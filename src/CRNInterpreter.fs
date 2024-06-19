module CRNInterpreter

open CRNpp

type State = Map<species,float>
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




