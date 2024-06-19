module CRNInterpreter

open CRNpp
open Treecode
open CRNTypecheck

type State = Map<species, float>


let getInitialState (concl: Conc List) : State =
    List.fold (fun state (C(s, c)) -> Map.add s c state) Map.empty concl

let getValue (s: species) (state: State) =
    if Map.containsKey s state then Map.find s state else 0

let eqCheck x y = abs (x - y) <= 0.5

let rec doCommandList (cl: Command List) (state: State) =
    let getState s = getValue s state

    match cl with
    | [] -> state
    | Ld(x, y) :: clt -> doCommandList clt (Map.add y (getState x) state)
    | Add(x, y, z) :: clt -> doCommandList clt (Map.add z ((getState x) + (getState y)) state)
    | Sub(x, y, z) :: clt -> doCommandList clt (Map.add z ((getState x) - (getState y)) state)
    | Mul(x, y, z) :: clt -> doCommandList clt (Map.add z ((getState x) * (getState y)) state)
    | Div(x, y, z) :: clt -> doCommandList clt (Map.add z ((getState x) / (getState y)) state)
    | Sqrt(x, y) :: clt -> doCommandList clt (Map.add y (sqrt (getState x)) state)
    | Cmp(x, y) :: clt ->
        let xysum = (getState x) + (getState y)
        let Xgt, Xlt = (getState x) / xysum, (getState y) / xysum
        let XltState = Map.add "Xlt" Xlt state
        let XgtState = Map.add "Xgt" Xgt XltState
        doCommandList clt XgtState
    | Rx(p, r, c) :: clt -> failwith "Cannot interpret custom reactions!"
    | IfGT(cmdl) :: clt ->
        if getState "Xgt" > getState "Xlt" then
            doCommandList clt (doCommandList cmdl state)
        else
            doCommandList clt state
    | IfGE(cmdl) :: clt ->
        if getState "Xgt" > getState "Xlt" || eqCheck (getState "Xgt") (getState "Xlt") then
            doCommandList clt (doCommandList cmdl state)
        else
            doCommandList clt state
    | IfEQ(cmdl) :: clt ->
        if eqCheck (getState "Xgt") (getState "Xlt") then
            doCommandList clt (doCommandList cmdl state)
        else
            doCommandList clt state
    | IfLT(cmdl) :: clt ->
        if getState "Xgt" < getState "Xlt" then
            doCommandList clt (doCommandList cmdl state)
        else
            doCommandList clt state
    | IfLE(cmdl) :: clt ->
        if getState "Xgt" < getState "Xlt" || eqCheck (getState "Xgt") (getState "Xlt") then
            doCommandList clt (doCommandList cmdl state)
        else
            doCommandList clt state


let doStep (S(cl): Step) (state: State) : State = doCommandList cl state


let interpretProgram (R(concl, stepl)) =

    if not (isTyped (R(concl, stepl))) then
        failwith "Does not typecheck"
    else
        let initialState = getInitialState concl

        Seq.unfold
            (fun (state, i) ->
                let nextState = doStep (List.item (i % List.length stepl) stepl) state
                Some(nextState, (nextState, i + 1)))
            (initialState, 0)   


let rec commandToTree cmd =
    match cmd with
    | Ld(x, y) -> Node("Ld(" + x + "," + y + ")", [])
    | Add(x, y, z) -> Node("Add(" + x + "," + y + "," + z + ")", [])
    | Sub(x, y, z) -> Node("Sub(" + x + "," + y + "," + z + ")", [])
    | Mul(x, y, z) -> Node("Mul(" + x + "," + y + "," + z + ")", [])
    | Div(x, y, z) -> Node("Div(" + x + "," + y + "," + z + ")", [])
    | Sqrt(x, y) -> Node("Sqrt(" + x + "," + y + ")", [])
    | Cmp(x, y) -> Node("Cmp(" + x + "," + y + ")", [])
    | Rx(p, r, c) ->
        Node(
            "Rx("
            + List.fold (fun s x -> s + "+" + x) "" p
            + ","
            + List.fold (fun s x -> s + "+" + x) "" r
            + ","
            + (string c)
            + ")",
            []
        )
    | IfGT(cl) -> Node("IfGT", List.map commandToTree cl)
    | IfGE(cl) -> Node("IfGE", List.map commandToTree cl)
    | IfEQ(cl) -> Node("IfEQ", List.map commandToTree cl)
    | IfLT(cl) -> Node("IfLT", List.map commandToTree cl)
    | IfLE(cl) -> Node("IfLE", List.map commandToTree cl)


let concToTree (C(s, c)) =
    Node("C(" + s + "," + (string c) + ")", [])

let stepToTree (S(cl)) = Node("S", List.map commandToTree cl)


let astToTree (R(concl, stepl)) =
    Node("R", List.map concToTree concl @ List.map stepToTree stepl)
