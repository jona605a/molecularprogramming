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
    | Sub(x, y, z) :: clt -> doCommandList clt (Map.add z (max 0.0 ((getState x) - (getState y))) state)
    | Mul(x, y, z) :: clt -> doCommandList clt (Map.add z ((getState x) * (getState y)) state)
    | Div(x, y, z) :: clt -> doCommandList clt (Map.add z ((getState x) / (getState y)) state)
    | Sqrt(x, y) :: clt -> doCommandList clt (Map.add y (sqrt (getState x)) state)
    | Cmp(x, y) :: clt ->
        let xestate = if getState x + 0.5 > getState y then Map.add "Xegty" 1.0 (Map.add "Xelty" 0.0 state) else Map.add "Xegty" 0.0 (Map.add "Xelty" 1.0 state)
        let yestate = if getState y + 0.5 > getState x then Map.add "Yegtx" 1.0 (Map.add "Yeltx" 0.0 xestate) else Map.add "Yegtx" 0.0 (Map.add "Yeltx" 1.0 xestate)
        doCommandList clt yestate
    | Rx(p, r, c) :: clt -> failwith "Cannot interpret custom reactions!"
    | IfGT(cmdl) :: clt ->
        if getState "Yeltx" = 1.0 then
            doCommandList clt (doCommandList cmdl state)
        else
            doCommandList clt state
    | IfGE(cmdl) :: clt ->
        if getState "Xegty" = 1.0 then
            doCommandList clt (doCommandList cmdl state)
        else
            doCommandList clt state
    | IfEQ(cmdl) :: clt ->
        if getState "Xegty" = 1.0 && getState "Yegtx" = 1.0 then
            doCommandList clt (doCommandList cmdl state)
        else
            doCommandList clt state
    | IfLT(cmdl) :: clt ->
        if getState "Xelty" = 1.0 then
            doCommandList clt (doCommandList cmdl state)
        else
            doCommandList clt state
    | IfLE(cmdl) :: clt ->
        if getState "Yegtx" = 1.0 then
            doCommandList clt (doCommandList cmdl state)
        else
            doCommandList clt state


let doStep (S(cl): Step) (state: State) : State = doCommandList cl state


let interpretProgram (R(concl, stepl)) (initialState: State)=

    if not (isTyped (R(concl, stepl))) then
        failwith "Does not typecheck"
    else

        Seq.unfold
            (fun (state, i) ->
                let nextState = doStep (List.item (i % List.length stepl) stepl) state
                Some(nextState, (nextState, i + 1)))
            (initialState, 0)


// Code for converting a CRN program AST to a tree with nodes corresponding to the AST, for plotting.
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
