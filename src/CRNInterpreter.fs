module CRNInterpreter
open CRNpp

type State = (species*float) List



let isTyped (R(concs, steps): Root) : bool = 
    true


let getInitialState (r: Root): State =
    []


let doStep (step: Step) (state: State) : State =
    []








