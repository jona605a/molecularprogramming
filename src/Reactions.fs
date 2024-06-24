module Reactions

open CRNpp

open CRNInterpreter

type Reaction = Rxn of Map<species, float> * Map<species, float> * float

type CRN = Reaction List

let getValue map s =
    if Map.containsKey s map then Map.find s map else 0.0

let calcNetChange (Rxn(r, p, c)) (s: species) = (getValue p s) - (getValue r s)

let calcReactionEffect (state: State) (Rxn(r, p, c)) (s: species) =
    c
    * (calcNetChange ((Rxn(r, p, c))) s)
    * (Map.fold (fun st reac mult -> st * (getValue state reac) ** mult) 1.0 r)


let calcSpeciesChange (state: State) (crn: CRN) (s: species) =
    List.fold (fun chg rxn -> chg + (calcReactionEffect state rxn s)) 0.0 crn


let simulationStep (state: State) (crn: CRN) (timestep: float) =
    let occurringSpecies =
        Set.ofList (
            List.fold
                (fun sp (Rxn(r, p, c)) ->
                    Map.fold (fun keys k _ -> k :: keys) [] r
                    @ Map.fold (fun keys k _ -> k :: keys) [] p
                    @ sp)
                []
                crn
        )

    Map.add
        "Ø"
        0.0
        (Set.fold
            (fun st s -> Map.add s (max 0.00001 (max 0 ((getValue state s) + ((calcSpeciesChange state crn s) * timestep)))) st)
            state
            occurringSpecies)


let simulateReactions (state: State) (crn: CRN) (timestep: float) =
    Seq.unfold
        (fun (st, network, time) ->
            let nextstate = simulationStep st network time
            Some((nextstate, (nextstate, network, time))))
        (state, crn, timestep)
