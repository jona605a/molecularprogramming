// Hans Henrik Hermansen 26/06 // Rani Ey. í Bø 26/06
module Reactions

open CRNpp
open CRNInterpreter
open FSharp.Stats

type Reaction = Rxn of Map<species, float> * Map<species, float> * float

type CRN = Reaction List

let getValue map s =
    if Map.containsKey s map then Map.find s map else 0.0 


let calcNetChange (Rxn(r, p, c)) (s: species) = (getValue p s) - (getValue r s) 


let reactionToList (Rxn(r,_,_)) (sl : species List) = (List.foldBack (fun s st -> (getValue r s) :: st) sl [])


let genReactantMatrix (crn : CRN) (sl : species List) = 
    Matrix.ofJaggedList (List.fold (fun st r -> reactionToList r sl :: st) [] crn)
    

let genNetChangeList (crn : CRN) (s : species) = (List.foldBack (fun (Rxn(r,p,c)) st -> c * (calcNetChange (Rxn(r,p,c)) s) :: st) crn [])

let genNetChangeMatrix (crn : CRN) (sl : species List) = Matrix.ofJaggedList (List.foldBack (fun s st -> genNetChangeList crn s :: st) sl [])

let calcReactionProducts (state : State) (crn : CRN) = vector (List.foldBack (fun  (Rxn(r,_,_)) st -> Map.fold (fun prod s mp -> prod * (getValue state s) ** mp  ) 1.0 r :: st) crn []) 

let calcDerivatives (netChangeMatrix : Matrix<float>) (reactionProduct : Vector<float>) (timestep : float) = timestep * (netChangeMatrix * reactionProduct) 

let simulateStepMatrix (state : State) (crn : CRN) (netChangeMatrix : Matrix<float>) (sl : species List) (timestep : float) = 
    let change = calcDerivatives netChangeMatrix (calcReactionProducts state crn) timestep
    Map.add "Ø" 0.0 (List.fold (fun map (s,ds) -> Map.add s ((getValue state s) + ds) map) state (List.zip sl (List.ofArray (Vector.toArray change))))

let simulateReactionsMatrix (state : State) (crn : CRN) (timestep : float) = 
    let sl = Set.toList (Set.ofList (
            List.fold
                (fun sp (Rxn(r, p, _)) ->
                    Map.fold (fun keys k _ -> k :: keys) [] r
                    @ Map.fold (fun keys k _ -> k :: keys) [] p
                    @ sp)
                []
                crn
        ))
    let netChangeMatrix = genNetChangeMatrix crn sl

    Seq.unfold (fun st -> let nextState = simulateStepMatrix st crn netChangeMatrix sl timestep
                          Some(nextState,nextState)) state

