module CRNCompiler

open CRNpp
open Reactions
open ReactionsParser

let listsToReaction l1 l2 = Rxn(Map.ofList (List.map (fun x -> (x,1.0)) l1) , Map.ofList (List.map (fun x -> (x,1.0)) l2),1)


let commandToReactions cmd subcnt = match cmd with
                                    | Ld(x,y) -> [listsToReaction [x] [x;y]; listsToReaction [y] ["Ø"]]
                                    | Add(x,y,z) -> []