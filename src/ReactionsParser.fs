module ReactionsParser


open Reactions
open FParsec

let ws = spaces


let identifier: Parser<string, unit> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws


let speciesListToMap = List.fold (fun map s -> if Map.containsKey s map then Map.add s ((Map.find s map) + 1.0) map else Map.add s 1.0 map) Map.empty 


let preactants = (sepBy1 identifier (pstring "+")) .>> pstring "->"

let pproducts = (sepBy1 identifier (pstring "+")) 

let preaction = pipe3 preactants pproducts ((pstring "," >>. pfloat) <|> stringReturn "" 1.0 ) (fun x y z -> Rxn(speciesListToMap x,speciesListToMap y,z))

let preactions = sepBy preaction (pstring "/")