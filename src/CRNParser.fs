module CRNParser

open CRNpp
open FParsec

let ws = spaces

let identifier: Parser<string, unit> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws


// module parsers

let seperator s = ws .>> pstring s .>> ws

let comma = seperator ","

let parugment = ws >>. identifier .>> comma

let pfinalArgument = ws >>. identifier .>> (ws .>> pstring "]")

let p2argModule s f =
    pstring (s + "[") >>. ws >>. pipe2 parugment pfinalArgument f

let p3argModule s f =
    pstring (s + "[") >>. ws >>. pipe3 parugment parugment pfinalArgument f

let pld = p2argModule "ld" (fun x y -> Ld(x, y))

let psqrt = p2argModule "sqrt" (fun x y -> Sqrt(x, y))

let pcmp = p2argModule "cmp" (fun x y -> Cmp(x, y))

let padd = p3argModule "add" (fun x y z -> Add(x, y, z))

let psub = p3argModule "sub" (fun x y z -> Sub(x, y, z))

let pmul = p3argModule "mul" (fun x y z -> Mul(x, y, z))

let pdiv = p3argModule "div" (fun x y z -> Div(x, y, z))

let pmodule = (pld <|> psqrt <|> pcmp <|> padd <|> psub <|> pmul <|> pdiv)


// reactions

let pexpr = ws >>. (sepBy1 identifier (seperator "+"))

let prxn =
    pstring ("rxn" + "[")
    >>. ws
    >>. pipe3 (pexpr .>> comma) (pexpr .>> comma) (ws >>. pfloat) (fun x y z -> Rx(x, y, z))
    .>> ws
    .>> pstring "]"

// conditionals

let rec pstep = ws .>> (prxn <|> pmodule <|> pconditional)

and pconditional =
    (pstring "ifGT["
     <|> pstring "ifGE["
     <|> pstring "ifEQ["
     <|> pstring "ifLT["
     <|> pstring "ifLE[") >>. pstep .>> ws .>> pstring "]"
