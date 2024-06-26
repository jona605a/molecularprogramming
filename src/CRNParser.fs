// Hans Henrik Hermansen 26/06
module CRNParser

open CRNpp
open FParsec

let ws = spaces

let rmws (s: string) =
    s.Replace(" ", "").Replace("\t", "").Replace("\n", "").Replace("\r", "")

let identifier: Parser<string, unit> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws


// module parsers

let comma = pstring ","

let parugment = ws >>. identifier .>> comma

let pfinalArgument = identifier .>> pstring "]"

let p2argModule s f =
    pstring (s + "[") >>. pipe2 parugment pfinalArgument f

let p3argModule s f =
    pstring (s + "[") >>. pipe3 parugment parugment pfinalArgument f

let pld = p2argModule "ld" (fun x y -> Ld(x, y))

let psqrt = p2argModule "sqrt" (fun x y -> Sqrt(x, y))

let pcmp = p2argModule "cmp" (fun x y -> Cmp(x, y))

let padd = p3argModule "add" (fun x y z -> Add(x, y, z))

let psub = p3argModule "sub" (fun x y z -> Sub(x, y, z))

let pmul = p3argModule "mul" (fun x y z -> Mul(x, y, z))

let pdiv = p3argModule "div" (fun x y z -> Div(x, y, z))

let pmodule = (pld <|> psqrt <|> pcmp <|> padd <|> psub <|> pmul <|> pdiv)


// reactions

let pexpr = (sepBy1 identifier (pstring "+"))

let prxn =
    pstring ("rxn" + "[")
    >>. pipe3 (pexpr .>> comma) (pexpr .>> comma) (ws >>. pfloat) (fun x y z -> Rx(x, y, z))
    .>> pstring "]"

// conditionals

let pconditional s f =
    pipe2 (pstring (s + "[{")) (sepBy1 ((pmodule <|> prxn)) comma) f
    .>> pstring "}]"

let pifGT = pconditional "ifGT" (fun x y -> IfGT(y))

let pifGE = pconditional "ifGE" (fun x y -> IfGE(y))

let pifEQ = pconditional "ifEQ" (fun x y -> IfEQ(y))

let pifLT = pconditional "ifLT" (fun x y -> IfLT(y))

let pifLE = pconditional "ifLE" (fun x y -> IfLE(y))

let pconditionals = pifGT <|> pifGE <|> pifEQ <|> pifLT <|> pifLE


// steps

let pcommands = sepBy1 (prxn <|> pmodule <|> pconditionals) comma

let pstep = pipe2 (pstring "step[{") pcommands (fun x y -> S(y)) .>> pstring "}]"

// conc

let pconc =
    pstring "onc[" >>. pipe2 identifier (comma >>. pfloat) (fun x y -> C(x, y))
    .>> pstring "],"



let pprogram =
    pstring "crn={"
    >>. pipe2 ((pstring "c" >>. sepBy pconc (pstring "c")) <|> stringReturn "" []) (sepBy1 pstep comma) (fun x y ->
        R(x, y))
    .>> (pstring "}" <|> pstring "};")
