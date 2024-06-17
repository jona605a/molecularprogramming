module CRNpp

type Rxns = Rx of string List * string List * float

type Module =
    | Add of string * string * string
    | Sub of string * string * string
    | Mul of string * string * string
    | Div of string * string * string
    | Sqrt of string * string
    | Cmp of string * string

type Command =
    | Module
    | Conditional
    | Rxns
and Conditional =
    | IfGT of Command List
    | IfGE of Command List
    | IfEQ of Command List
    | IfLT of Command List
    | IfLE of Command List

type Step = S of Command List

type Conc = C of string * float

type Root = R of Conc List * Step List
