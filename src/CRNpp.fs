module CRNpp

type species = string

type Rxns = Rx of species List * species List * float

type Module =
    | Ld of species * species
    | Add of species * species * species
    | Sub of species * species * species
    | Mul of species * species * species
    | Div of species * species * species
    | Sqrt of species * species
    | Cmp of species * species

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

type Conc = C of species * float

type Root = R of Conc List * Step List