// Rani Ey. í Bø 26/06

#r "nuget: FsCheck"
#r "nuget: FParsec"
#r "nuget: FSharp.Stats"
#load "CRNpp.fs"
#load "CRNParser.fs"
#load "CRNTypecheck.fs"
#load "Treecode.fs"
#load "CRNInterpreter.fs"
#load "Reactions.fs"
#load "CRNCompiler.fs"
// #load "ReactionsParser.fs"

open FsCheck
open FParsec

open CRNpp
open CRNParser
open CRNTypecheck
open Treecode
open CRNInterpreter
open Reactions
open CRNCompiler
// open ReactionsParser

let validPrograms = ["crn = {
    conc[c,3], conc[cInitial, 3],
    conc[one,1], conc[zero,0],
    step[{
        sub[c,one,cnext],
        cmp[c,zero]
    }],
    step[{
        ifGT[{ ld[cnext,c] }],
        ifLE[{ ld[cInitial, c] }]
    }]
}"; 
"crn={
    conc[a,20], conc[b,3], conc[one,1],
    step[{
        cmp[a,b]
    }],
    step[{
        ifGE[{
            sub[a,b,anext],
            add[q,one,qnext]
        }]
    }],
    step[{
        ifGE[{
            ld[anext,a],
            ld[qnext,q]
        }],
        ifLT[{
            ld[a,r]
        }]
    }]
}";
"crn={
    conc[e,1],conc[element,1],
    conc[divisor,1],conc[one,1],
    conc[divisorMultiplier,1],
    step[{
        div[element,divisor,elementNext],
        add[divisor,one,divisorNext],
        add[e,elementNext,eNext]
    }],
    step[{
        ld[elementNext,element],
        ld[divisorNext,divisor],
        ld[eNext,e]
    }]
}";
"crn={
    conc[f,1],conc[one,1],conc[i,5],
    step[{
        cmp[i,one],
        mul[f,i,fnext],
        sub[i,one,inext]
    }],
    step[{
        ifGT[{
            ld[inext,i],
            ld[fnext,f]
        }]
    }]
}";
"crn = {
    conc[a,32],
    conc[b,12],
    step[{
        ld[a,atmp],
        ld[b,btmp],
        cmp[a,b]
    }],
    step[{
        ifGT[{ sub[atmp,btmp,a] }],
        ifLT[{ sub[btmp,atmp,b] }]
    }]
}
";
"crn={
    conc[four,4],conc[divisor1,1],conc[divisor2,3],
    conc[pi,0],
    step[{
        div[four,divisor1,factor1],
        add[divisor1,four,divisor1Next],
        div[four,divisor2,factor2],
        add[divisor2,four,divisor2Next],
        sub[factor1,factor2,factor],
        add[pi,factor,piNext]
    }],
    step[{
        ld[divisor1Next,divisor1],
        ld[divisor2Next,divisor2],
        ld[piNext,pi]
    }]
}";
"crn = {
    conc[one,1],conc[n,10],
    step[{
        add[z,one,znext],
        mul[znext,znext,zpow],
        cmp[zpow,n]
    }],
    step[{
        ifLT[{ld[znext,z]}],
        ifGE[{ld[z,out]}]
    }]
}
";
"crn={
    conc[a,20],conc[b,13],
    conc[one,1],conc[zero,0],
    step[{
        cmp[b,zero]
    }],
    step[{
        ifGE[{
            sub[a,one,anext],
            sub[b,one,bnext]
        }]
    }],
    step[{
        ifGE[{
            ld[anext,a],
            ld[bnext,b]
        }]
    }]
}
"
]

let inValidPrograms = ["crn = {
    conc[c,3], conc[cInitial, 3],
    conc[one,1], conc[zero,0],
    step[{
        sub[c,one,cnext],
        cmp[c,zero]
    }],
    step[{
        ifGE[{ ld[cnext,c] }],
        ifLE[{ ld[cInitial, c] }]
    }]
}"; 
"crn={
    conc[a,20], conc[b,3], conc[one,1],
    step[{
        cmp[a,b]
    }],
    step[{
        ifGE[{
            sub[a,b,anext],
            add[q,one,a]
        }]
    }],
    step[{
        ifGE[{
            ld[anext,a],
            ld[qnext,q]
        }],
        ifLT[{
            ld[a,r]
        }]
    }]
}";
"crn={
    conc[e,-1],conc[element,1],
    conc[divisor,1],conc[one,1],
    conc[divisorMultiplier,1],
    step[{
        div[element,divisor,elementNext],
        add[divisor,one,divisorNext],
        add[e,elementNext,eNext]
    }],
    step[{
        ld[elementNext,element],
        ld[divisorNext,divisor],
        ld[eNext,e]
    }]
}";
"crn={
    conc[f,1],conc[one,1],conc[i,5],
    step[{
        cmp[i,one],
        mul[f,i,fnext],
        sub[i,one,inext],
        sub[fnext, inext,f]
    }],
    step[{
        ifGT[{
            ld[inext,i],
            ld[fnext,f]
        }]
    }]
}";
"crn = {
    conc[a,32],
    conc[b,12],
    step[{
        ld[a,atmp],
        ld[a,atmp],
        cmp[a,b]
    }],
    step[{
        ifGT[{ sub[atmp,btmp,a] }],
        ifLT[{ sub[btmp,atmp,b] }]
    }]
}
";
"crn={
    conc[four,4],conc[divisor1,1],conc[divisor2,3],
    conc[pi,0],
    step[{
        div[four,divisor1,factor1],
        add[divisor1,four,divisor1Next],
        div[four,divisor2,factor2],
        add[divisor2,four,divisor2Next],
        sub[factor1,factor2,factor],
        add[pi,factor,piNext],
        add[piNext,factor,four],
    }],
    step[{
        ld[divisor1Next,divisor1],
        ld[divisor2Next,divisor2],
        ld[piNext,pi]
    }]
}";
"crn = {
    conc[one,1],conc[n,10],
    step[{
        add[z,one,znext],
        mul[znext,znext,zpow],
        cmp[zpow,n]
        cmp[znext,n]
    }],
    step[{
        ifLT[{ld[znext,z]}],
        ifGE[{ld[z,out]}]
    }]
}
";
"crn={
    conc[a,20],conc[b,13],
    conc[one,1],conc[zero,0],
    step[{
        cmp[b,zero]
    }],
    step[{
        ifGE[{
            sub[a,one,anext],
            sub[b,one,bnext],
            ifGE[{
                ld[anext,a],
                ld[bnext,b]
            }]
        }]
    }],
    step[{
        ifGE[{
            ld[anext,a],
            ld[bnext,b]
        }]
    }]
}
"
]


let programs = validPrograms @ inValidPrograms
let shuffleList list =
    let rng = System.Random()
    list |> List.sortBy (fun _ -> rng.Next())

let rec shuffleSteps (R(conc, steps)) =
    R(conc, List.map (fun (S(c)) -> S(shuffleList c)) steps)
 

let implies a b = (not a) || b

let sequenceEqual (s1 : seq<State>) (s2 : seq<State>) =
    let mapsEqual m1 m2 = 
        let k1 = Map.fold (fun st k v -> Set.add k st) Set.empty m1
        let k2 = Map.fold (fun st k v -> Set.add k st) Set.empty m2
        k1 = k2 && Set.forall (fun k -> getValue m1 k = getValue m2 k) k1

    
    let map1 = Seq.take 5 s1 |> Seq.toList
    let map2 = Seq.take 5 s2 |> Seq.toList
    List.forall2 (fun m1 m2 -> mapsEqual m1 m2) map1 map2



let moduleSimTestResultHelper initState crn absa absb op = 
    let res = (abs((simulateReactionsMatrix initState crn 0.01 |> Seq.item 1000 |>  Map.find "c") - (op absa absb)))
    //printfn "%A\n" res
    res < max 0.01 ((op absa absb) / 200.0)


let stepOrderDoesNotMatter (programIdx: int) =
    let inputProgram = programs.[abs(programIdx) % (List.length programs)] |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> R([], [])
    if ast = R([], []) then true
    else
        let initState, _ = compileCRN ast
        let shufSteps = shuffleSteps ast
        (isTyped ast = isTyped shufSteps) &&
        (implies (isTyped ast) ((sequenceEqual (interpretProgram ast initState) (interpretProgram shufSteps initState))))




let addWorks (a:int) (b:int) =
    let absa = abs a
    let absb = abs b
    let concA = sprintf "%d" absa
    let concB = sprintf "%d" absb
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}], conc[b,{concB}],
            step[{{ add[a,b,c] }}]
        }}" |> rmws
    
    printfn "%A" inputProgram

    let ast = 
        match run pprogram inputProgram with 
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith $"Should not be reachable {errorMsg}"

    let initState, _ = compileCRN ast
    abs((interpretProgram ast initState |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - float ((absa + absb)))  < 0.01

let subWorks  (a:int) (b:int) =
    let absa = abs a
    let absb = abs b
    let concA = sprintf "%d" absa
    let concB = sprintf "%d" absb
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}], conc[b,{concB}],
            step[{{ sub[a,b,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"

    let initState, _ = compileCRN ast
    
    abs((interpretProgram ast initState |> Seq.skip 30 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - float (max (absa - absb) 0)) < 0.01


let divWorks  (a:int) (b:int) =
    let absa = abs a
    let absb = (abs b) + 1
    let concA = sprintf "%d" absa
    let concB = sprintf "%d" absb
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}], conc[b,{concB}],
            step[{{ div[a,b,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"    

    let initState, _ = compileCRN ast
    abs((interpretProgram ast initState |> Seq.skip 30 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (float absa / (float absb))) < 0.01

let mulWorks (a: int) (b: int) =
    let absa = abs a
    let absb = abs b
    let concA = sprintf "%d" absa
    let concB = sprintf "%d" absb
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}], conc[b,{concB}],
            step[{{ mul[a,b,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    let initState, _ = compileCRN ast    
    abs((interpretProgram ast initState |> Seq.skip 30 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - float (absa * absb)) < 0.01


let sqrtWorks (a:int) =
    let absa = abs a
    let concA = sprintf "%d" absa
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}],
            step[{{ sqrt[a,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    let initState, _ = compileCRN ast
    abs((interpretProgram ast initState |> Seq.skip 30 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (sqrt (float absa))) < 0.01


let loadWorks (a:int) =
    let absa = abs a
    let concA = sprintf "%d" absa
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}],
            step[{{ ld[a,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    let initState, _ = compileCRN ast
    abs((interpretProgram ast initState |> Seq.skip 30 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (float absa)) < 0.01


let cmpWorks (a: int) (b: int) =
    let absa = abs a
    let concA = sprintf "%d" absa
    let absb = abs b
    let concB = sprintf "%d" absb
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}],conc[b,{concB}],
            step[{{ cmp[a,b]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    let initState, _ = compileCRN ast
    let results = interpretProgram ast initState |> Seq.skip 30 |> Seq.take 1 |> Seq.toList |> List.head
    let (Xegty, Xelty) = if float absa + 0.5 > float absb then (1.0, 0.0) else (0.0, 1.0)
    let (Yegtx, Yeltx) = if float absb + 0.5 > float absa then (1.0, 0.0) else (0.0, 1.0)
    abs(Map.find "Xegty" results - Xegty) < 0.01 && abs(Map.find "Xelty" results - Xelty) < 0.01 && 
        abs(Map.find "Yegtx" results - Yegtx) < 0.01 && abs(Map.find "Yeltx" results - Yeltx) < 0.01
        


let addSimWorks (a: int) (b: int) =
    let absa = abs a
    let absb = abs b
    let concA = sprintf "%d" absa
    let concB = sprintf "%d" absb
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}], conc[b,{concB}],
            step[{{ add[a,b,c] }}]
        }}" |> rmws
    
    printfn "%A" inputProgram

    let ast = 
        match run pprogram inputProgram with 
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith $"Should not be reachable {errorMsg}"
    let initState, CRN = compileCRN ast

    moduleSimTestResultHelper initState CRN absa absb (fun x y -> float (x + y))


let subSimWorks (a: int) (b: int) =
    let absa = abs a
    let absb = abs b
    let concA = sprintf "%d" absa
    let concB = sprintf "%d" absb
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}], conc[b,{concB}],
            step[{{ sub[a,b,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    let initState, CRN = compileCRN ast

    moduleSimTestResultHelper initState CRN absa absb (fun x y -> max 0 (float (x-y)))


let divSimWorks (a: int) (b: int) =
    let absa = abs a
    let absb = (abs b) + 1
    let concA = sprintf "%d" absa
    let concB = sprintf "%d" absb
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}], conc[b,{concB}],
            step[{{ div[a,b,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    let initState, CRN = compileCRN ast

    moduleSimTestResultHelper initState CRN absa absb (fun x y -> float x / float y)


let mulSimWorks (a: int) (b: int) =
    let absa = abs a
    let absb = abs b
    let concA = sprintf "%d" absa
    let concB = sprintf "%d" absb
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}], conc[b,{concB}],
            step[{{ mul[a,b,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    let initState, CRN = compileCRN ast

    moduleSimTestResultHelper initState CRN absa absb (fun x y -> float (x * y))



let sqrtSimWorks (a: int) =
    let absa = abs a
    let concA = sprintf "%d" absa
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}],
            step[{{ sqrt[a,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    let initState, CRN = compileCRN ast

    moduleSimTestResultHelper initState CRN absa 0 (fun x y -> sqrt (float x))


let loadSimWorks (a: int) =
    let absa = abs a
    let concA = sprintf "%d" absa
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}],
            step[{{ ld[a,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    let initState, CRN = compileCRN ast

    moduleSimTestResultHelper initState CRN absa 0 (fun x y -> float x)


let cmpSimWorks (a: int) (b: int) =
    let absa = abs a
    let concA = sprintf "%d" absa
    let absb = abs b
    let concB = sprintf "%d" absb
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}],conc[b,{concB}],
            step[{{ cmp[a,b]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    let initState, CRN = compileCRN ast
    let results = simulateReactionsMatrix initState CRN 0.01 |> Seq.skip 500 
    let (Xegty, Xelty) = if float absa + 0.5 > float absb then (1.0, 0.0) else (0.0, 1.0)
    let (Yegtx, Yeltx) = if float absb + 0.5 > float absa then (1.0, 0.0) else (0.0, 1.0)
    let samples = results
                |> Seq.indexed
                |> Seq.choose (fun (index, element) ->
                    if index % 50 = 0 then Some element
                    else None) |> Seq.take 200 |> Seq.toList 
    List.fold (fun acc res -> acc || ((abs(Map.find "Xegty" res - Xegty) < 0.1) && (abs(Map.find "Xelty" res - Xelty) < 0.1) && 
                                (abs(Map.find "Yegtx" res - Yegtx) < 0.1) && (abs(Map.find "Yeltx" res - Yeltx) < 0.1))) false samples



let validProgramsWithRandomInput = ["crn = {
    conc[c,{c}], conc[cInitial, 3],
    conc[one,1], conc[zero,0],
    step[{
        sub[c,one,cnext],
        cmp[c,zero]
    }],
    step[{
        ifGT[{ ld[cnext,c] }],
        ifLE[{ ld[cInitial, c] }]
    }]
}"; 
"crn={
    conc[a,{a}], conc[b,{b}], conc[one,1],
    step[{
        cmp[a,b]
    }],
    step[{
        ifGE[{
            sub[a,b,anext],
            add[q,one,qnext]
        }]
    }],
    step[{
        ifGE[{
            ld[anext,a],
            ld[qnext,q]
        }],
        ifLT[{
            ld[a,r]
        }]
    }]
}";
"crn={
    conc[e,1],conc[element,1],
    conc[divisor,1],conc[one,1],
    conc[divisorMultiplier,1],
    step[{
        div[element,divisor,elementNext],
        add[divisor,one,divisorNext],
        add[e,elementNext,eNext]
    }],
    step[{
        ld[elementNext,element],
        ld[divisorNext,divisor],
        ld[eNext,e]
    }]
}";
"crn={
    conc[f,1],conc[one,1],conc[i,5],
    step[{
        cmp[i,one],
        mul[f,i,fnext],
        sub[i,one,inext]
    }],
    step[{
        ifGT[{
            ld[inext,i],
            ld[fnext,f]
        }]
    }]
}";
"crn = {
    conc[a,{a}],
    conc[b,{b}],
    step[{
        ld[a,atmp],
        ld[b,btmp],
        cmp[a,b]
    }],
    step[{
        ifGT[{ sub[atmp,btmp,a] }],
        ifLT[{ sub[btmp,atmp,b] }]
    }]
}
";
"crn={
    conc[four,4],conc[divisor1,1],conc[divisor2,3],
    conc[pi,0],
    step[{
        div[four,divisor1,factor1],
        add[divisor1,four,divisor1Next],
        div[four,divisor2,factor2],
        add[divisor2,four,divisor2Next],
        sub[factor1,factor2,factor],
        add[pi,factor,piNext]
    }],
    step[{
        ld[divisor1Next,divisor1],
        ld[divisor2Next,divisor2],
        ld[piNext,pi]
    }]
}";
"crn = {
    conc[one,1],conc[n,10],
    step[{
        add[z,one,znext],
        mul[znext,znext,zpow],
        cmp[zpow,n]
    }],
    step[{
        ifLT[{ld[znext,z]}],
        ifGE[{ld[z,out]}]
    }]
}
";
"crn={
    conc[a,{a}],conc[b,{b}],
    conc[one,1],conc[zero,0],
    step[{
        cmp[b,zero]
    }],
    step[{
        ifGE[{
            sub[a,one,anext],
            sub[b,one,bnext]
        }]
    }],
    step[{
        ifGE[{
            ld[anext,a],
            ld[bnext,b]
        }]
    }]
}
"
]

let interpreterAndSimAgree (programIdx : int) (a: int) (b: int) (c: int) =
    let inputPro = validProgramsWithRandomInput.[abs(programIdx) % (List.length validProgramsWithRandomInput)] |> rmws
    let inputProgram = 
        inputPro.Replace("{a}", (abs(a)+1).ToString())
                .Replace("{b}", (abs(b)+1).ToString())
                .Replace("{c}", (abs(c)+1).ToString())
    printfn "%A" inputProgram
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> R([], [])

    
    let ignoreSet = Set.ofList ["Xegty";"Xelty";"Yegtx";"Yeltx";"T1";"T2";"T3";"T4";"T5";"T6";"T7";"T8";"T9";"T10";"T11";"T12"; "CMP1";"CMP2";"H0";""]


    if ast = R([], []) then true else 
    let initState, _ = compileCRN ast    
    not (isTyped ast) ||
    let interpreterState = Seq.item 0 (interpretProgram ast initState)
    let initState,crn = compileCRN ast
    let simulationState = Seq.item 2000 (simulateReactionsMatrix initState crn 0.01)
    let speciesToCheck = Map.fold (fun st k v -> if not (Set.contains k ignoreSet) then Set.add k st else st) Set.empty interpreterState
    Set.forall (fun s ->abs  ((getValue simulationState s) - (getValue interpreterState s)) < 0.2) speciesToCheck


// FSCheck tests ////////////////////////

let config = { Config.Quick with MaxTest = 1000 }

Check.One(config, stepOrderDoesNotMatter)

Check.One(config, addWorks)
Check.One(config, subWorks)
Check.One(config, divWorks)
Check.One(config, mulWorks)
Check.One(config, sqrtWorks)
Check.One(config, loadWorks)
Check.One(config, cmpWorks)

Check.One(config, addSimWorks)
Check.One(config, subSimWorks)
Check.One(config, divSimWorks)
Check.One(config, mulSimWorks)
Check.One(config, sqrtSimWorks)
Check.One(config, loadSimWorks)
Check.One(config, cmpSimWorks)

Check.One(config, interpreterAndSimAgree)