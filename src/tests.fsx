#r "nuget: FsCheck"
#r "nuget: FParsec"
#load "CRNpp.fs"
#load "CRNParser.fs"
#load "CRNTypecheck.fs"
#load "Treecode.fs"
#load "CRNInterpreter.fs"
#load "Reactions.fs"
#load "ReactionsParser.fs"
#load "CRNCompiler.fs"

open System.Collections.Generic
open FsCheck
open CRNpp
open CRNTypecheck
open CRNParser
open Treecode
open CRNInterpreter
open Reactions
open ReactionsParser
open CRNCompiler
open FParsec

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

let sequenceEqual s1 s2 =
    let mapsEqual m1 m2 = 
        let k1 = Map.keys m1 |> Seq.cast |> Set.ofSeq
        let k2 = Map.keys m2  |> Seq.cast |> Set.ofSeq
        let keystEqual = Set.difference k1 k2 = (Set.difference k2 k1) && Set.difference k1 k2 = Set.empty
        keystEqual && Set.forall (fun k -> 
            let v1 = Map.find m1 k
            let v2 = Map.find m2 k
            (System.Double.IsNaN(v1) && System.Double.IsNaN(v2)) || (v1 = v2)) k1
    
    let map1 = Seq.take 50 s1 |> Seq.toList
    let map2 = Seq.take 50 s2 |> Seq.toList
    List.forall2 (fun m1 m2 -> mapsEqual m1 m2) map1 map2


let stepOrderDoesNotMatter (programIdx: int) =
    let inputProgram = programs.[programIdx % (List.length programs)] |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> R([], [])
    if ast = R([], []) then true
    else
        let shufSteps = shuffleSteps ast
        (isTyped ast = isTyped shufSteps)
        && (implies (isTyped ast) ((sequenceEqual (interpretProgram ast) (interpretProgram shufSteps))))



let addWorks (NormalFloat(a)) (NormalFloat(b)) =
    let absa = abs a
    let absb = abs b
    let concA = sprintf "%.1f" absa
    let concB = sprintf "%.1f" absb
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

    printfn "%A\n" (abs((interpretProgram ast |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (absa + absb)))
    abs((interpretProgram ast |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (absa + absb)) < 0.01


let subWorks (NormalFloat(a)) (NormalFloat(b)) =
    let concA = sprintf "%.1f" (abs a)
    let concB = sprintf "%.1f" (abs b)
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}], conc[b,{concB}],
            step[{{ sub[a,b,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    
    abs((interpretProgram ast |> Seq.skip 30 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (max(a - b) 0)) < 0.01


let divWorks (NormalFloat(a)) (NormalFloat(b)) =
    let concA = sprintf "%.1f" (abs a)
    let concB = sprintf "%.1f" ((abs b) + 0.01)
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}], conc[b,{concB}],
            step[{{ div[a,b,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"

    printfn "%A\n" (abs((interpretProgram ast |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a / b)))
    abs((interpretProgram ast |> Seq.skip 30 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a / b)) < 0.01

let mulWorks (NormalFloat(a)) (NormalFloat(b)) =
    let concA = sprintf "%.1f" (abs a)
    let concB = sprintf "%.1f" (abs b)
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}], conc[b,{concB}],
            step[{{ mul[a,b,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    printfn "%A\n" (abs((interpretProgram ast |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a * b)))
    
    abs((interpretProgram ast |> Seq.skip 30 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a * b)) < 0.01


let sqrtWorks (NormalFloat(a)) =
    let concA = sprintf "%.1f" (abs a)
    let inputProgram = 
        $"crn = {{
            conc[a,{concA}],
            step[{{ sqrt[a,c]}}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"

    printfn "%A\n" (abs((interpretProgram ast |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (sqrt a)))
    abs((interpretProgram ast |> Seq.skip 30 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (sqrt a)) < 0.01



let cmpWorks (NormalFloat(a)) (NormalFloat(b)) =
    let inputProgram = 
        $"crn = {{
            conc[a,{a}], conc[cInitial,{b}],
            step[{{ cmp[a,b], }}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    if a-b > 0.01 then // Todo
        true
    elif b-a > 0.01 then 
        true 
    else 
        true






let addSimWorks (NormalFloat(a)) (NormalFloat(b)) =
    let concA = sprintf "%.1f" (abs a)
    let concB = sprintf "%.1f" (abs b)
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

    printfn "%A\n" (abs((simulateReactions initState CRN 0.01 |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a + b)))
    abs((simulateReactions initState CRN 0.01 |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a + b)) < 0.01


let subSimWorks (NormalFloat(a)) (NormalFloat(b)) =
    let concA = sprintf "%.1f" (abs a)
    let concB = sprintf "%.1f" (abs b)
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

    printfn "%A\n" (abs((simulateReactions initState CRN 0.01 |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a - b)))
    abs((simulateReactions initState CRN 0.01 |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a - b)) < 0.01


let divSimWorks (NormalFloat(a)) (NormalFloat(b)) =
    let concA = sprintf "%.1f" (abs a)
    let concB = sprintf "%.1f" ((abs b) + 0.01)
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

    printfn "%A\n" (abs((simulateReactions initState CRN 0.01 |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a / b)))
    abs((simulateReactions initState CRN 0.01 |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a / b)) < 0.01


let mulSimWorks (NormalFloat(a)) (NormalFloat(b)) =
    let concA = sprintf "%.1f" (abs a)
    let concB = sprintf "%.1f" (abs b)
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

    printfn "%A\n" (abs((simulateReactions initState CRN 0.01 |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a * b)))
    abs((simulateReactions initState CRN 0.01 |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (a * b)) < 0.01



let sqrtSimWorks (NormalFloat(a)) =
    let concA = sprintf "%.1f" (abs a)
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

    printfn "%A\n" (abs((simulateReactions initState CRN 0.01 |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (sqrt a)))
    abs((simulateReactions initState CRN 0.01 |> Seq.skip 100 |> Seq.take 1 |> Seq.toList |> List.head |>  Map.find "c") - (sqrt a)) < 0.01



let cmpSimWorks (NormalFloat(a)) (NormalFloat(b)) =
    let inputProgram = 
        $"crn = {{
            conc[a,{a}], conc[cInitial,{b}],
            step[{{ cmp[a,b], }}]
        }}" |> rmws
    let ast = 
        match run pprogram inputProgram with
            | Success((res: CRNpp.Root), _, _) -> res
            | Failure(errorMsg, _, _) -> failwith "Should not be reachable"
    if a-b > 0.01 then // Todo
        true
    elif b-a > 0.01 then 
        true 
    else 
        true
// FSCheck tests ////////////////////////

let config = { Config.Quick with MaxTest = 1000 }

Check.One(config, stepOrderDoesNotMatter)

Check.One(config, addWorks)
Check.One(config, subWorks)
Check.One(config, divWorks)
Check.One(config, mulWorks)
Check.One(config, sqrtWorks)

Check.One(config, addSimWorks)
Check.One(config, subSimWorks)
Check.One(config, divSimWorks)
Check.One(config, mulSimWorks)
Check.One(config, sqrtSimWorks)