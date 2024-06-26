// Jonathan Højlev, 26/6

module Treecode

type Tree<'a> = Node of 'a * (Tree<'a> list)

let movetree tree x' =
    match tree with
    | Node((label, x), subtrees) -> Node((label, x + x'), subtrees)

type Extent = (float * float) list

// Functions for extents using Absolute coordinates. The relative are better.
let moveextent (e: Extent, x) : Extent =
    List.map (fun (p, q) -> (p + x, q + x)) e

let rec merge (e1: Extent) (e2: Extent) =
    match (e1, e2) with
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p, _) :: ps, (_, q) :: qs) -> (p, q) :: merge ps qs


let mergelist (es: Extent list) : Extent = List.fold merge [] es


let rec fit (e1: Extent) (e2: Extent) : float =
    match (e1, e2) with
    | ((_, p) :: ps, (q, _) :: qs) -> max (fit ps qs) (p - q + 1.0)
    | _ -> 0.0


let fitlistl es =
    let rec fitlistl' acc ex =
        match ex with
        | [] -> []
        | e :: es ->
            let x = fit acc e
            x :: fitlistl' (merge acc (moveextent (e, x))) es

    fitlistl' [] es


let fitlistr es =
    let rec fitlistr' acc ex =
        match ex with
        | [] -> []
        | e :: es ->
            let x = -(fit e acc)
            x :: fitlistr' (merge (moveextent (e, x)) acc) es

    List.rev (fitlistr' [] (List.rev es))

let mean (x, y) = (x + y) / 2.0

let fitlist es =
    List.map mean (List.zip (fitlistl es) (fitlistr es))

let design tree =
    let rec design' =
        function
        | Node(label, subtrees) ->
            let (trees, extents) = List.unzip (List.map design' subtrees)
            let positions = fitlist extents
            let ptrees = List.map2 movetree trees positions
            let pextents = List.map moveextent (List.zip extents positions)
            let resultextent = (0.0, 0.0) :: mergelist pextents
            let resulttree = Node((label, 0.0), ptrees)
            (resulttree, resultextent)

    fst (design' tree)



let designtostring (designtree: Tree<'a * float>) : string =
    let rec tostring dt depth parentx (parent: int, nodeid: int) : string * int =
        match dt with
        | Node((l, x), ch) ->
            let pos = parentx + x

            let descstring, newid =
                List.fold
                    (fun (acc, lastgivenid) c ->
                        let s, i = (tostring c (depth + 1) pos (nodeid, lastgivenid + 1))
                        s + acc, i)
                    ("", nodeid)
                    ch

            (sprintf "%A, %A, %A" l (pos, depth) (nodeid, parent) + "\n" + descstring), newid

    fst (tostring designtree 0 0.0 (0, 0))
