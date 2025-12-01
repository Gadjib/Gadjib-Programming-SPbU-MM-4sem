module LambdaInterpreter

type Expr = 
    | Var of string
    | App of Expr * Expr
    | Abs of string * Expr

let rec freeVars expr =
    match expr with
    | Var x -> set [x]
    | App (s, t) -> Set.union (freeVars s) (freeVars t)
    | Abs (x, s) -> Set.remove x (freeVars s)

let newName old (used: Set<string>) =
    let rec newNameHelper i =
        let t = old + string i
        if used.Contains t then newNameHelper (i + 1)
        else t
    in
    newNameHelper 0

let rec substitude x t s =
    match s with
    | Var y -> if y = x then t else s
    | App (s1, s2) -> App (substitude x t s1, substitude x t s2)
    | Abs (y, s1) -> 
        if y = x then s
        else
            let freeVarsT = freeVars t in
            let freeVarsS = freeVars s1 in
            if not (freeVarsT.Contains y) || not (freeVarsS.Contains x) then
                Abs (y, substitude x t s1)
            else
                let used = Set.unionMany [ freeVarsT; freeVarsS ]
                let z = newName y used
                Abs (z, substitude x t (substitude y (Var z) s1))


let rec betaReductionStep expr =
    match expr with
    | App (Abs (x, s), t) -> Some (substitude x t s)
    | App (s, u) -> 
        match betaReductionStep s with
        | Some s1 -> Some (App (s1, u))
        | None -> 
            match betaReductionStep u with
            | Some u1 -> Some (App (s, u1))
            | None -> None
    | Abs (x, s) ->
        match betaReductionStep s with
        | Some t -> Some (Abs (x, t))
        | None -> None
    | Var x -> None
in
let rec betaReduction expr =
    match betaReductionStep expr with
    | Some expr1 -> betaReduction expr1
    | None -> expr