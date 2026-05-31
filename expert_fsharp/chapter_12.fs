module chapter_12

    module representing_propositional_logic =
        
        type Var = string

        type Prop =
            | And of Prop * Prop
            | Var of Var
            | Not of Prop
            | Exists of Var * Prop
            | False

        let True
            = Not False
        let Or(p, q)
            = Not(And(Not(p), Not(q)))
        let Iff(p, q)
            = Or(And(p,q), And(Not(p), Not(q)))
        let Implies(p, q) =
            Or(Not(p), q)
        let Forall(v, p) =
            Not(Exists(v, Not(p)))

        let (&&&) p q = And(p, q)
        let (|||) p q = Or(p, q)
        let (~~~) p = Not p
        let (<=>) p q = Iff(p, q)
        let (===) p q = (p <=> q)
        let (==>) p q = Implies (p, q)
        let (^^^) p q = Not(p <=> q)

        let var (nm: Var) = Var nm

        //let fresh =
        //    let count = ref 0
        //    fun nm -> incr count; (sprintf "_%s%d" nm !count : Var)
            
        let fresh =
            let count = ref 0
            fun nm -> count.Value <- count.Value + 1; (sprintf "_%s%d" nm count.Value : Var)

        let run() =
            And (Var("raining"), Var("Cold"))
            Implies (Var("raining"), Var("Cold"))
            
    module evaluating_propositional_logic_naively =
        
        type Var = string

        type Prop =
            | And of Prop * Prop
            | Var of Var
            | Not of Prop
            | Exists of Var * Prop
            | False

        let True
            = Not False
        let Or(p, q)
            = Not(And(Not(p), Not(q)))
        let Iff(p, q)
            = Or(And(p,q), And(Not(p), Not(q)))
        let Implies(p, q) =
            Or(Not(p), q)
        let Forall(v, p) =
            Not(Exists(v, Not(p)))

        let (&&&) p q = And(p, q)
        let (|||) p q = Or(p, q)
        let (~~~) p = Not p
        let (<=>) p q = Iff(p, q)
        let (===) p q = (p <=> q)
        let (==>) p q = Implies (p, q)
        let (^^^) p q = Not(p <=> q)

        let var (nm: Var) = Var nm

        let fresh =
            let count = ref 0
            fun nm -> count.Value <- count.Value + 1; (sprintf "_%s%d" nm count.Value : Var)

        let rec eval (env: Map<Var,bool>) inp =
            match inp with
            | Exists(v, p)
                                    -> eval (env.Add(v, false)) p || eval (env.Add(v, true)) p
            | And(p1, p2)
                                    -> eval env p1 && eval env p2
            | Var v
                                    -> if env.ContainsKey(v) then env.[v]
                                       else failwithf "env did not contain a value for %A" v
            | Not p
                                    -> not (eval env p)
            | False
                                    -> false
        
        let rec support f =
            match f with
            | And(x, y)
                                    -> Set.union (support x) (support y)
            | Exists(v, p)
                                    -> (support p).Remove(v)
            | Var p
                                    -> Set.singleton p
            | Not x 
                                    -> support x
            | False
                                    -> Set.empty

        let rec cases supp =
            seq {
                match supp with
                | []
                        -> yield Map.empty
                | v :: rest
                        -> yield! rest |> cases |> Seq.map (Map.add v false)
                           yield! rest |> cases |> Seq.map (Map.add v true)
            }

        let truthTable x =
            x |> support |> Set.toList |> cases
                |> Seq.map (fun env -> env, eval env x)

        let satisfiable x = 
            x |> truthTable |> Seq.exists (fun (env, res) -> res)

        let tautology x = 
            x |> truthTable |> Seq.forall (fun (env, res) -> res)

        let tautologyWithCounterExample x = 
            x |> truthTable |> Seq.tryFind (fun (env, res) -> not res)
                |> Option.map fst

        let printCounterExample = function
            | None
                -> printfn "tautology verified OK"
            | Some env
                -> printfn "tautology failed on %A" (Seq.toList env)

        let stringOfBit b = if b then "T" else "F"

        let stringOfEnv env =
            Map.fold 
                (fun acc k v -> sprintf "%s=%s" k (stringOfBit v) + acc) 
                ""
                env

        let stringOfLine (env, res) =
            sprintf "%20s %s" (stringOfEnv env) (stringOfBit res)

        let stringOfTruthTable tt =
            "\n" + (tt |> Seq.toList |> List.map stringOfLine |> String.concat "\n")

        let run() =
            truthTable (var "x")
            truthTable (var "x" &&& var "y")

    // THIS IS BUGGY
    module from_circuits_to_propositional_logic =
        type Var = string

        type Prop =
            | And of Prop * Prop
            | Var of Var
            | Not of Prop
            | Exists of Var * Prop
            | False

        let True = Not False
        let Or(p, q) = Not(And(Not(p), Not(q)))            
        let Iff(p, q)= Or(And(p,q), And(Not(p), Not(q)))
        let Implies(p, q) = Or(Not(p), q)            
        let Forall(v, p) = Not(Exists(v, Not(p)))            

        let (&&&) p q = And(p, q)
        let (|||) p q = Or(p, q)
        let (~~~) p = Not p
        let (<=>) p q = Iff(p, q)
        let (===) p q = (p <=> q)
        let (==>) p q = Implies (p, q)
        let (^^^) p q = Not(p <=> q)

        let var (nm: Var) = Var nm

        let fresh =
            let count = ref 0
            fun nm -> count.Value <- count.Value + 1; (sprintf "_%s%d" nm count.Value : Var)

        let sumBit x y = (x ^^^ y)
        let carryBit x y = (x &&& y)
        let halfAdder x y sum carry =
            (sum === sumBit x y) &&&
            (carry === carryBit x y)

        let fullAdder x y z sum carry =
            let xy = (sum === sumBit x y)
            (sum === sumBit xy z) &&&
            (carry === (carryBit x y ||| carryBit xy z))

        let twoBitAdder (x1,x2) (y1,y2) (sum1,sum2) carryInner carry =
            halfAdder x1 y1 sum1 carryInner &&&
            fullAdder x2 y2 carryInner sum2 carry

        type bit = Prop
        type bitvec = bit []

        let Lo: bit = False
        let Hi: bit = True
        let vec n nm :bitvec = Array.init n (fun i -> var (sprintf "%s%d" nm i))
        let bitEq (b1: bit) (b2: bit) = (b1 <=> b2)
        let AndL l = Seq.reduce (fun x y -> And(x,y)) l
        let vecEq (v1: bitvec) (v2: bitvec) = AndL (Array.map2 bitEq v1 v2)

        let fourBitAdder (x: bitvec) (y: bitvec) (sum: bitvec) (carry: bitvec) =
            halfAdder x.[0] y.[0]              sum.[0] carry.[0] &&&
            fullAdder x.[1] y.[1] carry.[0] sum.[1] carry.[1] &&&
            fullAdder x.[2] y.[2] carry.[1] sum.[2] carry.[2] &&&
            fullAdder x.[3] y.[3] carry.[2] sum.[3] carry.[3]

        let Blocks l = AndL l

        let nBitCarryRippleAdder (n: int) (x: bitvec) (y: bitvec) (sum: bitvec) (carry: bitvec) =
            Blocks [ for i in 0 .. n-1 ->
                            if i = 0
                            then halfAdder x.[i] y.[i] sum.[i] carry[i]
                            else fullAdder x.[i] y.[i] carry[i-1] sum.[i] carry[i] ]

        let rippleAdder (n: int) (x: bitvec) (y: bitvec) (sum: bitvec) (carry: bitvec) =
            Blocks [ for i in 0 .. n-1 ->
                            fullAdder x.[i] y.[i] carry[i-1] sum.[i] carry[i+1] ]

        let twoBitAdderWithHiding (x1,x2) (y1,y2) (sum1,sum2) carry =
            let carryInnerVar = fresh "carry"
            let carryInner = var(carryInnerVar)
            Exists(carryInnerVar, halfAdder x1 y1 sum1 carryInner &&&
                                    fullAdder x2 y2 carryInner sum2 carry)

        let run() =
            halfAdder (var"x") (var"y") (var"sum") (var"carry") |> printfn "%A"
            twoBitAdderWithHiding ((var"x1"), (var"x2")) ((var"y1"), (var"y2")) ((var"sum1"), (var"sum2")) (var"carry")  |> printfn "%A"
            ()

    module checking_simple_properties_of_circuits =
        type Var = string

        type Prop =
            | And of Prop * Prop
            | Var of Var
            | Not of Prop
            | Exists of Var * Prop
            | False

        let True = Not False
        let Or(p, q) = Not(And(Not(p), Not(q)))
        let Iff(p, q) = Or(And(p,q), And(Not(p), Not(q)))
        let Implies(p, q) = Or(Not(p), q)
        let Forall(v, p) = Not(Exists(v, Not(p)))

        let (&&&) p q = And(p, q)
        let (|||) p q = Or(p, q)
        let (~~~) p = Not p
        let (<=>) p q = Iff(p, q)
        let (===) p q = (p <=> q)
        let (==>) p q = Implies (p, q)
        let (^^^) p q = Not(p <=> q)

        let var (nm: Var) = Var nm

        let fresh =
            let count = ref 0
            fun nm -> count.Value <- count.Value + 1; (sprintf "_%s%d" nm count.Value : Var)

        let rec eval (env: Map<Var,bool>) inp =
            match inp with
            | Exists(v, p)
                                    -> eval (env.Add(v, false)) p || eval (env.Add(v, true)) p
            | And(p1, p2)
                                    -> eval env p1 && eval env p2
            | Var v
                                    -> if env.ContainsKey(v) then env.[v]
                                       else failwithf "env did not contain a value for %A" v
            | Not p
                                    -> not (eval env p)
            | False
                                    -> false

        let rec support f =
            match f with
            | And(x, y)      -> Set.union (support x) (support y)
            | Exists(v, p)   -> (support p).Remove(v)
            | Var p              -> Set.singleton p
            | Not x              -> support x
            | False              -> Set.empty

        let sumBit x y = (x ^^^ y)
        let carryBit x y = (x &&& y)
        let halfAdder x y sum carry =
            (sum === sumBit x y) &&&
            (carry === carryBit x y)

        let fullAdder x y z sum carry =
            let xy = (sum === sumBit x y)
            (sum === sumBit xy z) &&&
            (carry === (carryBit x y ||| carryBit xy z))

        let twoBitAdder (x1,x2) (y1,y2) (sum1,sum2) carryInner carry =
            halfAdder x1 y1 sum1 carryInner &&&
            fullAdder x2 y2 carryInner sum2 carry

        type bit = Prop
        type bitvec = bit []

        let Lo: bit = False
        let Hi: bit = True
        let vec n nm :bitvec = Array.init n (fun i -> var (sprintf "%s%d" nm i))
        let bitEq (b1: bit) (b2: bit) = (b1 <=> b2)
        let AndL l = Seq.reduce (fun x y -> And(x,y)) l
        let vecEq (v1: bitvec) (v2: bitvec) = AndL (Array.map2 bitEq v1 v2)

        let fourBitAdder (x: bitvec) (y: bitvec) (sum: bitvec) (carry: bitvec) =
            halfAdder x.[0] y.[0]              sum.[0] carry.[0] &&&
            fullAdder x.[1] y.[1] carry.[0] sum.[1] carry.[1] &&&
            fullAdder x.[2] y.[2] carry.[1] sum.[2] carry.[2] &&&
            fullAdder x.[3] y.[3] carry.[2] sum.[3] carry.[3]

        let Blocks l = AndL l

        let nBitCarryRippleAdder (n: int) (x: bitvec) (y: bitvec) (sum: bitvec) (carry: bitvec) =
            Blocks [ for i in 0 .. n-1 ->
                            if i = 0
                            then halfAdder x.[i] y.[i] sum.[i] carry[i]
                            else fullAdder x.[i] y.[i] carry[i-1] sum.[i] carry[i] ]

        let rec cases supp =
            seq {
                match supp with
                | []    -> yield Map.empty
                | v :: rest
                        -> yield! rest |> cases |> Seq.map (Map.add v false)
                           yield! rest |> cases |> Seq.map (Map.add v true)
            }

        let truthTable x =
            x |> support |> Set.toList |> cases
                |> Seq.map (fun env -> env, eval env x)

        let satisfiable x = 
            x |> truthTable |> Seq.exists (fun (env, res) -> res)

        let tautology x = 
            x |> truthTable |> Seq.forall (fun (env, res) -> res)

        let run() =
            tautology (fullAdder Lo Lo Lo Lo Lo) |> printfn "%b"
            satisfiable (fullAdder Lo Lo Lo Hi Lo)  |> printfn "%b"
            tautology (halfAdder (var "x") (var "x") Lo (var "x"))  |> printfn "%b"
            tautology (nBitCarryRippleAdder 2 (vec 2 "x") (vec 2 "y") (vec 2 "sum") (vec 3 "carry")
                            === nBitCarryRippleAdder 2 (vec 2 "x") (vec 2 "y") (vec 2 "sum") (vec 3 "carry")) |> printfn "%b"

    /// CONTINUE FROM CHAPTER 12: REPRESENTING PROPOSITIONAL FORMULAR EFFEICIENTLY USING BDD
    /// EXPERT F# 3.0 :PAGE 303 - Representing Propositional Formulae Efficiently Using BDD's
    /// EXPERT F# 4.0 :PAGE 326 - Representing Propositional Formulae Efficiently Using BDD's    
    module representing_propositional_formulae_effectively_using_bdds =        
        open System.Collections.Generic

        let memoize f = 
            let tab = new Dictionary<_,_>()
            fun x ->
                if tab.ContainsKey(x) then tab.[x]
                else let res = f x in tab.[x] <- res; res

        type Var = string

        type BddIndex = int
        type Bdd = Bdd of BddIndex
        type BddNode = Node of Var * BddIndex * BddIndex

        type Prop =
            | And of Prop * Prop
            | Var of Var
            | Not of Prop
            | Exists of Var * Prop
            | False
        
        let True = Not False
        let Or(p, q) = Not(And(Not(p), Not(q)))
        let Iff(p, q) = Or(And(p,q), And(Not(p), Not(q)))
        let Implies(p, q) = Or(Not(p), q)
        let Forall(v, p) = Not(Exists(v, Not(p)))

        let (&&&) p q = And(p, q)
        let (|||) p q = Or(p, q)
        let (~~~) p = Not p
        let (<=>) p q = Iff(p, q)
        let (===) p q = (p <=> q)
        let (==>) p q = Implies (p, q)
        let (^^^) p q = Not(p <=> q)

        let var (nm: Var) = Var nm

        let fresh =
            let count = ref 0
            fun nm -> count.Value <- count.Value + 1; (sprintf "_%s%d" nm count.Value : Var)

        type BddBuilder (order: Var -> Var -> int) =

            // The core data structures that preserve uniqueness
            let nodeToIndex = new Dictionary<BddNode, BddIndex>()
            let indexToNode = new Dictionary<BddIndex, BddNode>()

            // Keep track of the next index
            let mutable nextIdx = 2
            let trueIdx = 1
            let falseIdx = -1
            let trueNode = Node("", trueIdx, trueIdx)
            let falseNode = Node("", falseIdx, falseIdx)

            // Map indexes to nodes. Negative indexes go to their negation.
            // The special indexes -1 and 1 go to special true/false nodes.
            let idxToNode(idx) =
                if idx = trueIdx then trueNode
                elif idx = falseIdx then falseNode
                elif idx > 0 then indexToNode.[idx]
                else
                    let (Node(v, l, r)) = indexToNode.[-idx]
                    Node(v, -l, -r)

            // Map nodes to indexes. Add an entry to the table if needed.
            let nodeToUniqueIdx(node) =
                if nodeToIndex.ContainsKey(node) then 
                    nodeToIndex.[node]
                else
                    let idx = nextIdx
                    nodeToIndex.[node] <- idx
                    indexToNode.[idx] <- node
                    nextIdx <- nextIdx + 1
                    idx

            // Get the canonocal index for a node. Preserve the invariant that the
            // left-hand node of a conditional is always a positive node
            let mkNode(v: Var, l: BddIndex, r: BddIndex) =
                if l = r then l
                elif l > 0 then nodeToUniqueIdx(Node(v, l, r))
                else -nodeToUniqueIdx(Node(v, -l, -r))

            // Construct the BDD for a conjunction "m1 AND m2"
            let rec mkAnd(m1, m2) =
                if m1 = falseIdx || m2 = falseIdx then falseIdx
                elif m1 = trueIdx then m2
                elif m2 = trueIdx then m1
                else
                    let (Node(x, l1, r1)) = idxToNode(m1)
                    let (Node(y, l2, r2)) = idxToNode(m2)
                    let v, (la, lb), (ra, rb) =
                        match order x y with
                        | c when c = 0 -> x, (l1, l2), (r1, r2)
                        | c when c < 0 -> x, (l1, m2), (r1, m2)
                        | c -> y, (m1, l2), (m1, r2)
                    mkNode(v, mkAnd(la, lb), mkAnd(ra, rb))

            // Memoize this function
            let mkAnd = memoize mkAnd

            // Publish the construction functions that make BDD's from existing BDD's
            member g.False = Bdd falseIdx
            member g.And (Bdd m1, Bdd m2) = Bdd (mkAnd(m1,m2))
            member g.Not (Bdd m) = Bdd (-m)
            member g.Var (nm) = Bdd (mkNode(nm, trueIdx, falseIdx))
            member g.NodeCount = nextIdx

            member g.ToString(Bdd idx) =
                let rec fmt dep idx =
                    if dep > 3 then "..." else
                        let (Node(p, l, r)) = idxToNode(idx)
                        if p = "" then if l = trueIdx then "T" else "F"
                        else sprintf "(%s => %s | %s)" p (fmt (dep+1) 1) (fmt (dep+1) r)
                fmt 1 idx

            member g.Build(f) =
                match f with
                | And(x,y) -> g.And(g.Build x , g.Build y)
                | Var(p) -> g.Var(p)
                | Not(x) -> g.Not(g.Build x)
                | False -> g.False
                | Exists(v,p) -> failwith "Exists node"

            member g.Equiv(p1, p2) = g.Build(p1) = g.Build(p2)

        let run() =
            let bddBuilder = BddBuilder(compare)
            bddBuilder.Build(var "x") |> ignore

            // ONLY USE ON THE REPL
            // fsi.AddPrinter(fun bdd -> bddBuilder.ToString(bdd));;

            bddBuilder.Build(var "x" &&& var "x") |> ignore
            bddBuilder.Build(var "x") = bddBuilder.Build(var "x" &&& var "x") |> ignore
            (var "x") = (var "x" &&& var "x")  |> ignore
            bddBuilder.Build(var "x" &&& var "y") |> ignore
            bddBuilder.Equiv(var "x", var "x" &&& var "x")


    module execute_modules =
        let run() =
            representing_propositional_logic.run() |> printfn "%A"
            evaluating_propositional_logic_naively.run() |> printfn "%A"
            from_circuits_to_propositional_logic.run() |> printfn "%A"
            checking_simple_properties_of_circuits.run() |> printfn "%A"
            representing_propositional_formulae_effectively_using_bdds.run() |> printfn "%A"
