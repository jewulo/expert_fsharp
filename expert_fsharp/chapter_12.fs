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
        /// DECIDED NOT TO DO THIS.
        /// THE EFFORT TO GAIN RATIO IS VERY SMALL.
        let run() = ()

    /// CONTINUE FROM CHAPTER 12: REPRESENTING PROPOSITIONAL FORMULAR EFFEICIENTLY USING BDD
    /// EXPERT F# 3.0 :PAGE 303 - Representing Propositional Formulae Efficiently Using BDD's
    /// EXPERT F# 4.0 :PAGE 326 - Representing Propositional Formulae Efficiently Using BDD's
    module representing_propositional_formulae_effectively_using_bdds =
        let run() = ()

    module execute_modules =
        let run() =
            representing_propositional_logic.run() |> printfn "%A"
            evaluating_propositional_logic_naively.run() |> printfn "%A"
            from_circuits_to_propositional_logic.run() |> printfn "%A"

