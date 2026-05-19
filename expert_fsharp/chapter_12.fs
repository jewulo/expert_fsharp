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

    /// CONTINUE FROM CHAPTER 12 SYMBOLIC PROGRAMMING WITH STRUCTURED DATA
    /// EXPERT F# 3.0 :PAGE 300 - From Circuits to Propostional Logic 
    /// EXPERT F# 4.0 :PAGE 322 - From Circuits to Propostional Logic 
    module execute_modules =
        let run() =
            representing_propositional_logic.run() |> printfn "%A"
            evaluating_propositional_logic_naively.run() |> printfn "%A"

