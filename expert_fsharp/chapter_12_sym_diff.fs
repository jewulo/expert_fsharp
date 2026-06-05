module chapter_12_sym_diff

    open System

    module modelling_simple_algebraic_expressions =
        type Expr =
            | Var
            | Num of int
            | Sum of Expr * Expr
            | Prod of Expr * Expr

        let rec derive expr =
            match expr with
            | Var                       -> Num 1
            | Num _                     -> Num 0
            | Sum (e1,e2)     -> Sum (derive e1, derive e2)
            | Prod (e1,e2)    -> Sum (Prod (e1, derive e2), Prod (e2, (derive e1)))

        // define precedence context for bracketing displayed expressions
        let precSum = 10
        let precProd = 20

        let rec stringOfExpr prec expr =
            match expr with
            | Var       -> "x"
            | Num i -> i.ToString()
            | Sum (e1, e2) ->                        
                let sum = stringOfExpr precSum e1 + "+" + stringOfExpr precSum e2
                if prec > precSum then
                    "(" + sum + ")"
                else
                    sum
            | Prod (e1, e2) ->
                stringOfExpr precProd e1 + "*"  + stringOfExpr precProd e2

        // only usabled on F# REPL
        //fsi.AddPrinter (fun expr -> stringOfExpr 0 expr)

        let run () =
            let e1 = Sum (Num 1, Prod (Num 2, Var))
            e1 |> printfn "%A"
            derive e1 |> printfn "%A"            
            stringOfExpr 0 e1 |> printfn "%A"
            stringOfExpr 0 (derive e1)  |> printfn "%A"

            let e2 = Prod (Var, Prod (Var, Num 2))
            e2 |> printfn "%A"
            derive e2 |> printfn "%A"            
            stringOfExpr 0 e2 |> printfn "%A"
            stringOfExpr 0 (derive e2)  |> printfn "%A"

    module implementing_local_simplifications =
        type Expr =
            | Var
            | Num of int
            | Sum of Expr * Expr
            | Prod of Expr * Expr

        let rec derive expr =
            match expr with
            | Var                       -> Num 1
            | Num _                     -> Num 0
            | Sum (e1,e2)     -> Sum (derive e1, derive e2)
            | Prod (e1,e2)    -> Sum (Prod (e1, derive e2), Prod (e2, (derive e1)))

        // define precedence context for bracketing displayed expressions
        let precSum = 10
        let precProd = 20

        let rec stringOfExpr prec expr =
            match expr with
            | Var       -> "x"
            | Num i -> i.ToString()
            | Sum (e1, e2) ->                        
                let sum = stringOfExpr precSum e1 + "+" + stringOfExpr precSum e2
                if prec > precSum then
                    "(" + sum + ")"
                else
                    sum
            | Prod (e1, e2) ->
                stringOfExpr precProd e1 + "*"  + stringOfExpr precProd e2

        // only usabled on F# REPL
        //fsi.AddPrinter (fun expr -> stringOfExpr 0 expr)


        let simpSum = function
            | Num n, Num m -> Num (n+m)          // constants
            | Num 0, e | e, Num 0 -> e          // 0+e = e+0 = e
            | e1, e2 -> Sum (e1, e2)

        let simpProd = function
            | Num n, Num m -> Num (n*m)          // constants
            | Num 0, e | e, Num 0 -> Num 0      // 0*e = 0
            | Num 1, e | e, Num 1 -> e          // 1*e = e*1 = e
            | e1, e2 -> Prod (e1, e2)

        let rec simpDerive = function
            | Var               -> Num 1
            | Num _             -> Num 0
            | Sum (e1, e2)  -> simpSum (simpDerive e1, simpDerive e2)
            | Prod (e1, e2) -> simpSum (simpProd (e1, simpDerive e2),
                                                    simpProd (e2, simpDerive e1))

        let run () =
            let e1 = Sum (Num 1, Prod (Num 2, Var))
            e1 |> printfn "%A"
            simpDerive e1 |> printfn "%A"            
            stringOfExpr 0 e1 |> printfn "%A"
            stringOfExpr 0 (simpDerive e1)  |> printfn "%A"

            let e2 = Prod (Var, Prod (Var, Num 2))
            e2 |> printfn "%A"
            simpDerive e2 |> printfn "%A"            
            stringOfExpr 0 e2 |> printfn "%A"
            stringOfExpr 0 (simpDerive e2)  |> printfn "%A"

    module execute_modules =
        let run () =
            modelling_simple_algebraic_expressions.run()
            implementing_local_simplifications.run()


