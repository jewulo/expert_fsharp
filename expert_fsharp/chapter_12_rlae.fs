namespace chapter_12_rlae

    // PAGE 312

    /// CONTINUE FROM CHAPTER 12: SYMBOLIC DIFFERENTIATION AND EXPRESSION RENDERING
    /// EXPERT F# 3.0 :PAGE 312 - A Richer Language of Algebraic Expressions
    /// EXPERT F# 4.0 :PAGE 334 - A Richer Language of Algebraic Expressions

    module a_richer_language_of_algebraic_expressions =
        
        type Expr =
            | Num  of decimal
            | Var  of string
            | Neg  of Expr
            | Add  of Expr list
            | Sub  of Expr * Expr list
            | Prod of Expr * Expr
            | Frac of Expr * Expr
            | Pow  of Expr * decimal
            | Sin  of Expr
            | Cos  of Expr
            | Exp  of Expr

            static member StartNeeded e1 e2 =
                match e1, e2 with
                | Num _, Neg _ | _, Num _ -> true
                | _ -> false

            member self.IsNumber =
                match self with
                | Num _ -> true | _ -> false

            member self.NumOf =
                match self with
                | Num num -> num | _ -> failwith "NumOf: Not a Num"

            member self.IsNegative =
                match self with
                | Num num | Prod (Num num, _) -> num < 0M
                | Neg e -> true | _ -> false

            member self.Negate =
                match self with
                | Num num -> Num (-num)
                | Neg e -> e
                | exp -> Neg exp

            

        let run () = ()

    module execute_modules =
        let run () = ()
