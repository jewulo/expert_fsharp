module chapter_12_sym_diff

    open System

    /// CONTINUE FROM CHAPTER 12: SYMBOLIC DIFFERENTIATION AND EXPRESSION RENDERING
    /// EXPERT F# 3.0 :PAGE 309 - Symbolic Differentiaion and Expression Rendering 
    /// EXPERT F# 4.0 :PAGE 332 - Expression Simplification and Differentiation

    module modelling_simple_algebraic_expressions =
        type Expr =
            | Var
            | Num of int
            | Sum of Expr * Expr
            | Prod of Expr * Expr

        let run () = ()

    let run () = ()



