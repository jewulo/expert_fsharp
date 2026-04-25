module chapter_9
    
    module using_range_expressions =

        let run () =
            let s1 = seq {0 .. 2}
            s1 |> printfn "%A"

            let s2 = seq {-100.0 .. 100.0}
            s2 |> printfn "%A"

            // sequences are a lazy data structure
            let s3 = seq {1I .. 1000000000000I}
            s3 |> printfn "%A"

            // skipped sequence
            let s4 = seq {1 .. 2 .. 5}
            s4 |> printfn "%A"
            let s5 = seq {1 .. -2 .. -5}
            s5 |> printfn "%A"

            // if a skipped sequence overshoots its upper-bound, the skipped value is not included
            let s6= seq {0 .. 2 .. 5}
            s6 |> printfn "%A"

            // NOTE THAT (..) and (.. ..) ARE OPERATORS IN F#
            // THEY CAN BE OVERLOADED

    module iterating_a_sequence =

        let run () =
            let range = seq {0 .. 2 .. 6}
            for i in range do printfn "i = %d" i

    module transforming_sequences_with_aggregate_operators =

        let run () =
            let range = seq {0 .. 10}
            range |> printfn "%A"
            range |> Seq.map (fun i -> (i, i*i)) |> printfn "%A"
            
    module using_lazy_sequences_from_external_sources =
        open System.IO

        let rec allFiles dir =
            Seq.append
                (dir |> Directory.GetFiles)
                (dir |> Directory.GetDirectories |> Seq.map allFiles |> Seq.concat)

        let run () =
            // allFiles @"c:\WINDOWS\system32" |> printfn "%A"
            allFiles "." |> printfn "%A"

    module using_sequence_expressions =

        let run () =
            let squares = seq { for i in 0 .. 10 -> (i, i * i) }
            squares |> printfn "%A"

            let triples = seq { for (i, iSquared) in squares -> (i, iSquared, i * iSquared) }
            triples |> printfn "%A"

    module enriching_sequence_expressions_with_additional_logic =
        
        open System.IO

        let checkerboardCoordinates n =
            seq {
                for row in 1 .. n do
                    for col in 1 .. n do
                        let sum = row + col
                        if sum % 2 = 0 then
                            yield (row,col)
                }
        
        let fileInfo dir = 
            seq {
                for file in Directory.GetFiles dir do
                    let creationTime = File.GetCreationTime file
                    let lastAccessTime = File.GetLastAccessTime file
                    yield (file, creationTime, lastAccessTime)
                }

        let rec allFiles dir =
            seq {
                for file in Directory.GetFiles dir do
                    yield file
                for subdir in Directory.GetDirectories dir do
                    yield! allFiles subdir
                }

        let run () =
            checkerboardCoordinates 3 |> printfn "%A"
            fileInfo @"c:\Program Files" |> printfn "%A"
            allFiles @"c:\Program Files" |> printfn "%A"

    module generating_lists_and_arrays_using_sequence_expressions =

        let run () =
            let l1 = [1 .. 4]
            l1 |> printfn "%A"

            let l2 = [for i in 0 .. 3 -> (i, i * i)]
            l2 |> printfn "%A"

            let a1 = [|for i in 0 .. 3 -> (i, i * i)|]
            a1 |> printfn "%A"

            printfn ""

    module more_on_working_with_sequences =

        /// A table of people in our startup
        let people =
            [("Amber", 27, "Design")
             ("Wendy", 35, "Events")
             ("Antonio", 40, "Sales")
             ("Petra", 31, "Design")
             ("Carlos", 34, "Marketing")]

        /// Extract information from the table of people
        let namesOfPeopleStartingWithA =
            people
                |> Seq.map (fun (name, age, dept) -> name)
                |> Seq.filter (fun name -> name.StartsWith "A")
                |> Seq.toList

        /// Extract the names of designers from, the table of people
        let namesOfDesigners =
            people
                |> Seq.filter (fun (_, _, dept) -> dept = "Design")
                |> Seq.map (fun (name, _, _) -> name)
                |> Seq.toList

        let run () =
            "Full List of Staff:" |> printfn "%s"
            people |> printfn "%A"

            printfn ""

            "List of Staff with Names Starting with 'A':" |> printfn "%s"
            namesOfPeopleStartingWithA |> printfn "%A"

            printfn ""

            "List of Staff in Design Department:" |> printfn "%s"
            namesOfDesigners |> printfn "%A"

    module using_other_sequence_operators_truncate_and_sort =

        // A random number generator
        let rand = System.Random()

        /// An infinite sequence of numbers
        let randomNumbers = seq { while true do yield rand.Next(100000) }

        /// The first 10 random numbers, sorted
        let firstTenRandomNumbers =
            randomNumbers
                |> Seq.truncate 10
                |> Seq.sort     // sort ascending
                |> Seq.toList

        /// The first 3000 even random numbers and sort them
        let firstThreeThousandEvenNumbersWithSquares =
            randomNumbers
                |> Seq.filter (fun i -> i % 2 = 0)     // "where"
                |> Seq.truncate 3000
                |> Seq.sort                                         // sort ascending
                |> Seq.map (fun i -> i, i*i)            // "select"
                |> Seq.toList

        /// The first 10 random numbers, sorted by last digit
        let firstTenRandomNumbersSortedbyLastDigit =
            randomNumbers
                |> Seq.truncate 10
                |> Seq.sortBy (fun x -> x % 10)     // sort ascending
                |> Seq.toList

        let run () =
            printfn "[Random Numbers]"
            rand |> printfn "%A"

            printfn "[First 10 Random Numbers]"
            firstTenRandomNumbers |> printfn "%A"

            printfn "[First 3000 Even Random Numbers Sorted]"
            firstThreeThousandEvenNumbersWithSquares |> printfn "%A"

            printfn "[First 3000 Even Random Numbers Sorted By Last Digit]"
            firstTenRandomNumbersSortedbyLastDigit |> printfn "%A"

    module selecting_multiple_elements_from_sequences =

        // A random number generator
        let rand = System.Random()

        let triangleNumbers = 
            [1 .. 10]
                |> Seq.collect (fun i -> [1 .. i])
                |> Seq.toList

        let gameBoard =
            [ for i in 0 .. 7 do
                for j in 0 .. 7 do
                    yield (i, j, rand.Next(10)) ]

        let evenPositions = 
            gameBoard
                |> Seq.choose (fun (i,j,v) -> if v % 2 = 0 then Some (i, j) else None)
                |> Seq.toList

        let run () =
            printfn "[Triangle Numbers]"
            triangleNumbers |> printfn "%A"

            printfn "[Game Board]"
            gameBoard |> printfn "%A"

            printfn "[Even Numbers]"
            evenPositions |> printfn "%A"


    module finding_elements_and_indexes_in_sequences =

        // A random number generator
        let rand = System.Random()

        let gameBoard =
            [ for i in 0 .. 7 do
                for j in 0 .. 7 do
                    yield (i, j, rand.Next(10)) ]

        let firstElementScoringZero =
            gameBoard |> Seq.tryFind (fun (i, j, v) -> v = 0)

        let firstPositionScoringZero =
            gameBoard |> Seq.tryPick (fun (i, j, v) -> if v = 0 then Some (i, j) else None)

        let run () =
            firstElementScoringZero |> printfn "%A"
            firstPositionScoringZero |> printfn "%A"

    module grouping_and_indexeing_sequences =

        // A random number generator
        let rand = System.Random()

        let gameBoard =
            [ for i in 0 .. 7 do
                for j in 0 .. 7 do
                    yield (i, j, rand.Next(10)) ]

        let positionGroupedByGameValue =
            gameBoard
                |> Seq.groupBy (fun (i, j, v) -> v)
                |> Seq.sortBy (fun (k, v) -> k)
                |> Seq.toList

        let positionsIndexedByGameValue =
            gameBoard
                |> Seq.groupBy (fun (i, j, v) -> v)
                |> Seq.sortBy (fun (k, v ) -> k)
                |> Seq.map (fun (k, v) -> (k, Seq.toList v))

        let worstPositions = positionGroupedByGameValue.[0]
        let bestPositions  = positionGroupedByGameValue.[9]

        let run () =
            worstPositions |> printfn "%A"
            bestPositions |> printfn "%A"

    module folding_sequences =

        let run () =
           let a1 = List.fold (fun acc x -> acc + x) 0 [4; 5; 6]
           a1 |> printfn "%d"

           let a2 = Seq.fold (fun acc x -> acc + x) 0.0 [4.0; 5.0; 6.0]
           a2 |> printfn "%f"

           let a3 = List.foldBack (fun acc x -> min x acc) [4; 5; 6; 3; 5] System.Int32.MaxValue
           a3 |> printfn "%d"

           // The following are quivalent to the above
           let a4 = List.fold (+) 0 [4; 5; 6]
           a4 |> printfn "%d"

           let a5 = Seq.fold (+) 0.0 [4.0; 5.0; 6.0]
           a5 |> printfn "%f"

           let a6 = List.foldBack min [4; 5; 6; 3; 5] System.Int32.MaxValue
           a6 |> printfn "%d"

           // you can compose the functions passed too List.foldBack
           let a7 = List.foldBack (fst >> min) [(3, "three"); (1, "one"); (5, "two"); (5, "five"); (4, "four")] System.Int32.MaxValue
           a7 |> printfn "%d"

           let a8 = List.foldBack (fst >> min) [(3, "three"); (5, "five")] System.Int32.MaxValue
           a8 |> printfn "%d"

    module cleaning_up_in_sequence_expressions =

        open System.IO
        
        let firstTwoLines file =
            seq {
                use s = File.OpenText(file)
                yield s.ReadLine()
                yield s.ReadLine()
            }

        let run () =
            File.WriteAllLines("test1.txt", [|"Ex kommt ein Schiff"; "A ship is coming"|])
            let twolines () = firstTwoLines "test1.txt"
            twolines() |> Seq.iter (printfn "line = '%s'")

    module expressing_some_operations_using_sequence_expressions =

        let rand = System.Random()

        let gameBoard =
            [ for i in 0 .. 7 do
                for j in 0 .. 7 do
                    yield (i, j, rand.Next(10)) ]

        let triangleNumbers =
            [ for i in 1 .. 10 do
                for j in 1 .. i do
                    yield (i, j) ]

        let evenPositions =
            [ for (i, j, v) in gameBoard do
                if v % 2 = 0 then
                    yield (i, j) ]

        let run () =
            gameBoard |> printfn "%A"
            triangleNumbers |> printfn "%A"
            evenPositions |> printfn "%A"

    module structure_beyond_sequences_working_with_trees =

        open System.Xml
        open System.Drawing
        // open System.Globalization   // for FSI printing. DOES NOT WORK

        let inp = """<?xml version="1.0" encoding="utf-8" ?> 
                             <Scene>
                                <Composite>
                                    <Circle radius='2' x='1' y='0'/>
                                    <Composite>
                                        <Circle radius='2' x='4' y='0'/>
                                        <Square side='2' left='-3' y='0'/>
                                    </Composite>
                                    <Ellipse top='2' left='-2' width='3' height='4'/>
                                </Composite>
                             </Scene>"""

        type Scene =
            | Ellipse of RectangleF
            | Rect of RectangleF
            | Composite of Scene list

            /// A derived constructor
            static member Circle(center : PointF, radius) =
                Ellipse(RectangleF(center.X - radius, center.Y - radius,
                                        radius * 2.0f, radius * 2.0f))

            /// A derived constructor
            static member Square(left, top, side) =
                Rect(RectangleF(left, top, side, side))
        
        /// Extract a number from an XML attribute collection
        let extractFloat32 attrName (attribs : XmlAttributeCollection) =
            float32(attribs.GetNamedItem(attrName).Value)

        /// Extract a Point from an XML attribute collection
        let extractPointF (attribs : XmlAttributeCollection) =
            PointF(extractFloat32 "x" attribs, extractFloat32 "y" attribs)

        /// Extract a Rectangle from an XML attribute collection
        let extractRectangleF (attribs : XmlAttributeCollection) =
            RectangleF(extractFloat32 "left" attribs, extractFloat32 "top" attribs,
                    extractFloat32 "width" attribs, extractFloat32 "height" attribs)

        /// Extract a Scene from an XML node
        let rec extractScene(node : XmlNode) =
            let attribs = node.Attributes
            let childNodes = node.ChildNodes
            match node.Name with
            | "Circle" ->
                Scene.Circle(extractPointF(attribs),extractFloat32 "top" attribs)
            | "Ellipse" ->
                Scene.Ellipse(extractRectangleF(attribs))
            | "Rectangle" ->
                Scene.Rect(extractRectangleF(attribs))
            | "Square" ->
                Scene.Square(extractFloat32 "left" attribs, extractFloat32 "top" attribs, extractFloat32 "side" attribs)
            | "Composite" ->
                Scene.Composite [for child in childNodes -> extractScene(child)]
            | _ -> failwithf "unable to convert XML '%s'" node.OuterXml

        /// Extract a list Scene from an XML node
        let extractScenes (doc : XmlDocument) =
            [for node in doc.ChildNodes do
                if node.Name = "Scene" then
                    yield (Composite
                                [for child in node.ChildNodes -> extractScene(child)])]

        /// Recursively Flatten an XML scene
        let rec flatten scene =
            seq {
                match scene with
                | Composite scenes -> for x in scenes do yield! flatten x
                | Ellipse _ | Rect _ -> yield scene
            }

        /// Accumulatively Flatten an XML scene
        /// traverses the tree eagerly accumulating in a parameter.
        let rec flattenAux scene acc =
            match scene with
            | Composite scenes -> List.foldBack flattenAux scenes acc
            | Ellipse _
            | Rect _ -> scene :: acc

        /// Invoke the accumulative recursion
        let flatten2 scene = flattenAux scene [] |> Seq.ofList

        /// Use eager traversal using a local mutable variable
        let flatten3 scene =
            let acc = new ResizeArray<_>()
            let rec flattenAux s =
                match s with
                | Composite(scenes) -> scenes |> List.iter flattenAux
                | Ellipse _ | Rect _ -> acc.Add s
            flattenAux scene
            Seq.readonly acc

        // IT CRASHES AND I DO NOT KNOW WHY
        let run () =
            let doc = new XmlDocument()
            doc.LoadXml(inp)
            let scenes = extractScenes doc

            scenes |> List.map flatten |> printfn "%A"
            scenes |> List.map flatten2 |> printfn "%A"
            scenes |> List.map flatten3 |> printfn "%A"
    
    // COULD NOT SOLVE THE BUG IN MODULE
    // structure_beyond_sequences_working_with_trees
    // SO NO POINT IN TRYING THIS YET
    module transforming_abstract_syntax_representations =
        let run () = ()

    // COULD NOT SOLVE THE BUG IN MODULE
    // structure_beyond_sequences_working_with_trees
    // SO NO POINT IN TRYING THIS YET
    module using_on_demand_computation_with_abstract_syntax_trees =
        let run () = ()

    // COULD NOT SOLVE THE BUG IN MODULE
    // structure_beyond_sequences_working_with_trees
    // SO NO POINT IN TRYING THIS YET
    module caching_properties_in_abstract_syntax_trees =
        let run () = ()

    module memoizing_construction_of_syntax_tree_nodes =

        /// Non-Cached version of expression
        module non_cached_version_of_expression = 

            type Prop = 
                | And of Prop * Prop
                | Or of Prop * Prop
                | Not of Prop
                | Var of string
                | True

            let run () = ()

        module cached_version_of_expression =
        
            type Prop = 
                | Prop of int

            type PropRepr = 
                | AndRepr of Prop * Prop
                | OrRepr of Prop * Prop
                | NotRepr of Prop
                | VarRepr of string
                | TrueRepr
            
            open System.Collections.Generic

            module PropOps =
                
                let internal uniqStamp = ref 0

                type internal PropTable () =
                    let fwdTable = new Dictionary<PropRepr, Prop>(HashIdentity.Structural)
                    let bwdTable = new Dictionary<int, PropRepr>(HashIdentity.Structural)

                    member t.ToUnique repr =
                        if fwdTable.ContainsKey repr then                        
                            fwdTable.[repr]
                        else
                            // let stamp = incr uniqStamp; !uniqStamp -- // USE OF ! is DEPRECATED
                            // let stamp = incr uniqStamp; uniqStamp.Value // USE OF incr is DEPRECATED
                            let stamp = uniqStamp.Value <- uniqStamp.Value + 1; uniqStamp.Value
                            let prop = Prop stamp
                            fwdTable.Add (repr, prop)
                            bwdTable.Add (stamp, repr)
                            prop

                    member t.FromUnique (Prop stamp) =
                        bwdTable.[stamp]
                        
                let internal table = PropTable ()

                // public construction functions
                let And (p1, p2) = table.ToUnique (AndRepr (p1, p2))
                let Not p = table.ToUnique (NotRepr p)
                let Or (p1, p2) = table.ToUnique (OrRepr (p1, p2))
                let Var p = table.ToUnique (VarRepr p)
                let True = table.ToUnique TrueRepr
                let False = Not True

                // deconstruction function
                let getRepr p = table.FromUnique p                    
        
            open PropOps
            let run () = 
                let p = True
                p |> printfn "%A"

                let p1 = And (Var "x", Var "y")
                p1 |> printfn "%A"

                let p2 = getRepr p1
                p2 |> printfn "%A"

                let p3 = And (Var "x", Var "y")
                p3 |> printfn "%A"

                ()

        /// run code for module:
        /// MEMOIZING CONSTRUCTION OF SYNTAX TREE NODES
        let run () =
            non_cached_version_of_expression.run()
            cached_version_of_expression.run()

    module converting_the_same_data_to_many_views =
        
        [<Struct>]
        type Complex (r : float, i : float) =
            static member Polar (mag, phase) = Complex (mag * cos phase, mag * sin phase)
            member x.Magnitude = sqrt(r * r + i * i)
            member x.Phase = atan2 i r
            member x.RealPart = r
            member x.ImaginaryPart = i

        // Defining Active Patterns:

        // Here is an active pattern that lets you view complex numbers as rectangular coordinates
        let (|Rect|) (x : Complex) = (x.RealPart, x.ImaginaryPart)

        // Here is a active pattern that lets you view complex numbers in polar coordinates
        let (|Polar|) (x : Complex) = (x.Magnitude, x.Phase)

        // we can noe patter match on both Rect and Polar

        let addViaRect a b =
            match a, b with
            | Rect (ar, ai), Rect (br, bi) ->
                Complex (ar + br, ai + bi)

        let mulViaRect a b =
            match a, b with
            | Rect (ar, ai), Rect (br, bi) ->
                Complex (ar * br - ai * bi, ai * br + bi * ar)

        // multiplication on complex numbers is easier to express using polar coordinates
        let mulViaPolar a b =
            match a, b with
            | Polar (m, p), Polar (n, q) -> 
                Complex.Polar (m * n, p + q)

        // used on the console
        // fsi.AddPrinter (fun (c : Complex) -> sprintf "%gr + %gi" c.RealPart c.ImaginaryPart)
        
        // NOTE:
        // addViaRect and mulViaPolar can also be written using explicit patterns
        // by using pattern matching in argument positions
        let add2 (Rect (ar, ai)) (Rect (br, bi)) 
            = Complex (ar + br, ai + bi)
        let mul2 (Polar (r1, th1)) (Polar (r2, th2)) 
            = Complex (r1 * r2, th1 + th2)
        
        let run () =
            let c = Complex (3.0, 4.0)
            match c with | Rect (x, y) -> printfn "x = %g, y = %g" x y
            match c with | Polar (x, y) -> printfn "x = %g, y = %g" x y

            let c1 = addViaRect c c
            match c1 with | Rect (x, y) -> printfn "x = %g, y = %g" x y
            match c1 with | Polar (x, y) -> printfn "x = %g, y = %g" x y

            let c2 = mulViaRect c c
            match c2 with | Rect (x, y) -> printfn "x = %g, y = %g" x y
            match c2 with | Polar (x, y) -> printfn "x = %g, y = %g" x y

            let c3 = mulViaPolar c c
            match c3 with | Rect (x, y) -> printfn "x = %g, y = %g" x y
            match c3 with | Polar (x, y) -> printfn "x = %g, y = %g" x y

            let c4 = add2 c c
            match c4 with | Rect (x, y) -> printfn "x = %g, y = %g" x y
            match c4 with | Polar (x, y) -> printfn "x = %g, y = %g" x y

            let c5 = mul2 c c
            match c5 with | Rect (x, y) -> printfn "x = %g, y = %g" x y
            match c5 with | Polar (x, y) -> printfn "x = %g, y = %g" x y

    /// CONTINUE FROM CHAPTER 9
    /// PAGE 216
    /// SECTION: EQUALITY, HASHING AND COMPARISISON FOR NEW STRUCTURED DATA TYPES
    module matching_on_dotnet_object_types =
        open System

        // declare active types that can be used fro pattern matching
        // |Named|Array|Ptr|Param| is the active type declaration
        let (|Named|Array|Ptr|Param|) (typ : System.Type) =
            if typ.IsGenericType        then Named (typ.GetGenericTypeDefinition(), typ.GetGenericArguments())
            elif typ.IsGenericParameter then Param (typ.GenericParameterPosition)
            elif not typ.HasElementType then Named (typ, [||])
            elif typ.IsArray            then Array (typ.GetElementType(), typ.GetArrayRank())
            elif typ.IsByRef            then Ptr (true, typ.GetElementType())
            elif typ.IsPointer          then Ptr (false, typ.GetElementType())
            else                        failwith "MSDN says this can't happen"

        // defining the active patterns above, then allows you to
        // pattern match on Named, Array, Ptr and Param
        let rec formatType typ =
            match typ with
            | Named (con, [||]) ->
                sprintf "%s" con.Name
            | Named (con, args) ->
                sprintf "%s<%s>" con.Name (formatTypes args)
            | Array (arg, rank) ->
                sprintf "Array(%d,%s)" rank (formatType arg)
            | Ptr (true, arg) ->
                sprintf "%s&" (formatType arg)
            | Ptr (false, arg) ->
                sprintf "%s*" (formatType arg)
            | Param (pos) ->
                sprintf "!%d" pos

        and formatTypes typs =
            String.Join (",", Array.map formatType typs)

        // Collect free generic parameter positions appearing in a System.Type
        let rec freeVarsAcc typ acc =
            match typ with
            | Array (arg, rank) ->
                freeVarsAcc arg acc
            | Ptr (_, arg) ->
                freeVarsAcc arg acc
            | Param _ ->
                (typ :: acc)
            | Named (con, args) ->
                // fold over generic argument types
                Array.foldBack freeVarsAcc args acc

        let freeVars typ = freeVarsAcc typ []

        let run () = ()

    module defining_partial_and_parameterize_active_patterns =

        let (|MulThree|_|) inp = if inp % 3 = 0 then Some(inp / 3) else None
        let (|MulSeven|_|) inp = if inp % 7 = 0 then Some(inp / 7) else None

        // active paterns can take multiple arguments
        let (|MulN|_|) n inp = if inp % 7 = 0 then Some(inp / n) else None

        let isMultiple_of_3_or_7 n =
            match n with
            | MulThree n
            | MulSeven n -> true
            | _ -> false

        let run () =
            isMultiple_of_3_or_7 12 |> printfn "%b"
            isMultiple_of_3_or_7 49 |> printfn "%b"
            isMultiple_of_3_or_7 13 |> printfn "%b"

    module hiding_abstract_syntax_implementations_with_active_patterns =

        type Prop = Prop of int
        and PropRepr = 
            | AndRepr of Prop * Prop
            | OrRepr of Prop * Prop
            | NotRepr of Prop
            | VarRepr of string
            | TrueRepr
            
        open System.Collections.Generic

        module PropOps =
                
            let internal uniqStamp = ref 0

            type internal PropTable () =
                let fwdTable = new Dictionary<PropRepr, Prop>(HashIdentity.Structural)
                let bwdTable = new Dictionary<int, PropRepr>(HashIdentity.Structural)

                member t.ToUnique repr =
                    if fwdTable.ContainsKey repr then                        
                        fwdTable.[repr]
                    else
                        // let stamp = incr uniqStamp; !uniqStamp -- // USE OF ! is DEPRECATED
                        // let stamp = incr uniqStamp; uniqStamp.Value // USE OF incr is DEPRECATED
                        let stamp = uniqStamp.Value <- uniqStamp.Value + 1; uniqStamp.Value
                        let prop = Prop stamp
                        fwdTable.Add (repr, prop)
                        bwdTable.Add (stamp, repr)
                        prop

                member t.FromUnique (Prop stamp) =
                    bwdTable.[stamp]
                        
            let internal table = PropTable ()

            // public construction functions
            let And (p1, p2) = table.ToUnique (AndRepr (p1, p2))
            let Not p = table.ToUnique (NotRepr p)
            let Or (p1, p2) = table.ToUnique (OrRepr (p1, p2))
            let Var p = table.ToUnique (VarRepr p)
            let True = table.ToUnique TrueRepr
            let False = Not True

            let (|And|Or|Not|Var|True|) prop =
                match table.FromUnique prop with
                | AndRepr (x, y) -> And (x, y)
                | OrRepr (x, y) -> Or (x, y)
                | NotRepr x -> Not x
                | VarRepr x -> Var x
                | TrueRepr -> True

            // deconstruction function
            let getRepr p = table.FromUnique p                    
        
        open PropOps

        let rec showProp precedence prop =
            let parenIfPrec lim s = if precedence < lim then "(" + s + ")" else s
            match prop with
            | Or (p1, p2) ->
                parenIfPrec 4 (showProp 4 p1 + " || " + showProp 4 p2)
            | And (p1, p2) ->
                parenIfPrec 4 (showProp 3 p1 + " && " + showProp 3 p2)
            | Not p ->
                parenIfPrec 2 (" not " + showProp 1 p)
            | Var v -> v
            | True -> "T"

        let rec nnf sign prop =
            match prop with
            | And (p1, p2) ->
                if sign then And (nnf sign p1, nnf sign p2)
                else Or (nnf sign p1, nnf sign p2)
            | Or (p1, p2) ->
                if sign then Or (nnf sign p1, nnf sign p2)
                else And (nnf sign p1, nnf sign p2)
            | Not p ->
                nnf (not sign) p
            | Var _ | True ->
                if sign then prop else Not prop

        let NNF prop = nnf true prop

        let run () = 
            let t1 = Not(And(Not(Var("x")),Not(Var("y"))))

            showProp 5 |> printfn "%A"
            showProp 5 |> printfn "%O"

            let t2 = Or(Not(Not(Var("x"))), Var("y"))

            (t1 = t2) |> printfn "%A"
            (t1 = t2) |> printfn "%O"

            NNF t1 |> printfn "%A"
            NNF t1 |> printfn "%O"

            NNF t2 |> printfn "%A"
            NNF t2 |> printfn "%O"

            (NNF t1 = NNF t2) |> printfn "%A"
            (NNF t1 = NNF t2) |> printfn "%O"

            ()

    module execute_modules =

        let run () =
            using_range_expressions.run()
            iterating_a_sequence.run()
            transforming_sequences_with_aggregate_operators.run()
            using_lazy_sequences_from_external_sources.run()
            using_sequence_expressions.run()
            enriching_sequence_expressions_with_additional_logic.run()
            generating_lists_and_arrays_using_sequence_expressions.run()
            more_on_working_with_sequences.run()
            using_other_sequence_operators_truncate_and_sort.run()
            selecting_multiple_elements_from_sequences.run()
            finding_elements_and_indexes_in_sequences.run()
            grouping_and_indexeing_sequences.run()
            // BUGGY // structure_beyond_sequences_working_with_trees.run() 
            memoizing_construction_of_syntax_tree_nodes.run()
            converting_the_same_data_to_many_views.run()
            defining_partial_and_parameterize_active_patterns.run()
            hiding_abstract_syntax_implementations_with_active_patterns.run()

