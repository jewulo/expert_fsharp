module chapter_6

    module getting_started_with_objects_and_members =

        /// Two dimensional vectors
        type Vector2D =
            { DX : float; DY : float }

            /// Get the length of the vector
            member v.Length = sqrt(v.DX * v.DX + v.DY * v.DY)

            /// Get a vector scaled by the given factor
            member v.Scale(k) = { DX = k * v.DX; DY = k * v.DY }

            /// Return a vector shifted by the given delta in the X coordinate
            member v.ShiftX(x) = { v with DX = v.DX + x }

            /// Return a vector shifted by the given delta in the Y coordinate
            member v.ShiftY(x) = { v with DY = v.DY + x }

            /// Return a vector shifted by the given distance in both coordinates
            member v.ShiftXY(x, y) = { DX = v.DX + x; DY = v.DY + y }

            /// Get the zero vector
            static member Zero = { DX = 0.0; DY = 0.0 }

            /// Return a constant vector along the X axis
            static member ConstX(dx) = { DX = dx; DY = 0.0 }

            /// Return a constant vector along the Y axis
            static member ConstY(dy) = { DX = 0.0; DY = dy }

        let run () =
            let v = { DX = 3.0; DY = 4.0 }
            v |> printfn "%A"
            v.Length |> printfn "%f"
            v.Scale(2.0).Length |> printfn "%f"
            let u = Vector2D.ConstX(3.0)
            u |> printfn "%A"
    
    module discriminated_union_members =

        // a type of binary tree, generic in the type of values
        // carried at the nodes and tips
        type Tree<'T> = 
            | Node of 'T * Tree<'T> * Tree<'T>
            | Tip of 'T

            /// compute the number of values in the tree
            member t.Size = 
                match t with
                | Node(_,l,r) -> 1 + l.Size + r.Size
                | Tip _ -> 1    // include the tips, the book returns 0 for Tips

        let run () = 
            let smallTree = Node ("1", Node ("2",
                                                          Tip "a", Tip "b"),
                                                    Tip "c")

            smallTree |> printfn "smallTree = %A"
            smallTree.Size |> printfn "smallTree.Size = %d"

    module using_classes_1 =

        /// Two dimensional vectors via a Class Type
        /// A Vector2D Type with length precomputation via a Class Type
        type Vector2D(dx : float, dy : float) =

            /// Calculate the length of the vector at initialisation.
            let len = sqrt(dx * dx + dy * dy)

            /// Get the X component of the vector
            member v.DX = dx

            /// Get the Y component of the vector
            member v.DY = dy

            // Store the length in member Length
            member v.Length = len

            // member property with a side-effect
            member v.LengthWithSideEffect =
                printfn "Computing"     // used to show that the object property is computed
                sqrt(dx * dx + dy * dy)

            /// Return a vector scaled by the given factor
            member v.Scale(k) = Vector2D(k * dx, k * dy)

            /// Return a vector shifted by the given delta in the X coordinate
            member v.ShiftX(x) = Vector2D(dx = dx + x, dy = dy)

            /// Return a vector shifted by the given delta in the Y coordinate
            member v.ShiftY(y) = Vector2D(dx = dx, dy = dy + y)

            /// Return a vector shifted by the given distance in both coordinates
            member v.ShiftXY(x, y) = Vector2D(dx = dx + x, dy = dy + y)

            /// Get the zero vector
            static member Zero = Vector2D(dx = 0.0, dy = 0.0)

            /// Return a constant vector along the X axis
            static member OneX = Vector2D(dx = 1.0, dy = 0.0)

            /// Return a constant vector along the Y axis
            static member OneY = Vector2D(dx = 0.0, dy = 1.0)

        let run () =
            let v = Vector2D(3.0,4.0)
            //v |> printfn "%A"   // prints long type name - chapter_6+using_classes+Vector2D
            v.Length |> printfn "%f"
            v.LengthWithSideEffect |> printfn "%f"
            v.Scale(2.0).Length |> printfn "%f"
            v.LengthWithSideEffect |> printfn "%f"
            //let u1 = Vector2D.OneX
            //u1 |> printfn "%A"    // prints long type name - chapter_6+using_classes+Vector2D
            //let u2 = Vector2D.OneY
            //u2 |> printfn "%A"  // prints long type name - chapter_6+using_classes+Vector2D
            
    module using_classes_2 =

        // Vectors whose length is checked to be close to length one
        // type UnitVector2D(dx : float, dy : float) = float can be inferred without explicit typing
        type UnitVector2D(dx, dy) = 
            let tolerance = 0.000001

            let length = sqrt (dx * dx + dy * dy)

            do if abs (length - 1.0) >= tolerance then failwith "not a unit vector";

            member v.DX = dy

            member v.DY = dy

            new () = UnitVector2D (1.0, 0.0)

        let run () =
            let i = UnitVector2D(1.0,0.0)
            i |> printfn "%A"
            let j = UnitVector2D(0.0,1.0)
            j |> printfn "%A"
            
            // this should throw an exception because it is not a unit vector
            // System.Exception: not a unit vector
            // let k = UnitVector2D(1.0,1.0)
            // k |> printfn "%A"

    module using_classes_3 =

        // A class including some static bindings
        type Vector2D(dx : float, dy : float) = 
            static let zero = Vector2D(0.0,0.0)
            static let onex = Vector2D(1.0,0.0)
            static let oney = Vector2D(0.0,1.0)

            /// Get the zero vector
            static member Zero = zero

            /// Get a constant vector along the X axis of length one
            static member OneX = onex

            /// Get a constant vector along the Y axis of length one
            static member OneY = oney

            new () = Vector2D (1.0, 0.0)

        let run () =
            let i = Vector2D()
            let j = Vector2D.Zero
            let k = Vector2D.OneX
            let l = Vector2D.OneY
            i |> printfn "%A"
            j |> printfn "%A"
            k |> printfn "%A"
            l |> printfn "%A"
        
    module working_with_indexer_properties =

        open System.Collections.Generic

        type SparseVector(items : seq<int * float>) =
            let elems = new SortedDictionary<_,_>()
            do items |> Seq.iter (fun (k, v) -> elems.Add(k, v))

            /// This defines an indexer property
            member t.Item
                with get(idx) =
                    if elems.ContainsKey(idx) then elems.[idx]
                    else 0.0

        let run () =
            let sv = SparseVector [(3, 547.0)]
            sv.[4] |> printfn "%f"
            sv.[3] |> printfn "%f"

    module adding_overloaded_operators =

        /// Two dimensional vectors via a Class Type
        /// A Vector2D Type with length precomputation via a Class Type
        type Vector2DWithOperators(dx : float, dy : float) =

            /// Calculate the length of the vector at initialisation.
            let len = sqrt(dx * dx + dy * dy)

            /// Get the X component of the vector
            member v.DX = dx

            /// Get the Y component of the vector
            member v.DY = dy

            // Store the length in member Length
            member v.Length = len

            static member (+) (v1 : Vector2DWithOperators, v2 : Vector2DWithOperators) =
                Vector2DWithOperators(v1.DX + v2.DX, v1.DY + v2.DY)

            static member (-) (v1 : Vector2DWithOperators, v2 : Vector2DWithOperators) =
                Vector2DWithOperators(v1.DX - v2.DX, v1.DY - v2.DY)

        let run () =
            let v1 = Vector2DWithOperators(3.0,4.0)
            let v2 = v1 + v1;
            v1 |> printfn "%A"   // prints long type name - chapter_6+using_classes+Vector2D
            
    // CONTINUE FROM CHAPTER 6: PAGE 120
    // USING NAMED AND OPTIONAL ARUGUMENTS

    module using_named_and_optional_arguments = 
        let run () = ()

    module execute_modules =

        let run () =
            printfn "[---- Expert F#: START CHAPTER 6 ----]"

            getting_started_with_objects_and_members.run()
            discriminated_union_members.run()
            using_classes_1.run()
            using_classes_2.run()
            using_classes_3.run()
            working_with_indexer_properties.run()

            printfn "[---- Expert F#: END CHAPTER 6 ----]"

