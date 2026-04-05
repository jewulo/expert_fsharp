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
            allFiles @"c:\WINDOWS\system32" |> printfn "%A"

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




    /// CONTINUE FROM CHAPTER 9
    /// PAGE 202
    /// SECTION: CLEANING UP IN SEQUENCE EXPRESSIONS

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
