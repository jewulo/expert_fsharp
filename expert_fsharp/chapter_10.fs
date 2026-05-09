module chapter_10

    // on fsi you need to load the following
    // #load "packages/FSharp.Charting/FSharp.Charting.fsx"

    
    open Microsoft.FSharp
    open FSharp.Charting

    module basic_charting_with_fsharp =
    
        let run () =
            let rnd = System.Random()
            let rand() = rnd.NextDouble()

            let randomPoints = [for i in 0 .. 1000 -> 10.0 * rand(), 10.0 * rand()]

            // randomPoints |> FSharpChart.Point // FSharpChart is deprecated
            randomPoints |> Chart.Point |> Chart.Show

            let randomTrend1 = [for i in 0.0 .. 0.1 .. 10.0 -> i, sin i + rand()]
            let randomTrend2 = [for i in 0.0 .. 0.1 .. 10.0 -> i, sin i + rand()]
    
            Chart.Combine [Chart.Line randomTrend1; Chart.Point randomTrend2] |> Chart.Show
            Chart.Combine [Chart.Line randomTrend1; Chart.Line randomTrend2] |> Chart.Show

            Chart.Line (randomPoints, Title = "Expected Trend") |> Chart.Show
            randomPoints |> fun c -> Chart.Line (c, Title = "Expected Trend")

    module basic_numeric_types_and_literals =

        let run () = ()

    module bitwise_operations =

        let encode (n : int32) =
            if (n >= 0 && n <= 0x7F) then [n]
            elif (n >= 0x80 && n <= 0x3FFF) then
                [(0x80 ||| (n >>> 8)) &&& 0xFF;
                 (n &&& 0xFF)]
            else [0xC0;
                  ((n >>> 24) &&& 0xFF);
                  ((n >>> 16) &&& 0xFF);
                  ((n >>> 8) &&& 0xFF);
                  (n &&& 0xFF)]

        let run () =
            encode 32 |> printfn "%A"
            encode 320 |> printfn "%A"
            encode 32000 |> printfn "%A"

    module summing_averaging_maximizing_minimizing_sequences =
        
        let run_1 () =
            let rnd = new System.Random()
            let rand() = rnd.NextDouble()

            let data = [for i in 1 .. 1000 -> rand() * rand()]
            data |> printfn "%A"

            let averageOfData = data |> Seq.average
            averageOfData |> printfn "%f"

            let sumOfData = data |> Seq.sum
            sumOfData |> printfn "%f"

            let maxOfData = data |> Seq.max
            maxOfData |> printfn "%f"

            let minOfData = data |> Seq.min
            minOfData |> printfn "%f"

        type RandomPoint = {X : float; Y : float; Z : float}        
        let run_2 () =
            
            let rnd = new System.Random()
            let rand() = rnd.NextDouble()
            
            let random3Dpoints =
                [for i in 1 .. 1000 -> {X = rand(); Y = rand(); Z = rand()}]

            let averageX = random3Dpoints |> Seq.averageBy (fun p -> p.X)
            averageX |> printfn "%f"

            let averageY = random3Dpoints |> Seq.averageBy (fun p -> p.Y)
            averageY |> printfn "%f"

            let averageZ = random3Dpoints |> Seq.averageBy (fun p -> p.Z)
            averageZ |> printfn "%f"

            let maxY = random3Dpoints |> Seq.maxBy (fun p -> p.Y)
            maxY |> printfn "%A"

            let norm (p : RandomPoint) = sqrt (p.X * p.X + p.Y * p.Y + p.Z * p.Z)
            let closest = random3Dpoints |> Seq.minBy (fun p -> norm p)
            closest |> printfn "%A"

        let run () =
            run_1()
            run_2()
            ()

    module counting_and_categorizing =
        
        open FSharp.Core

        type RandomPoint = {X : float; Y : float; Z : float}        
        let run () =
            
            let rnd = new System.Random()
            let rand() = rnd.NextDouble()
            let norm (p : RandomPoint) = sqrt (p.X * p.X + p.Y * p.Y + p.Z * p.Z)

            let random3Dpoints =
                [for i in 1 .. 1000 -> {X = rand(); Y = rand(); Z = rand()}]

            let histogram =
                random3Dpoints
                |> Seq.countBy (fun p -> int (norm p * 10.0 / sqrt 3.0))
                |> Seq.sortBy fst
                |> Seq.toList
            histogram |> printfn "%A"

            /// THIS DOES NOT COMPILE
            //let histogram2 =
            //    random3Dpoints
            //        .countBy(fun p -> int (norm p * 10.0 / sqrt 3.0))
            //        .sortBy fst
            //        .toList()
            ()

    module writing_fresh_numeric_code_1 =

        /// Compute the variance of an array of inputs
        let variance (values : float[]) =
            let sqr x = x * x
            let avg = values |> Array.average
            let sigma2 = values |> Array.averageBy (fun x -> sqr (x - avg))
            sigma2

        let standardDeviation values =
            sqrt (variance values)

        let run () = 
            let rnd = new System.Random()
            let rand() = rnd.NextDouble()

            let sampleTimes = [|for x in 0 .. 1000 -> 50.0 + 10.0 * rand()|]

            let exampleDeviation = standardDeviation sampleTimes
            exampleDeviation |> printfn "%f"

            let exampleVariance = variance sampleTimes
            exampleVariance |> printfn "%f"

    /// making it more generic
    module writing_fresh_numeric_code_2 =

        module Seq =

            /// Compute the variance of an array of inputs
            let varianceBy (f : 'T -> float) values =
                let sqr x = x * x
                let xs = values |> Seq.map f |> Seq.toArray
                let avg = xs |> Array.average
                let res = xs |> Array.averageBy (fun x -> sqr (x - avg))
                res

            let standardDeviationBy f values =
                sqrt (varianceBy f values)

        let run () = 
            let rnd = new System.Random()
            let rand() = rnd.NextDouble()

            let sampleTimes = [|for x in 0 .. 1000 -> 50.0 + 10.0 * rand()|]

            let exampleDeviation = Seq.standardDeviationBy (fun x -> x * 2.0) sampleTimes
            exampleDeviation |> printfn "%f"

    module making_numeric_code_generic =
        
        let inline variance values =
            let sqr x = x * x
            let avg = values |> Array.average
            let sigma2 = values |> Array.averageBy (fun x -> sqr (x - avg))
            sigma2

        let inline standardDeviation values =
            sqrt (variance values)

        let run () =
            let rnd = new System.Random()
            let rand() = rnd.NextDouble()

            // use arrays
            let sampleTimes1 = [|for x in 0 .. 1000 -> 50.0 + 10.0 * rand()|]

            let exampleVariance = variance sampleTimes1
            exampleVariance |> printfn "%f"

            let exampleDeviation = standardDeviation sampleTimes1
            exampleDeviation |> printfn "%f"

    module kmeans =
        
        type Input<'T> = {  Data : 'T; Features : float[] }
        type Centroid = float[]

        module Array =
            /// Like Seq.groupBy, but returns arrays
            /// xs : _[] implies xs is a generic array
            let classifyBy f (xs : _[]) =
                xs |> Seq.groupBy f |> Seq.map (fun (k,v) -> (k, Seq.toArray v)) |> Seq.toArray

        module Seq =
            /// Return x, f(x), f(f(x)), f(f(f(x))), .... 
            let iterate f x = x |> Seq.unfold (fun x -> Some (x, f x))

        /// Compute the norm distance between an input and a centroid
        let distance (xs : Input<_>) (ys : Centroid) =
            (xs.Features, ys)
                ||> Array.map2 (fun x y -> (x - y) * (x - y))
                |> Array.sum

        /// Find the average set of inputs. First compute xs1 + ... +xsN, pointwise,
        /// then divide each element of the sum by the number of inputs.
        let computeCentroidOfGroup (_, group : Input<_>[]) =
            let e0 = group.[0].Features
            [| for i in 0 .. e0.Length - 1 -> group |> Array.averageBy (fun e -> e.Features.[i]) |]

        /// Group all the inputs by the nearest centroid
        let classifyIntoGroups inputs centroids =
            inputs |> Array.classifyBy (fun v -> centroids |> Array.minBy (distance v))

        /// Repeatedly classify the inputs, starting with the initial centroids
        let rec computeCentroids inputs centroids =
            seq {
                let classification = classifyIntoGroups inputs centroids
                yield classification

                let newCentroids = Array.map computeCentroidOfGroup classification
                yield! computeCentroids inputs newCentroids
            }

        /// Extract the features and repeatedly classify the inputs, starting with the
        /// initial centroids
        let kmeans inputs featuresExtractor initialCentroids =
            let inputs =
                inputs
                |> Seq.map (fun i -> {Data = i; Features = featuresExtractor i})
                |> Seq.toArray
            let initialCentroids = initialCentroids |> Seq.toArray
            computeCentroids inputs initialCentroids

        /// Generate a synthetic input data set that features four clusters of data
        open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

        type Observation = { Time : float<s>; Location : float<m> }

        let rnd = System.Random()
        let rand () = rnd.NextDouble()
        let randZ () = rnd.NextDouble() - 0.5
        
        let run () =
            // Create a point near the given point
            let near p= { Time = p.Time + randZ() * 20.0<s>;
                                      Location = p.Location + randZ() * 5.0<m> }
            let data =
                [for i in 1 .. 1000 -> near {Time = 100.0<s>; Location = 60.0<m>}
                 for i in 1 .. 1000 -> near {Time = 120.0<s>; Location = 80.0<m>}
                 for i in 1 .. 1000 -> near {Time = 180.0<s>; Location = 30.0<m>}
                 for i in 1 .. 1000 -> near {Time = 70.0<s>; Location = 40.0<m>}]

            let maxTime = data |> Seq.maxBy (fun p -> p.Time) |> fun p -> p.Time
            let maxLoc = data |> Seq.maxBy (fun p -> p.Location) |> fun p -> p.Location

            let initialCentroids = [ for i in 0 .. 9 -> [|rand(); rand()|] ]
            let featureExtractor (p : Observation) =
                [| p.Time / maxTime; p.Location / maxLoc|]

            // Gives an infinite sequence of centroid/classification representing
            // repeated iterations of the algorithm.
            let ccent_class =
                kmeans data featureExtractor initialCentroids

            // Take only the 100th iteration of the algorithm, and renormalise
            let ccent_class_100th =
                kmeans data featureExtractor initialCentroids
                    |> Seq.map (Array.map (fun (c, _) -> c.[0] * maxTime, c.[1] * maxLoc))

            ccent_class |> printfn "%A"
            ccent_class_100th |> printfn "%A"
             
    module statistics_linear_algebra_and_distributions_with_math_dotnet =

        open MathNet.Numerics.Statistics
        open MathNet.Numerics.Distributions
        open System.Collections.Generic

        let histogram n data = 
            let h = Histogram(data, n)
            [| for i in 0 .. h.BucketCount - 1 ->
                (sprintf "%.0f-%.0f" h.[i].LowerBound h.[i].UpperBound, h.[i].Count) |]

        let run () =
            let data = [for i in 0.0 .. 0.01 .. 10.0 -> sin i]
            data |> printfn "%A"

            let exampleVariance = data |> Statistics.Variance
            exampleVariance |> printfn "%f"

            let exampleMean = data |> Statistics.Mean
            exampleMean |> printfn "%f" 

            let exampleMinimum = data |> Statistics.Minimum
            exampleMinimum |> printfn "%f"

            let exampleMaximum = data |> Statistics.Maximum
            exampleMaximum |> printfn "%f"

            let exampleBellCurve = Normal(100.0, 10.0)
            exampleBellCurve |> printfn "%A"
            exampleBellCurve.Samples() |> printfn "%A"

            // this is not working
            exampleBellCurve.Samples()
                |> Seq.truncate 1000
                |> histogram 10
                |> Chart.Column
                |> Chart.Show

    module using_matrices_and_vectors_from_math_dotnet =
        
        open MathNet.Numerics
        open MathNet.Numerics.LinearAlgebra
        open MathNet.Numerics.LinearAlgebra.Double

        //// THESE fsi invocations do not work

        // Only call PrettyFsi.addPrinters() when running inside FSI/interactive to avoid
        // introducing a compile-time dependency on FSharp.Compiler.Interactive.Settings.
        #if INTERACTIVE
        PrettyFsi.addPrinters()
        fsi.AddPrintTransformer (fun (x : DenseVector) ->
            box [|for i in 0 .. x.Count - 1 -> x.[i]|])

        fsi.AddPrintTransformer (fun (x : DenseMatrix) ->
            box (array2D [for i in 0 .. x.RowCount - 1 ->
                            [for i in 0 .. x.ColumnCount - 1 -> x.[i, j]]]))
        #endif

        let working_with_vectors () =
            let vector1 = vector [1.0; 2.4; 3.0]
            let vector2 = vector [7.0; 2.1; 5.4]
            let vector3 = vector1 + vector2

            vector1 |> printfn "vector1 : %A"
            vector2 |> printfn "vector2 : %A"
            vector3 |> printfn "vector1 + vector2 : %A"

        let working_with_matrices () =
            let matrix1 = matrix [[1.0; 2.0]; [1.0; 3.0]]
            let matrix2 = matrix [[1.0; -2.0]; [0.5; 3.0]]
            let matrix3 = matrix1 * matrix2

            matrix1 |> printfn "matrix1 : %A"
            matrix2 |> printfn "matrix2 : %A"
            matrix3 |> printfn "matrix1 * matrix2 : %A"

        let run () =
            working_with_vectors()
            working_with_matrices()

    module matrices_inverses_decompositions_and_eigenvalues =
        
        open MathNet.Numerics
        open MathNet.Numerics.LinearAlgebra
        open MathNet.Numerics.LinearAlgebra.Double
        // open MathNet.Numerics.LinearAlgebra.Generic // DOES NOT EXIST

        #if INTERACTIVE
        PrettyFsi.addPrinters()
        fsi.AddPrinter (fun (c : System.Numerics.Complex) ->
            sprintf "%fr + %fi" c.Real c.Imaginary)

        fsi.AddPrintTransformer (fun (x : DenseMatrix) ->
            box (array2D [for i in 0 .. x.RowCount - 1 ->
                            [for i in 0 .. x.ColumnCount - 1 -> x.[i, j]]]))
        #endif

        let rnd = System.Random()
        let rand () = rnd.NextDouble()
        

        let run () =
            let largeMatrix = matrix [ for i in 1 .. 100 -> [for j in 1 .. 100 -> rand()]]
            let laregMatrixInverse = largeMatrix.Inverse()
            let check = largeMatrix * largeMatrix.Inverse()

            largeMatrix |> printfn "laregMatrix : %A"
            laregMatrixInverse |> printfn " laregMatrixInverse : %A"
            check |> printfn "laregMatrix * laregMatrixInverse : %A"

            let evd = largeMatrix.Evd()
            let eigenValues = evd.EigenValues
            let determinant = evd.Determinant
            let eigenVectors = evd.EigenVectors

            evd |> printfn "laregMatrix.Evd : Factorization : %A"            
            eigenValues |> printfn "EigenValues : %A"
            determinant |> printfn "Determinant : %A"
            eigenVectors |> printfn "EigenVectors : %A"

            evd.IsFullRank |> printfn "Is Full Rank? : %b"
            evd.Rank |> printfn "Rank: : %d"
            evd.IsSymmetric |> printfn "Is Symmetric? : %b"            
            evd.D |> printfn "Diagonal EigenValue Matrix : %A"

    /// EXPERT F# 4.0 PAGE 276 :
    module time_series_and_data_frames_with_deedle =

        open System
        open Deedle
        open MathNet.Numerics.Distributions

        let start = DateTimeOffset(DateTime.Today)

        let randomPrice drift volatility initial (span : TimeSpan) count =
            let dist = Normal(0.0, 1.0, RandomSource = Random())
            let dt = span.TotalDays / 250.0
            let driftExp = (drift - 0.5 * pown volatility 2) * dt
            let randExp = volatility * (sqrt dt)

            (start, initial)
                |> Seq.unfold
               (fun (dt, price) ->
                            let price = price * exp (driftExp + randExp * dist.Sample())
                            Some((dt, price), (dt + span, price)))
                |> Seq.take count

        let stock1 = randomPrice 0.1 3.0 20.0 (TimeSpan.FromMinutes(1.0)) 500
        let stock2 = randomPrice 0.2 1.6 20.0 (TimeSpan.FromSeconds(30.0)) 1000

        let run () =
                // turn the raw data into a time series using the Deedle Library
            let stockSeries1 = series stock1
            let stockSeries2 = series stock2

            stockSeries1 |> printfn "%A"
            stockSeries2 |> printfn "%A"

            let zippedSeriesWhereBothHaveData = stockSeries1.Zip (stockSeries2, JoinKind.Left)
            zippedSeriesWhereBothHaveData |> printfn "%A"

            let zippedSeriesWhereOneHasData = stockSeries1.Zip (stockSeries2, JoinKind.Right)            
            zippedSeriesWhereOneHasData |> printfn "%A"

            // Contains value every minute
            let f1 = Frame.ofColumns ["S1" => stockSeries1]
            f1 |> printfn "%A"

            // Contains value every 30 seconds
            let f2 = Frame.ofColumns ["S2" => stockSeries2]
            f2 |> printfn "%A"

            let alignedData = f1.Join(f2, JoinKind.Outer)
            alignedData

    module units_of_measure =
        
        open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
        open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

        module intro =
            // defining a unit of measure using the Measure attribute
            [<Measure>] type click

            [<Measure>] type pixel

            [<Measure>] type money

            let run () =
                let rateOfClicks = 200.0<click/s>
                let durationOfExecution = 3.5<s>

                let numberOfClicks = rateOfClicks * durationOfExecution
                numberOfClicks |> printfn "%O"

        module adding_units_to_numeric_algorithms =

            module non_unitized =
                
                let integrateByMidPointRule f (a, b) = (b - a) * f ((a + b) / 2.0)
                let integrateByTrapezoidRule f (a, b) = (b - a) * ((f a + f b) / 2.0)
                let integrateByIterativeRule f (a, b) n =
                    (b - a) / float n *
                    ((f a + f b) / 2.0 +
                      List.sum [for k in 1 .. n - 1 -> f (a + float k * (b - a) / float n)])

                //let cubed x:float = x * x * x
                let velocityFunction t  = 100.0 + t * -9.81

                let run () =                    
                    let f1 = integrateByMidPointRule velocityFunction (0.0, 10.0)
                    printfn "%f" f1

                    let f2 = integrateByTrapezoidRule velocityFunction (0.0, 10.0)
                    printfn "%f" f2

                    let f3 = integrateByIterativeRule velocityFunction (0.0, 10.0) 10
                    printfn "%f" f3

            module unitized =

                let integrateByMidPointRule (f : float<'u> -> float<'v>) (a : float<'u>, b : float<'u>) =
                    (b - a) * f ((a + b) / 2.0)

                let integrateByTrapezoidRule (f : float<'u> -> float<'v>) (a : float<'u>, b : float<'u>) =
                    (b - a) * ((f a + f b) / 2.0)

                let integrateByIterativeRule  (f : float<'u> -> float<'v>) (a : float<'u>, b : float<'u>)  n =
                    (b - a) / float n *
                    ((f a + f b) / 2.0 +
                      List.sum [for k in 1 .. n - 1 -> f (a + float k * (b - a) / float n)])

                let velocityFunction (t : float<s>) = 100.0<m/s> + t * -9.81<m/s^2>

                let run () =                    
                    let distance1 = integrateByMidPointRule velocityFunction (0.0<s>, 10.0<s>)
                    printfn "%f" distance1

                    let distance2 = integrateByTrapezoidRule velocityFunction (0.0<s>, 10.0<s>)
                    printfn "%f" distance2

                    let distance3 = integrateByIterativeRule velocityFunction (0.0<s>, 10.0<s>) 10
                    printfn "%f" distance3

            module unitized_variance =

                open MathNet.Numerics.Statistics
                open MathNet.Numerics.Distributions
                open System.Collections.Generic

                // for variance to use units you have to use the more abstract Seq type rather than Array type
                let inline variance (values: seq<float<_>>) =
                    let sqr x = x * x
                    let xs = values |> Seq.toArray  // then convert to Array
                    let avg = xs |> Array.average
                    let variance = xs |> Array.averageBy (fun x -> sqr (x - avg))
                    variance

                let inline standardDeviation values =
                    sqrt (variance values)

                let run () =
                    let rnd = new System.Random()
                    let rand() = rnd.NextDouble()

                    // time is labeled in seconds <s>
                    let sampleTimes = [for x in 0 .. 1000 -> 50.0<s> + 10.0<s> * rand()]

                    // standard deviation is calulated in seconds <s>
                    let exampleDeviation = standardDeviation sampleTimes
                    exampleDeviation |> printfn "%f"

                    // variance is calulated in seconds squared <s^2>
                    let exampleVariance = variance sampleTimes
                    exampleVariance |> printfn "%f"

            let run () =
                non_unitized.run()
                unitized.run()
                unitized_variance.run()

        let run () = 
            intro.run()
            adding_units_to_numeric_algorithms.run()

    module adding_units_to_a_type_definition =
        
        //type Vector2D<[<Measure>] 'u> = {DX : float<'u>; DY : float<'u>}

        // defined to be used to test this module
        [<Measure>] type m
        [<Measure>] type sec
        [<Measure>] type kg

        /// Two-dimensional vectors
        type Vector2D<[<Measure>] 'u> (dx : float<'u>, dy : float<'u>) =

            /// Get the X component of the vector
            member v.DX = dx

            /// Get the Y component of the vector
            member v.DY = dy

            /// Get the length of the vector
            member v.Length = sqrt(dx * dx + dy * dy) 

            /// Get a vector scaled by the given factor k
            member v.Scale k = Vector2D(k * dx, k * dy)

            /// Return a vector shifted by the given delta in the X coordinate
            member v.ShiftX x = Vector2D(dx + x, dy)

            /// Return a vector shifted by the given delta in the Y coordinate
            member v.ShiftY y = Vector2D(dx, y * dy)

            /// Get the zero vector
            static member Zero = Vector2D<'u>(0.0<_>, 0.0<_>)

            /// Return a constant vector along the X coordinate
            static member ConstX dx = Vector2D<'u>(dx, 0.0<_>)

            /// Return a constant vector along the Y coordinate
            static member ConstY dy = Vector2D<'u>(0.0<_>, dy)

            /// Return a the sum of two vectors
            static member (+) (v1 : Vector2D<'u>, v2 : Vector2D<'u>) =
                Vector2D(v1.DX + v2.DX, v1.DY + v2.DY)

            /// Return a the difference of two vectors
            static member (-) (v1 : Vector2D<'u>, v2 : Vector2D<'u>) =
                Vector2D(v1.DX - v2.DX, v1.DY - v2.DY)

            /// Return a the pointwise-product of two vectors
            static member (.*) (v1 : Vector2D<'u>, v2 : Vector2D<'u>) =
                Vector2D(v1.DX * v2.DX, v1.DY * v2.DY)

            /// This enables the Vector2D to be printed ot to console easily:
            /// THIS IS NOT IN THE BOOK.
            override v.ToString() = sprintf "[%f,%f]" dx dy
            
        let run () =
            let d1 = Vector2D<m>(1.0<m>, 2.0<m>)
            let d2 = Vector2D<m>(4.0<m>, 2.0<m>)

            d1.Length |> printfn "%A"

            let d3 = d1 + d2
            d3 |> printfn "%A"

            let d4 = d1 - d2
            d4 |> printfn "%A"

            let d5 = d1 .* d2
            d5 |> printfn "%A"

    module applying_and_removing_units =
        
        [<Measure>] type kg

        let run () =            
            let three = float 3.0<kg>
            let sixKg = LanguagePrimitives.FloatWithMeasure<kg> (three + three)

            three |> printfn "%A"
            sixKg |> printfn "%A"

    /// CONTINUE FROM CHAPTER 10 UNITIZING KMEANS
    /// EXPERT F# 3.0 :PAGE 255
    /// EXPERT F# 4.0 :PAGE 284
    module exercise_unitizing_kmeans =
        
        type Input<'T, [<Measure>] 'u> =
            {   Data : 'T
                Features : float<'u>[] }

        type Centroid<[<Measure>] 'u> = float<'u>[]

        module Array =
            /// Like Seq.groupBy, but returns arrays
            /// xs : _[] implies xs is a generic array
            let classifyBy f (xs : _[]) =
                xs |> Seq.groupBy f |> Seq.map (fun (k,v) -> (k, Seq.toArray v)) |> Seq.toArray

        module Seq =
            /// Return x, f(x), f(f(x)), f(f(f(x))), .... 
            let iterate f x = x |> Seq.unfold (fun x -> Some (x, f x))

        /// Compute the norm distance between an input and a centroid
        let distance (xs : Input<_,_>) (ys : Centroid<_>) =
            (xs.Features, ys)
                ||> Array.map2 (fun x y -> (x - y) * (x - y))
                |> Array.sum

        /// Find the average set of inputs. First compute xs1 + ... +xsN, pointwise,
        /// then divide each element of the sum by the number of inputs.
        let computeCentroidOfGroup (_, group : Input<_,_>[]) =
            let e0 = group.[0].Features
            [| for i in 0 .. e0.Length - 1 -> group |> Array.averageBy (fun e -> e.Features.[i]) |]

        /// Group all the inputs by the nearest centroid
        let classifyIntoGroups inputs centroids =
            inputs |> Array.classifyBy (fun v -> centroids |> Array.minBy (distance v))

        /// Repeatedly classify the inputs, starting with the initial centroids
        let rec computeCentroids inputs centroids =
            seq {
                let classification = classifyIntoGroups inputs centroids
                yield classification

                let newCentroids = Array.map computeCentroidOfGroup classification
                yield! computeCentroids inputs newCentroids
            }

        /// Extract the features and repeatedly classify the inputs, starting with the
        /// initial centroids
        let kmeans inputs featuresExtractor initialCentroids =
            let inputs =
                inputs
                |> Seq.map (fun i -> {Data = i; Features = featuresExtractor i})
                |> Seq.toArray
            let initialCentroids = initialCentroids |> Seq.toArray
            computeCentroids inputs initialCentroids

        /// Generate a synthetic input data set that features four clusters of data
        open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

        type Observation = { Time : float<s>; Location : float<m> }

        let rnd = System.Random()
        let rand () = rnd.NextDouble()
        let randZ () = rnd.NextDouble() - 0.5
        
        let run () =
            // Create a point near the given point
            let near p= { Time = p.Time + randZ() * 20.0<s>;
                                      Location = p.Location + randZ() * 5.0<m> }
            let data =
                [for i in 1 .. 1000 -> near {Time = 100.0<s>; Location = 60.0<m>}
                 for i in 1 .. 1000 -> near {Time = 120.0<s>; Location = 80.0<m>}
                 for i in 1 .. 1000 -> near {Time = 180.0<s>; Location = 30.0<m>}
                 for i in 1 .. 1000 -> near {Time = 70.0<s>; Location = 40.0<m>}]

            let maxTime = data |> Seq.maxBy (fun p -> p.Time) |> fun p -> p.Time
            let maxLoc = data |> Seq.maxBy (fun p -> p.Location) |> fun p -> p.Location

            let initialCentroids = [ for i in 0 .. 9 -> [|rand(); rand()|] ]
            let featureExtractor (p : Observation) =
                [| p.Time / maxTime; p.Location / maxLoc|]

            // Gives an infinite sequence of centroid/classification representing
            // repeated iterations of the algorithm.
            let ccent_class =
                kmeans data featureExtractor initialCentroids

            // Take only the 100th iteration of the algorithm, and renormalise
            let ccent_class_100th =
                kmeans data featureExtractor initialCentroids
                    |> Seq.map (Array.map (fun (c, _) -> c.[0] * maxTime, c.[1] * maxLoc))

            ccent_class |> printfn "%A"
            ccent_class_100th |> printfn "%A"

    module execute_modules =

        let run () =
            basic_charting_with_fsharp.run() |> ignore
            basic_numeric_types_and_literals.run()
            bitwise_operations.run()
            summing_averaging_maximizing_minimizing_sequences.run()
            counting_and_categorizing.run()
            writing_fresh_numeric_code_1.run()
            writing_fresh_numeric_code_2.run()
            making_numeric_code_generic.run()
            kmeans.run()
            statistics_linear_algebra_and_distributions_with_math_dotnet.run()
            using_matrices_and_vectors_from_math_dotnet.run()
            matrices_inverses_decompositions_and_eigenvalues.run()
            time_series_and_data_frames_with_deedle.run()
            units_of_measure.run()
            adding_units_to_a_type_definition.run()
            applying_and_removing_units.run()
            exercise_unitizing_kmeans.run()
