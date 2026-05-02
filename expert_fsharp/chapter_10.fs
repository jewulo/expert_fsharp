module chapter_10

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
            
    /// CONTINUE FROM CHAPTER 10
    /// PAGE 244 :
    /// SECTION: STATISTICS, LINEAR ALGERBRA AND DISTRIBUTIONS WITH MATH.NET

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

