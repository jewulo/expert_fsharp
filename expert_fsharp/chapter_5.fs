module chapter_5

open System

    /// Collection of code used in most examples in Chapter 3
    module http_stuff =

        open System.Net.Http
        open System.Threading.Tasks

        let delimiters  = [| ' '; '\n'; '\t'; '<'; '>'; '='|]

        let getWords (s: string) = s.Split delimiters

        /// Get the contents of the URL via a web request using HttpClient (async)
        let http (url: string) =
            use client = new HttpClient()
            let task: Task<string> = client.GetStringAsync(url)
            task.Result

        let fetch url = 
            try Some (http url)
            with :? System.Net.WebException -> None

    module time_stuff =

        // from Chapter 2, but modified to use stop watch.
        let time f = 
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let res = f()
            let finish = sw.Stop()
            (res, sw.Elapsed)

    module defining_record_types =
        
        type Person =
            {   Name : string
                DateOfBirth : System.DateTime   }

        type PageStats =
            {   Site : string
                Time : System.TimeSpan
                Length : int
                NumWords : int
                NumHRefs : int  }

        let stats site =
            let url = "http://" + site
            let html, t = time_stuff.time (fun () -> http_stuff.http url)
            let words = html |> http_stuff.getWords
            let hrefs = words |> Array.filter (fun s -> s = "href")
            {Site = site; Time = t; Length = html.Length;
            NumWords = words.Length; NumHRefs = hrefs.Length}

        let run () =
            let bill = {Name = "Bill"; DateOfBirth = new System.DateTime(1962, 09, 02)}
            let anna = {Name = "Anna"; DateOfBirth = new System.DateTime(1968, 07, 23)}
            bill |> printfn "%A"
            anna |> printfn "%A"

            // this crashes 
            // stats "www.live.com" |> printfn "%A"
            ()

    module handling_non_unique_record_field_names =

        type Person =
            {   Name : string
                DateOfBirth : System.DateTime   }

        type Company =
            {   Name : string
                Address : string    }

        type Dot = {X : int; Y : int}
        type Point = {X : float; Y : float}
        type Point3D = {X : float; Y : float; Z : float}

        let intro () = 
            let coords1 (p : Point) = (p.X, p.Y)
            let coords2 (d : Dot) = (d.X, d.Y)
            let dist p = sqrt (p.X * p.X + p.Y * p.Y)   // use of X and Y implies  type Point
            ()

        let cloning_records () = 
            let p1 = {X = 3.0; Y = 3.0; Z = 0.5}
            let p2 = {p1 with Y = 0.0; Z = 0.0}
            ()

        let run () =
            intro()
            cloning_records()

    module defining_discrimiated_unions =

        type Route = int
        type Make = string
        type Model = string
        type Transport =
            | Car of Make * Model
            | Bicycle
            | Bus of Route

        let averageSpeed (tr : Transport) =
            match tr with
            | Car _ -> 35
            | Bicycle -> 16
            | Bus _ -> 24

        let run () =
            let ian = Car("BMW", "360")
            let don = [Bicycle; Bus 8]
            let peter = [Car("Ford", "Fiesta"); Bicycle]
            averageSpeed ian |> printfn "%d"
            List.map averageSpeed don |> printfn "%A"
            List.map averageSpeed peter |> printfn "%A"


    module defining_recursive_discrimiated_unions =

        type Proposition =
            | True
            | And of Proposition * Proposition
            | Or of Proposition * Proposition
            | Not of Proposition

        // Recursive functions can be used to traverse
        // recursive discrimiated unions
        let rec eval (p : Proposition) =
            match p with
            | True -> true
            | And(p1,p2) -> eval p1 && eval p2
            | Or (p1,p2) -> eval p1 || eval p2
            | Not(p1) -> not (eval p1)

        // You can use discriminators in Pattern Matching
        let intro () =
            let p1 = True
            let p2 = Not p1
            let p3 = And (p1, p2)
            let p4 = Or (p1, p2)

            eval p1 |> printfn "%A"
            eval p2 |> printfn "%A"
            eval p3 |> printfn "%A"
            eval p4 |> printfn "%A"
            ()

        // common examples recursive discrimiated unions in f-sharp
        module common_examples_in_fSharp =

            // the option type
            type 'T option = 
                | None
                | Some of 'T

            // the list type
            type 'T list =
                | ([])
                | (::) of 'T * 'T list

        type Tree<'T> = 
            | Tree of 'T * Tree<'T> * Tree<'T>
            | Tip of 'T
        
        let rec sizeOfTree tree = 
            match tree with
            | Tree(_,l,r) -> 1 + sizeOfTree l + sizeOfTree r
            | Tip _ -> 1

        let run () =
            intro()

            let smallTree = Tree ("1",
                                                    Tree ("2",
                                                          Tip "a", Tip "b"),
                                                    Tip "c")
            sizeOfTree smallTree |> printfn "%d"

    module using_discrimiated_unions_as_records =

        type Point3D = Vector3D of float * float * float
        
        let length (Vector3D (dx, dy, dz)) = sqrt(dx * dx + dy * dy + dz * dz)

        let run () =
            let origin = Vector3D(0., 0., 0.)
            let unitX = Vector3D(1., 0., 0.)
            let unitY = Vector3D(0., 1., 0.)
            let unitZ = Vector3D(0., 0., 1.)
            length origin |> printfn "length origin = %f"
            length unitX |> printfn "length unitX = %f"
            length unitY |> printfn "length unitY = %f"
            length unitZ |> printfn "length unitZ = %f"

    module defining_multiple_types_simultaneously =
        type Node =
            {   Name : string
                Links : Link list   }
        and Link =
            | Dangling
            | Link of Node

    module understanding_generics =

        // Type Generics
        type StringMap<'T> = Map<string, 'T>
        type Projections<'T, 'U> = ('T -> 'U) * ('U -> 'T)

        // Value Generics
        let rec map1 (f : 'T -> 'U) (l : 'T list) =
            match l with
            | h :: t -> f h :: map1 f t
            | [] -> []

        // ... or more explicitly
        let rec map2<'T, 'U> (f : 'T -> 'U) (l : 'T list) =
            match l with
            | h :: t -> f h :: map2 f t
            | [] -> []

        let run () =
            map1 (fun x -> x * x) [1; 2; 3; 4; 5] |> printfn "%A"
            map2 (fun x -> x * x) [1; 2; 3; 4; 5] |> printfn "%A"

    module writing_generics_functions =

        // a function with no specific types.
        // types become specific during in vocation
        let getFirst (a, b , c) = a

        // due to the body of the function parameters f and g
        // are deduced to be functions
        let mapPair f g (x, y) = (f x, g y)

        let run () =
            // getFirst int * int * int -> int
            getFirst (1, 2, 3) |> printfn "%A"

            // getFirst char * char * char -> char
            getFirst ('a', 'b', 'c') |> printfn "%A"

            // getFirst string * int * char -> string
            getFirst ("str", 2, 'c') |> printfn "%A"

            // deducing generic function parameters
            mapPair (fun x -> x * x) (fun x -> x + x) (3,  4) |> printfn "%A"
            mapPair (fun x -> 2.0 * x) (fun x -> 1.5 * x) (3.0,  4.0) |> printfn "%A"

    module some_important_generic_functions =

        let generic_comparison () =
            ("abc", "def") < ("abc", "xyz") |> printfn "%A"
            compare (10, 30) (10, 20) |> printfn "%A"       // compare tuples            
            compare [10; 30] [10; 20] |> printfn "%A"       // compare lists
            compare [10, 30] [10, 20] |> printfn "%A"       // compare lists of tuples
            compare [(10, 30)] [(10, 20)] |> printfn "%A"   // compare lists of tuples
            compare [|10; 30|] [|10; 20|] |> printfn "%A"   // compare arrays

        let generic_hashing () =
            hash 100 |> printfn "hash 100 = %d"
            hash "abc" |> printfn "hash \"abc\" = %d"
            hash (100, "abc") |> printfn "hash (100, \"abc\") = %d"
            hash ("abc", 100) |> printfn "hash (\"abc\", 100) = %d"

        let generic_pretty_printing () =
            sprintf "result = %A" ([1], [true])
            

        let generic_boxing_and_unboxing () =
            let v1 = 1
            printfn "%A" v1
            let b1 = box 1
            printfn "%A" b1

            let v2 = "abc"
            printfn "%A" v2
            let b2 = box "abc"
            printfn "%A" b2

            let stringObj = box "abc"
            printfn "%A" stringObj
            let stringVal1 = unbox stringObj
            printfn "%A" stringVal1
            let stringVal2 = unbox<string> stringObj
            printfn "%A" stringVal2

            // THIS WILL THROW AN EXCEPTION : System.InvalidCastException
            try 
                let valex1 = (unbox stringObj : int)
                valex1 |> printfn "%A"
            with
            | :? System.InvalidCastException as e -> printfn "Invalid Cast"

            // THIS WILL THROW AN EXCEPTION : System.InvalidCastException
            try 
                let valex2 = unbox<int> stringObj
                valex2 |> printfn "%A"
            with
            | :? System.InvalidCastException as e -> printfn "Invalid Cast"
            
        let run () =
            generic_comparison()
            generic_hashing()
            generic_pretty_printing() |> ignore
            generic_boxing_and_unboxing()

    module generic_binary_serialization_via_dotnet_libraries =
         let run () = ()
    
    module execute_modules =

        let run () =
            printfn "[---- Expert F#: START CHAPTER 5 ----]"

            defining_record_types.run()
            handling_non_unique_record_field_names.run()
            defining_discrimiated_unions.run()
            defining_recursive_discrimiated_unions.run()
            using_discrimiated_unions_as_records.run()
            understanding_generics.run()
            writing_generics_functions.run()
            some_important_generic_functions.run()
            generic_binary_serialization_via_dotnet_libraries.run()

            printfn "[---- Expert F#: END CHAPTER 5 ----]"
