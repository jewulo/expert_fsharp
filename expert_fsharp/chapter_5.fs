module chapter_5

open System

    /// Collection of code used in most examples in Chapter 3
    module http_stuff =

        open System.Net

        let delimiters  = [| ' '; '\n'; '\t'; '<'; '>'; '='|]

        let getWords (s: string) = s.Split delimiters

        /// Get the contents of the URL via a web request using WebClient (synchronous)
        let http (url: string) =
            use client = new WebClient()
            client.DownloadString(url)

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

        open System.IO
        open System.Runtime.Serialization.Formatters.Binary

        let writeValue outputStream (x : 'T) =
            let formatter = new BinaryFormatter()
            formatter.Serialize(outputStream, box x)

        let readValue inputStream =
            let formatter = new BinaryFormatter()
            let res = formatter.Deserialize(inputStream)
            unbox res

        let run () =
            let addresses =
                Map.ofList ["Jeff", "123 Main Street, Redmond, WA 98052";
                                     "Fred", "987 Pine Road, Phila., PA 19116";
                                     "Mary", "PO Box 112233, Palo Alto, CA 94301"]

            let fsOut = new FileStream("Data.dat", FileMode.Create)
            writeValue fsOut addresses
            fsOut.Close()
            
            let fsIn = new FileStream("Data.dat", FileMode.Open)
            let res = readValue fsIn
            res |> printfn "%A"
            fsIn.Close() 

    module generic_algorithms_through_explicit_arguments =

        let rec hcf a b =
            if a = 0 then b
            elif a < b then hcf a (b - a)
            else hcf (a - b) b

        // a more generic version of hcf
        let hcfGeneric (zero, sub, lessThan) =
            let rec hcf a b =
                if a = zero then b
                elif lessThan a b then hcf a (sub b a)
                else hcf (sub a b) b
            hcf

        let hcfInt = hcfGeneric (0, (-), (<))
        let hcfInt64 = hcfGeneric (0L, (-), (<))
        let hcfBigInt = hcfGeneric (0I, (-), (<))

        let run () =
            hcf 18 12 |> printfn "hcf (18 12) = %d"
            hcfInt 18 12 |> printfn "hcfInt (18 12) = %d"
            hcfInt 36 24 |> printfn "hcfInt (36 24) = %d"
            hcfInt64 255486129 18612930 |> printfn "hcfInt64 (255486129 18612930) = %d"
            hcfBigInt 1810287116162232383039576I 1239028178293092830480239032I |> printfn "hcfBigInt (18102871161223283039576I 1239028178293092830480239032I) = %O"

    module generic_algorithms_through_function_parameters =

        module via_record_types =

            // define the generic record type
            type Numeric<'T> =
                {   Zero : 'T;
                    Subtract : ('T -> 'T -> 'T);
                    LessThan : ('T -> 'T -> bool);  }

            // create record values
            let intOps = {Zero = 0; Subtract = (-); LessThan = (<)}
            let bigintOps = {Zero = 0I; Subtract = (-); LessThan = (<)}
            let int64Ops = {Zero = 0L; Subtract = (-); LessThan = (<)}

            // generic function via record values
            let hcfGeneric (ops : Numeric<'T>) =
                let rec hcf a b =
                    if a = ops.Zero then b
                    elif ops.LessThan a b then hcf a (ops.Subtract b a)
                    else hcf (ops.Subtract a b) b
                hcf

            // define the specific function via record values
            let hcfInt = hcfGeneric intOps
            let hcfInt64 = hcfGeneric int64Ops
            let hcfBigInt = hcfGeneric bigintOps

            let run () =
                printfn "[ Via Record Types: ]"
                hcfInt 18 12 |> printfn "hcfInt (18 12) = %d"
                hcfInt 36 24 |> printfn "hcfInt (36 24) = %d"
                hcfInt64 255486129 18612930 |> printfn "hcfInt64 (255486129 18612930) = %d"
                hcfBigInt 1810287116162232383039576I 1239028178293092830480239032I |> printfn "hcfBigInt (18102871161223283039576I 1239028178293092830480239032I) = %O"

        module via_object_interface_types =

            // define the generic interface type
            type INumeric<'T> =
                abstract Zero : 'T
                abstract Subtract : 'T * 'T -> 'T;
                abstract LessThan : 'T * 'T -> bool;

            // define the specific inteface types
            let intOps =
                {   new INumeric<int> with 
                        member ops.Zero = 0
                        member ops.Subtract(x, y) = x - y
                        member ops.LessThan(x, y) = x < y   }

            let bigintOps =
                {   new INumeric<bigint> with
                        member ops.Zero = 0I
                        member ops.Subtract(x, y) = x - y
                        member ops.LessThan(x, y) = x < y   }

            let int64Ops =
                {   new INumeric<int64> with
                        member ops.Zero = 0L
                        member ops.Subtract(x, y) = x - y
                        member ops.LessThan(x, y) = x < y   }

            // generic function via interface type
            let hcfGeneric (ops : INumeric<'T>) =
                let rec hcf a b =
                    if a = ops.Zero then b
                    elif ops.LessThan(a, b) then hcf a (ops.Subtract(b, a))
                    else hcf (ops.Subtract(a, b)) b
                hcf

            // specific function via interface type
            let hcfInt = hcfGeneric intOps
            let hcfInt64 = hcfGeneric int64Ops
            let hcfBigInt = hcfGeneric bigintOps

            let run () =
                printfn "[ Via Object Interface Types: ]"
                hcfInt 18 12 |> printfn "hcfInt (18 12) = %d"
                hcfInt 36 24 |> printfn "hcfInt (36 24) = %d"
                hcfInt64 255486129 18612930 |> printfn "hcfInt64 (255486129 18612930) = %d"
                hcfBigInt 1810287116162232383039576I 1239028178293092830480239032I |> printfn "hcfBigInt (18102871161223283039576I 1239028178293092830480239032I) = %O"

        let run () =
            via_record_types.run()
            via_object_interface_types.run()

    module generic_algorithms_through_inlining =

        let convertToFloat x = float x

        // adding inline makes the code more generic
        let inline convertToFloatAndAdd x y = float x + float y

        // define the generic interface type
        type INumeric<'T> =
            abstract Zero : 'T
            abstract Subtract : 'T * 'T -> 'T;
            abstract LessThan : 'T * 'T -> bool;

        // define the specific inteface types
        let intOps =
            {   new INumeric<int> with 
                    member ops.Zero = 0
                    member ops.Subtract(x, y) = x - y
                    member ops.LessThan(x, y) = x < y   }

        let bigintOps =
            {   new INumeric<bigint> with 
                    member ops.Zero = 0I
                    member ops.Subtract(x, y) = x - y
                    member ops.LessThan(x, y) = x < y   }

        let int64Ops =
            {   new INumeric<int64> with 
                    member ops.Zero = 0L
                    member ops.Subtract(x, y) = x - y
                    member ops.LessThan(x, y) = x < y   }

        // generic function via interface type
        let hcfGeneric (ops : INumeric<'T>) =
            let rec hcf a b =
                if a = ops.Zero then b
                elif ops.LessThan(a, b) then hcf a (ops.Subtract(b, a))
                else hcf (ops.Subtract(a, b)) b
            hcf

        // redefine hcf as inline of hcfGeneric
        let inline hcf a b =
            hcfGeneric
                {   new INumeric<'T> with
                            member ops.Zero = LanguagePrimitives.GenericZero<'T>
                            member ops.Subtract (x: 'T, y: 'T): 'T = x - y
                            member ops.LessThan (x: 'T, y: 'T): bool = x < y    }
                     a b

        let run () =
            printfn "[ Via Generic Type via Inlining: ]"
            hcf 18 12 |> printfn "hcf (18 12) = %d"
            hcf 36 24 |> printfn "hcf (36 24) = %d"
            hcf 255486129 18612930 |> printfn "hcf (255486129 18612930) = %d"
            hcf 1810287116162232383039576I 1239028178293092830480239032I |> printfn "hcfBigInt (18102871161223283039576I 1239028178293092830480239032I) = %O"

    module understanding_subtyping =

        let casting_up_statically () = 
            let xint = 1    // an inter value type
            let xobj = (1 :> obj)   // upcast an int value to an obj type
            
            xint.GetType() |> printfn "xint.GetType() = %A"
            xobj.GetType() |> printfn "xobj.GetType() = %A"

            let sstr = "abc"   // a string value type
            let sobj = ("abc" :> obj)   // upcast a string value to an obj type
            sstr.GetType() |> printfn "sstr.GetType() = %A"
            sobj.GetType() |> printfn "sobj.GetType() = %A"

        let casting_down_dynamically () =
            let boxedObject = box "abc"
            let downcastString = (boxedObject :?> string)
            boxedObject.GetType() |> printfn "boxedObject.GetType() = %A"
            downcastString.GetType() |> printfn "downcastString.GetType() = %A"

            // an exception is thrown if the object is unsuitable to downcast to
            let xobj = box 1
            xobj.GetType() |> printfn "xobj.GetType() = %A"

            // System.InvalidCastException
            // HResult=0x80004002
            // Message=Unable to cast object of type 'System.Int32' to type 'System.String'.
            // Source=expert_fsharp
            //let xstr = (xobj :?> string)
            //xstr.GetType() |> printfn "xstr.GetType() = %A"

        let run () =
            casting_up_statically()
            casting_down_dynamically()
            

    module performing_type_tests_via_pattern_matching =

        let checkObject (x : obj) =
            match x with
            | :? string -> printfn "The object is a string"
            | :? int -> printfn "The object is an integer"
            | _ -> printfn "The input is something else"

        let reportObject (x : obj) =
            match x with
            | :? string as s -> printfn "The input is the string '%s'" s
            | :? int as d -> printfn "The input is the integer '%d'" d
            | _ -> printfn "The input is something else"

        let run () =
            checkObject (box "abc") |> printfn "checkObject (box \"abc\") = %A"
            checkObject (box 17) |> printfn "checkObject (box 17) = %A"
            reportObject (box "abc") |> printfn "reportObject (box \"abc\") = %A"
            reportObject (box 17) |> printfn "reportObject (box 17) = %A"

    module knowing_when_upcasts_are_applied_automatically =
        // Make Windows Forms usage optional to avoid FS0039 when Forms is not referenced.
        open System
        open System.IO

        #if USE_WINDOWS_FORMS
        open System.Windows.Forms

        let setTextOfControl (c : Control) (s : string) = c.Text <- s

        let run_forms () =
            let form = new Form()
            let textBox = new TextBox()
            setTextOfControl form "Form Text"
            setTextOfControl textBox "Text Box Text"
        #else

        // Fallback stubs when System.Windows.Forms is unavailable.
        // These keep the module compiling without requiring the Forms assembly.
        let setTextOfControl (_: obj) (_: string) = ()

        let run_forms () = ()
        #endif

        // Note file input.txt does not exist so there is an exception
        let run_text_reader () =
            let textReader1 =
                if DateTime.Today.DayOfWeek = DayOfWeek.Monday
                then Console.In
                else File.OpenText("input.txt")
  
            let textReader2 =
                if DateTime.Today.DayOfWeek = DayOfWeek.Monday
                then Console.In
                else (File.OpenText("input.txt") :> TextReader)

            (textReader1, textReader2)

        // Coercsing a StreamReader to as TextReader        
        let getTextReader () : TextReader =
            (File.OpenText("input.txt") :> TextReader)

        let run () =
            run_forms()
            run_text_reader()

    module flexible_types =
        //open System.Windows.Forms

        //let setTextOfControl (c: 'T when 'T :> Control) (s : string) = c.Text <- s

        let s1 = Seq.concat [[1; 2; 3]; [4; 5; 6]]
        let s2 = Seq.concat [[|1; 2; 3|]; [|4; 5; 6|]]

        let run () =
            s1 |> printfn "%A"
            s2 |> printfn "%A"

    module using_type_annotations =

        // this generates an error FS0072
        //let getLengths inp =
        //    inp |> Seq.map (fun y -> y.Length)

        // this solves the error FS0072 by explicitly specfing the type of y
        let getLengths inp =
            inp |> Seq.map (fun (y : string) -> y.Length)

        // warning FS0064: This construct causes code to be less generic than
        // indicated by the type annotations.
        // The type variable 'T has been constrained to be type 'int'.
        let printSecondElements1 (inp : seq<'T * int>) =
            inp
            |> Seq.iter (fun (x, y) -> printf "y = %d" x)

        //  error FS0001: The type 'PingPong' is not compatible with any of the types byte,int16,int32,int64,sbyte,uint16,uint32,uint64,nativeint,unativeint, arising from the use of a printf-style format string
        //type PingPong = Ping | Pong
        //let printSecondElements2 (inp : seq<PingPong * int>) =
        //    inp
        //    |> Seq.iter (fun (x, y) -> printf "y = %d" x)

        let run () =
            let s1 = Seq.concat [["one"; "two"; "three"]; ["four"; "five"; "six"]]
            let s2 = Seq.concat [[|"one"; "two"; "three"|]; [|"four"; "five"; "six"|]]

            getLengths s1 |> printfn "%A"
            getLengths s2 |> printfn "%A"

    module understanding_value_restrictions =

        let run () =
            // error FS0030: Value restriction: The value 'empties' has an inferred generic type
            // val empties: '_a list array. However, values cannot have generic type variables
            // like '_a in "let x: '_a".
            // let empties = Array.create 100 []

            let emptyList = []
            let initialLists = ([], [2])
            let listOfEmptyLists = [[]; []]
            let makeArray () = Array.create 100 []
            ()

    module working_around_value_restriction =

        // Technique 1 : Constrain Values to Be Nongeneric
        let technique_1 () =
            // error FS0030:
            // let empties = Array.create 100 []

            // best to use explicit type specifications
            let empties: int list [] = Array.create 100 []
            ()

        // Technique 2 : Ensure Generic Functions Have Explicit Arguments
        let technique_2 () =
            let mapFirst1 = List.map fst
            let mapFirst2 inp = List.map fst inp
            let mapFirst3 inp = inp |> List.map (fun (x, y) -> x)

            let printFstElements1 = List.map fst >> List.iter (printf "res = %d")
            let printFstElements2 inp = inp |> List.map fst |> List.iter (printf "res = %d")
            ()

        // Technique 3 : Add Dummy Arguments to Generic Functions When Necessary
        let technique_3 () =
            // let empties = Array.create 100 []

            // use a dummy argument
            let empties () = Array.create 100 []
            let intEmpties : int list [] = empties()
            let stringEmpties : string list [] = empties()
            ()

        // Technique 4 : Add Explicit Type Arguments When Necessary
        //let emptyLists = Seq.init 100 (fun _ -> [])
        let emptyLists<'T> : seq<'T list> = Seq.init 100 (fun _ -> [])

        let technique_4 () =
            Seq.length emptyLists |> printfn"%d"
            emptyLists |> printfn"%A"
            ()

        let run () =
            technique_1()
            technique_2()
            technique_3()
            technique_4()

    module understanding_generic_overloaded_operators =

        // this is a constrained function despite no expliocit type specifications
        let twice x = (x + x)
        let twiceFloat (x: float) = (x + x)
        let threeTimes x = (x + x + x)
        let sizTimesInt64 (x:Int64) = threeTimes x + threeTimes x

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
            generic_algorithms_through_explicit_arguments.run()
            generic_algorithms_through_function_parameters.run()
            generic_algorithms_through_inlining.run()
            understanding_subtyping.run()
            performing_type_tests_via_pattern_matching.run()
            //knowing_when_upcasts_are_applied_automatically.run() |> ignore
            flexible_types.run()
            using_type_annotations.run()

            understanding_value_restrictions.run()
            working_around_value_restriction.run()
            understanding_generic_overloaded_operators.run()

            printfn "[---- Expert F#: END CHAPTER 5 ----]"
