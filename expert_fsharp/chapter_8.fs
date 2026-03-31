module chapter_8
    
    module building_strings_and_formatting_data =

        let building_strings () =
            let buf = new System.Text.StringBuilder()
            buf.Append("Humpty Dumpty") |> printfn "%A"
            buf.Append(" sat on the wall") |> printfn "%A"
            buf.ToString() |> printfn "%s"

        let more_about_string_literals () =
            let s1 = "Humpty Dumpty"
            let s2 = "c:\\Program Files"
            let s3 = @"c:\Program Files"
            let s4 = """I "like" you"""
            let s5 = "xyZy3d2"B
            let c1 = 'c'

            let s6 = "MAGIC"B

            let s6 = "All the kings horses
            - and all the kings men"

            let s7 = """All the kings' horses
            - and all the kings men"""

            s1 |> printfn "%s"
            s2 |> printfn "%s"
            s3 |> printfn "%s"
            s4 |> printfn "%s"
            s5 |> printfn "%A"
            c1 |> printfn "%c"
            s6 |> printfn "%s"
            s7 |> printfn "%s"
            ()

        let using_printf_and_friends () =
            let s1 = sprintf "Name: %s, Age: %d" "Anna" 3
            s1 |> printfn "%s"

            // let s2 = sprintf "Name: %s, Age: %d" 3 10 // error FS0001: This expression was expected to have type 'string'

            let s3 = System.DateTime.Now.ToString()
            let s4 = sprintf "It is now %O" s3
            s4 |> printfn "%s"
            ()

        let generic_structural_formatting () =
            printf "The result is %A\n" [1; 2; 3]

        let formatting_strings_usning_dotnet_formatting () =
            System.String.Format("{0} {1} {2}", 12, "a", 1.23)

        let run () =
            building_strings()
            more_about_string_literals()
            using_printf_and_friends()
            generic_structural_formatting()
            formatting_strings_usning_dotnet_formatting()

    module parsing_strings_and_textual_data =
        
        open System
        open System.IO

        let parsing_basic_values_1 () =
            DateTime.Parse("13/07/1968") |> printfn "date = %A"

            let date x = DateTime.Parse(x)
            (date "13 July 1968") |> printfn "date %A"
            (date "13 July 1968, 6:21:01pm") |> printfn "birth %A"

        let parsing_basic_values_2 () =
            Uri.TryCreate("http://www.thebritishmuseum.ac.uk/", UriKind.Absolute) |> printfn "%A"
            Uri.TryCreate("http://e3£%.gibberish.ac.uk/", UriKind.Absolute) |> printfn "%A"

        let processing_line_based_input_1 () =
            let line = "Smith, John, 20 January 1986, Software Developer"
            line.Split ',' |> printfn "%A"
            line.Split ',' |> Array.map (fun s -> s.Trim())  |> printfn "%A"
        
        // process each line
        let splitLine (line : string) =
            line.Split [|','|] |> Array.map (fun s -> s.Trim())

        let parseEmployee (line : string) =
            match splitLine line with
            | [|last; first; startDate; title|] ->
                last, first, System.DateTime.Parse(startDate), title
            | _ ->
                failwithf "invalid employee format: '%s'" line
            
        let processing_line_based_input_2 () =
            let line = "Smith, John, 20 January 1986, Software Developer"
            parseEmployee line |> printfn "%A"

        let processing_file_based_input () =
            let create_1000_line_employee_file_from_record () =
                let line = "Smith, John, 20 January 1986, Software Developer"
                File.WriteAllLines("employees.txt", Array.create 10000 line)

            create_1000_line_employee_file_from_record()

            let readEmployees (fileName) =
                fileName |> File.ReadAllLines |> Seq.map parseEmployee

            let firstThree = readEmployees "employees.txt" |> Seq.truncate 3 |> Seq.toList

            firstThree |> Seq.iter (fun (last, first, startDate, title) ->
                                            printfn "%s %s started on %A" first last startDate)

        let run () =
            parsing_basic_values_1()
            parsing_basic_values_2()
            processing_line_based_input_1()
            processing_line_based_input_2()
            processing_file_based_input()

    module using_regular_expressions_to_parse_files =

        open System.Text.RegularExpressions

        let parseHttpRequest line = 
            let result = Regex.Match(line, @"GET (.*?) HTTP/1\.([01])$")
            let file = result.Groups.[1].Value
            let version = result.Groups.[2].Value
            file, version

        let run () =
            let HTMLRequestPart = "GET /favicon.ico HTTP/1.1"
            parseHttpRequest HTMLRequestPart |> printfn "%A"

    module more_on_matching_with_system_text_regular_expr_1 =

        open System.Text.RegularExpressions

        // define a simple function to create a new Regular Expression
        let regex s = new Regex(s)

        // define Perl like operators for Regular Expressions
        let (=~) s (re : Regex) = re.IsMatch(s)
        let (<>~) s (re : Regex) = not (s =~ re)

        // examples uses of regular expressions


        let check_substring () =
            let samplestring = "This is a string"

            // check if samplestring contains "his"
            if samplestring =~ regex "his" then
                printfn "A Match! "

            // check if "This is a string" contains "his"
            "This is a string" =~ regex "(is)+" |> printfn "%A"

        let split_string () =
            // regular expressions can also be used to split string
            regex(" ").Split("This is a string") |> printfn "%A"

            // use " +" to match multiple spaces
            regex(" ").Split("This is a     string") |> printfn "%A"
            regex(" +").Split("This is a     string") |> printfn "%A"

            // use @"\s" to match any unicode whitespace character including end-of-line markers
            regex("\s").Split("I'm a little         teapot") |> printfn "%A"
            regex("\s+").Split("I'm a little         teapot") |> printfn "%A"
            regex(@"\s").Split("I'm a little         teapot") |> printfn "%A"
            regex(@"\s+").Split("I'm a little         teapot") |> printfn "%A"
            regex(@"\s").Split("I'm a little   \t\t\n\t\n\t  teapot") |> printfn "%A"
            regex(@"\s+").Split("I'm a little   \t\t\n\t\n\t  teapot") |> printfn "%A"

        let check_substring_position () =
            let m = regex("joe").Match("maryjoewashere")

            if m.Success then
                m.Index |> printfn "Matched at position %d"

        let replace_text () =
            let text = "was a dark and stormy night"
            let t2 = regex(@"\w+").Replace(text, "WORD")
            t2 |> printfn "%A"

        let replace_text_2 () =
            let text = "was a dark and stormy night"
            let t2 = regex("\w+").Replace(text, "WORD")
            t2 |> printfn "%A"

        let case_sensitive_check_substring () =
            let samplestring = "This is a string"

            // using regex "(?i)" for case sensitivity

            // check if "This is a string" contains "his" or "HIS"
            samplestring =~ regex "(?i)HIS" |> printfn "%A"

            // check if "This is a string" contains "HIS"
            samplestring =~ regex "HIS" |> printfn "%A"

        let run () =
            check_substring()
            split_string()
            check_substring_position()
            replace_text()
            replace_text_2()
            case_sensitive_check_substring()

    module matching_with_named_groups =
        
        open System.Text.RegularExpressions

        // define a simple function to create a new Regular Expression
        let regex s = new Regex(s)

        let entry = @"
        Jolly Jethro
        13 Kings Parade
        Cambridge, Cambs CB2 1TJ
        "

        let re = regex @"(?<=\n)\s*(?<city>[^\n]+)\s*,\s*(?<countr>\w+)\s+(?<pcode>.{3}\s*.{3}).*$"

        // define an active pattern
        let (|IsMatch|_|) (re : string) (inp : string) =
            if Regex(re).IsMatch(inp) then Some() else None

        let use_is_active_pattern (str : string) =
            match str with
            | IsMatch "(?i)HIS" -> "yes, it matched"
            | IsMatch "ABC" -> "this will not match"
            | _ -> "nothing matched"

        let firstAndSecondWord (inp : string) =
            let re = regex "(?<word1>\w+)\s+(?<word2>\w+)"
            let results = re.Match(inp)
            if results.Success then
                Some(results.Groups.["word1"].Value, results.Groups.["word2"].Value)
            else
                None

        let (?) (results : Match) (name : string) =
            results.Groups.[name].Value

        let firstAndSecondWord2 (inp : string) =
            let re = regex "(?<word1>\w+)\s+(?<word2>\w+)"
            let results = re.Match(inp)
            if results.Success then
                Some(results ? word1, results ? word2)
            else
                None

        let run () =
            let r = re.Match entry
            r |> printfn "%A"
            r.Groups.["city"].Value   |> printfn "%A"
            r.Groups.["county"].Value |> printfn "%A"
            r.Groups.["pcode"].Value  |> printfn "%A"

            use_is_active_pattern"This is a string" |> printfn "%s"
            use_is_active_pattern"That is a string" |> printfn "%s"

            firstAndSecondWord "This is a super string"



    module encoding_and_decoding_unicode_strings =

        open System.Xml
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
        
        let run () =
            let doc = new XmlDocument()
            doc.LoadXml(inp)
            
            doc.ChildNodes |> printfn "%A"

            // fsi namespace is only available in the console.
            //fsi.AddPrinter(fun (x:XmlNode) -> x.OuterXml)

            doc.ChildNodes |> printfn "%A"
            doc.ChildNodes.Item(1) |> printfn "%A"
            doc.ChildNodes.Item(1).ChildNodes.Item(0) |> printfn "%A"
            doc.ChildNodes.Item(1).ChildNodes.Item(0).ChildNodes.Item(0) |> printfn "%A"
            doc.ChildNodes.Item(1).ChildNodes.Item(0).ChildNodes.Item(0).Attributes  |> printfn "%A"

    module from_concrete_xml_to_abstract_syntax =

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

        let run () =
            let doc = new XmlDocument()
            doc.LoadXml(inp)
            
            doc.ChildNodes |> printfn "%A"

            // fsi namespace is only available in the console.
            //fsi.AddPrinter(fun (r : RectangleF) ->
            //    sprintf "[%A%A%A%A]" r.Left r.Top r.Width r.Height)

            // this fails in the console
            extractScenes doc

    module some_recursive_descent_parsing =

        type Term =
            | Term of int * string * int
            | Const of int

        type Polynomial = Term list

        let simple_term () =
            // we can therefore represent a simple polynomial
            // x^5 - 2x^3 + 20 as the following list
            let p1 : Polynomial = [Term (1,"x",5); Term (-2,"x",3); Const 20]
            p1 |> printfn "%A"

        let run () =
            simple_term()

    module a_simple_tokenizer =

        open System.Text.RegularExpressions

        // define a simple function to create a new Regular Expression
        let regex s = new Regex(s)

        type Token = 
            | ID of string
            | INT of int
            | HAT
            | PLUS
            | MINUS

        let tokenR = regex @"((?<token>(\d+|\w+|\^|\+|-))\s*)*"

        let tokenize (s : string) =
            [for x in tokenR.Match(s).Groups.["token"].Captures do
                let token =
                    match x.Value with
                    | "^" -> HAT
                    | "-" -> MINUS
                    | "+" -> PLUS
                    | s when System.Char.IsDigit s.[0] -> INT (int s)
                    | s -> ID(s)
                yield token]

        let run () =
            tokenize "x^5 - 2x^3 + 20" |> printfn "%A"
            
    module recursive_descent_parsing =

        open System.Text.RegularExpressions

        // define a simple function to create a new Regular Expression
        let regex s = new Regex(s)

        type Token = 
            | ID of string
            | INT of int
            | HAT
            | PLUS
            | MINUS

        let tokenR = regex @"((?<token>(\d+|\w+|\^|\+|-))\s*)*"

        let tokenize (s : string) =
            [for x in tokenR.Match(s).Groups.["token"].Captures do
                let token =
                    match x.Value with
                    | "^" -> HAT
                    | "-" -> MINUS
                    | "+" -> PLUS
                    | s when System.Char.IsDigit s.[0] -> INT (int s)
                    | s -> ID(s)
                yield token]

        type Term =
            | Term of int * string * int
            | Const of int

        type Polynomial = Term list
        type TokenStream = Token list

        let tryToken (src : TokenStream) =
            match src with
            | tok :: rest -> Some(tok, rest)
            | _ -> None

        let parseIndex src =
            match tryToken src with
            | Some (HAT, src) ->
                match tryToken src with
                | Some (INT num2, src) ->
                    num2, src
                | _ -> failwith "expected an integer after '^'"
            | _ -> 1, src

        let parseTerm src =
            match tryToken src with
            | Some (INT num, src) ->
                match tryToken src with
                | Some (ID id, src) ->
                    let idx, src = parseIndex src
                    Term (num, id, idx), src
                | _ -> Const num, src
            | Some (ID id, src) ->
                let idx, src = parseIndex src
                Term (1, id, idx), src
            | _ -> failwith "end of token stream in term"

        let rec parsePolynomial src =
            let t1, src = parseTerm src
            match tryToken src with
            | Some (PLUS, src) ->
                let p2, src = parsePolynomial src
                (t1 :: p2), src
            | _-> [t1], src

        let parse input =
            let src = tokenize input
            let result, src = parsePolynomial src
            match tryToken src with
            | Some _ -> failwith "unexpected input at the end of token stream!"
            | None -> result
        
        let run () =
            parse "1 + 3" |> printfn "%A"
            parse "2x^2 + 3x + 5" |> printfn "%A"

            // while testing I noticed some crashes, the code does not handle MINUS
            //parse "x^5 - 2x^3 + 20" |> printfn "%A"

    module binary_parsing_and_formatting =

        type OutState = System.IO.BinaryWriter
        type InState = System.IO.BinaryReader

        type Pickler<'T> = 'T -> OutState -> unit
        type Unpickler<'T> = InState -> 'T

        // P is the suffix for pickling and U is the suffix for unpickling
        let byteP (b : byte) (st : OutState) = st.Write(b)
        let byteU (st : InState) = st.ReadByte()

        // Additional pickler/unppickler pairs:
        let boolP b st = byteP (if b then 1uy else 0uy) st
        let boolU st = let b = byteU st in (b = 1uy)

        let int32P i st =
            byteP (byte (i &&& 0xFF)) st
            byteP (byte ((i >>> 8) &&& 0xFF)) st
            byteP (byte ((i >>> 16) &&& 0xFF)) st
            byteP (byte ((i >>> 24) &&& 0xFF)) st

        let int32U st =
            let b0 = int (byteU st)
            let b1 = int (byteU st)
            let b2 = int (byteU st)
            let b3 = int (byteU st)
            b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24)

        let tup2P p1 p2 (a, b) (st : OutState) =
            (p1 a st : unit)
            (p2 b st : unit)

        let tup3P p1 p2 p3 (a, b, c) (st : OutState) =
            (p1 a st : unit)
            (p2 b st : unit)
            (p2 c st : unit)

        let tup2U p1 p2 (st : InState) =
            let a = p1 st
            let b = p2 st
            (a, b)

        let tup3U p1 p2 p3 (st : InState) =
            let a = p1 st
            let b = p2 st
            let c = p3 st
            (a, b, c)

        /// Outputs a list into the given output stream by pickling each element via f.
        /// A zero indicates the end of the lits, a 1 indicates another element of a list.
        let rec listP f lst st =
            match lst with
            | [] -> byteP 0uy st
            | h :: t -> byteP 1uy st; f h st; listP f t st

        /// Reads a list from a given input stream by unpickling each element via f.
        let rec listU f st =
            let rec loop acc =
                let tag = byteU st
                match tag with
                | 0uy -> List.rev acc
                | 1uy -> let a = f st in loop (a :: acc)
                | n -> failwithf "listU: found number % d" n
            loop []

        type format = list<int32 * bool>

        let formatP = listP (tup2P int32P boolP)
        let formatU = listU (tup2U int32U boolU)

        open System.IO

        let writeData file data =
            use outStream = new BinaryWriter(File.OpenWrite(file))
            formatP data outStream

        let readData file =
            use inStream = new BinaryReader(File.OpenRead(file))
            formatU inStream

        let run () = 
            writeData "out.bin" [(102, true); (108, false)]
            readData "out.bin" |> printfn "%A"

    module execute_modules =

        let run () =
            building_strings_and_formatting_data.run() |> ignore
            parsing_strings_and_textual_data.run()
            using_regular_expressions_to_parse_files.run()
            more_on_matching_with_system_text_regular_expr_1.run()
            matching_with_named_groups.run()  |> ignore
            encoding_and_decoding_unicode_strings.run()
            some_recursive_descent_parsing.run()
            a_simple_tokenizer.run()
            recursive_descent_parsing.run()
            binary_parsing_and_formatting.run()
