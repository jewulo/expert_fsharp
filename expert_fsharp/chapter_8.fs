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

        /// CONTINUE FROM CHAPTER 8 PAGE 165
        /// USING PRINTF AND FRIENDS.
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


    /// CONTINUE FROM BOTTOM OF PAGE 174.

    // let firstAndSercondWord (inp : string) =

        let run () =
            let r = re.Match(entry)
            r |> printfn "%A"
            r.Groups.["city"].Value   |> printfn "%A"
            r.Groups.["county"].Value |> printfn "%A"
            r.Groups.["pcode"].Value  |> printfn "%A"

            use_is_active_pattern("This is a string") |> printfn "%s"
            use_is_active_pattern("That is a string") |> printfn "%s"






    module execute_modules =
        let run () =
            building_strings_and_formatting_data.run()
            parsing_strings_and_textual_data.run()
            using_regular_expressions_to_parse_files.run()
            more_on_matching_with_system_text_regular_expr_1.run()
            matching_with_named_groups.run()
