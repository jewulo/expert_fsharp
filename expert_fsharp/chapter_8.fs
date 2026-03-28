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

    /// CONTINUE FROM PAGE 170.
    /// USING REGULAR EXPRESSIONS TO PARSE LINES.
    module using_regular_exprtessions_to_parse_files =

        open System.Text.RegularExpressions


        let run () = ()

    module execute_modules =
        let run () =
            building_strings_and_formatting_data.run()
            parsing_strings_and_textual_data.run()
            using_regular_exprtessions_to_parse_files.run()

