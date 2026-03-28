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
            ()

        let run () =
            building_strings()
            more_about_string_literals()

    module execute_modules =
        let run () =
            building_strings_and_formatting_data.run()

