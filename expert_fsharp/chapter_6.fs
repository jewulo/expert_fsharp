module chapter_6

    module getting_started_with_objects =

        let run () = ()

    module execute_modules =

        let run () =
            printfn "[---- Expert F#: START CHAPTER 6 ----]"

            getting_started_with_objects.run()

            printfn "[---- Expert F#: END CHAPTER 6 ----]"

