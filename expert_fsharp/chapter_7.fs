module chapter_7

    module hiding_things_with_local_definitions =

        let generateTicket = 
            let count = ref 0
            (fun () -> incr count; !count)

        let run_generate_ticket () =
            generateTicket() |> printfn "generateTicket() = %d"
            generateTicket() |> printfn "generateTicket() = %d"
            generateTicket() |> printfn "generateTicket() = %d"
            generateTicket() |> printfn "generateTicket() = %d"

        type IPeekPoke =
            abstract member Peek : unit -> int
            abstract member Poke : int -> unit

        let makeCounter initialState =
            let state = ref initialState
            { new IPeekPoke with
                member x.Poke n = state := !state + n
                member x.Peek () = !state }

        let run_make_counter () =
            let mc = makeCounter 0
            mc.Peek() |> printfn "makeCounter = %d"
            mc.Peek() |> printfn "makeCounter = %d"
            mc.Poke 5
            mc.Peek() |> printfn "makeCounter = %d"
            mc.Peek() |> printfn "makeCounter = %d"

        type TicketGenerator () =

            // Note: let bindings in a type definition are implicitly private
            // to the object being constructed. Members are implicitly public.

            let mutable count = 0

            member x.Next() =
                count <- count + 1
                count

            member x.Reset() =
                count <- 0

        let run_ticket_generator () =
            let tg = new TicketGenerator()
            tg.Next() |> printfn "TicketGenerator::Next = %d"
            tg.Next() |> printfn "TicketGenerator::Next = %d"
            tg.Next() |> printfn "TicketGenerator::Next = %d"
            printfn ("TicketGenerator::Reset")
            tg.Reset()                        
            tg.Next() |> printfn "TicketGenerator::Next = %d"


        type IStatistics<'T, 'U> =
            abstract Record : 'T -> unit
            abstract Value : 'U

        let makeAverager(toFloat : 'T -> float) =
            let count = ref 0
            let total = ref 0.0
            { new IStatistics<'T, float> with
                member stat.Record(x) = incr count; total := !total + toFloat x
                member stat.Value = (!total / float !count) }

        let run_make_averager () =
            let istats = makeAverager(fun a -> sqrt a)
            istats.Record(1.0)
            istats.Record(2.0)
            istats.Record(3.0)
            istats.Record(4.0)
            istats.Record(5.0)
            istats.Value |> printfn "makeAverager %f"

        let run () =
            run_generate_ticket()
            run_make_counter()
            run_ticket_generator()
            run_make_averager()

    module hiding_things_with_accessibility_annotations =
        
        let run() = ()
    
    // Continue from page 150
    // HIDING THINGS WITH ACCESSIBILITY ANNOTATIONS
    module execute_modules =

        let run () =
            printfn "[---- Expert F#: START CHAPTER 7 ----]"

            hiding_things_with_local_definitions.run()

            printfn "[---- Expert F#: END CHAPTER 7 ----]"



