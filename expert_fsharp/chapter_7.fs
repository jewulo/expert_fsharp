module chapter_7

    open System

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

        module public VisitorCredentials =

            /// The internal table of permitted visitors and the
            /// days they are allowed to visit.
            let private visitorTable =
                dict [("Anna", set [DayOfWeek.Tuesday; DayOfWeek.Wednesday]);
                                    ("Carolyn", set [DayOfWeek.Friday])]

            /// This is a function to check if a person is a permitted visitor.
            /// Note: this is public and can be used by external code
            let public checkVisitor(person) =
                visitorTable.ContainsKey(person) &&
                visitorTable.[person].Contains(DateTime.Today.DayOfWeek)

            /// This is a function to return all known permited visitors.
            /// Note: this is internal and can be used only by code in this assembly.
            let internal allKnownVisitors() =
                visitorTable.Keys

        let run_visitor_credntials () =
            // error FS1084 (not accessible) VisitorCredentials.visitorTable.Keys |> printfn "%A" 
            VisitorCredentials.checkVisitor("Anna") |>  printfn "%b"
            VisitorCredentials.checkVisitor("James") |>  printfn "%b"
            VisitorCredentials.allKnownVisitors() |> printfn "%A"
        
        module public GlobalClock = 

            type TickTock = Tick | Tock

            let mutable private clock = Tick

            let private tick = new Event<TickTock>()

            let internal oneTick() = 
                (clock <- match clock with Tick -> Tock | Tock -> Tick)
                tick.Trigger (clock)

            let tickEvent = tick.Publish

        module internal TickTockDriver =

            open System.Threading

            let timer = new Timer(callback = (fun _ -> GlobalClock.oneTick()),
                                                state = null, dueTime = 0, period = 100)

        module TickTockListener =
            ()
            //GlobalClock.tickEvent.Add(function
            //    | GlobalClock.Tick -> printfn "tick!"
            //    | GlobalClock.Tock -> printfn "tock!")

        let run_tick_tock () =
            // GlobalClock.tickEvent
            ()
            
        
        module sparse_vector =

            open System.Collections.Generic

            type public SparseVector () =

                let elems = new SortedDictionary<int, float>()

                member internal vec.Add (k, v) = elems.Add(k, v)

                member public vec.Count = elems.Keys.Count
                member vec.Item
                    with public get i =
                        if elems.ContainsKey(i) then elems.[i]
                        else 0.0
                    and internal set i v =
                        elems.[i] <- v

            let run_sparse_vector () =
                let sv = new SparseVector()
                sv.Add(0, 1.0)
                sv.Add(1, 2.0)
                sv.[0] |> printfn "SparseVector[0] = %f"
                sv.[1] |> printfn "SparseVector[1] = %f"
                sv.[0] <- 3.0
                sv.[1] <- 4.0
                sv.[0] |> printfn "SparseVector[0] = %f"
                sv.[1] |> printfn "SparseVector[1] = %f"
                printfn ""

        let run() =
            run_visitor_credntials ()
            // run_tick_tock () // thread runs forever
            sparse_vector.run_sparse_vector ()
    
    // Continue from page 152
    // ORGANISING CODE WITH NAMESPACES AND MODULES
    module organising_code_with_namespaces_and_modules =

        module putting_your_code_in_a_module =
            
            type Vector2D =
                { DX : float; DY : float }

            module Vector2DOps =
                let length v = sqrt(v.DX * v.DX + v.DY * v.DY)
                let scale k v = {DX = k * v.DX; DY = k * v.DY}
                let shiftX x v = {v with DX = v.DX + x}
                let shiftY y v = {v with DY = v.DY + y}
                let shiftXY (x, y) v = {DX = v.DX + x; DY = v.DY + y}
                let zero = {DX = 0.0; DY = 0.0}
                let constX dx = {DX = dx; DY = 0.0}
                let constY dy = {DX = 0.0; DY = dy}

            let run() = 
                let v1 = {DX=0.1; DY=0.3}
                let v2 = {DX=0.1; DY=0.5}
                Vector2DOps.length(v1) |> printfn "length of vector {DX=0.1; DY=0.3} is %f"
                Vector2DOps.length(v2) |> printfn "length of vector {DX=0.1; DY=0.5} is %f"
                Vector2DOps.scale 2.5 v1 |> printfn "scale of vector {DX=0.1; DY=0.3} by 2.5 is %A"
                Vector2DOps.shiftX 2.1 v2 |> printfn "shiftX of vector {DX=0.1; DY=0.5} by 2.1 is %A"
                Vector2DOps.shiftY 2.1 v2 |> printfn "shiftY of vector {DX=0.1; DY=0.5} by 2.1 is %A"
                Vector2DOps.shiftXY (2.0, 3.0) v2 |> printfn "shiftXY of vector {DX=0.1; DY=0.5} by (2.0,3.0) is %A"
                Vector2DOps.zero |> printfn "zero vector is %A"
                Vector2DOps.constX 2.0 |> printfn "constX 2.0 vector is %A"
                Vector2DOps.constY 3.0 |> printfn "constY 3.0 vector is %A"
                
        let run() =
            putting_your_code_in_a_module.run()
                
    module execute_modules =

        let run () =
            printfn "[---- Expert F#: START CHAPTER 7 ----]"

            hiding_things_with_local_definitions.run()
            hiding_things_with_accessibility_annotations.run() |> ignore
            organising_code_with_namespaces_and_modules.run()

            printfn "[---- Expert F#: END CHAPTER 7 ----]"



