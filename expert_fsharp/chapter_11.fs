module chapter_11

    open System

    module events =
        open System.Windows

        //let form = new Form()
        let run() = ()

    module events_as_first_class_values =
        let run() = ()

    module creating_and_publishing_events =
        open System.Timers

        type RandomTicker (approxInterval) =

            let timer = new Timer()
            let rnd = new System.Random(99)
            let tickEvent = new Event<int>()

            let chooseInterval() : int =
                approxInterval + approxInterval / 4 - rnd.Next(approxInterval / 2)

            // Interval expects a float (milliseconds)
            do timer.Interval <- float (chooseInterval())
            do timer.AutoReset <- true

            // Elapsed is an event; attach a handler that triggers your Event<int>
            do timer.Elapsed.AddHandler(
                new ElapsedEventHandler(fun _ _ ->
                let interval = chooseInterval()
                tickEvent.Trigger interval
                timer.Interval <- interval))

            //member x.Tick = tickEvent.Publish
            member x.RandomTick =  tickEvent.Publish
            member x.Start() = timer.Start()
            member x.Stop() = timer.Stop()

            interface IDisposable with
                member x.Dispose() = timer.Dispose()

        let run() =
            // YOU NEED THE CONSOLE TO RUN THIS
            // COPY AND PASTE THE CODE LINE-BY-LINE
            let rt = new RandomTicker(1000)
            rt.RandomTick.Add(fun nextInterval -> printfn "Tick, next = %A" nextInterval)
            rt.Start()
            rt.Stop()

    /// CONTINUE FROM CHAPTER 11 REACTIVE, ASYNCHRONOUS, AND PARALLEL PROGRAMMING 
    /// EXPERT F# 3.0 :PAGE 262 - Using and Designing Background Worker
    /// EXPERT F# 4.0 :PAGE 284 - Events as First-Class Values

    module execute_modules =    
        let run() =
            creating_and_publishing_events.run();;

