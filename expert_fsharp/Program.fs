open System

///
/// https://stackoverflow.com/questions/15308268/how-to-pause
///
// When running in debug mode and using Visual Studio to run 
// one may miss the results as the program runs to the end an
// Since running normally, i.e. Visual Studio Ctrl-F5, will a
// automatically the pause is only shown when in debug mode. 
let pause () =  
    match System.Diagnostics.Debugger.IsAttached with  
    | true ->  
        printfn "\nPress any key to continue."  
        System.Console.ReadKey(true) |> ignore  
    | false -> ()  


[<EntryPoint>]
let main argv =
    printfn "Expert F#: Hello from main. Args: %A" argv
    
    //chapter_2.execute_modules.run()
    //chapter_3.execute_modules.run()
    //chapter_4.execute_modules.run()
    //chapter_5.execute_modules.run()
    //chapter_6.execute_modules.run()
    //chapter_7.execute_modules.run()
    chapter_8.execute_modules.run()
    chapter_9.execute_modules.run()

    System.Console.ReadKey(true) |> ignore  
    pause()  
    0 // exit codeopen System
