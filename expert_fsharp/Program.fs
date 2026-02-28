
open System

///
/// https://stackoverflow.com/questions/15308268/how-to-pause-the-console-in-f-language
///
// When running in debug mode and using Visual Studio to run the program,  
// one may miss the results as the program runs to the end and exists.  
// Since running normally, i.e. Visual Studio Ctrl-F5, will add an pause
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
    chapter_5.execute_modules.run()

    System.Console.ReadKey(true) |> ignore  
    pause()  
    0 // exit codeopen System
