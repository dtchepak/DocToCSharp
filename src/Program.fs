open DocToCSharp
open FSharpx

[<EntryPoint>]
let main argv = 
    match List.ofArray argv with
    | namespace'::srcPath::targetPath::[] -> 
        match validatePath srcPath with
        | Some src  -> convert namespace' src (ensurePath targetPath); 0
        | _         -> printf "DocToCSharp.exe: Source directory '%s' does not exist." srcPath; -2
    | _ -> 
        printf "Usage: DocToCSharp.exe <namespace> <srcDirectory> <targetDirectory>"
        -1
