open DocToCSharp.Conversion
open FSharpx

[<EntryPoint>]
let main argv = 
    match List.ofArray argv with
    | srcPath::targetPath::[] -> 
        match validatePath srcPath with
        | Some src  -> convert src (ensurePath targetPath); 0
        | _         -> printf "DocToCSharp.exe: Source directory '%s' does not exist." srcPath; -2
    | _ -> 
        printf "Usage: DocToCSharp.exe <srcDirectory> <targetDirectory>"
        -1
