module DocToCSharp.Conversion

open System.IO
open System.Linq
open FSharpx
open FSharpx.IO

let any (s : _ seq) = s.Any()
    
let extensionIs extensions file =
    let ext = Path.GetExtension file
    extensions
    |> Seq.map (fun x -> x = ext)
    |> any

type ValidDirectory = private Valid of string

let validatePath path : ValidDirectory option = 
    if Directory.Exists path then Valid path |> Some else None

let ensurePath path : ValidDirectory = 
    Directory.CreateDirectory path |> ignore
    Valid path

type CodeBlock 
    = Declaration of string
    | TestSnippet of string

let toCodeBlocks s : CodeBlock seq=
    Seq.empty

let trace format value = printfn format value; value
let convert (Valid srcDir) (Valid targetDir) =
    Directory.EnumerateFiles(srcDir, "*", SearchOption.AllDirectories)
        |> Seq.where (extensionIs ["markdown"; "html"])
        |> Seq.map (toCodeBlocks << readFileAsString << trace "   converting %s")
        |> Seq.toArray
        |> ignore
    ()

