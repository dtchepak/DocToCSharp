[<AutoOpen>]
module DocToCSharp.IO

open System.IO
open System.Linq

let any (s : _ seq) = s.Any()
    
let extensionIs extensions file =
    let ext = Path.GetExtension file
    extensions
    |> Seq.where (fun x -> ext = "." + x)
    |> any

type ValidDirectory = private Valid of string
    with override x.ToString() = match x with (Valid dir) -> dir

let validatePath path : ValidDirectory option = 
    if Directory.Exists path then Valid path |> Some else None

let ensurePath path : ValidDirectory = 
    Directory.CreateDirectory path |> ignore
    Valid path

let getDir (Valid d) = d

/// Enumerates all files in this directory and its sub-directories.
let filesInDirectoryTree (Valid dir) =
    Directory.EnumerateFiles(dir, "*", SearchOption.AllDirectories)