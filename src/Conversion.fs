[<AutoOpen>]
module DocToCSharp.Conversion

open System
open FSharp.RegexProvider
open FSharpx
open FSharpx.Strings
open FSharpx.IO
open DocToCSharp

type LiquidTagRegex = Regex< @"\{%\s*(?<tag>\w+).*\%}(?<contents>(?s:.*?))\{%\s*end\1\s*%\}" >
type TypeDeclRegex = Regex< @"(public |private |protected )?(class |interface )\w+\s*\{" >

type CodeBlock =
    /// Code block that declares a member.
    | Declaration of string
    /// Snippet of runnable code that must be placed inside a method
    | Snippet of string
    with override x.ToString() = sprintf "%A" x

let trimStr (s : string) = s.Trim()

type LiquidTag = LiquidTag of name : string * contents : string
    with override x.ToString() = sprintf "%A" x
let makeTag name contents = LiquidTag (name, trimStr contents)

let toCodeBlock (LiquidTag (name, c)) =
    let isTypeDecl s = TypeDeclRegex().IsMatch(s, 0)
    match name with
    | "examplecode"  -> if isTypeDecl c then Some (Declaration c) else Some (Snippet c)
    | "requiredcode" -> Some (Declaration c)
    | _              -> None

let tags s : LiquidTag seq =
    LiquidTagRegex().Matches(s)
    |> Seq.map (fun m -> makeTag (m.tag.Value) (m.contents.Value))

let toCodeBlocks s : CodeBlock seq =
    tags s
    |> Seq.choose toCodeBlock

let convert src target =
    let trace format value = printfn format value; value
    filesInDirectoryTree src
        |> Seq.where (extensionIs ["markdown"; "html"])
        |> Seq.map (toCodeBlocks << readFileAsString << trace "   converting %s")
        |> Seq.toArray
        |> trace "   %A"
        |> ignore
    ()

