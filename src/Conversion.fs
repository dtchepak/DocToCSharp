[<AutoOpen>]
module DocToCSharp.Conversion

open System
open System.IO
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

type LiquidTag = LiquidTag of name : string * contents : string
    with override x.ToString() = sprintf "%A" x
let makeTag name (contents:string) = LiquidTag (name, contents.Trim())

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

let toFixture = sprintf """
using System;
using NUnit.Framework;
using System.Linq;
using System.Collections.Generic;
using System.ComponentModel;

namespace %s {
    public class Tests_%d {
        %s
    }
}
"""

let appendCodeBlock (code, testNum) (cb:CodeBlock) =
    match cb with
    | Declaration d -> (code + Environment.NewLine + d, testNum)
    | Snippet s -> 
        let test = sprintf "[Test] public void Test_%d() { %s }" testNum s
        (code + Environment.NewLine + test, testNum + 1)

let strToFixture namespace' fixtureNum s : string =
    s
    |> toCodeBlocks
    |> Seq.fold appendCodeBlock ("", 0)
    |> fst
    |> toFixture namespace' fixtureNum
    
let convert namespace' src target =
    let trace format value = printfn format value; value
    filesInDirectoryTree src
        |> Seq.where (extensionIs ["markdown"; "html"])
        |> Seq.iteri (fun num file ->
            let outputFile = combinePaths (getDir target) (Path.GetFileNameWithoutExtension(file)+".cs")
            printfn "   converting %s to %s" file outputFile
            let input = readFileAsString file
            let output = strToFixture namespace' num input
            writeStringToFile false outputFile output
        )
