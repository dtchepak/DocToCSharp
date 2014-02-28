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
type TypeOrTestDeclRegex = Regex< @"(\[Test\]|(public |private |protected )?(class |interface )\w+\s*\{)" >

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
    let isTypeOrTestDecl s = TypeOrTestDeclRegex().IsMatch(s, 0)
    match name with
    | "examplecode"  -> if isTypeOrTestDecl c then Some (Declaration c) else Some (Snippet c)
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

let toTest = sprintf """[Test] public void Test_%d() {
%s
}"""

let appendCodeBlock (code, testNum) (cb:CodeBlock) =
    match cb with
    | Declaration d -> (code + Environment.NewLine + d, testNum)
    | Snippet s -> 
        let test = toTest testNum s
        (code + Environment.NewLine + test, testNum + 1)

let strToFixture namespace' fixtureNum s : string =
    s
    |> toCodeBlocks
    |> Seq.fold appendCodeBlock ("", 0)
    |> fst
    |> toFixture namespace' fixtureNum

let csProj =
    sprintf """<Project ToolsVersion="4.0" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
  <Import Project="$(MSBuildToolsPath)\Microsoft.CSharp.targets" Condition="'$(Configuration)|$(Platform)' != 'AllBuild|AnyCPU' "/>
  <ItemGroup>
    <Reference Include="NSubstitute.dll" />
    <Reference Include="nunit.framework.dll" />
    <CSFile Include="*.cs" />
  </ItemGroup>
  <Target Name="Build">
    <Csc 
        Sources="@(CSFile)"
        References="@(Reference)"
        OutputAssembly="%s.dll"
        TargetType="library"
    />  
  </Target>
</Project>"""
    
let convert namespace' src target =
    let fileInTargetDir = combinePaths (getDir target)
    let csProjFile = fileInTargetDir (namespace'+".csproj")
    printfn "Converting docs in %A to %s." src csProjFile
    filesInDirectoryTree src
        |> Seq.where (extensionIs ["markdown"; "html"])
        |> Seq.iteri (fun num file ->
            let outputFile = fileInTargetDir (Path.GetFileNameWithoutExtension(file)+".cs")
            printfn "   converting %s to %s" file outputFile
            let input = readFileAsString file
            let output = strToFixture namespace' num input
            writeStringToFile false outputFile output
        )
    writeStringToFile false csProjFile (csProj namespace')
    printfn "================================================"
    printfn "Done. Compile with: msbuild.exe %s /p:TargetFrameworkVersion=v3.5" csProjFile
    printfn "NSubstitute.dll and nunit.framework.dll will need to be in %A for the project to compile." target

