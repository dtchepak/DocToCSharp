[<AutoOpen>]
module DocToCSharp.Conversion

open System
open System.Text.RegularExpressions
open FSharpx
open FSharpx.Strings
open FSharpx.IO
open FParsec
open DocToCSharp

let private maxInt = System.Int32.MaxValue

type CodeBlock =
    /// Code block that declares a member.
    | Declaration of string
    /// Snippet of runnable code that must be placed inside a method
    | Snippet of string
    with override this.ToString() = sprintf "%A" this

let trimStr (s : string) = s.Trim()

let betweenStr s t = between (pstring s) (pstring t)
let allTill s = charsTillString s false maxInt
let skipAllTill s = skipCharsTillString s false maxInt
let word : Parser<string, unit> = many1Satisfy (not << Char.IsWhiteSpace)

type LiquidTag = private LiquidTag of name : string * contents : string
let makeTag n c = LiquidTag (n, trimStr c)

let liquidTag =
    let pStartTag = betweenStr "{%" "%}" (spaces >>. word .>> spaces .>> skipAllTill "%}")
    let pEndTag tagName = betweenStr "{%" "%}" (spaces >>. pstring ("end" + tagName) .>> spaces)
    let tagContents = allTill "{%"
    pStartTag >>= fun name -> 
                    tagContents 
                        .>> pEndTag name 
                        |>> makeTag name

let tags = many (skipAllTill "{%" >>. liquidTag)

let toCodeBlock (LiquidTag (name, c)) =
    let isTypeDecl s = Regex.IsMatch(s, "(public |private |protected )?(class |interface )\w+\s*\{")
    match name with
    | "examplecode"  -> if isTypeDecl c then Some (Declaration c) else Some (Snippet c)
    | "requiredcode" -> Some (Declaration c)
    | _              -> None

let codeBlocks : Parser<CodeBlock list, Unit> =
    tags |>> List.choose toCodeBlock

let runp (p : Parser<'a, _>) s : Choice<'a, string> =
    match run p s with
    | Success (result, _, _) -> Choice1Of2 result
    | Failure (msg, err, _)  -> Choice2Of2 (sprintf "%A: %A" err msg)

let toCodeBlocks s : CodeBlock seq =
    runp codeBlocks s
    |> Choice.choice 
        (fun result -> printfn "   --- win!"; Seq.ofList result)
        (fun failed -> printfn "   !!! %s" failed; Seq.empty)

let convert src target =
    let trace format value = printfn format value; value
    filesInDirectoryTree src
        |> Seq.where (extensionIs ["markdown"; "html"])
        |> Seq.map (toCodeBlocks << readFileAsString << trace "   converting %s")
        |> Seq.toArray
        |> trace "   %A"
        |> ignore
    ()

