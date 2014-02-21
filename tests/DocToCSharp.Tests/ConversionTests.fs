module DocToCSharp.Tests.ConversionTests

open FSharpx.Choice
open FsUnit.Xunit
open FParsec
open Xunit

open DocToCSharp.Conversion

let isLeftWith (expected:'a) (actual:Choice<'a,'b>) =
    match actual with
    | Choice1Of2 result -> result |> should equal expected
    | Choice2Of2 result -> failwithf "Expected: Choice1Of2 %A\n\nActual: Choice2Of2 %A" expected result

let isRight (actual:Choice<_,_>) =
    match actual with
    | Choice1Of2 result -> failwithf "Expected Choice2Of2, got Choice1Of2"
    | Choice2Of2 _ -> ()

[<Fact>]
let ``parse liquidtag`` ()=
    runp liquidTag "{% test %}line0\nline1{% endtest %}"
        |> isLeftWith (makeTag "test" "line0\nline1")

[<Fact>]
let ``parse liquidtag with additional start tag attributes`` ()=
    runp liquidTag "{% test extra stuff %}line0\nline1{% endtest %}"
        |> isLeftWith (makeTag "test" "line0\nline1")

[<Fact>]
let ``parse liquidtag with inconsistent spacing around tags`` ()=
    runp liquidTag "{%test extra stuff%}line0\nline1{%endtest %}"
        |> isLeftWith (makeTag "test" "line0\nline1")

[<Fact>]
let ``invalid liquid tag`` ()=
    runp liquidTag "{% test %}line0\nline1{% test %}"
        |> isRight

[<Fact>]
let ``multiple code blocks`` ()=
    let input = @"This is a test.
More testing.

{% examplecode csharp %}
ABC
DEF
{% endexamplecode %}

More stuff.

{% othertag %}
123
{% endothertag %}

{% requiredcode %}
GHI
{% endrequiredcode %}"
    runp codeBlocks input
        |> isLeftWith [Snippet "ABC\nDEF"; Declaration "GHI"]



