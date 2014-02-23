module DocToCSharp.Tests.ConversionTests

open Xunit
open FsUnit.Xunit
open DocToCSharp.Conversion

let equalsList (expected:'a list) (actual:'a seq) =
    actual
        |> List.ofSeq
        |> should equal expected

[<Fact>]
let ``parse liquidtag`` ()=
    tags "{% test %}abc{% endtest %}"
        |> equalsList [makeTag "test" "abc"]

[<Fact>]
let ``parse liquidtag with newlines`` ()=
    tags "{% test %}line0\nline1{% endtest %}"
        |> equalsList [makeTag "test" "line0\nline1"]

[<Fact>]
let ``parse liquidtag with additional start tag attributes`` ()=
    tags "{% test extra stuff %}line0\nline1{% endtest %}"
        |> equalsList [makeTag "test" "line0\nline1"]

[<Fact>]
let ``parse liquidtag with inconsistent spacing around tags`` ()=
    tags "{%test extra stuff%}line0\nline1{%endtest %}"
        |> equalsList [makeTag "test" "line0\nline1"]

[<Fact>]
let ``invalid liquid tag`` ()=
    tags "{% test %}line0\nline1{% test %}"
        |> equalsList []

[<Fact>]
let ``multiple code blocks`` ()=
    let newline = System.Environment.NewLine
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
    toCodeBlocks input
        |> equalsList [Snippet ("ABC" + newline + "DEF"); Declaration "GHI"]


