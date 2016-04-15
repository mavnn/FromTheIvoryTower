(**
- title : From The Ivory Tower
- description : Solving real world problems with unusual language features
- author : Michael Newton
- theme : sky
- transition : slide

***

![tower](images/tower.jpg)

***

## Solving Real World Problems...

### ...from the Ivory Tower

***

## Active Patterns

---

### Handy helpers

Liberally stolen from [this gist](https://gist.github.com/kjnilsson/a56040cdd32fd686ffd1).

Thanks [Karl Nilsson](https://twitter.com/kjnilsson)!

*)

let isBob name =
  match name with
  | "bob" -> "It's bob!"
  | _ -> "It's not bob!"

(**

What about writing an ``isName`` function?

---

*)

let isName1 matchName personName =
  match personName with
  | personName ->
    sprintf "It's %s!" matchName
  | _ ->
    // This rule will never be matched...
    sprintf "It's not %s!" matchName

(**

---

*)

let isName2 matchName personName =
  match personName with
  | str when str = personName ->
    sprintf "It's %s!" matchName
  | _ ->
    sprintf "It's not %s!" matchName

(**

---

*)

let (|Eq|_|) expected value =
  if expected = value then Some ()
  else None

let niceIsName matchName personName =
  match personName with
  | Eq matchName ->
    sprintf "It's %s!" matchName
  | _ ->
    sprintf "It's not %s!" matchName

(*** hide ***)
open System

let dbForwarder str = ()
let consoleForwarder str = ()

(**

---

*)

let (|Val|_|) = Map.tryFind

let makeForwarder =
  function
  | Val "type" "db" & Val "connection" conn ->
    dbForwarder conn 
  | Val "type" "console" & Val "prefix" prefix ->
    consoleForwarder prefix
  | _ -> failwith "wat?"

let (|AsInt64|_|) s =
  match Int64.TryParse s with
  | true, v -> Some v
  | _ -> None

match "1234" with
| AsInt64 x -> "ok"
| _ -> "not ok"
(**

---

## Put 'em together

*)

let tryGetPort hostName config =
  match config with
  | Val hostName (AsInt64 portNum) -> Some portNum
  | _ -> None


(**

---

### Partitioning existing data

F# Quotations represent the AST of a piece of F# code; nodes in the tree
[can be one of 38 cases](https://github.com/fsharp/fsharp/blob/master/src/fsharp/FSharp.Core/quotations.fs#L157)

Let's say we want to transform ``if`` statements and carry everything else through unchanged

That's 38 lines of code, or...

---

*)

open FSharp.Quotations
open FSharp.Quotations.ExprShape

let expr = <@@ if 10 < 5 then "really?!" else "good" @@>

let rec traverse expr =
  match expr with
  | ShapeVar (v) ->
    Expr.Var(v)
  | ShapeLambda(v, body) ->
    Expr.Lambda(v, traverse body)
  | ShapeCombination(a, args) ->
    let traversed = args |> List.map traverse
    ExprShape.RebuildShapeCombination(a, traversed)

let result =
  if expr = traverse expr then "Hurrah!" else "Boo!"

(**

result:

*)
(*** include-value: result ***)

(**
---

Derived patterns are part of [FSharp.Core](https://github.com/fsharp/fsharp/blob/master/src/fsharp/FSharp.Core/quotations.fs#L2208)

***

## Quotations

---

*)

(*** hide ***)
#r "../packages/Unquote/lib/net40/Unquote.dll"
#r "FSharp.Data.TypeProviders.dll"
#r "System.Data.Linq.dll"
#r "../packages/FSharpComposableQuery/lib/net40/FSharpComposableQuery.dll"

(**

### [Unquote](http://www.swensensoftware.com/unquote)

#### Not just failing, but failing with style

---

*)

(*** define-output:test1 ***)
open Swensen.Unquote.Assertions

let test1() =
  test <@ 1 = 2 @>

test1()

(*** include-output:test1 ***)

(**

---

*)

(*** define-output:test2 ***)

let test2() =
  test <@
    [3;2;1;0]
    |> List.map ((+) 1)
    |> (=) [1 + 3..1 + 0]
  @>

test2()

(*** include-output:test2 ***)

(**

---

### [FSharp.Linq.ComposableQuery](http://fsprojects.github.io/FSharp.Linq.ComposableQuery/)

([SkillsCast from Philip Wadler](https://skillsmatter.com/skillscasts/4486-a-practical-theory-of-language-integrated-query))

---

*)

open FSharpComposableQuery

type Person = { name: string; age: int }

let people = [
    { name = "bob"; age = 25 }
    { name = "fred"; age = 27 }
  ]

let aQuery =
  query {
    for p in people do
      sortBy p.age
      last
  }

(*** include-value:aQuery ***)

(**

---

*)

let filter = <@ fun (p : Person) -> p.age < 27 @>

let q2 filter =
  query {
    for p in people do
      if (%filter) p then yield p
  }

let filtered = q2 filter

(*** include-value:filtered ***)

(**

***

## Computational Expressions

*)

(*** hide ***)

#r "../packages/Hopac/lib/net45/Hopac.Core.dll"
#r "../packages/Hopac/lib/net45/Hopac.Platform.dll"
#r "../packages/Hopac/lib/net45/Hopac.dll"

(**

---

### The basics

---

*)


type OptionBuilder () =
  member x.Bind(m, f) =
    match m with
    | Some value -> f value
    | None -> None
  member x.Return value =
    Some value

let option = OptionBuilder()

let maybeAdd maybeA maybeB =
  option {
    let! a = maybeA
    let! b = maybeB
    return a + b
  }

// None
maybeAdd (Some 10) None

// Some 20
maybeAdd (Some 10) (Some 10)


(**

---

Excellent tutorial on [Scott's blog](https://fsharpforfunandprofit.com/series/computation-expressions.html)

Lots of examples in [FSharpx.Extras](https://github.com/fsprojects/FSharpx.Extras/tree/master/src/FSharpx.Extras/ComputationExpressions) and
[ExtCore](https://github.com/jack-pappas/ExtCore/tree/master/ExtCore)

---

### [Hopac](https://github.com/Hopac/Hopac)

Playing nice with your friends

([Also check out Marcus Griep's blog](https://neoeinstein.github.io/blog/2016/04-08-hopac-getting-started-with-jobs/index.html))

---

*)

open Hopac

let workToDo = job {
    let! a = job { return 10 }
    let! b = async { return 10 }
    return a + b
  }

let workDone = Job.Global.run workToDo

(*** include-value:workDone ***)

(**

---

*)

type AsyncBuilder with
  member x.Bind(aJob : Job<'a>, f : 'a -> Async<'b>) =
    job {
      let! jobResult = aJob
      return! f jobResult
    } |> Extensions.Async.Global.ofJob

let asyncWorkToDo = async {
    let! a = job { return 10 }
    let! b = async { return 10 }
    return a + b
  }

let asyncWorkDone = asyncWorkToDo |> Async.RunSynchronously

(*** include-value:asyncWorkDone ***)

(*** hide ***)

#r "../packages/FSharp.Text.RegexProvider/lib/net40/FSharp.Text.RegexProvider.dll"


(**

---

See [my recent blog post](http://blog.mavnn.co.uk/expanding-existing-computational-expressions/) for more details

***

## Type Providers

---

### [FSharp.Text.RegexProvider](http://fsprojects.github.io/FSharp.Text.RegexProvider/)

#### Regex with one less problem

Also: [text](http://blog.mavnn.co.uk/type-providers-from-the-ground-up/) and [video](http://blog.mavnn.co.uk/type-providers-live-the-movie/) type provider tutorials on my blog

---

*)

open FSharp.Text.RegexProvider

type MyRegex = Regex<"""(?<name>.+):\s+(?<age>\d+)""">

let regexed = MyRegex().Match("Bob McBob: 42")

let bobMcBob =
  { name = regexed.name.Value
    age = regexed.age.Value |> Int32.Parse }

(*** include-value:bobMcBob ***)


(**

---

*)

let (|IsMatch|NotMatch|) str =
  match MyRegex.IsMatch str with
  | true -> IsMatch (MyRegex().Match(str))
  | false -> NotMatch

let (|AsInt|_|) str =
  match Int32.TryParse str with
  | true, i -> Some i
  | false, _ -> None

let (|StringToPerson|_|) str =
  match str with
  | IsMatch regexed ->
    match regexed.age.Value with
    | AsInt i -> { name = regexed.name.Value
                   age = i } |> Some
    | _ -> None
  | NotMatch -> None

(**

***

# Thanks

### Michael Newton ([@mavnn](http://twitter.com/mavnn))

<img width="15%" src="images/swirl.png" />

More at http://blog.mavnn.co.uk

Training and consultancy through http://mavnn.co.uk

<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-37561687-1', 'auto');
  ga('send', 'pageview');

</script>

*)


