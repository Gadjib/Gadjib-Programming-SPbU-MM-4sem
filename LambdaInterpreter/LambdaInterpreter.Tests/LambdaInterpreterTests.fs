module LambdaInterpreter.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open LambdaInterpreter

[<TestClass>]
type LambdaInterpreterTests () =

    [<TestMethod>]
    member _.``freeVars of variable is itself`` () =
        freeVars (Var "x") |> should equal (set ["x"])

    [<TestMethod>]
    member _.``freeVars of abstraction removes bound var`` () =
        freeVars (Abs ("x", Var "x")) |> should equal Set.empty<string>

    [<TestMethod>]
    member _.``freeVars of abstraction keeps only free vars`` () =
        let expr = Abs ("x", App (Var "x", Var "y")) 
        freeVars expr |> should equal (set ["y"])

    [<TestMethod>]
    member _.``freeVars of application is union of both sides`` () =
        let expr = App (Abs ("x", Var "x"), Var "y")
        freeVars expr |> should equal (set ["y"])

    [<TestMethod>]
    member _.``newName skips used names with index`` () =
        let used = set ["x0"; "x1"; "y"]
        let fresh = newName "x" used
        fresh |> should equal "x2"

    [<TestMethod>]
    member _.``substitude replaces free occurrences of variable`` () =
        let expr = App (Var "x", Var "y")          
        let t = Abs ("z", Var "z")             
        let result = substitude "x" t expr
        result |> should equal (App (t, Var "y"))

    [<TestMethod>]
    member _.``substitude does nothing if variable not present`` () =
        let expr = Abs ("y", App (Var "y", Var "z"))
        let t = Var "x"
        substitude "w" t expr |> should equal expr

    [<TestMethod>]
    member _.``betaReductionStep reduces simple beta-redex`` () =
        let expr = App (Abs ("x", Var "x"), Var "y")    
        let result = betaReductionStep expr
        result |> should equal (Some (Var "y"))

    [<TestMethod>]
    member _.``betaReductionStep reduces leftmost outermost redex`` () =
        let expr =
            App (
                App (Abs ("x", Var "x"), Var "y"),
                App (Abs ("z", Var "z"), Var "w")
            )
        let expected =
            App (
                Var "y",
                App (Abs ("z", Var "z"), Var "w")
            )
        betaReductionStep expr |> should equal (Some expected)

    [<TestMethod>]
    member _.``betaReduction reduces identity application`` () =
        let expr = App (Abs ("x", Var "x"), Var "y")
        betaReduction expr |> should equal (Var "y")

    [<TestMethod>]
    member _.``betaReduction with nested abstractions and alpha conversion`` () =
        let expr =
            App (
                Abs ("x", Abs ("y", Var "x")),
                Var "y"
            )
        let result = betaReduction expr
        match result with
        | Abs (yName, body) ->
            yName |> should equal "y0"
            body  |> should equal (Var "y")
        | _ -> Assert.Fail "Expected abstraction"