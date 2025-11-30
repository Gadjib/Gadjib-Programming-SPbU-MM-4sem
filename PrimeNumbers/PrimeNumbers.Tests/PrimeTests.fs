module PrimeNumbers.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open PrimeNumbers

[<TestClass>]
type PrimeTests () =

    [<TestMethod>]
    member _.``The first element is two`` () =
        primeNumbers () |> Seq.item 0 |> should equal 2

    [<TestMethod>]
    member _.``The tenth element is 29`` () =
        primeNumbers () |> Seq.item 9 |> should equal 29