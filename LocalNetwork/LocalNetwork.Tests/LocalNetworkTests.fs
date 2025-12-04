namespace NetworkSimulation.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open LocalNetwork
open System

/// Mock: детерминированный генератор случайных чисел
type ConstantRandom(value : float) =
    interface IRandom with
        member _.NextDouble() = value

[<TestClass>]
type InfectionTests () =

    ///Helper: создаёт сеть-цепочку 0-1-2-...-(n-1)
    let makeChainNetwork (length : int)
                         (infectionProbs : float array)
                         (initiallyInfected : Set<int>)
                         (random : IRandom) =
        let n = length

        let osArray = [|Windows; Linux; MacOS|]

        let rnd = Random()

        let computers =
            [|
                for i in 0 .. n - 1 ->
                    let os =
                        { Name = osArray[rnd.Next(osArray.Length)]
                          InfectionProbability = infectionProbs.[i] }

                    let infectedInitially = initiallyInfected.Contains i
                    Computer(i, os, infectedInitially)
            |]

        let adjacency = Array2D.create n n false

        let connect i j =
            adjacency.[i, j] <- true
            adjacency.[j, i] <- true

        // Соединяем в цепочку
        for i in 0 .. n - 2 do
            connect i (i + 1)

        let net = Network(computers, adjacency, random)
        net, computers

    [<TestMethod>]
    member _.InfectionProbabilityOneBehavesLikeBFS () =
        let n = 4
        // Вероятность заражения = 1 для всех
        let infectionProbs = Array.create n 1.0
        let initiallyInfected = Set.ofList [ 0 ]

        // Mock: всегда возвращает 0.0 (<= 1.0, значит заражение всегда успешно)
        let random = ConstantRandom(0.0) :> IRandom

        let network, computers =
            makeChainNetwork n infectionProbs initiallyInfected random

        let assertInfected (expectedSet : Set<int>) =
            for i in 0 .. n - 1 do
                let expected = expectedSet.Contains i
                Assert.AreEqual(
                    expected,
                    computers.[i].IsInfected,
                    sprintf "Computer %d has wrong infection state" i
                )

        // Шаг 0: заражён только 0
        assertInfected (Set.ofList [ 0 ])

        // Шаг 1: заразится только 1 (как первый слой BFS)
        Assert.IsTrue(network.Step())
        assertInfected (Set.ofList [ 0; 1 ])

        // Шаг 2: добавится 2
        Assert.IsTrue(network.Step())
        assertInfected (Set.ofList [ 0; 1; 2 ])

        // Шаг 3: добавится 3
        Assert.IsTrue(network.Step())
        assertInfected (Set.ofList [ 0; 1; 2; 3 ])

        // Шаг 4: изменений уже нет
        Assert.IsFalse(network.Step())
        assertInfected (Set.ofList [ 0; 1; 2; 3 ])

    [<TestMethod>]
    member _.InfectionProbabilityZeroNoSpread () =
        let n = 4
        // Вероятность заражения = 0 для всех
        let infectionProbs = Array.create n 0.0
        let initiallyInfected = Set.ofList [ 0 ]

        // Значение рандома неважно, всё равно p = 0
        let random = ConstantRandom(0.0) :> IRandom

        let network, computers =
            makeChainNetwork n infectionProbs initiallyInfected random

        // Один шаг
        let changed = network.Step()

        // Никто не должен заразиться
        Assert.IsFalse(changed)

        for i in 0 .. n - 1 do
            let expected = (i = 0) // только 0 заражён изначально
            Assert.AreEqual(
                expected,
                computers.[i].IsInfected,
                sprintf "Computer %d has wrong infection state" i
            )
