module LocalNetwork

open System

/// Интерфейс генератора случайных чисел (для мока в тестах)
type IRandom =
    abstract member NextDouble : unit -> float

/// Реальный генератор случайных чисел
type RandomProvider() =
    let rnd = Random()
    interface IRandom with
        member _.NextDouble() = rnd.NextDouble()

type OperatingSystemNames = Windows | Linux | MacOS

type OperatingSystem =
    { Name : OperatingSystemNames
      InfectionProbability : float }

type Computer(id : int, os : OperatingSystem, initiallyInfected : bool) =
    let mutable infected = initiallyInfected

    member _.Id = id
    member _.OS = os

    member _.IsInfected
        with get() = infected

    member _.Infect() =
        infected <- true

open System

type Network(computers : Computer array, adjacency : bool [,], random : IRandom) =
    member _.Computers = computers

    member _.Step() : bool =
        let n = computers.Length

        let infectedNow =
            [| for i in 0 .. n - 1 do
                   if computers.[i].IsInfected then
                       yield i |]

        let mutable toInfect = Set.empty

        for i in infectedNow do
            for j in 0 .. n - 1 do
                if adjacency.[i, j] && not computers.[j].IsInfected then
                    let p = computers.[j].OS.InfectionProbability
                    let r = random.NextDouble()
                    if r < p then
                        toInfect <- Set.add j toInfect

        let mutable changed = false

        for idx in toInfect do
            if not computers.[idx].IsInfected then
                computers.[idx].Infect()
                changed <- true

        changed
