module LazyTests

open System
open System.Threading
open Microsoft.VisualStudio.TestTools.UnitTesting
open Lazy

/// Общие тесты для разных реализаций ILazy<'a>


/// Проверяем, что в однопоточном режиме supplier вызывается ровно один раз,
/// а Get() всегда возвращает один и тот же объект.
let singleThread_basicTests (lazyFactory : (unit -> obj) -> ILazy<obj>) =
    let callCount = ref 0

    let supplier () =
        Interlocked.Increment(callCount) |> ignore
        // возвращаем новый объект, чтобы можно было проверить ссылочное равенство
        box (obj())

    let lazyVal = lazyFactory supplier

    let v1 = lazyVal.Get()
    let v2 = lazyVal.Get()
    let v3 = lazyVal.Get()

    // supplier должен быть вызван ровно один раз
    Assert.AreEqual(1, !callCount, "Supplier должен быть вызван один раз в однопоточном режиме")

    // Все результаты — один и тот же объект (reference equality)
    Assert.IsTrue(Object.ReferenceEquals(v1, v2), "v1 и v2 должны быть одним и тем же объектом")
    Assert.IsTrue(Object.ReferenceEquals(v2, v3), "v2 и v3 должны быть одним и тем же объектом")

/// Запускаем много потоков, каждый много раз зовёт Get(),
/// собираем все результаты в массив.
let runParallelGets (lazyVal : ILazy<obj>) (threadCount : int) (iterationsPerThread : int) =
    let results = Array.zeroCreate<obj> (threadCount * iterationsPerThread)

    let mutable index = 0

    let threads =
        [|
            for t in 0 .. threadCount - 1 ->
                new Thread(ThreadStart(fun () ->
                    for i in 0 .. iterationsPerThread - 1 do
                        let v = lazyVal.Get()
                        let idx = Interlocked.Increment(&index) - 1
                        results.[idx] <- v
                ))
        |]

    for th in threads do th.Start()
    for th in threads do th.Join()

    results

/// Проверка, что во всех results лежит один и тот же объект
let assertAllSameObject (results : obj array) =
    Assert.IsTrue(results.Length > 0, "Результатов должно быть хотя бы одно")
    let first = results.[0]
    for i in 1 .. results.Length - 1 do
        Assert.IsTrue(
            Object.ReferenceEquals(first, results.[i]),
            sprintf "Элемент %d не равен по ссылке первому" i
        )

[<TestClass>]
type SingleThreadLazyTests () =

    /// Однопоточный сценарий — базовые проверки
    [<TestMethod>]
    member _.SingleThread_Basic() =
        let factory (f : unit -> obj) =
            SingleThreadLazy(f) :> ILazy<obj>

        singleThread_basicTests factory

[<TestClass>]
type ThreadSafeLazyTests () =

    /// Однопоточный сценарий — такой же, как для SingleThreadLazy
    [<TestMethod>]
    member _.SingleThread_Basic() =
        let factory (f : unit -> obj) =
            ThreadSafeLazy(f) :> ILazy<obj>

        singleThread_basicTests factory

    /// Многопоточный сценарий: supplier должен быть вызван ровно один раз,
    /// а все потоки должны получить один и тот же объект.
    [<TestMethod>]
    member _.MultiThread_ExecutesOnce_AllGetSameObject() =
        let callCount = ref 0

        let supplier () =
            // чуть притормозим, чтобы увеличить шанс гонки
            Thread.Sleep(10)
            Interlocked.Increment(callCount) |> ignore
            box (obj())

        let lazyVal = ThreadSafeLazy(supplier) :> ILazy<obj>

        let results = runParallelGets lazyVal 16 50

        // supplier должен выполниться ровно один раз
        Assert.AreEqual(1, !callCount, "Supplier должен быть вызван один раз в многопоточной версии")

        // все потоки получили один и тот же объект
        assertAllSameObject results

[<TestClass>]
type LockFreeLazyTests () =

    /// В однопоточном режиме lock-free версия ведёт себя так же:
    /// один вызов supplier, один и тот же объект.
    [<TestMethod>]
    member _.SingleThread_Basic() =
        let factory (f : unit -> obj) =
            LockFreeLazy(f) :> ILazy<obj>

        singleThread_basicTests factory

    /// В многопоточном режиме supplier может вызваться несколько раз,
    /// но все потоки должны получить один и тот же объект.
    [<TestMethod>]
    member _.MultiThread_MayExecuteManyTimes_AllGetSameObject() =
        let callCount = ref 0

        let supplier () =
            // Делаем вычисление "тяжёлым", чтобы повысить шанс параллельных вызовов
            Thread.Sleep(10)
            Interlocked.Increment(callCount) |> ignore
            box (obj())

        let lazyVal = LockFreeLazy(supplier) :> ILazy<obj>

        let results = runParallelGets lazyVal 16 50

        // supplier мог выполниться много раз, но как минимум один
        Assert.IsTrue(!callCount >= 1, "Supplier должен быть вызван хотя бы один раз")

        // все потоки получили один и тот же объект (тот, который победил в CAS)
        assertAllSameObject results
