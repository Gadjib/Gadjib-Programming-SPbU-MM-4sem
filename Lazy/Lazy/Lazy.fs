module Lazy

open System
open System.Threading

/// Интерфейс ленивого вычисления
type ILazy<'a> =
    abstract member Get : unit -> 'a

/// Простая однопоточная реализация ILazy<'a>
type SingleThreadLazy<'a>(supplier : unit -> 'a) =
    let mutable hasValue = false
    let mutable value : 'a = Unchecked.defaultof<'a>

    interface ILazy<'a> with
        member _.Get() =
            if not hasValue then
                let v = supplier()
                value <- v
                hasValue <- true
                v
            else
                value

/// Потокобезопасная реализация ILazy<'a>, вычисление строго один раз
type ThreadSafeLazy<'a>(supplier : unit -> 'a) =
    let mutable hasValue = false
    let mutable value : 'a = Unchecked.defaultof<'a>
    let locker = obj()

    interface ILazy<'a> with
        member _.Get() =
            if hasValue then
                value
            else
                lock locker (fun () ->
                    if not hasValue then
                        let v = supplier()
                        value <- v
                        hasValue <- true
                    value)

/// Lock-free реализация ILazy<'a>
type LockFreeLazy<'a>(supplier : unit -> 'a) =
    let sentinel = obj()
    let mutable valueObj : obj = sentinel

    interface ILazy<'a> with
        member _.Get() =
            let current = valueObj
            if not (Object.ReferenceEquals(current, sentinel)) then
                unbox<'a> current
            else
                let v = supplier()
                let boxed = box v

                let original =
                    Interlocked.CompareExchange(&valueObj, boxed, sentinel)

                let final = valueObj
                unbox<'a> final