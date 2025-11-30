module PrimeNumbers

let isPrime n =
    if n < 2 then false
    else
        let root = int (sqrt (float n))
        let rec isPrimeHelper t =
            if t > root then true
            elif n % t = 0 then false
            else isPrimeHelper (t + 1)
        isPrimeHelper 2

let primeNumberOfIndex n =
    let rec primeNumberOfIndexHelper t ans =
        if isPrime ans then
            if t = n then ans
            else primeNumberOfIndexHelper (t + 1) (ans + 1)
        else primeNumberOfIndexHelper t (ans + 1)
    primeNumberOfIndexHelper 0 2

let primeNumbers () =
        Seq.initInfinite primeNumberOfIndex

let aboba = primeNumbers () |> Seq.take 10 |> Seq.toList
printfn "%A" aboba