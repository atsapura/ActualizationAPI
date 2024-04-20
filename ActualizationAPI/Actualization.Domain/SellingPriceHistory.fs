namespace Actualization.Domain

open NodaTime
open CoreTypes

[<RequireQualifiedAccess>]
module SellingPriceHistory =

    let CutoffPeriod = Period.FromDays 30

    let private cutoffDate : LocalDate -> LocalDate =
        fun d -> d.Minus CutoffPeriod

    let private latestEntry priceLog =
        if priceLog |> Map.isEmpty then
            None
        else
            priceLog
            |> Map.toArray
            |> Array.maxBy fst
            |> Some

    let add date price (log: PriceLog) =
        match latestEntry log with
        | Some (_, p) when p = price -> log
        | _ -> log.Add(date, price)

    let removeOutdatedFromLog today (priceLog: PriceLog) : PriceLog =
        let cutoffDay = cutoffDate today
        // if price change didn't happen on cutoffDay there'd be no entry in log for that day.
        // But we can't just cut off straight away, we need to keep the latest entry, which
        // was still active that day
        let priceLog =
            match priceLog.TryFind cutoffDay with
            | Some _ -> priceLog
            | None ->
                let latestPresent =
                    priceLog
                    |> Map.toArray
                    |> Array.sortByDescending fst
                    |> Array.tryFind (fun (date, p) -> date < cutoffDay)
                match latestPresent with
                | None -> priceLog
                | Some (date, price) -> priceLog.Add(cutoffDay, price)
        priceLog
        |> Map.filter (fun d _ -> d >= cutoffDay)

    let removeOutdated now storeTimezone (priceLog: PriceLog) : PriceLog =
        let today = localDate storeTimezone now
        removeOutdatedFromLog today priceLog

    let private findLowestOriginalPriceByPriceListId today priceListId (log: PriceLog) =
        let log = Map.toArray log |> Array.where (fun (d, p) -> d <= today && p.PriceListId <> priceListId)
        match log with
        | [||] -> None
        | log ->
            log
            |> Array.minBy (fun (_, x) -> x.Value) |>  Some

    let findLowestOriginalPriceWithDate today priceListId (log: PriceLog) =
        let log = removeOutdatedFromLog today log
        findLowestOriginalPriceByPriceListId today priceListId log

    let findLowestOriginalPrice today priceListId (log: PriceLog) =
        findLowestOriginalPriceWithDate today priceListId log
        |> Option.map snd
