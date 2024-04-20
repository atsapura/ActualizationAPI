namespace Actualization.Domain

open CoreTypes

[<RequireQualifiedAccess>]
module PriceLog =

    let private toOriginalPrice : CurrentPrice -> OriginalPrice =
        fun price ->
            {
                PriceId = PublicPrice.priceId price.PublicPrice
                PriceListId = PublicPrice.priceListId price.PublicPrice
                Value = PublicPrice.amount price.PublicPrice
                VatRate = price.VatRate
            }

    let addOriginalPrice now (history: PriceLog) (storeTimezone, price: CurrentPrice) : PriceLog =
        let today = localDate storeTimezone now
        let originalPrice = toOriginalPrice price
        history |> SellingPriceHistory.add today originalPrice

    let private currentPrice now price store =
        Price.current now store price
        |> Option.map (fun p -> store, p)

    let append now (price: ProductItemPrice) =
        let historicPrice = currentPrice now price StoreTimeZone.Moscow
        match historicPrice with
        | None -> price
        | Some (tz, p) ->
            let history = addOriginalPrice now price.SellingPriceHistory (tz, p)
            { price with SellingPriceHistory = history }
