namespace Actualization.Domain

open NodaTime
open CoreTypes

[<RequireQualifiedAccess>]
module Price =

    let expired (now: Instant) (price: Price) =
        match price.End with
        | None -> false
        | Some endDate -> endDate < now

    let activeNow (now: Instant) (price: Price) =
        match price.End, price.Start with
        | None, None -> true
        | Some endDate, None -> endDate >= now
        | None, Some startDate -> startDate <= now
        | Some endDate, Some startDate ->
            endDate >= now && startDate <= now

    let private pricesForStore store filter allPrices =
        Map.tryFind store allPrices
        |> Option.map (Array.filter filter)
        |> Option.defaultValue [||]

    let private lowestListPrice (listPrices: ListPrice[]) =
        listPrices |> Array.minBy (fun p -> p.Price.Value)

    let private lowestPrice (prices: Price[]) =
        prices |> Array.minBy (fun p -> p.Value)

    let inline calculateDiscount listPrice campaignPrice =
        if listPrice = 0M then
            0.
        else
            let share = campaignPrice / listPrice |> float
            System.Math.Round(100. - (share * 100.), 2)

    let private limitedPriceValue : Instant -> StoreTimeZone -> PriceLog -> Price -> LimitedPrice -> LimitedCampaignPriceValue =
        fun now storeTimezone priceLog listPrice limitedPrice ->
            let today = localDate storeTimezone now
            let lowestHistoricPrice =
                SellingPriceHistory.findLowestOriginalPrice today limitedPrice.Price.PriceListId priceLog
                |> Option.defaultValue
                    {
                        PriceId = listPrice.PriceId
                        PriceListId = listPrice.PriceListId
                        Value = listPrice.Value
                        VatRate = listPrice.VatRate
                    }
            {
                Price =
                    {
                        DiscountPriceValue.Amount = limitedPrice.Price.Value
                        ListPrice = listPrice.Value
                        Discount = calculateDiscount lowestHistoricPrice.Value limitedPrice.Price.Value
                        LowestHistoricPrice = lowestHistoricPrice
                        PriceId = limitedPrice.Price.PriceId
                        PriceListId = limitedPrice.Price.PriceListId
                    }
                Quantity = limitedPrice.Quantity
            }

    let private appendLimitedPrices : PublicPrice -> LimitedCampaignPriceValue[] -> PublicPrice =
        fun fallback limitedPrices ->
            let limitedPrices =
                limitedPrices
                |> Array.where (fun p -> p.Price.Amount< PublicPrice.amount fallback)
                |> Array.sortByDescending (fun p -> p.Price.Amount)
                |> List.ofArray
            let rec loop limitedPrices acc =
                match limitedPrices with
                | [] -> acc
                | price :: prices ->
                    let acc =
                        {
                            DefaultPrice = price
                            FallbackPrice = acc
                        } |> LimitedCampaignPrice
                    loop prices acc
            fallback |> loop limitedPrices

    let private priceToCurrent memberPrices (limitedPrices: LimitedCampaignPriceValue[]) (price: Price) =
        let listPriceValue =
            {
                Amount = price.Value
                PriceId = price.PriceId
                PriceListId = price.PriceListId
            } |> ListPrice
        let currentPriceValue = appendLimitedPrices listPriceValue limitedPrices
        let memberPrices =
            memberPrices
                |> Map.filter(fun _ p -> p < price.Value)
                |> Map.map (fun _ v ->
                {
                    MemberSellingPrice.Value = v
                    PublicPriceDiscount = calculateDiscount price.Value v
                    ListPriceDiscount = calculateDiscount price.Value v
                })
        {
            PublicPrice = currentPriceValue
            RecommendedPrice = None
            VatRate = price.VatRate
            MemberPrices = memberPrices
        }

    let private listPriceToCurrentPrice now store priceLog memberPrices limitedPrices (lowest: Price) =
       let limitedPrices = limitedPrices |> Array.map (limitedPriceValue now store priceLog lowest)
       lowest |> priceToCurrent memberPrices limitedPrices

    // because of historic prices we want to remove duplicates,
    // giving preference to those that started earlier
    let private sanitizePrices (toPrice : 'T -> Price) =
        fun prices ->
            match prices with
            | null | [||] -> [||]
            | prices ->
                prices
                |> Array.sortBy (fun p -> (toPrice p).Start)
                |> Array.distinctBy(fun p -> (toPrice p).Value)

    let private sanitizeCampaignPrices : Price[] -> Price[] =
        sanitizePrices id

    let private sanitizeLimitedPrices : LimitedPrice[] -> LimitedPrice[] =
        sanitizePrices (fun p -> p.Price)

    let inline withTax tax price =
        let tax = decimal tax
        let price = decimal price
        System.Math.Round(price * (1M + tax), 2)

    let current (now: Instant) storeTimezone price =
        let activeNow = activeNow now
        let findLowestHistoricPrice (campaignPrice: Price) =
            let today = localDate storeTimezone now
            SellingPriceHistory.findLowestOriginalPrice today campaignPrice.PriceListId price.SellingPriceHistory
        let listPrices =
            price.ListPrices
            |> Array.filter (fun p -> p.Price |> activeNow)
        let campaignPrices =
            price.CampaignPrices
            |> Array.where activeNow
            |> sanitizeCampaignPrices

        let memberPrices =
            price.MemberPrices
            |> Array.filter (fun p -> p.Price |> activeNow)
            |> Array.sortByDescending (fun p -> p.Price.Value)
            |> Seq.map (fun p -> p.Tier, p.Price.Value)
            |> Map.ofSeq
        let limitedPrices =
            price.LimitedPrices
            |> Array.filter (fun p -> p.Price |> activeNow && p.Quantity > 0)
            |> sanitizeLimitedPrices

        let listPriceToCurrentPrice = listPriceToCurrentPrice now storeTimezone price.SellingPriceHistory

        match listPrices, campaignPrices with
        | [||], [||] -> None
        | listPrices, [||] ->
            let lowest = listPrices |> lowestListPrice
            lowest.Price |> listPriceToCurrentPrice memberPrices limitedPrices |> Some
        | [||], campaignPrices ->
            let lowest = lowestPrice campaignPrices
            lowest |> listPriceToCurrentPrice memberPrices limitedPrices |> Some
        | listPrice, campaignPrice ->
            let lowestCampaignPrice = campaignPrice |> lowestPrice
            let lowestListPrice = listPrice |> lowestListPrice
            let lowestPriceAvailable =
                    [|
                        yield! campaignPrice
                        yield! limitedPrices |> Seq.map (fun p -> p.Price)
                        yield lowestListPrice.Price
                    |] |> lowestPrice
            if lowestListPrice.Price.Value < lowestCampaignPrice.Value then
                lowestListPrice.Price |> listPriceToCurrentPrice memberPrices limitedPrices
            else
                let listPrice = lowestListPrice.Price.Value
                let lowestPrice = lowestListPrice.Price

                let lowestPriceThisMonth =
                    findLowestHistoricPrice lowestPriceAvailable
                    |> Option.defaultValue
                        {
                            PriceId = lowestPrice.PriceId
                            PriceListId = lowestPrice.PriceListId
                            Value = listPrice
                            VatRate = lowestPrice.VatRate
                        }
                let calculateDiscountsForCampaign (campaignPrice: Price) =
                    let discountBase = lowestPriceThisMonth.Value
                    {
                        DiscountPriceValue.Amount = campaignPrice.Value
                        ListPrice = listPrice
                        Discount = calculateDiscount discountBase campaignPrice.Value
                        LowestHistoricPrice = lowestPriceThisMonth
                        PriceId = campaignPrice.PriceId
                        PriceListId = campaignPrice.PriceListId
                    }

                let campaignPriceValue = calculateDiscountsForCampaign lowestCampaignPrice
                let limitedPrices =
                    limitedPrices
                    |> Array.where (fun lp -> lp.Price.Value < campaignPriceValue.Amount)
                    |> Array.map (fun lp ->
                        {
                            Price = calculateDiscountsForCampaign lp.Price
                            Quantity = lp.Quantity
                        })
                let publicPrice = appendLimitedPrices (CampaignPrice campaignPriceValue) limitedPrices
                {
                    PublicPrice = publicPrice
                    RecommendedPrice = lowestListPrice.RecommendedPrice
                    VatRate = lowestCampaignPrice.VatRate
                    MemberPrices =
                        memberPrices
                        |> Map.filter(fun _ p -> p < campaignPriceValue.Amount)
                        |> Map.map (fun _ v ->
                                {
                                    MemberSellingPrice.Value = v
                                    PublicPriceDiscount = calculateDiscount campaignPriceValue.Amount v
                                    ListPriceDiscount = calculateDiscount lowestListPrice.Price.Value v
                                })
                }
            |> Some

