namespace Actualization.API

open NodaTime
open Actualization.Domain.CoreTypes
open Actualization.Domain.LocalizedModels
open Actualization.Domain

module StatefulCatalog =

    type ProductId = string

    type ItemId = string

    type PartialResult<'Ok, 'Error> =
        | FullSuccess of 'Ok
        | PartialSuccess of 'Ok * errors: 'Error[]
        | NoSuccess of id:string * 'Error[]

    let mapError f partialResult =
        match partialResult with
        | FullSuccess s -> FullSuccess s
        | PartialSuccess (s, errs) -> (s, Array.map f errs) |> PartialSuccess
        | NoSuccess (id, errs) -> (id, Array.map f errs) |> NoSuccess

    let appendErrors errors result =
        match errors with
        | [||] -> result
        | errors ->
            match result with
            | FullSuccess s -> PartialSuccess (s, errors)
            | PartialSuccess (s, errs) -> PartialSuccess (s, Array.append errs errors)
            | NoSuccess(id, errs) -> NoSuccess (id, Array.append errs errors)

    type IncompleteProductItem =
        {
            ItemId: ItemId
            ProductItem: ProductItem option
            Price: ProductItemPrice option
            Inventory: FullInventory option
            StockBalance : ProductItemStockBalance option
            BackorderAvailability : ProductItemBackorderAvailability option
        }
        with
        static member empty itemId =
            {
                ItemId = itemId
                ProductItem = None
                Price = None
                Inventory = None
                StockBalance = None
                BackorderAvailability = None
            }

    type IncompleteProduct =
        {
            ProductId: ProductId
            IncompleteItems: Map<ItemId, IncompleteProductItem>
        }
        with
        static member empty productId =
            {
                ProductId = productId
                IncompleteItems = Map.empty
            }
        member this.FindFullItem itemId =
            this.IncompleteItems.TryFind itemId
        member this.FindProductItem itemId =
            optional {
                let! item = this.FindFullItem itemId
                return! item.ProductItem
            }
        member this.FindInventory itemId =
            optional {
                let! item = this.FindFullItem itemId
                return! item.Inventory
            }
        member this.FindPrice itemId =
            optional {
                let! item = this.FindFullItem itemId
                return! item.Price
            }

    [<RequireQualifiedAccess>]
    module IncompleteProductItem =

        let private mergeLimitedPrices: LimitedPrice[] Merge =
            fun oldPrices newPrices ->
                [|
                    for price in newPrices do
                        match oldPrices |> Array.tryFind (fun p -> p.Price.PriceId = price.Price.PriceId) with
                        | None -> price
                        | Some oldPrice ->
                            {
                                price with Quantity = min oldPrice.Quantity price.Quantity
                            }
                |]

        let private mergePrices : ProductItemPrice Merge =
            fun oldPrice newPrice ->
                {
                    newPrice with
                        LimitedPrices =
                            mergeLimitedPrices oldPrice.LimitedPrices newPrice.LimitedPrices
                }

        let private evalFreightClass (item: ProductItem) (inventory: FullInventory) =
            let evalProductItemFreightClass () =
                item.Dimensions.FreightClass
                |> tryParse<int>
                |> Option.defaultValue 0

            inventory.FreightClass |> Option.defaultWith evalProductItemFreightClass

        let private mergeFreightClass incompleteProductItem =
            (incompleteProductItem.ProductItem, incompleteProductItem.Inventory)
            ||> Option.map2 (fun productItem inventory ->
                let freightClass =
                    evalFreightClass productItem inventory

                let updatedProductItem =
                    let stringFreightClass =
                        if freightClass = 0 then null else string freightClass
                    let dimensions = { productItem.Dimensions with FreightClass = stringFreightClass }
                    Some { productItem with Dimensions = dimensions }

                let updatedInventory =
                    let freightClassOpt =
                        if freightClass = 0 then None else Some freightClass
                    Some { inventory with FreightClass = freightClassOpt }

                { incompleteProductItem with ProductItem = updatedProductItem; Inventory = updatedInventory })
            |> Option.defaultValue incompleteProductItem

        let withProductItem (item: ProductItem) incompleteProductItem =
            if incompleteProductItem.ItemId <> item.Sku then
                incompleteProductItem
            else
                { incompleteProductItem with ProductItem = Some item }
                |> mergeFreightClass

        let withInventory (inventory: FullInventory) incompleteProductItem =
            if incompleteProductItem.ItemId <> inventory.Sku then
                incompleteProductItem
            else
                { incompleteProductItem with Inventory = Some inventory }
                |> mergeFreightClass

        let withStockBalance (stockBalance : ProductItemStockBalance) incompleteProductItem =
            if incompleteProductItem.ItemId <> stockBalance.ProductItemId then
                incompleteProductItem
            else
            { incompleteProductItem with StockBalance = Some stockBalance }

        let withBackorderAvailability (backorderAvailability: ProductItemBackorderAvailability) incompleteProductItem =
            if incompleteProductItem.ItemId <> backorderAvailability.ProductItemId then
                incompleteProductItem
            else
            { incompleteProductItem with BackorderAvailability = Some backorderAvailability }

        let withPrice (price: ProductItemPrice) incompleteProductItem =
            if incompleteProductItem.ItemId <> price.ItemId then
                incompleteProductItem
            else
            let price =
                match incompleteProductItem.Price with
                | None -> price
                | Some p -> mergePrices p price
            { incompleteProductItem with Price = Some price }

        let removeInventory (incompleteProductItem : IncompleteProductItem) =
            { incompleteProductItem with Inventory = None }

        let removeStockBalance (incompleteProductItem : IncompleteProductItem) =
            { incompleteProductItem with StockBalance = None }

        let removeBackorderAvailability (incompleteProductItem : IncompleteProductItem) =
            { incompleteProductItem with BackorderAvailability = None }

        let removePrice (incompleteProductItem: IncompleteProductItem) =
            { incompleteProductItem with Price = None }

        let removeProductItem (incompleteProductItem : IncompleteProductItem) =
            { incompleteProductItem with ProductItem = None }

    [<RequireQualifiedAccess>]
    module IncompleteProduct =

        let withIncompleteItem product item =
            let items = product.IncompleteItems.Add (item.ItemId, item)
            { product with IncompleteItems = items }

        let withProductItem product (item: ProductItem) =
            let incompleteItem =
                product.IncompleteItems.TryFind item.Sku
                |> Option.defaultValue (IncompleteProductItem.empty item.Sku)
                |> IncompleteProductItem.withProductItem item
            withIncompleteItem product incompleteItem

        let withPrice product (price: ProductItemPrice) =
            let incompleteItem =
                product.IncompleteItems.TryFind price.ItemId
                |> Option.defaultValue (IncompleteProductItem.empty price.ItemId)
                |> IncompleteProductItem.withPrice price
            withIncompleteItem product incompleteItem

        let withInventory product (inventory : FullInventory) =
            let incompleteItem =
                product.IncompleteItems.TryFind inventory.Sku
                |> Option.defaultValue (IncompleteProductItem.empty inventory.Sku)
                |> IncompleteProductItem.withInventory inventory
            withIncompleteItem product incompleteItem

        let withStockBalance product (stockBalance : ProductItemStockBalance) =
            let incompleteItem =
                product.IncompleteItems.TryFind stockBalance.ProductItemId
                |> Option.defaultValue (IncompleteProductItem.empty stockBalance.ProductItemId)
                |> IncompleteProductItem.withStockBalance stockBalance
            withIncompleteItem product incompleteItem

        let withBackorderAvailability product (backorderAvailability : ProductItemBackorderAvailability) =
            let incompleteItem =
                product.IncompleteItems.TryFind backorderAvailability.ProductItemId
                |> Option.defaultValue (IncompleteProductItem.empty backorderAvailability.ProductItemId)
                |> IncompleteProductItem.withBackorderAvailability backorderAvailability
            withIncompleteItem product incompleteItem

        let removeInventory product itemId =
            optional {
                let! item = product.IncompleteItems.TryFind itemId
                let item = IncompleteProductItem.removeInventory item
                return withIncompleteItem product item
            } |> Option.defaultValue product

        let removeStockBalance product itemId =
            optional {
                let! item = product.IncompleteItems.TryFind itemId
                let item = IncompleteProductItem.removeStockBalance item
                return withIncompleteItem product item
            } |> Option.defaultValue product

        let removeBackorderAvailability product itemId =
            optional {
                let! item = product.IncompleteItems.TryFind itemId
                let item = IncompleteProductItem.removeBackorderAvailability item
                return withIncompleteItem product item
            } |> Option.defaultValue product

        let removePrice product itemId =
            optional {
                let! item = product.IncompleteItems.TryFind itemId
                let item = IncompleteProductItem.removePrice item
                return withIncompleteItem product item
            } |> Option.defaultValue product

        let removeProductItem product itemId =
            optional {
                let! item = product.IncompleteItems.TryFind itemId
                let item = IncompleteProductItem.removeProductItem item
                return withIncompleteItem product item
            } |> Option.defaultValue product

        let removeFullItem product itemId =
            let items = product.IncompleteItems.Remove itemId
            { product with IncompleteItems = items }

    type MissingProductItemPart =
        | ProductItem of itemId: ItemId
        | Price of itemId: ItemId
        | Inventory of itemId: ItemId
        | ShipmentMapping of itemId : ItemId

    type ItemCompletionResult =
        | CompletedItem of CompleteProductItem
        | IncompleteItem of MissingProductItemPart[]

    let tryCompleteItem now (incompleteItem: IncompleteProductItem) =
        match incompleteItem.ProductItem, incompleteItem.Price, incompleteItem.Inventory, incompleteItem.StockBalance with
        // Allow missing ShipmentMapping for bundles as they are virtual products and don't have ShipmentMapping from Visma
        | Some productItem, Some price, Some inventory, stockBalance ->
            {
                ProductItem = productItem
                Price = price
                Inventory = inventory
                StockBalance = stockBalance
            } |> CompletedItem
        | maybeItem, maybePrice, maybeInventory, _ ->
            [|
                if maybeItem |> Option.isNone then
                    ProductItem incompleteItem.ItemId
                if maybePrice |> Option.isNone then
                    Price incompleteItem.ItemId
                if maybeInventory |> Option.isNone then
                    Inventory incompleteItem.ItemId
            |] |> IncompleteItem

    let completeProduct now product =
        let completionResults = product.IncompleteItems |> Seq.map (fun p -> tryCompleteItem now p.Value)
        let completeItems =
            completionResults
            |> Seq.choose (function | CompletedItem i -> Some i | _ -> None)
            |> Array.ofSeq
        let incompleteItems =
            completionResults
            |> Seq.choose (function | IncompleteItem i -> Some i | _ -> None)
            |> Array.concat
        match completeItems, incompleteItems with
        | [||], _ -> (product.ProductId, incompleteItems) |> NoSuccess
        | items, [||] ->
            {
                ProductId = product.ProductId
                Items = items
            } |> FullSuccess
        | items, incomplete ->
            let product =
                {
                    ProductId = product.ProductId
                    Items = items
                }
            PartialSuccess (product, incomplete)

    type ItemLocalizationError =
        | ItemLocalizationError of ItemId * Language * ProductItemLocalizationError list
        | NoActivePrice of ItemId
        with
        member this.ItemId =
            match this with
            | NoActivePrice id
            | ItemLocalizationError (id, _, _) -> id

    let private totalQuantity : ItemStock -> float =
        fun stock ->
            let available itemStock =
                match itemStock with
                | ItemStock.OutOfStock _ -> [||]
                | ItemStock.Available availableStock -> availableStock
            available stock
            |> Array.distinct
            |> Array.sumBy (fun s -> AvailableStock.quantity s)

    let localizeCompleteItem now timezone language (item: CompleteProductItem) =
        let localizationResult store =
            localizeProductItem item.ProductItem store
            |> Result.mapError (fun e -> item.ProductItem.Sku, store, e)
        let currentPrice storeTimezone = Price.current now storeTimezone item.Price
        let stock = itemStock item.StockBalance

        // TODO remove - temporary until the Quantity is removed from inventory
        let stockBalanceQuantity = totalQuantity stock

        match localizationResult language, currentPrice timezone with
        | Ok localizedItem, Some price ->
            {
                LocalizedCompleteItem.ProductItem = localizedItem
                Price = price
                Inventory = item.Inventory
                Language = language
                Stock = stock
            } |> Ok
        | localizationResult, price ->
            [|
                match localizationResult with
                | Error e -> ItemLocalizationError e
                | Ok _-> ()
                match price with
                | None -> NoActivePrice item.ProductItem.Sku
                | Some _ -> ()
            |] |> Error

    let private mapItemLocalizationError : ItemLocalizationError -> FullItemError =
        fun e ->
            match e with
            | NoActivePrice _ -> FullItemError.NoActivePrice
            | ItemLocalizationError(_, _, err) -> FullItemError.LocalizationErrors err

    let private toFullItemErrors : ItemLocalizationError[] -> Map<ItemId, FullItemError list> =
        fun errors ->
            let find i map =
                match map |> Map.tryFind i with
                | None -> []
                | Some e -> e
            let addError map (e: ItemLocalizationError)  =
                let existing = find (e.ItemId) map
                map.Add(e.ItemId, (mapItemLocalizationError e)::existing)
            errors
            |> Array.fold addError Map.empty

    let tryLocalizeCompleteProduct now product timezone language =
        let itemLocalizations =
            product.Items
            |> Array.map (fun i ->
                localizeCompleteItem now timezone language i)
        let oks = Array.choose Result.toOption itemLocalizations
        let errors = Array.choose Result.chooseError itemLocalizations |> Array.concat
        match oks, errors with
        | [||], errors ->
            NoSuccess (product.ProductId, errors)
        | oks, errors ->
            let product =
                {
                    ProductId = product.ProductId
                    Language = language
                    Variations = oks |> Array.sortBy (fun v -> v.ProductItem.Sku)
                    UnavailableVariations = toFullItemErrors errors
                }
            if errors.Length > 0 then
                PartialSuccess (product, errors)
            else
                FullSuccess product

    type ProductExportError =
        | CompletionError of MissingProductItemPart
        | LocalizationError of ItemLocalizationError
        with
            member this.ItemId() =
                match this with
                | CompletionError (ProductItem itemId)
                | CompletionError (Inventory itemId)
                | CompletionError (Price itemId)
                | CompletionError (ShipmentMapping itemId) -> itemId
                | LocalizationError (ItemLocalizationError (itemId,_,_))
                | LocalizationError (NoActivePrice itemId) -> itemId

    let completeAndLocalizeItem now item tz language =
        let completionResult = tryCompleteItem now item
        match completionResult with
        | IncompleteItem missingParts ->
            missingParts
            |> Array.map CompletionError
            |> Error
        | CompletedItem item ->
            let localizedItem = localizeCompleteItem now tz language item
            localizedItem |> Result.mapError (Array.map LocalizationError)

    let completeAndLocalizeItemForAllStores now item =
        let localizeFor = completeAndLocalizeItem now item
        {
            Moscow =
                let localizeFor = localizeFor Moscow
                {
                    Russian = localizeFor Russian
                    English = localizeFor English
                }
            Yekaterinburg =
                let localizeFor = localizeFor Yekaterinburg
                {
                    Russian = localizeFor Russian
                    English = localizeFor English
                }
            Omsk =
                let localizeFor = localizeFor Omsk
                {
                    Russian = localizeFor Russian
                    English = localizeFor English
                }
            Irkutsk =
                let localizeFor = localizeFor Irkutsk
                {
                    Russian = localizeFor Russian
                    English = localizeFor English
                }
        }

    let completeAndLocalizeProduct now product tz language =
        let completionResult = completeProduct now product |> mapError CompletionError
        match completionResult with
        | FullSuccess product ->
            tryLocalizeCompleteProduct now product tz language |> mapError LocalizationError
        | PartialSuccess (product, errs) ->
            tryLocalizeCompleteProduct now product tz language
            |> mapError LocalizationError
            |> appendErrors errs
        | NoSuccess (id, errs) -> NoSuccess (id, errs)

    let completeAndLocalizeForAllLanguages now product tz =
        Language.values
        |> Seq.map (fun s ->
            s, completeAndLocalizeProduct now product tz s)
        |> Map.ofSeq

    type ProductErrorReport =
        {
            ProductId: string
            Errors: Localized<ProductExportError[]>
            Timestamp: Instant
        }

    type ActualizedItem =
        {
            Sku: string
            IsActive: bool
            Price: CurrentPrice
            Inventory: FullInventory
            StockBalance : ItemStock
        }

    let actualizedInfo: LocalizedCompleteItem -> ActualizedItem =
        fun item ->
            {
                Sku = item.ProductItem.Sku
                Price = item.Price
                IsActive = item.Inventory.IsActive
                Inventory = item.Inventory
                StockBalance = item.Stock
            }
