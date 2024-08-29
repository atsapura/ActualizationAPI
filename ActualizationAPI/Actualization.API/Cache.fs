namespace Actualization.API

open NodaTime
open Microsoft.Extensions.Caching.Memory
open Actualization.Domain.CoreTypes
open Actualization.Domain.LocalizedModels
open Actualization.Domain
open Actualization.API.StatefulCatalog

[<RequireQualifiedAccess>]
module Cache =

    let tryRemove<'T> (memoryCache: IMemoryCache) key =
        match memoryCache.TryGetValue<'T>(key) with
        | true, value ->
            memoryCache.Remove key |> ignore
            Some value
        | false, _ -> None

    let countEntries (memoryCache : IMemoryCache) =
        match memoryCache with
        | :? MemoryCache as memoryCache -> memoryCache.Count
        | _ -> 0

    let private preserveInventoryKey productItemId = $"preserve-inventory-%s{productItemId}"
    let private preserveBackorderAvailabilityKey productItemId = $"preserve-backorder-availability-%s{productItemId}"
    let private preservePriceKey productItemId = $"preserve-price-%s{productItemId}"
    let private preserveStockBalanceKey productItemId = $"preserve-stock-balance-%s{productItemId}"

    let preserveLostInventory (memoryCache: IMemoryCache) (inventory: FullInventory) =
        memoryCache.Set(preserveInventoryKey inventory.Sku, inventory)

    let preserveLostBackorderAvailability (memoryCache: IMemoryCache) (backorderAvailability: ProductItemBackorderAvailability) =
        memoryCache.Set(preserveBackorderAvailabilityKey backorderAvailability.ProductItemId, backorderAvailability)

    let preserveLostPrice (memoryCache: IMemoryCache) (price: ProductItemPricePool) =
        memoryCache.Set(preservePriceKey price.ItemId, price)

    let preserveLostStockBalance (memoryCache: IMemoryCache) (stockBalance: ProductItemStockBalance) =
        memoryCache.Set(preserveStockBalanceKey stockBalance.Sku, stockBalance)

    let tryRemovePreservedInventory memoryCache productItemId =
        let key = preserveInventoryKey productItemId
        tryRemove<FullInventory> memoryCache key

    let tryRemovePreservedBackorderAvailability memoryCache productItemId =
        let key = preserveBackorderAvailabilityKey productItemId
        tryRemove<ProductItemBackorderAvailability> memoryCache key

    let tryRemovePreservedPrice memoryCache productItemId =
        let key = preservePriceKey productItemId
        tryRemove<ProductItemPricePool> memoryCache key

    let tryRemovePreservedStockBalance memoryCache productItemId =
        let key = preserveStockBalanceKey productItemId
        tryRemove<ProductItemStockBalance> memoryCache key

    let private itemInfoKey itemId = $"item_%s{itemId}"

    let setItemInfo : IMemoryCache -> ItemInfo -> unit =
        fun cache item ->
            cache.Set(itemInfoKey item.Sku, item)
            |> ignore

    let removeItemInfo : IMemoryCache -> ItemId -> ItemInfo option =
        fun cache itemId ->
            let key = itemInfoKey itemId
            tryRemove<ItemInfo> cache key

    let getItemInfo : IMemoryCache -> ItemId -> ItemInfo option =
        fun cache itemId ->
            match cache.TryGetValue<ItemInfo>(itemInfoKey itemId) with
            | true, value -> Some value
            | false, _ -> None
    let private versionKey = "version"

    let setVersion (cache: IMemoryCache) (v: string) =
        cache.Set(versionKey, v) |> ignore

    let getVersion (cache: IMemoryCache) =
        match cache.TryGetValue<string> versionKey with
        | true, version -> version
        | false, _ -> "no version found in cache"

