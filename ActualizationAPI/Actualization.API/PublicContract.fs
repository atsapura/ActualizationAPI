namespace Actualization.API

open NodaTime
open Actualization.Domain.CoreTypes
open Actualization.Domain.LocalizedModels
open Actualization.Domain

module PublicContract =

    [<CLIMutable>]
    type ActualizeGetRequest =
        {
            ProductItemIds : string[]
            Language: Language
            Timezone: StoreTimeZone
        }

    type FullProductRequest =
        {
            SelectedVariationId : string
            VariantIds : string[]
        }

    type IdsRequest =
        {
            Ids : string[]
        }

    [<RequireQualifiedAccess>]
    type ProductRequestType =
        | FullProduct of FullProductRequest
        | ShortProducts of IdsRequest

    type ProductsRequest =
        {
            ProductRequestType : ProductRequestType
            Language : Language
            Timezone: StoreTimeZone
        }

    type BufferedWarehouseStockDto =
        {
            QuantityAvailableNow: float
            QuantityToBeAvailable: float
        }

    type ExternalWarehouseStockDto =
        {
            WarehouseId: int
            Quantity: float
        }

    type WarehouseDto =
        {
            Id: int
            IsDropshipment: bool
        }

    type OutOfStockDto =
        {
            LeadTime: int
            ShippingFromWarehouse: WarehouseDto
        }

    type DropshipmentStockDto =
        {
            WarehouseId : int
            Quantity : float
        }

    [<RequireQualifiedAccess>]
    type WarehouseStockDto =
        | Buffered of BufferedWarehouseStockDto
        | External of ExternalWarehouseStockDto

    [<RequireQualifiedAccess>]
    type AvailableStockDto =
        | InStock of WarehouseStockDto
        | Dropshipment of DropshipmentStockDto

    [<RequireQualifiedAccess>]
    type ItemStockDto =
        | Available of AvailableStockDto[]
        | OutOfStock of OutOfStockDto

    type ProductItemDto =
        {
            Sku : string
            ProductId: string
            SellingStart : Instant option

            PrimaryCategoryId : string
            CategoryIds : CategoryTree[]

            Brand : BrandShortInfo

            Warranty : string option
            CampaignIds : string[]

            Mpn : string
            ProTerm : string
            Feature : string option
            Variant : string option

            ShortDescription : string
            FullReview : FullReview

            PrimaryImageId : string
            SecondaryImageIds : string[]
            Assets : Asset[]

            Dimensions: Dimensions

            Seo : SeoUrlData

            Keywords :  string[]

            UserRating : UserRating


            TechSpecs : LocalizedTechSpec[]

            EnergyClass: EnergyClass option
            SortOrder : int option
        }

    type CurrentPriceDto =
        {
            PublicPrice: PublicPrice
            RecommendedPrice: decimal option
            VatRate: decimal
            MemberPrices: Map<MemberPriceTier, MemberSellingPrice>
        }

    type InventoryDto =
        {
            Sku : string
            OutOfStockLeadTime : int
            BackorderAvailability : BackorderAvailability option
            UnavailabilityReason : TextDeliveryNotice option
            DropShipmentOnly : bool
            IsBuyable : bool
            IsActive : bool
            DisabledForFeeds : bool
            FreightClass : int option
        }

    type CompleteItemDto =
        {
            Sku : string
            ProductItem : ProductItemDto
            Price : CurrentPriceDto
            Inventory : InventoryDto
            Stock: ItemStockDto
        }

    module Stock =

        let private bufferedStockDto : BufferedWarehouseStock -> BufferedWarehouseStockDto =
            fun stock ->
                {
                    QuantityAvailableNow = stock.QuantityAvailableNow
                    QuantityToBeAvailable = stock.QuantityToBeAvailable
                }

        let private externalStockDto : ExternalWarehouseStock -> ExternalWarehouseStockDto =
            fun stock ->
                {
                    WarehouseId = stock.WarehouseId
                    Quantity = stock.Quantity
                }

        let private warehouseDto : Warehouse -> WarehouseDto =
            fun wh ->
                {
                    Id = wh.Id
                    IsDropshipment = wh.IsDropshipment
                }

        let private outOfStockDto : OutOfStock -> OutOfStockDto =
            fun stock ->
                {
                    LeadTime = stock.LeadTime
                    ShippingFromWarehouse = stock.ShippingFromWarehouse |> warehouseDto
                }

        let private dropshipmentStockDto : DropshipmentStock -> DropshipmentStockDto =
            fun stock ->
                {
                    WarehouseId = stock.WarehouseId
                    Quantity = stock.Quantity
                }

        let private warehouseStockDto : WarehouseStock -> WarehouseStockDto =
            fun stock ->
                match stock with
                | WarehouseStock.Buffered s -> s |> bufferedStockDto |> WarehouseStockDto.Buffered
                | WarehouseStock.External s -> s |> externalStockDto |> WarehouseStockDto.External

        let private availableStockDto : AvailableStock -> AvailableStockDto =
            fun stock ->
                match stock with
                | AvailableStock.InStock s -> s |> warehouseStockDto |> AvailableStockDto.InStock
                | AvailableStock.Dropshipment s -> s |> dropshipmentStockDto |> AvailableStockDto.Dropshipment

        let itemStockDto : ItemStock -> ItemStockDto =
            fun stock ->
                match stock with
                | ItemStock.Available s -> s |> Array.map availableStockDto |> ItemStockDto.Available
                | ItemStock.OutOfStock s -> s |> outOfStockDto |> ItemStockDto.OutOfStock

    let inventoryDto : FullInventory -> InventoryDto =
        fun inventory ->
            {
                Sku = inventory.Sku
                OutOfStockLeadTime = inventory.OutOfStockLeadTime
                BackorderAvailability = inventory.BackorderAvailability
                UnavailabilityReason = inventory.TextDeliveryNotice
                DropShipmentOnly = inventory.DropShipmentOnly
                IsBuyable = inventory.IsBuyable
                IsActive = inventory.IsActive
                DisabledForFeeds = inventory.DisabledForFeeds
                FreightClass = inventory.FreightClass
            }

    let currentPriceDto : CurrentPrice -> CurrentPriceDto =
        fun price ->
            {
                PublicPrice = price.PublicPrice
                MemberPrices = price.MemberPrices
                RecommendedPrice = price.RecommendedPrice
                VatRate = price.VatRate
            }

    let productItemDto : LocalizedProductItem -> ProductItemDto =
        fun item ->
            {
                Sku = item.Sku
                ProductId = item.ProductId
                PrimaryCategoryId = item.PrimaryCategoryId
                PrimaryImageId = item.PrimaryImageId
                SellingStart = item.SellingStart
                CategoryIds = item.CategoryIds
                Brand = item.Brand
                Warranty = item.Warranty
                CampaignIds = item.CampaignIds
                Mpn = item.Mpn
                ProTerm = item.ProTerm
                Feature = item.Feature
                Variant = item.Variant
                ShortDescription = item.ShortDescription
                FullReview = item.FullReview
                SecondaryImageIds = item.SecondaryImageIds
                Assets = item.Assets
                Dimensions = item.Dimensions
                Seo = item.Seo
                Keywords = item.Keywords
                UserRating = item.UserRating
                TechSpecs = item.TechSpecs
                EnergyClass = item.EnergyClass
                SortOrder = item.SortOrder
            }

    let completeItemDto : LocalizedCompleteItem -> CompleteItemDto =
        fun item ->
            {
                Price = item.Price |> currentPriceDto
                Inventory = item.Inventory |> inventoryDto
                Stock = item.Stock |> Stock.itemStockDto
                Sku = item.ProductItem.Sku
                ProductItem = item.ProductItem |> productItemDto
            }
