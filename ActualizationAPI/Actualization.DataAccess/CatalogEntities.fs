namespace Actualization.DataAccess

open Newtonsoft.Json
open System.Text
open NodaTime
open System
open Actualization.Serialization.Json
open Actualization.Domain.CoreTypes

module CatalogEntities =

    let [<Literal>] ProductItemsCollectionName = "product-item"
    let [<Literal>] PriceCollectionName = "product-item-price"
    let [<Literal>] InventoryCollectionName = "inventory"
    let [<Literal>] ProductItemStockBalanceCollectionName = "product-item-stock-balance"
    let [<Literal>] ProductItemBackorderAvailabilityCollectionName = "product-item-backorder-availability"

    [<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct, AllowMultiple = false, Inherited = false)>]
    type CollectionNameAttribute(name: string) =
        inherit Attribute()
        member this.Name = name

    let private toCollectionName (str: string) =
        let sb = StringBuilder()
        for c in str do
            if Char.IsUpper c && sb.Length > 0 then
                sb.Append('-') |> ignore
            sb.Append(Char.ToLower c) |> ignore
        sb.ToString()

    let getCollectionName<'a>() =
        let aType = typeof<'a>
        let attribute =
            aType.GetCustomAttributes(typeof<CollectionNameAttribute>, false)
            |> Array.tryHead
            |> Option.map (fun a -> a :?> CollectionNameAttribute)
        match attribute with
        | Some attr -> attr.Name
        | None -> toCollectionName aType.Name

    [<Interface>]
    type ICosmosEntity =
        abstract member EntityId: unit -> string
        abstract member PartitionKey: unit -> string

    type CosmosDocument<'a when 'a:> ICosmosEntity> =
        {
            [<JsonProperty("id")>]
            Id: string
            [<JsonProperty("_etag")>]
            Etag: string
            [<JsonProperty("_ts")>]
            [<JsonConverter(typeof<UnixTimeConverter>)>]
            Timestamp: Instant
            Payload: 'a
            DocumentPartition: string
            DeletedAt: Instant option
        }
        with
        [<JsonIgnore>]
        member this.IsDeleted = this.DeletedAt.IsSome
        static member ofEntityWithEtag (e: #ICosmosEntity, etag) =
            {
                Id = e.EntityId()
                DocumentPartition = e.PartitionKey()
                Etag = etag
                Payload = e
                DeletedAt = None
                Timestamp = Instant() // Ignored by cosmos anyway
            }
        static member ofEntity (e: #ICosmosEntity) =
            CosmosDocument<_>.ofEntityWithEtag(e, null)

    [<CollectionName(ProductItemsCollectionName)>]
    type ProductItemEntity =
        {
            Sku : string
            ProductId: string
            SellingStartDate : Instant option

            PrimaryCategoryId : string
            CategoryIds : CategoryTree[]

            Brand : BrandShortInfo

            CampaignIds : string[]
            Mpn : string
            ProTerm : Localized<string>
            Feature : Localized<string> // Optional for product item
            Variant : Localized<string> // Optional for product item

            ShortDescription : Localized<string>
            FullReview : Localized<FullReview>

            PrimaryImageId : string
            SecondaryImageIds : string[]
            Assets : Asset[]

            Dimensions: Dimensions

            Seo : Localized<SeoUrlData>

            Keywords : Localized<string[]>

            UserRating : Localized<UserRating>

            TechSpecs : TechSpec[]

            EnergyClass: EnergyClass option

            SortOrder : int option
            Warranty : string option
        }

    type PriceLogEntryEntity =
        {
            PriceId: string
            PriceListId: int
            Value: decimal
            VatRate: decimal
        }

    type MemberSellingPriceEntity =
        {
            Value: decimal
            PublicPriceDiscount: float
            ListPriceDiscount: float
        }

    type PriceEntity =
        {
            PriceId: string
            PriceListId: int
            Value: decimal
            VatRate : decimal
            Start: Instant option
            End: Instant option
        }

    type LimitedPriceEntity =
        {
            Price: PriceEntity
            Quantity: int
        }

    type MemberPriceEntity =
        {
            Price: PriceEntity
            Tier: MemberPriceTier
        }

    type ListPriceEntity =
        {
            Price: PriceEntity
            RecommendedPrice: decimal option
        }

    type PriceLogEntity = Map<LocalDate, PriceLogEntryEntity>

    type ProductItemPricePoolEntity = // ProductItemPricePool
        {
            ItemId: string
            ListPrices: ListPriceEntity[]
            CampaignPrices: PriceEntity[]
            LimitedPrices: LimitedPriceEntity[]
            MemberPrices: MemberPriceEntity[]
            SellingPriceHistory: PriceLogEntity
        }

    let private priceToEntity : Price -> PriceEntity =
        fun price ->
            {
                PriceId = price.PriceId
                PriceListId = price.PriceListId
                Value = price.Value
                VatRate = price.VatRate
                Start = price.Start
                End = price.End
            }

    let private priceToDomain : PriceEntity -> Price =
        fun price ->
            {
                PriceId = price.PriceId
                PriceListId = price.PriceListId
                Value = price.Value
                VatRate = price.VatRate
                Start = price.Start
                End = price.End
            }
