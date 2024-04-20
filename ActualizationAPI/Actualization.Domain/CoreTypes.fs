namespace Actualization.Domain

open NodaTime

module CoreTypes =

    type Language =
        | Russian
        | English
        static member ofString (x:string) =
            match (string x).ToLower() with
            | "russian" | "rus" -> Some Russian
            | "english" | "eng" -> Some English
            | _ -> None
        static member op_Explicit (x:string) =
            Language.ofString x
            |> Option.defaultWith (fun () -> failwith $"Invalid Language value: {x}")

    type 'A Localized = Map<Language, 'A>

    type StringSpecValue =
        {
            Value: string
            LocalizedValues: Localized<string>
        }

    type NumberSpecValue =
        {
            Value: float
            MeasureUnit: string option
            LocalizedValues: Localized<string>
            LocalizedUnitNames: Localized<string> option
        }

    [<RequireQualifiedAccess>]
    type RichSpecValue =
        | Number of NumberSpecValue
        | Text of StringSpecValue

    [<RequireQualifiedAccess>]
    type SpecValue =
        | Bool of bool
        | Single of RichSpecValue
        | Multi of RichSpecValue[]

    type TechSpec =
        {
            SpecName: string
            SpecValue: SpecValue
            SpecLocalization: Localized<string>
        }

    type Asset =
        {
            Id : string
            Name : string
            FileName : string
        }

    type WeightInfo =
        {
            Tara: float
            Netto: float
            Gross: float
        }

    type Dimensions =
        {
            Height: float
            Length: float
            Width: float
            Weight: WeightInfo
            FreightClass: string
        }

    type CategoryTree =
        {
            CategoryId: string
            IsDefault: bool
            ParentCategory: CategoryTree option
        }

    type BrandShortInfo =
        {
            Id : string
            Name : string
        }

    type FullReview =
        {
            CampaignIntroText : string option
            LongDescription : string
            UniqueSellingPoints : string[]
        }

    type UserRating =
        {
            Rating : float
            ReviewsCount : int
            IsVisible : bool
        }
        static member defaultValue =
            {
                UserRating.IsVisible = false
                ReviewsCount = 0
                Rating = 0.
            }

    type PageUrl =
        {
            IsActive : bool
            Slug : string
        }

    type SeoData =
        {
            PageTitle : string
            MetaDescription : string
            Urls : PageUrl[]
        }

    type SeoUrlData =
        {
            Urls : PageUrl[]
        }

    [<RequireQualifiedAccess>]
    type EnergyClassLabel =
        | A | B | C | D | E | F | G
        static member ofString (v : string) =
            match (string v).ToLower() with
            | "a" -> Some A
            | "b" -> Some B
            | "c" -> Some C
            | "d" -> Some D
            | "e" -> Some E
            | "f" -> Some F
            | "g" -> Some G
            | _ -> None
        static member op_Explicit x =
            EnergyClassLabel.ofString x
            |> Option.defaultWith (fun () -> failwithf "Incorrect energy class value %s" x)

    type EnergyClass =
        {
            Label: EnergyClassLabel
            Description : Localized<string>
        }

    type ProductItem =
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

    type OriginalPrice = // PriceLogEntry
        {
            PriceId: string
            PriceListId: int
            Value: decimal
            VatRate: decimal
        }

    type MemberSellingPrice =
        {
            Value: decimal
            PublicPriceDiscount: float
            ListPriceDiscount: float
        }

    type Price =
        {
            PriceId: string
            PriceListId: int
            Value: decimal
            VatRate : decimal
            Start: Instant option
            End: Instant option
        }

    [<RequireQualifiedAccess>]
    type MemberPriceTier =
        | Standard
        | Gold
        static member values = [|Standard|]
        static member ofString s =
            match (string s).ToLower() with
            | "standard" -> Some Standard
            | "gold" -> Some Gold
            | _ -> None
        // for newtonsoft deserialization of map
        static member op_Explicit (str: string) =
            MemberPriceTier.ofString str
            |> Option.defaultWith (fun () -> failwithf "unexpected value for MemberPriceTier %s" str)

    type LimitedPrice =
        {
            Price: Price
            Quantity: int
        }

    type MemberPrice =
        {
            Price: Price
            Tier: MemberPriceTier
        }

    type ListPrice =
        {
            Price: Price
            RecommendedPrice: decimal option
        }

    type PriceLog = Map<LocalDate, OriginalPrice>

    type ProductItemPrice = // ProductItemPricePool
        {
            ItemId: string
            ListPrices: ListPrice[]
            CampaignPrices: Price[]
            LimitedPrices: LimitedPrice[]
            MemberPrices: MemberPrice[]
            SellingPriceHistory: PriceLog
        }
        static member Empty itemId =
            {
                ItemId = itemId
                ListPrices = [||]
                CampaignPrices = [||]
                MemberPrices = [||]
                LimitedPrices = [||]
                SellingPriceHistory = Map.empty
            }

    type DiscountPriceValue =
        {
            Amount: decimal
            ListPrice: decimal
            Discount: float
            LowestHistoricPrice: OriginalPrice
            PriceId: string
            PriceListId: int
        }

    type LimitedCampaignPriceValue =
        {
            Price: DiscountPriceValue
            Quantity: int
        }

    type ListPriceValue =
        {
            Amount: decimal
            PriceId: string
            PriceListId: int
        }

    type LimitedCampaignSellingPrice =
        {
            DefaultPrice: LimitedCampaignPriceValue
            FallbackPrice: PublicPrice
        }

    and PublicPrice =
        | ListPrice of ListPriceValue
        | CampaignPrice of DiscountPriceValue
        | LimitedCampaignPrice of LimitedCampaignSellingPrice

    [<RequireQualifiedAccess>]
    module PublicPrice =

        let amount p =
            match p with
            | ListPrice p -> p.Amount
            | CampaignPrice cp -> cp.Amount
            | LimitedCampaignPrice lp -> lp.DefaultPrice.Price.Amount

        let priceId p =
            match p with
            | ListPrice p -> p.PriceId
            | CampaignPrice cp -> cp.PriceId
            | LimitedCampaignPrice lp -> lp.DefaultPrice.Price.PriceId

        let priceListId p =
            match p with
            | ListPrice p -> p.PriceListId
            | CampaignPrice cp -> cp.PriceListId
            | LimitedCampaignPrice lp -> lp.DefaultPrice.Price.PriceListId

        let relativeDiscount p =
            match p with
            | CampaignPrice cp -> cp.Discount
            | ListPrice _ -> 0.
            | LimitedCampaignPrice lp -> lp.DefaultPrice.Price.Discount

        let absoluteDiscount p =
            match p with
            | CampaignPrice cp ->
                cp.ListPrice - cp.Amount
            | ListPrice _ -> 0m
            | LimitedCampaignPrice lp ->
                let value = lp.DefaultPrice.Price.Amount
                let baseline = value / (1. - (lp.DefaultPrice.Price.Discount/100.) |> decimal)
                baseline - value

        let isCampaign p =
            match p with
            | ListPrice _ -> false
            | CampaignPrice _
            | LimitedCampaignPrice _ -> true

    type CurrentPrice =
        {
            PublicPrice: PublicPrice
            RecommendedPrice: decimal option
            VatRate: decimal
            MemberPrices: Map<MemberPriceTier, MemberSellingPrice>
        }

    type TextDeliveryNotice = // UnavailabilityReason
        | OutOfSupply
        | NotAvailable
        | OutForSeason
        //| LongDelivery

    type WarehouseBalance =
        {
            Balance : float
            Timestamp : Instant
        }

    type WarehouseStockBalance =
        {
            QuantityAvailableNow : float
            QuantityToBeAvailable : float
            Timestamp : Instant
        }

    type WarehouseId = int

    type AvailabilityKind =
        | PartialBackorderCoverage
        | SupplierPredicted
        | SupplierConfirmed
        | DeliveredToWarehouse

    [<RequireQualifiedAccess>]
    type OrderExceptionCode =
        | UnconfirmedDate

    type CalculatedBackorderAvailability =
        {
            AvailabilityDate : LocalDate
            AvailabilityKind : AvailabilityKind
            QuantityToBeAvailable : float
            ExceptionCode : OrderExceptionCode option
        }

    [<RequireQualifiedAccess>]
    type BackorderAvailability =
        | Calculated of CalculatedBackorderAvailability
        | Manual of backorderAvailabilityDate : LocalDate
        member this.Date =
            match this with
            | Calculated { AvailabilityDate = date }
            | Manual date -> date

    type ProductItemBackorderAvailability =
        {
            ProductItemId : string
            BackorderAvailability : CalculatedBackorderAvailability option
            Timestamp : Instant
        }

    type ProductItemStockBalance =
        {
            ProductItemId : string
            StockBalance : Map<WarehouseId, WarehouseStockBalance>
        }
        with
        static member createDefault productItemId =
            {
                ProductItemStockBalance.ProductItemId = productItemId
                StockBalance = Map.empty
            }

    type FullInventory =
        {
            ProductItemId : string
            InStockMessage : int // Values are numbers mapped to translated values on Storefront
            OutOfStockMessage : int
            BackorderAvailability : BackorderAvailability option

            // UnavailabilityReason
            TextDeliveryNotice : TextDeliveryNotice option
            DropShipmentOnly : bool
            IsBuyable : bool
            IsActive : bool
            DisabledForFeeds : bool
            FreightClass : int option
        }
