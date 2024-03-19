namespace Actualization.Domain

open NodaTime
open CoreTypes

module LocalizedModels =

    type LocalizedStringSpecValue =
        {
            Value: string
            LocalizedValue: string option
        }

    type LocalizedNumberSpecValue =
        {
            Value: float
            MeasureUnit: string option
            LocalizedValue: string option
            LocalizedUnitName: string option
        }

    [<RequireQualifiedAccess>]
    type LocalizedRichSpecValue =
        | Number of LocalizedNumberSpecValue
        | Text of LocalizedStringSpecValue

    [<RequireQualifiedAccess>]
    type LocalizedSpecValue =
        | Bool of bool
        | Single of LocalizedRichSpecValue
        | Multi of LocalizedRichSpecValue[]

    type LocalizedTechSpec =
        {
            SpecName: string
            SpecValue: LocalizedSpecValue
            SpecLocalization: string option
        }

    type LocalizedProductItem =
        {
            Sku : string
            ProductId: string
            SellingStartDate : Instant option

            PrimaryCategoryId : string // CategoryId from the blue STEP structure. Not sure if we need it.
            CategoryIds : CategoryTree[]

            Brand : BrandShortInfo

            Warranty : string option
            CampaignIds : string[] // Former Marketing tags

            Mpn : string
            ProTerm : string
            Feature : string option
            Variant : string option

            ShortDescription : string
            FullReview : FullReview

            PrimaryImageId : string
            SecondaryImageIds : string[]
            Assets : Asset[]

            Dimensions: Dimensions // Must have at least default value

            Seo : SeoUrlData

            Keywords :  string[] // Optional for each store

            UserRating : UserRating


            TechSpecs : LocalizedTechSpec[]

            BlogPageSlugs: string[]

            EnergyClass: EnergyClass option
            SortOrder : int option
        }

    type BufferedWarehouseStock =
        {
            QuantityAvailableNow: float
            QuantityToBeAvailable: float
        }

    type ExternalWarehouseStock =
        {
            WarehouseId: int
            Quantity: float
        }

    type Warehouse =
        {
            Id: int
            IsDropshipment: bool
        }

    type OutOfStock =
        {
            LeadTime: int
            ShippingFromWarehouse: Warehouse
        }

    type DropshipmentStock =
        {
            WarehouseId : int
            Quantity : float
        }

    [<RequireQualifiedAccess>]
    type WarehouseStock =
        | Buffered of BufferedWarehouseStock
        | External of ExternalWarehouseStock

    [<RequireQualifiedAccess>]
    type AvailableStock =
        | InStock of WarehouseStock
        | Dropshipment of DropshipmentStock
        static member quantity availableStock =
            match availableStock with
            | InStock (WarehouseStock.Buffered b) -> b.QuantityToBeAvailable + b.QuantityAvailableNow
            | InStock (WarehouseStock.External { Quantity = q })
            | Dropshipment { Quantity = q } -> q

    [<RequireQualifiedAccess>]
    type ItemStock =
        | Available of AvailableStock[]
        | OutOfStock of OutOfStock
        member this.IsInStock() =
            match this with
            | OutOfStock _ -> false
            | Available stocks ->
                let warehousesAndDropshipmentsWithQuantity =
                    stocks |> Array.where (
                        function
                        | AvailableStock.Dropshipment { Quantity = 0. } -> false
                        | _ -> true)
                match warehousesAndDropshipmentsWithQuantity with
                | [||] -> false
                | _ -> true
        member this.InStockQuantity() =
            match this with
            | OutOfStock _ -> 0.
            | Available stocks ->
                stocks
                |> Array.tryFind (fun s -> AvailableStock.quantity s > 0.)
                |> Option.map (fun s -> AvailableStock.quantity s)
                |> Option.defaultValue 0.

    type ProductId = string
    type ItemId = string

    type CompleteProductItem =
        {
            ProductItem: ProductItem
            Price: ProductItemPrice
            Inventory: FullInventory
            WasLocalizedFor : Set<Language>
            StockBalance : ProductItemStockBalance option
        }

    type CompleteProduct =
        {
            ProductId: ProductId
            Items: CompleteProductItem[]
        }

    type LocalizedCompleteItem =
        {
            ProductItem: LocalizedProductItem
            Price: CurrentPrice
            Inventory: FullInventory
            Stock : ItemStock
            Language: Language
        }

    type ProductItemRequiredField =
        | Proterm
        | Mpn
        | FullReview
        | ShortDescription
        | Seo
        | CategoryIds

    type AdditionalLocalizationRules =
        | HasToBeStockedOrInStockForVp

    type ProductItemLocalizationError =
        | MissingField of ProductItemRequiredField

    type InventoryLocalizationError =
        | UnavailableForStore
        | LocalizationRulesViolated of AdditionalLocalizationRules
        | ErpStatusFalse

    type StockBalanceLocalizationError =
        | MissingB2BShipmentMapping
        | MissingB2CShipmentMapping

    [<RequireQualifiedAccess>]
    type FullItemError =
        | LocalizationErrors of ProductItemLocalizationError list
        | AvailabilityErrors of InventoryLocalizationError list
        | StockBalanceLocalizationError of StockBalanceLocalizationError list
        | NoActivePrice

    type LocalizedCompleteProduct =
        {
            ProductId: ProductId
            Language: Language
            Variations: LocalizedCompleteItem[]
            UnavailableVariations: Map<ItemId, FullItemError list>
        }

    type ProductForFanout =
        {
            ProductId: ProductId
            LocalizedProducts: Localized<LocalizedCompleteProduct>
        }

    let localizeStringTechSpec (stringSpec: StringSpecValue) storeCode =
        {
            LocalizedStringSpecValue.Value = stringSpec.Value
            LocalizedValue = stringSpec.LocalizedValues.TryFind storeCode
        }

    let localizeNumberSpec (numberSpec: NumberSpecValue) language =
        {
            LocalizedNumberSpecValue.Value = numberSpec.Value
            LocalizedValue = numberSpec.LocalizedValues.TryFind language
            MeasureUnit = numberSpec.MeasureUnit
            LocalizedUnitName =
                numberSpec.LocalizedUnitNames
                |> Option.bind (fun ln -> ln.TryFind language)
        }

    let localizeRichTechSpec spec storeCode =
        match spec with
        | RichSpecValue.Number n ->
            localizeNumberSpec n storeCode
            |> LocalizedRichSpecValue.Number
        | RichSpecValue.Text t ->
            localizeStringTechSpec t storeCode
            |> LocalizedRichSpecValue.Text

    let localizeSpecValue spec storeCode =
        match spec with
        | SpecValue.Bool b -> LocalizedSpecValue.Bool b |> Some
        | SpecValue.Single r ->
            localizeRichTechSpec r storeCode
            |> LocalizedSpecValue.Single
            |> Some
        | SpecValue.Multi m ->
            let localized = m |> Array.map (fun r -> localizeRichTechSpec r storeCode)
            match localized with
            | [||] -> None
            | l -> l |> LocalizedSpecValue.Multi |> Some

    let localizeSpec (spec: TechSpec) storeCode =
        optional {
            let! localizedValue = localizeSpecValue spec.SpecValue storeCode
            return
                {
                    LocalizedTechSpec.SpecName = spec.SpecName
                    SpecValue = localizedValue
                    SpecLocalization = spec.SpecLocalization.TryFind storeCode
                }
        }

    let localizeAllSpecs specs storeCode =
        specs |> Array.choose (fun s -> localizeSpec s storeCode)
