namespace Actualization.Domain

open NodaTime
open CoreTypes
open FsToolkit.ErrorHandling
open System

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
            Price: ProductItemPricePool
            Inventory: FullInventory
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

    [<RequireQualifiedAccess>]
    type FullItemError =
        | LocalizationErrors of ProductItemLocalizationError list
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

    let localizeProductItem : ProductItem -> Language -> Validation<LocalizedProductItem, ProductItemLocalizationError> =
        fun item language ->
            let localize map = Map.tryFind language map
            let notEmpty = Option.filter (not << String.IsNullOrWhiteSpace)
            validation {
                let! proterm = localize item.ProTerm |> notEmpty |> Result.requireSome (MissingField Proterm)
                and! fullReview = localize item.FullReview |> Result.requireSome (MissingField FullReview)
                and! shortDescription = localize item.ShortDescription |> notEmpty |> Result.requireSome (MissingField ShortDescription)
                and! seo = localize item.Seo |> Result.requireSome (MissingField Seo)
                and! mpn = Some item.Mpn |> notEmpty |> Result.requireSome (MissingField Mpn)

                let variant = localize item.Variant
                let feature = localize item.Feature
                let userRating = localize item.UserRating |> Option.defaultValue UserRating.defaultValue
                let keywords = localize item.Keywords |> Option.defaultValue [||]
                let techSpecs = localizeAllSpecs item.TechSpecs language
                return
                    {
                        Sku = item.Sku
                        ProductId = item.ProductId
                        ProTerm = proterm
                        Mpn = mpn
                        PrimaryCategoryId = item.PrimaryCategoryId
                        CategoryIds = item.CategoryIds
                        SellingStart = item.SellingStartDate
                        Brand = item.Brand
                        Assets = item.Assets
                        TechSpecs = techSpecs
                        Feature = feature
                        Variant = variant
                        CampaignIds = item.CampaignIds
                        ShortDescription = shortDescription
                        FullReview = fullReview
                        PrimaryImageId = item.PrimaryImageId
                        SecondaryImageIds = item.SecondaryImageIds
                        Dimensions = item.Dimensions
                        Seo = seo
                        Keywords = keywords
                        UserRating = userRating
                        SortOrder = item.SortOrder
                        Warranty = item.Warranty
                        EnergyClass = item.EnergyClass
                    }
            }

    let private warehouseStock warehouseId (stockBalance : WarehouseStockBalance) =
        let bufferedWh = WarehouseId.warehousesWithLeadtime.Contains warehouseId
        let totalQuantity =
            if bufferedWh then
                stockBalance.QuantityToBeAvailable + stockBalance.QuantityAvailableNow
            else stockBalance.QuantityAvailableNow
        if totalQuantity <= 0. then
            None
        elif bufferedWh then
            {
                BufferedWarehouseStock.QuantityAvailableNow = stockBalance.QuantityAvailableNow
                QuantityToBeAvailable = stockBalance.QuantityToBeAvailable
            } |> WarehouseStock.Buffered |> AvailableStock.InStock |> Some
        else
            {
                ExternalWarehouseStock.Quantity = stockBalance.QuantityAvailableNow
                WarehouseId = warehouseId
            } |> WarehouseStock.External |> AvailableStock.InStock |> Some

    let itemStock (stockBalance : ProductItemStockBalance option) =
        let stockBalances =
            stockBalance
            |> Option.map (fun s -> s.StockBalance)
            |> Option.defaultValue Map.empty

        let warehouseIds = stockBalances.Keys |> WarehouseId.sanitize
        let outOfStockWarehouseId =
            warehouseIds |> WarehouseId.pickWarehouseForOutOfStock

        let outOfStockWarehouse =
            {
                Warehouse.Id = outOfStockWarehouseId
                IsDropshipment = WarehouseId.isDropshipmentWarehouse outOfStockWarehouseId
            }
        match warehouseIds with
        | [||] -> ItemStock.OutOfStock { LeadTime = 0; ShippingFromWarehouse = outOfStockWarehouse }
        | _ ->
            let available =
                [|
                    for warehouseId in warehouseIds do
                        if WarehouseId.isDropshipmentWarehouse warehouseId then
                            let quantity =
                                stockBalances
                                |> Map.tryFind warehouseId
                                |> Option.map (fun s -> s.QuantityAvailableNow)
                            match quantity with
                            | None -> ()
                            | Some quantity ->
                                if quantity > 0. then
                                    {
                                        DropshipmentStock.WarehouseId = warehouseId
                                        Quantity = quantity
                                    } |> AvailableStock.Dropshipment
                                else
                                    ()
                        else
                            let stock =
                                optional {
                                    let! stockBalance = stockBalances.TryFind warehouseId
                                    let! result = warehouseStock warehouseId stockBalance
                                    return result
                                }
                            match stock with
                            | None -> ()
                            | Some stock -> stock

                |]

            match available with
            | [||] -> ItemStock.OutOfStock { LeadTime = 0; ShippingFromWarehouse = outOfStockWarehouse }
            | available -> ItemStock.Available available
