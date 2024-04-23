namespace Actualization.Domain

[<RequireQualifiedAccess>]
module WarehouseId =

    let [<Literal>] Main = 1

    // This should never be used, since workflow implies that
    // empty warehouse list doesn't get through validation process,
    // however type system can't express it directly easily.
    let [<Literal>] DefaultOosWarehouse = -1

    let warehousesWithLeadtime = [| Main |] |> Set.ofArray

    let sanitize (warehouseIds: #seq<int>) =
        match warehouseIds |> Array.ofSeq with
        | null | [||] -> [||]
        | warehouseIds ->
            warehouseIds
            |> Array.where (fun w -> w <> 230) // we are told to ignore wh 230. 13.06.2023

    let isDropshipmentWarehouse warehouseId =
        warehouseId = 0 || warehouseId >= 100000

    let pickWarehouseForOutOfStock warehouseIds =
        match warehouseIds with
        | null | [||] -> DefaultOosWarehouse
        | warehouseIds ->
            optional {
                return! warehouseIds |> Array.where isDropshipmentWarehouse |> Array.tryLast
                return! warehouseIds |> Array.tryHead
            } |> Option.defaultValue DefaultOosWarehouse