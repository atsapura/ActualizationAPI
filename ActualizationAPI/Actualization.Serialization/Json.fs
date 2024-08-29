namespace Actualization.Serialization

open System
open NodaTime
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Microsoft.FSharpLu.Json
open Microsoft.FSharpLu.Json.Compact
open NodaTime.Serialization.JsonNet
open Newtonsoft.Json.Converters

module Json =

    let cosmosSettings =
        let jsonSettings = JsonSerializerSettings()
        jsonSettings.ContractResolver <- CamelCasePropertyNamesContractResolver()
        jsonSettings.Converters.Add(CompactUnionJsonConverter())
        jsonSettings.ConfigureForNodaTime TimeZoneProvider

    type UnixTimeConverter() =
        inherit JsonConverter()
        let dtConverter = UnixDateTimeConverter()

        override _.CanConvert(t) =
            t = typeof<Instant> || t = typeof<Option<Instant>> || t = typeof<Nullable<Instant>>
        override _.ReadJson(reader, t, existingValue, serializer) =
            if reader.TokenType = JsonToken.Null then
                if t = typeof<Option<Instant>> then
                    box None
                elif t = typeof<Nullable<Instant>> then
                    Nullable() |> box
                else failwith "unexpected null in json"
            else
                let dt = dtConverter.ReadJson(reader, t, existingValue, serializer)
                let dt = (dt :?> DateTime) |> Instant.FromDateTimeUtc
                if t = typeof<Instant> then
                    box dt
                elif t = typeof<Option<Instant>> then
                    Some dt |> box
                elif t = typeof<Nullable<Instant>> then
                    Nullable dt |> box
                else
                    failwith "IMPOSIBRU"
        override _.WriteJson(writer, value, serializer) =
            let value =
                match value with
                | :? Instant as v -> v.ToDateTimeUtc() |> Nullable
                | :? Option<Instant> as v -> v |> Option.map (fun x -> x.ToDateTimeUtc()) |> Option.toNullable
                | :? Nullable<Instant> as v ->
                    if v.HasValue then v.Value.ToDateTimeUtc() |> Nullable
                    else Nullable()
                | _ -> failwith "IMPOSIBRUUUU"
            dtConverter.WriteJson(writer, value, serializer)
            ()

