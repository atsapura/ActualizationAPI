[<AutoOpen>]
module Time

open NodaTime

type StoreTimeZone =
    | Moscow // Europe
    | Yekaterinburg // Asia
    | Irkutsk // Asia
    | Omsk // Asia

let TimeZoneProvider = DateTimeZoneProviders.Tzdb
let [<Literal>] private MoscowTz = "Europe/Moscow"
let [<Literal>] private YekaterinburgTz = "Asia/Yekaterinburg"
let [<Literal>] private IrkutskTz = "Asia/Irkutsk"
let [<Literal>] private OmskTz = "Asia/Omsk"

let private storeTimezoneName store =
    match store with
    | Moscow -> MoscowTz
    | Yekaterinburg -> YekaterinburgTz
    | Irkutsk -> IrkutskTz
    | Omsk -> OmskTz

let storeTimezone store = TimeZoneProvider[storeTimezoneName store]

let storeTime store (instant: Instant) =
    let tz = storeTimezone store
    instant.InZone tz

let localDate store instant =
    (storeTime store instant).Date

let inStoreTimeZone store (localDateTime: LocalDateTime) =
    let tz = storeTimezone store
    localDateTime.InZoneLeniently tz

let inErpTimeZone (localDateTime: LocalDateTime) =
    TimeZoneProvider[MoscowTz] |> localDateTime.InZoneLeniently

let currentMoment() = SystemClock.Instance.GetCurrentInstant()



let [<Literal>] MaxShippingDays = 52

[<RequireQualifiedAccess>]
module TimeFormatting =

    open NodaTime.Text
    let [<Literal>] private DateFormat = "dd'.'MM'.'uuuu"
    let private dateFormatter = LocalDatePattern.CreateWithInvariantCulture DateFormat

    let localDate date =
        dateFormatter.Format date

    let parseLocalTime timeStr =
        let timePattern = LocalTimePattern.Create("HHmm", System.Globalization.CultureInfo.InvariantCulture)
        let result = timePattern.Parse timeStr
        if result.Success
        then Ok result.Value
        else Error result.Exception
