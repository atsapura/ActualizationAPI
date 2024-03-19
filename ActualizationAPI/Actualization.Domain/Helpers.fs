namespace global

[<AutoOpen>]
module Helpers =

    open System
    // This wraps ugly results of <type>.TryParse methods to an option
    let inline tryParse<'a when 'a: (static member TryParse: string * byref<'a> -> bool)> x =
        let mutable res = Unchecked.defaultof<'a>
        if (^a: (static member TryParse: string * byref<'a> -> bool) (x, &res))
        then Some res
        else None

    let inline (|HasId|) x =
        fun () -> (^a: (member Id : string) x)
    let inline idProp (HasId f) = f()
    let inline toMapById l = Seq.map (fun e -> idProp e, e) l |> Map.ofSeq

    let inline force (Lazy a) = a

    type OptionalBuilder() =
        member __.Bind(opt, f) =
            match opt with
            | Some x -> f x
            | None -> None
        member _.Delay f = f
        member _.Run f = f()
        member _.Zero() = Some ()
        member _.Return x = Some x
        member _.ReturnFrom x = x
        member _.Combine (a, b) =
            match a with
            | Some _ -> a
            | None -> b()
        member this.While(guard, body) =
            if not (guard())
            then
                this.Zero()
            else
                this.Bind( body(), fun () ->
                    this.While(guard, body))

    let optional = OptionalBuilder()

    type StringBuilder = B of (Text.StringBuilder -> unit)

    let build (B f) =
        let b = new Text.StringBuilder()
        do f b
        b.ToString ()

    type StringBuilderM () =
        let (!) = function B f -> f
        member __.Yield (txt : string) = B(fun b -> b.Append txt |> ignore)
        member __.Yield (c : char) = B(fun b -> b.Append c |> ignore)
        member __.YieldFrom f = f : StringBuilder

        member __.Combine(f,g) = B(fun b -> !f b; !g b)
        member __.Delay f = B(fun b -> !(f ()) b) : StringBuilder
        member __.Zero () = B(fun _ -> ())
        member __.For (xs : 'a seq, f : 'a -> StringBuilder) =
                        B(fun b ->
                            let e = xs.GetEnumerator ()
                            while e.MoveNext() do
                                !(f e.Current) b)
        member __.While (p : unit -> bool, f : StringBuilder) =
                        B(fun b -> while p () do !f b)

    let stringBuilder = StringBuilderM ()

    type HashSet<'T> = System.Collections.Generic.HashSet<'T>

    type AsyncBuilder with
        member async.MergeSources(left : Async<'T>, right : Async<'S>) : Async<'T * 'S> = async {
            let box f = async { let! x = f in return x :> obj }
            let! results = Async.Parallel [|box left; box right|]
            return (results.[0] :?> 'T, results.[1] :?> 'S)
        }

[<RequireQualifiedAccess>]
module Option =
    let ofString = function
    | null | "" -> None
    | str -> Some str

    let ofArray = function
        | [||] -> None
        | arr -> Some arr

    let stringOrEmpty = function
        | None | Some (null) -> ""
        | Some str -> str

[<RequireQualifiedAccess>]
module Map =
    let singleton (key, value) = Map.empty |> Map.add key value
    let values map = map |> Map.toSeq |> Seq.map snd

[<RequireQualifiedAccess>]
module String =

    let toLower s =
        match s with
        | null | "" -> ""
        | s -> s.ToLower()

    let toUpper s =
        match s with
        | null | "" -> ""
        | s -> s.ToUpper()

