namespace DependencyWalker

open System
open System.Text.RegularExpressions
open System.Runtime.InteropServices

[<Struct; StructuredFormatDisplay("{AsString}"); CustomEquality; CustomComparison>]
type SemVer(major : int, minor : int, build : int, revision : int, suffix : string) =
    static let rx = Regex @"^([0-9]+)\.([0-9]+)(\.[0-9]+)?(\.[0-9]+)?(-.*)?$"
    
    member x.Major = major
    member x.Minor = minor
    member x.Build = if build < 0 then 0 else build
    member x.Revision = if revision < 0 then 0 else revision
    member x.Suffix = if String.IsNullOrEmpty suffix then "" else suffix

    member private x.AsString = x.ToString()

    override x.ToString() =
        if String.IsNullOrWhiteSpace suffix then
            if revision >= 0 then String.Format("{0}.{1}.{2}.{3}", major, minor, build, revision)
            elif build >= 0 then String.Format("{0}.{1}.{2}", major, minor, build)
            else String.Format("{0}.{1}", major, minor)
        else
            if revision >= 0 then String.Format("{0}.{1}.{2}.{3}-{4}", major, minor, build, revision, suffix)
            elif build >= 0 then String.Format("{0}.{1}.{2}-{3}", major, minor, build, suffix)
            else String.Format("{0}.{1}-{2}", major, minor, suffix)
         
    static member TryParse(str : string, [<Out>] value : byref<SemVer>) =
        let m = rx.Match str
        if m.Success then
            let major = m.Groups.[1].Value |> int
            let minor = m.Groups.[2].Value |> int
            let build = if m.Groups.[3].Success then m.Groups.[3].Value.Substring(1) |> int else -1
            let revision = if m.Groups.[4].Success then m.Groups.[4].Value.Substring(1) |> int else -1
            let suffix = if m.Groups.[5].Success then m.Groups.[5].Value.Substring(1) else ""
            value <- SemVer(major, minor, build, revision, suffix)
            true
        else
            false
            
    member x.CompareTo (o : SemVer) =
        let c = compare x.Major o.Major
        if c <> 0 then c
        else
            let c = compare x.Minor o.Minor
            if c <> 0 then c
            else
                let c = compare x.Build o.Build
                if c <> 0 then c
                else
                    let c = compare x.Revision o.Revision
                    if c <> 0 then c
                    else
                        let xs = x.Suffix
                        let os = o.Suffix
                        if xs = "" && os = "" then 0
                        elif xs = "" then 1
                        elif os = "" then -1
                        else compare xs os

    override x.GetHashCode() =
        x.Major.GetHashCode() ^^^ 
        x.Minor.GetHashCode() ^^^ 
        x.Build.GetHashCode() ^^^ 
        x.Revision.GetHashCode() ^^^ 
        x.Suffix.GetHashCode()

    override x.Equals (o : obj) =
        match o with
        | :? SemVer as o ->
            x.Major = o.Major &&
            x.Minor = o.Minor &&
            x.Build = o.Build &&
            x.Revision = o.Revision &&
            x.Suffix = o.Suffix
        | _ ->
            false

    interface IComparable with
        member x.CompareTo (o : obj) =
            match o with
            | :? SemVer as o -> x.CompareTo o
            | _ -> failwith "uncomparable"

    new(major : int, minor : int, build : int, revision : int) = SemVer(major, minor, build, revision, "")
    new(major : int, minor : int, build : int) = SemVer(major, minor, build, -1, "")
    new(major : int, minor : int) = SemVer(major, minor, -1, -1, "")

    new(major : int, minor : int, build : int, suffix : string) = SemVer(major, minor, build, -1, suffix)
    new(major : int, minor : int, suffix : string) = SemVer(major, minor, -1, -1, suffix)

