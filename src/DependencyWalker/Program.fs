open System
open System.Net.Http
open System.Text.RegularExpressions
open FSharp.Data.JsonProvider
open System.Net
open DependencyWalker
open System.IO
open Paket

type ILogger =
    inherit IDisposable
    abstract member line : Printf.StringFormat<'a, unit> -> 'a
    abstract member start : Printf.StringFormat<'a, unit> -> 'a
    abstract member startTimed : Printf.StringFormat<'a, unit> -> 'a
    abstract member stop : unit -> unit

    abstract member sub : string -> ILogger

module Log =
    open System.Threading
    open System.Diagnostics

    [<AutoOpen>]
    module private Helpers = 
        type LogRegion = { indent : string; name : string; stopwatch : Option<Stopwatch> }
    
        type Logger(prefix : Option<string>, s : list<LogRegion>) =
            static let mutable currentId = 1
            static let newId() = Interlocked.Increment(&currentId)

        
            let id = 
                match prefix with
                | Some p -> p
                | None -> newId() |> string

            let mutable disposed = false
            let stack : ref<list<LogRegion>> = ref s

            let indent (self : Logger) =
                lock self (fun () ->
                    if not disposed then
                        match !stack with
                        | [] -> ""
                        | { indent = i } :: _ -> i
                    else
                        ""
                )

            let push (self : Logger) (name : string) (timed : bool) =
                lock self (fun () ->
                    if not disposed then
                        match !stack with
                        | [] ->
                            stack := [{ indent = "  "; name = name; stopwatch = if timed then Some (Stopwatch.StartNew()) else None }]
                        | l:: _ ->
                            stack := { indent = l.indent + "  "; name = name; stopwatch = if timed then Some (Stopwatch.StartNew()) else None } :: !stack 
                )

            let pop(self : Logger) =
                lock self (fun () ->
                    if not disposed then
                        match !stack with
                        | [] -> 
                            None
                        | h::t ->
                            match h.stopwatch with
                            | Some sw -> sw.Stop()
                            | None -> ()
                            stack := t
                            Some h
                    else
                        None
                )
            
            let write (self : Logger) (line : string) =
                lock self (fun () -> 
                    if not disposed then
                        Console.WriteLine("{0}:{1}{2}", id, indent self, line)
                )
            
            member x.line fmt = Printf.kprintf (write x) fmt

            member x.start fmt = 
                fmt |> Printf.kprintf (fun str ->
                    write x str
                    push x str false
                )

            member x.startTimed fmt = 
                fmt |> Printf.kprintf (fun str ->
                    write x str
                    push x str true
                )

            member x.stop() = 
                match pop x with
                | None -> ()
                | Some { name = n; stopwatch = sw } ->
                    match sw with
                    | Some sw -> x.line "%s %.3fs" n sw.Elapsed.TotalSeconds
                    | None -> ()
        
            abstract member Dispose : unit -> unit
            default x.Dispose() =
                lock x (fun () ->
                    disposed <- true
                    stack := []
                )

            interface ILogger with
                member x.Dispose() = x.Dispose()
                member x.start a = x.start a
                member x.startTimed a = x.startTimed a
                member x.line a = x.line a
                member x.stop() = x.stop()
                member x.sub(name : string) =
                    x.startTimed "%s" name
                    { new Logger(Some name, !stack) with
                        member __.Dispose() =
                            base.Dispose()
                            x.stop()
                    } :> ILogger

        type EmptyLogger() =
            static let instance = new EmptyLogger()

            static member Instance = instance

            interface ILogger with
                member x.Dispose() = ()
                member x.start a = Printf.kprintf ignore a
                member x.startTimed a = Printf.kprintf ignore a
                member x.line a = Printf.kprintf ignore a
                member x.stop() = ()
                member x.sub(name : string) = x :> ILogger

    let create (name : string) = new Logger(Some name, []) :> ILogger
    let empty = EmptyLogger.Instance :> ILogger

//type Repos = JsonProvider< "repos.json" >
//type Repo = JsonProvider< "repo.json" >

//type GithubSource =
//    | Organization of string
//    | User of string
//    | Repo of string

//module GithubSource =
//    let private client = new WebClient()

//    let listRepos (s : GithubSource) =
//        client.Headers.Add("user-agent", "request")
//        client.Headers.Add("Authorization: Token c2b7333516822599f2ef88f8397cf8a4eb9eb748")
//        match s with
//        | Organization name ->
//            try
//                client.DownloadString("https://api.github.com/orgs/" + name + "/repos")
//                |> Repos.Parse
//                |> Array.map (fun r -> r.Name, r.SshUrl)
//            with e ->
//                [||]
//        | User name ->
//            try
//                client.DownloadString("https://api.github.com/users/" + name + "/repos")
//                |> Repos.Parse
//                |> Array.map (fun r -> r.Name, r.SshUrl)
//            with e ->
//                [||]
//        | Repo fullName ->
//            try
//                client.DownloadString("https://api.github.com/repos/" + fullName)
//                |> Repo.Parse
//                |> Array.singleton
//                |> Array.map (fun r -> r.Name, r.SshUrl)
//            with e ->
//                [||]
            
module Proc =
    open System.Diagnostics
    
    let execStatus (log : ILogger) (dir : Option<string>) (cmd : string) (args : list<string>) : int * string =
        let args = String.concat " " (Seq.map (fun (str : string) -> if str.Contains(" ") then sprintf "\"%s\"" str else str) args)
        log.line "%s %s" cmd args

        let cmd, args =
            if Environment.OSVersion.Platform = PlatformID.Unix then "/bin/sh", (sprintf "\"%s\" %s "cmd args)
            else cmd, args

        let proc = ProcessStartInfo(cmd, args)
        proc.UseShellExecute <- false
        proc.RedirectStandardOutput <- true
        proc.RedirectStandardInput <- true
        proc.RedirectStandardError <- true

        match dir with
        | Some d -> proc.WorkingDirectory <- d
        | None -> ()

   
        let proc = Process.Start(proc)
    
        proc.WaitForExit()

        if proc.ExitCode <> 0 then
            proc.ExitCode, proc.StandardError.ReadToEnd()
        else
            0, proc.StandardOutput.ReadToEnd()
    
    let execPrint (log : ILogger) (dir : Option<string>) (cmd : string) (args : list<string>) =
        let args = String.concat " " (Seq.map (fun (str : string) -> if str.Contains(" ") then sprintf "\"%s\"" str else str) args)
        log.startTimed "%s %s" cmd args

        let cmd, args =
            if Environment.OSVersion.Platform = PlatformID.Unix then "/bin/sh", (sprintf "\"%s\" %s "cmd args)
            else cmd, args

        let proc = ProcessStartInfo(cmd, args)
        proc.UseShellExecute <- false
        proc.RedirectStandardOutput <- true
        proc.RedirectStandardInput <- true
        proc.RedirectStandardError <- true

        match dir with
        | Some d -> proc.WorkingDirectory <- d
        | None -> ()

        let builder = System.Text.StringBuilder()
        let err = System.Text.StringBuilder()
        let proc = Process.Start(proc)
    
        proc.OutputDataReceived.Add(fun e ->
            log.line "%s" e.Data
            builder.AppendLine(e.Data) |> ignore
        )
        proc.ErrorDataReceived.Add(fun e ->
            if not (String.IsNullOrWhiteSpace e.Data) then
                log.line "err: %s" e.Data
                err.AppendLine(e.Data) |> ignore
        )
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()

        proc.WaitForExit()
        log.stop()

        if proc.ExitCode = 0 then
            builder.ToString().Trim()
        else
            let err = err.ToString().Trim()
            failwith err
    
    let execOut (dir : Option<string>) (cmd : string) (args : list<string>) =
        execPrint Log.empty dir cmd args
        
    let exec (dir : Option<string>) (cmd : string) (args : list<string>) =
        execPrint Log.empty dir cmd args |> ignore


type RepoInfo =
    {
        name : string
        path : string
        currentTag : Option<Paket.SemVerInfo>
        needs : Map<string, Paket.VersionRequirement>
        creates : Set<string>
    }

module RepoInfo =
    open Paket


    let tryOfRepo (folder : string) =
        if Directory.Exists folder then
            try
                let deps = Path.Combine(folder, "paket.dependencies")

                let dependencies = 
                    if File.Exists deps then
                        let file = Paket.DependenciesFile.ReadFromFile(deps)
                        let mainGroup = file.GetGroup(Domain.GroupName Domain.MainGroup)
                        let mutable res = Map.empty
                        for p in mainGroup.Packages do
                            res <- Map.add p.Name.Name p.VersionRequirement res

                        res
                    
                    else
                        Map.empty
              
                let templates =
                    let typeRx = Regex @"type[ \t]+(project|file)"
                    let idRx = Regex @"id[ \t]+([^ \t\r\n]+)"
                    let files = Directory.GetFiles(folder, "*.template", SearchOption.AllDirectories)
                    files |> Array.choose (fun f ->
                        let c = File.ReadAllText f
                        let m = typeRx.Match c
                        if m.Success then
                            let kind = m.Groups.[1].Value
                            let m = idRx.Match c
                            if m.Success then
                                Some m.Groups.[1].Value
                            elif kind = "project" then
                                Directory.GetFiles(Path.GetDirectoryName f)
                                |> Array.tryPick (fun p ->
                                    let ext = Path.GetExtension p
                                    if ext = ".csproj" || ext = ".fsproj" then
                                        Path.GetFileNameWithoutExtension p |> Some
                                    else
                                        None
                                )
                            else
                                None
                        else
                            None
                    )


                let latest = Proc.execOut (Some folder) "git" ["describe"; "--abbrev=0"]

                let ver = 
                    try Paket.SemVer.Parse latest |> Some
                    with _ -> None


                Some {
                    name = Path.GetFileName folder
                    path = folder
                    currentTag = ver
                    needs = dependencies
                    creates = Set.ofArray templates
                }
            with _ ->
                None
        else    
            None


[<EntryPoint;STAThread>]
let main argv = 
    let scratchFolder = @"C:\Users\Schorsch\Desktop\buildtest"

    let newPackageVersions =
        Map.ofList [
            "FSharp.Core", VersionRequirement.Parse "[4.3.0, 4.4)"
        ]

    let log = Log.create ""

    //let repos =
    //    [
    //        Repo "krauthaufen/FShade"
    //        Repo "aardvark-platform/aardvark.base"
    //        Repo "aardvark-platform/aardvark.rendering"
    //        Repo "aardvark-platform/aardvark.media"
    //        Repo "aardvark-platform/aardvark.algodat"
    //        Repo "aardvark-community/aardvark.vr"
    //        Repo "aardvark-community/LibTessDotNet"
    //        Repo "aardvark-community/SplashDotNet"
    //        Repo "aardvark-community/unofficial.dotspatial.projections"
    //        Repo "aardvark-community/aardium"
    //        Repo "aardvark-community/CeresSharp"
    //        Repo "aardvark-community/MiniCV"
    //        Repo "aardvark-community/GLSLangSharp"
    //        Repo "aardvark-community/assimp-net"
    //        Repo "aardvark-community/Typography"
    //        Repo "aardvark-community/DevILSharp"
    //        Repo "aardvark-community/openvr"
    //    ]


    //let repos = repos |> List.toArray |> Array.collect GithubSource.listRepos

    //log.startTimed "found %d repositories" repos.Length
    //for (name,url) in repos do
    //    let outPath = Path.Combine(scratchFolder, name)
    //    if Directory.Exists outPath then
    //        log.startTimed "update %s" name
    //        Proc.exec (Some outPath) "git" ["fetch"; "origin"]
    //        Proc.exec (Some outPath) "git" ["reset"; "--hard"; "origin/master"]
    //        Proc.exec (Some outPath) "git" ["clean"; "-xdf"]
    //        log.stop()

    //    else 
    //        log.startTimed "cloning %s" name
    //        Proc.exec None "git" ["clone"; url; outPath]
    //        log.stop()

    //    let deps, created = RepoInfo.tryOfRepo outPath
    //    log.start "dependencies"
    //    for (k,v) in Map.toSeq deps do
    //        log.line "%s ~> %A" k v
    //    log.stop()

    //    log.start "created"
    //    for id in created do
    //        log.line "%s" id
    //    log.stop()

    //log.stop()

    log.startTimed "yeah"
    let mutable repos = Map.empty
    for dir in Directory.GetDirectories scratchFolder do
        log.start "updating %s" (Path.GetFileName dir)
        Proc.exec (Some dir) "git" ["fetch"; "origin"; "--tags"]
        Proc.exec (Some dir) "git" ["reset"; "--hard"; "origin/master"]
        Proc.exec (Some dir) "git" ["clean"; "-xdf"]

        match RepoInfo.tryOfRepo dir with
        | Some info ->
            repos <- Map.add info.name info repos
        | None ->
            log.line "no info for: %A" dir
        log.stop()

    let packageRepos =
        repos 
        |> Map.toSeq
        |> Seq.collect (fun (name, info) ->
            info.creates |> Seq.map (fun package ->
                package, name
            )
        )
        |> Map.ofSeq

    let neededBy =
        repos 
        |> Map.toSeq
        |> Seq.collect (fun (name, info) ->
            info.needs |> Map.toSeq |> Seq.map (fun (package,v) ->
                package, (name, v)
            )
        )
        |> Seq.groupBy fst
        |> Seq.map (fun (pn,repos) -> pn, Map.ofSeq (Seq.map snd repos))
        |> Map.ofSeq

    let getRepoDependencies (repo : RepoInfo) =
        repo.needs 
        |> Map.toSeq
        |> Seq.choose (fun (name,_) -> Map.tryFind name packageRepos)
        |> Set.ofSeq

    let graph =
        repos |> Map.map (fun name repo ->
            let deps = getRepoDependencies repo
            deps
        )

    let levels = 
        [
            let mutable graph = graph
            while not (Map.isEmpty graph) do
                let level0 = graph |> Map.toSeq |> Seq.choose (fun (name, deps) -> if Set.isEmpty deps then Some name else None) |> Set.ofSeq
                yield level0
                graph <-
                    level0 
                    |> Seq.fold (fun m r -> Map.remove r m) graph
                    |> Map.map (fun name deps -> Set.difference deps level0)
        ]

    let rec printRepo (name : string) = 
        match Map.tryFind name repos with
        | Some r ->
            log.start "%s" name
            for d in getRepoDependencies r do
                log.line "%s" d
            log.stop()
        | None ->
            log.line "%s" name


    let affectedRepos =
        newPackageVersions 
        |> Map.toSeq
        |> Seq.fold (fun m (name,_) ->
            match Map.tryFind name neededBy with
            | Some r -> 
                let mutable m = m
                for (k,v) in Map.toSeq r do 
                    match Map.tryFind k m with
                    | Some r -> 
                        m <- Map.add k (Map.add name v r) m
                    | None -> 
                        m <- Map.add k (Map.ofList [name, v]) m
                m
            | None -> m
        ) Map.empty

    for (repo, req) in Map.toSeq affectedRepos do
        log.start "%s" repo
        for (name, req) in Map.toSeq req do
            let newVersion = newPackageVersions.[name]
            if req.IsConflicting newVersion then
                let rs = req.ToString()
                log.line "%s: %s" name rs
            else
                let rs = req.ToString()
                log.line "%s: %s (already compatible)" name rs

        log.stop()
            
    let intersect (o : VersionRange) (n : VersionRange) =
        if o.IsIncludedIn n then o
        elif n.IsIncludedIn o then n
        else
            match n with
                | Minimum n -> 
                    match o with
                    | Range(_, _, c, d) -> Range(VersionRangeBound.Including, n, c, d)
                    | _ -> Minimum n
                | _ ->
                    failwith "asdasdasd"
                    

    let mutable newPackageVersions = newPackageVersions
    for (i, level) in Seq.indexed levels do
        log.start "level %d" i
        let mainGroup = Domain.GroupName Domain.MainGroup

        for repo in level do
            log.start "%s" repo
            let repo = repos.[repo]
            let path = repo.path

            // update dependencies
            let depsPath = Path.Combine(path, "paket.dependencies")
            let mutable updated = Map.empty
            let mutable deps = DependenciesFile.ReadFromFile depsPath
            for (name, newVersion) in Map.toSeq newPackageVersions do
                let packageName = Domain.PackageName name
                if deps.HasPackage(mainGroup, packageName) then
                    let old = deps.GetPackage(mainGroup, packageName).VersionRequirement
                    let newRange =
                        if old.IsConflicting newVersion then
                            newVersion
                        else
                            VersionRequirement(intersect old.Range newVersion.Range, PreReleaseStatus.No)
                     

                    if old <> newRange then
                        deps <- deps.UpdatePackageVersion(mainGroup, packageName, newRange.ToString())
                        updated <- Map.add name newVersion updated

            if not (Map.isEmpty updated) then
                log.start "updated"
                for (k, v) in Map.toSeq updated do
                    log.line "%s: %A" k (string v)
                log.stop()
                Environment.Exit 0

                deps.Save()


                // install 
                log.start "install %s" repo.name
                Proc.exec (Some path) (Path.Combine(path, ".paket", "paket.bootstrapper.exe")) []
                let rec install (count : int) =
                    if count > 0 then log.line "retry %d" count
                    let status, str = Proc.execStatus log (Some path) (Path.Combine(path, ".paket", "paket.exe")) [ "install" ]
                    if status <> 0 then
                        if str.Contains "Available versions" then
                            Threading.Thread.Sleep(20000)
                            install(count + 1)
                        else
                            log.line "real conflict"
                            log.line "please fix and press enter to continue"
                            Console.ReadLine() |> ignore
                            install 0

                log.stop()
                install 0

                let rec build() =
                    try
                        Proc.execOut (Some path) "cmd.exe" ["/C"; "build.cmd"; "CreatePackage"] |> ignore
                    with _ ->
                        log.line "could not build %s" repo.name
                        log.line "please fix and press enter to continue"
                        Console.ReadLine() |> ignore
                        build()
                log.startTimed "build"
                build()
                log.stop()

                let message =
                    updated 
                    |> Map.toSeq
                    |> Seq.map (fun (name, version) -> sprintf "  %s %s" name (version.ToString()))
                    |> String.concat "\n"
                    |> sprintf "updated packages:\n%s"
                
                
                log.start "commit & push"
                Proc.exec (Some path) "git" ["add"; "."]
                Proc.exec (Some path) "git" ["commit"; "-m"; message]
                let rec push() =
                    try Proc.execPrint log (Some path) "git" ["push"; "origin"; "master"] |> ignore
                    with _ ->   
                        log.line "could not push %s" repo.name
                        log.line "please fix and press enter to continue"
                        push()
                push()
                log.stop()
            
                log.start "push packages"
                let rec pushMinor() =
                    try
                        Proc.execPrint log (Some path) "cmd.exe" ["/C"; "build.cmd"; "PushMinor"] |> ignore
                    with _ ->
                        log.line "could not build %s" repo.name
                        log.line "please fix and press enter to continue"
                        Console.ReadLine() |> ignore
                        pushMinor()

                pushMinor()
                log.stop()

            let info = RepoInfo.tryOfRepo path |> Option.get
            let newVersion = info.currentTag.Value
            
            for p in info.creates do
                let req = VersionRequirement(VersionRange.Minimum newVersion, PreReleaseStatus.No)
                newPackageVersions <- Map.add p req newPackageVersions

            log.start "new package versions"
            for (k, v) in Map.toSeq newPackageVersions do
                log.line "%s: %s" k (string v)
            log.stop()
            
            log.stop()

        log.stop()

    log.stop()








    0
