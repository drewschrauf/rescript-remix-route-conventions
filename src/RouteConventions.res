module Map = Belt.MutableMap.String
@module("fs") external statSync: string => NodeJs.Fs.Stats.t = "statSync"

type defineRoute
type defineChildRoute = (. string, string) => unit
type defineParentRoute = (. string, string, unit => unit) => unit

external toDefineChildRoute: defineRoute => defineChildRoute = "%identity"
external toDefineParentRoute: defineRoute => defineParentRoute = "%identity"

type rec routeDefinitionNode = {
  mutable file: option<string>,
  mutable nested: option<routeDefinition>,
}
and routeDefinition = Map.t<routeDefinitionNode>

let filenameToSegment = (name: string): string => {
  (name->Js.String2.match_(%re("/([^_]+?)$/g"))->Js.Option.getExn)[0]
  ->Js.String2.replaceByRe(%re("/\[([^\]]+?)\]/g"), ":$1")
  ->Js.String2.replaceByRe(%re("/\./g"), "/")
}

let rec buildRoutesForDir = (path: string) => {
  let routes = Map.make()

  let files = NodeJs.Fs.readdirSync(NodeJs.Path.join(["app", path]))
  Js.Array2.forEach(files, file => {
    let fileDef = file->NodeJs.Path.parse
    let isDirectory = ["app", path, file]->NodeJs.Path.join->statSync->NodeJs.Fs.Stats.isDirectory

    let segment = (isDirectory ? fileDef.base : fileDef.name)->filenameToSegment
    let mapping = routes->Map.getWithDefault(segment, {file: None, nested: None})

    if isDirectory {
      mapping.nested = Some(buildRoutesForDir(NodeJs.Path.join([path, segment])))
    } else {
      mapping.file = Some(NodeJs.Path.join([path, file]))
    }

    routes->Map.set(segment, mapping)
  })

  routes
}

let rec registerBuiltRoutes = (
  defineRoute: defineRoute,
  routes: routeDefinition,
  ~segments=[],
  (),
) => {
  routes->Map.forEach((segment, definition) => {
    switch (definition.file, definition.nested) {
    | (Some(file), None) =>
      (defineRoute->toDefineChildRoute)(.
        segments->Js.Array2.concat([segment])->Js.Array2.joinWith("/"),
        file,
      )
    | (None, Some(nested)) =>
      registerBuiltRoutes(defineRoute, nested, ~segments=segments->Js.Array2.concat([segment]), ())
    | (Some(file), Some(nested)) =>
      (defineRoute->toDefineParentRoute)(.
        segments->Js.Array2.concat([segment])->Js.Array2.joinWith("/"),
        file,
        () => registerBuiltRoutes(defineRoute, nested, ()),
      )
    | (None, None) => Js.Exn.raiseError("Invariant error")
    }
  })
}

let registerRoutes = (defineRoute: defineRoute) => {
  let routes = buildRoutesForDir("res-routes")
  registerBuiltRoutes(defineRoute, routes, ())
}
