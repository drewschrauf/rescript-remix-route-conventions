open Jest
open Expect

module Map = Belt.MutableMap.String

type rec routeDefinition = {file: string, nested: option<Map.t<routeDefinition>>}

let first = (arr: array<'a>): 'a => arr[0]
let last = (arr: array<'a>): 'a => arr[arr->Js.Array2.length - 1]

module MockRouteDefiner = {
  type t = array<Map.t<routeDefinition>>

  let make = (): t => [Map.make()]

  let defineChildRoute = (. definer: t, path: string, file: string) => {
    definer->last->Map.set(path, {file: file, nested: None})
  }

  let defineParentRoute = (.
    definer: t,
    path: string,
    file: string,
    callback: (. unit) => unit,
  ) => {
    let nestedRoutes = Map.make()
    definer->last->Map.set(path, {file: file, nested: Some(nestedRoutes)})
    definer->Js.Array2.push(nestedRoutes)->ignore
    callback(.)
    definer->Js.Array2.pop->ignore
  }

  // using raw JS here to model the signature-overloaded `defineRoute` function provided by Remix
  let defineRoute = %raw(`
    function(definer) {
      return function(path, file, optsOrCallback, opts) {
        if (typeof optsOrCallback === "function") {
          defineParentRoute(definer, path, file, optsOrCallback)
        } else {
          defineChildRoute(definer, path, file)
        }
      }
    }
  `)

  let routes = (definer: t): Map.t<routeDefinition> => definer->first
}

afterEach(() => {
  MockFs.restore()
})

test("it should map a non-namespaced file into a simple route", () => {
  MockFs.mock({"app": {"res-routes": {"blog.js": ""}}})

  let routeDefiner = MockRouteDefiner.make()
  RouteConventions.registerRoutes(routeDefiner->MockRouteDefiner.defineRoute)

  expect(routeDefiner->MockRouteDefiner.routes)->toEqual(
    Map.fromArray([("blog", {file: "res-routes/blog.js", nested: None})]),
  )
})

test("it should map a namespaced file into a simple route", () => {
  MockFs.mock({"app": {"res-routes": {"namespaced_blog.js": ""}}})

  let routeDefiner = MockRouteDefiner.make()
  RouteConventions.registerRoutes(routeDefiner->MockRouteDefiner.defineRoute)

  expect(routeDefiner->MockRouteDefiner.routes)->toEqual(
    Map.fromArray([("blog", {file: "res-routes/namespaced_blog.js", nested: None})]),
  )
})

test("it should map a bracket-surrounded file into a dynamic route", () => {
  MockFs.mock({"app": {"res-routes": {"[blogId].js": ""}}})

  let routeDefiner = MockRouteDefiner.make()
  RouteConventions.registerRoutes(routeDefiner->MockRouteDefiner.defineRoute)

  expect(routeDefiner->MockRouteDefiner.routes)->toEqual(
    Map.fromArray([(":blogId", {file: "res-routes/[blogId].js", nested: None})]),
  )
})

test("it should map a period-delimeted file into a slash-delimted route", () => {
  MockFs.mock({"app": {"res-routes": {"blog.about.js": ""}}})

  let routeDefiner = MockRouteDefiner.make()
  RouteConventions.registerRoutes(routeDefiner->MockRouteDefiner.defineRoute)

  expect(routeDefiner->MockRouteDefiner.routes)->toEqual(
    Map.fromArray([("blog/about", {file: "res-routes/blog.about.js", nested: None})]),
  )
})

test("it should map a deep non-namespaced file into a simple route", () => {
  MockFs.mock({"app": {"res-routes": {"blog": {"blog.js": ""}}}})

  let routeDefiner = MockRouteDefiner.make()
  RouteConventions.registerRoutes(routeDefiner->MockRouteDefiner.defineRoute)

  expect(routeDefiner->MockRouteDefiner.routes)->toEqual(
    Map.fromArray([("blog/blog", {file: "res-routes/blog/blog.js", nested: None})]),
  )
})

test("it should nest non-namespace routes when a folder and file exist with the same name", () => {
  MockFs.mock({"app": {"res-routes": {"blog.js": "", "blog": {"blog.js": ""}}}})

  let routeDefiner = MockRouteDefiner.make()
  RouteConventions.registerRoutes(routeDefiner->MockRouteDefiner.defineRoute)

  expect(routeDefiner->MockRouteDefiner.routes)->toEqual(
    Map.fromArray([
      (
        "blog",
        {
          file: "res-routes/blog.js",
          nested: Some(Map.fromArray([("blog", {file: "res-routes/blog/blog.js", nested: None})])),
        },
      ),
    ]),
  )
})
