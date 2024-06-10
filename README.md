# purescript-ezfetch
High-level bindings to the native `fetch` API

## `Effect.Aff.HTTP`
The main entry point is `Effect.Aff.HTTP.fetch`:

```purescript
fetch <method> <url> <options>
```

 * `<method>` is `Effect.Aff.HTTP.Request.Method`:
        <!-- language: purescript -->
        data Method
          = GET
          | PUT
          | POST
          | DELETE
          | PATCH
          | HEAD
 * `<url>` is `Data.URL.URL` (from [`url-immutable`](https://pursuit.purescript.org/packages/purescript-url-immutable/))
 * `<options>` is a partial record of:
        <!-- language: purescript -->
        type OptionalFields =
          ( body :: Body
          , headers :: Headers
          , credentials :: Credentials
          )
