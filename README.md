# purescript-ezfetch
High-level bindings to the native `fetch` API

## `Effect.Aff.HTTP`
The main entry point is `Effect.Aff.HTTP.fetch`:

```purescript
fetch <method> <url> <options>
```

<ul>
<li>

`<method>` is `Effect.Aff.HTTP.Request.Method`:

```purescript
data Method
  = GET
  | PUT
  | POST
  | DELETE
  | PATCH
  | HEAD
```
</li>

<li>

`<url>` is `Data.URL.URL` (from [`url-immutable`](https://pursuit.purescript.org/packages/purescript-url-immutable/))
</li>
<li>

`<options>` is a partial record of:

```purescript
type OptionalFields =
  ( body :: Body
  , headers :: Headers
  , credentials :: Credentials
  )
```
</li>
</ul>
