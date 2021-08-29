# purescript-halogen-ihooks

A hooks library for halogen.

## Structure

The library is comprised of three modules:

- `Halogen.IHooks`: core hooks library.
- `Halogen.IHooks.Sugar`: one-liners for stuff that I found myself writing over and over.
- `Halogen.IHooks.Compat`: a "drop-in" library for [`purescript-halogen-hooks`](https://github.com/thomashoneyman/purescript-halogen-hooks). The types are different, but the syntax should be identical.

## Basic example

```purescript
myComponent = Hooks.component Hooks.defaultOptions \input -> Ix.do
  count <- Sugar.hookConsPure (Proxy :: _ "count") 0

  ipure $
    HH.button
      [ HE.onClick \_ -> Sugar.modify_ (Proxy :: _ "count") (_ + 1) ]
      [ HH.text $ show count ]
```

The main difference with [`purescript-halogen-hooks`](https://github.com/thomashoneyman/purescript-halogen-hooks) is that the index of the hook is a `Symbol` proxy determined by the calling code.

![Hook indices set by calling code](https://i.ibb.co/swSvkfN/hooks0.png)

This allows you to inspect the type in the IDE as you're putting together the hooks.

![Hook indices inspected in the IDE](https://i.ibb.co/7YLWqvD/hooks1.png)

## Example

Check out [the example](./example) code, and play with it live on [purescript-halogen-ihooks.surge.sh](https://purescript-halogen-ihooks.surge.sh/).

## Contributing

Contributions are welcome! If you feel anything could be improved or needs more clarity, please don't hesitate to open an issue or make a pull request.

## Goals

- Type-safety
- Small core API (less than 300 lines of code)
- Helpful error messages
- Semantic naming of hooks via symbols
- Performance
