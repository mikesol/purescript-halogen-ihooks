# purescript-halogen-ihooks

A hooks library for halogen.

## Structure

The library is comprised of three modules:

- `Halogen.IHooks`: core hooks library.
- `Halogen.IHooks.Sugar`: one-liners for stuff that I found myself writing over and over.
- `Halogen.IHooks.Compat`: a "drop-in" library for `purescript-halogen-hooks`. The types are different, but the syntax should be identical.

## Goals

- Type-safe hooks
- No hook-leaks (hooks leaking to other components)
- Small core API
- Performance

## Example

Check out [the example](./example).