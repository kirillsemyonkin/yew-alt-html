# yew-alt-html

[![crates.io](https://img.shields.io/crates/v/yew-alt-html)](https://crates.io/crates/yew-alt-html)
[![download](https://img.shields.io/crates/d/yew-alt-html)](https://crates.io/crates/yew-alt-html)
[![docs.rs](https://docs.rs/yew-alt-html/badge.svg)](https://docs.rs/yew-alt-html)

Alternative macro for building `Html` in [Yew](https://yew.rs/).

## Example

This example represents the root `App` component of a Yew application.
It shows interpolation in text nodes, interpolation in attributes,
multiple nodes in the root of the macro, shortened tag closing,
using tags in match expressions.

```rust
use yew::prelude::*;
use yew_alt_html::ah;

enum LoadState {
    Loading,
    Failed,
    Loaded,
}

#[function_component]
pub fn App() -> Html {
    let name = "Yew";
    let italic_style = "font-style: italic";

    use LoadState::*;
    let state = Loaded;
    let items = vec![1, 2, 3];
    ah! {
        <h1 style=italic_style>"Hello " name "!"</>
        match state {
            Loading => "Loading...",
            Failed => "Load failed!",
            Loaded => <p>"Welcome to "<code>"yew-alt-html"</>"!"</>,
        }
        <ul>
            for item in items {
                <li>item</>
            }
        </>
    }
}
```

## Why another `html!`?

This crate experiments on creating a macro that would be easier to use.
For this, the [`html!` syntax](https://yew.rs/docs/concepts/html)
that was a bit cumbersome to use
is replaced by direct usage of values in `ah!`.

Following problems should be solved by this crate:

- Having to use `{}` inside tags even when values are simple literals.
- Having to wrap attributes in `{}`
  (mind that shorthand still uses `{ variable }`).
- Having to repeat generics (Yew-only) and tag names
  (not very HTML to omit, but still neat to have considered)
  when closing tags.
- Having to use fragment `<></>` when using multiple nodes in the macro root.
- Not being able to use `match` just like `if`.
- Cumbersome `{ for ... }` notation (fixed in 0.4.0).

## Possible issues

Most [`html!` syntax](https://yew.rs/docs/concepts/html)
should be supported by the `ah!` macro.
If your code does not work by just replacing `html!` with `ah!`,
[submit an issue](https://github.com/kirillsemyonkin/yew-alt-html/issues).

Some syntax is limited (for example, using `>` in attributes without an `if`).
Suggested solution would be moving complex values into variables before `ah!`,
or wrapping values in `{}` braces just like you do in `html!`.

## Planned

- [x] Support for writing tags directly in `match` cases
      (requires wrapping the tag in `ah!` currently), similar to `if`
      (fixed in 0.2.0).
- [ ] Not using `html!` under the hood: adding more checks
      (that are currently handled by `html!`)
      and generating virtual dom manually.
- [ ] Using [Diagnostics API](https://github.com/rust-lang/rust/issues/54140)
      whenever it comes out for modifying spans and showing exact error spans
      instead of making everything red
      (partially fixed in 0.3.0, still needs more future work with spans).
- [ ] Proper testing. Currently the crate is tested manually on
      an extensive local project by the crate author.
