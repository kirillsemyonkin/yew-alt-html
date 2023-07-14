# yew-alt-html

Alternative macro for building `Html` in [Yew](https://yew.rs/).

## Example

This example represents the root `App` component of a Yew application.
It shows interpolation in text nodes, interpolation in attributes,
multiple nodes in root of the macro, shortened tag closing.

```rust
use yew::prelude::*;
use yew_alt_html::ah;

#[function_component]
pub fn App() {
    let name = "Yew";
    let italic_style = "font-style: italic";

    ah! {
        <h1 style=italic_style>"Hello " name "!"</>
        <p>"Welcome to "<code>"yew-alt-html"</>"!"</>
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
  (not very HTML to get rid of, but still neat to have considered)
  when closing tags.
- Having to use fragment `<></>` when using multiple nodes in the macro root.

## Possible issues

Most [`html!` syntax](https://yew.rs/docs/concepts/html)
should be supported by the `ah!` macro.
If your code does not work by just replacing `html!` with `ah!`,
[submit an issue](https://github.com/kirillsemyonkin/yew-alt-html/issues).

Some syntax is limited (for example, using `<` in attributes without an `if`).
Suggested solution would be moving complex values into variables before `ah!`,
or wrapping values in `{}` braces just like you do in `html!`.

## Planned

- [ ] Support for writing tags directly in `match` cases
      (requires wrapping the tag in `ah!` currently), similar to `if`.
- [ ] Not using `html!` under the hood: adding more checks
      (that are currently handled by `html!`)
      and generating virtual dom manually.
- [ ] Using [Diagnostics API](https://github.com/rust-lang/rust/issues/54140)
      whenever it comes out for modifying spans and showing exact error spans
      instead of making everything red.
- [ ] Proper testing. Currently the crate is tested manually on
      an extensive local project by the crate author.
