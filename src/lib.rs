//! Alternative macro for building Html in Yew.
//!
//! This crate experiments on creating a macro that would be easier to use.
//! For this, the [`html!` syntax](https://yew.rs/docs/concepts/html)
//! that was a bit cumbersome to use
//! is replaced by direct usage of values in `ah!`.
//!
//! Following problems should be solved by this crate:
//!
//! - Having to use `{}` inside tags even when values are simple literals.
//! - Having to wrap attributes in `{}`
//!   (mind that shorthand still uses `{ variable }`).
//! - Having to repeat generics (Yew-only) and tag names
//!   (not very HTML to omit, but still neat to have considered)
//!   when closing tags.
//! - Having to use fragment `<></>` when using multiple nodes in the macro root.
//! - Not being able to use `match` just like `if`.
//! - Cumbersome `{ for ... }` notation.
//!
//! ## Example
//!
//! ```rust
//! use yew::prelude::*;
//! use yew_alt_html::ah;
//!
//! enum LoadState {
//!     Loading,
//!     Failed,
//!     Loaded,
//! }
//!
//! #[function_component]
//! pub fn App() -> Html {
//!     let name = "Yew";
//!     let italic_style = "font-style: italic";
//!
//!     use LoadState::*;
//!     let state = Loaded;
//!     let items = vec![1, 2, 3];
//!     ah! {
//!         <h1 style=italic_style>"Hello " name "!"</>
//!         match state {
//!             Loading => "Loading...",
//!             Failed => "Load failed!",
//!             Loaded => <p>"Welcome to "<code>"yew-alt-html"</>"!"</>,
//!         }
//!         <ul>
//!             for item in items {
//!                 <li>item</>
//!             }
//!         </>
//!     }
//! }
//! ```

mod content;
mod logic;
mod reader;

#[macro_use]
mod util;

#[macro_use]
mod tt;

use proc_macro::*;
pub(crate) use tt_call_macro;
pub(crate) use tt_path;
pub(crate) use tt_stream;

use self::logic::errors::InvalidSyntax;
use self::logic::read_children;
use self::reader::TokenReader;

macro_rules! impl_proc_macro {
    ($($name:ident -> $yew_name:ident),* $(,)?) => {
        $(#[proc_macro]
        pub fn $name(item: TokenStream) -> TokenStream {
            let reader = TokenReader::from(item);

            match read_children::<read_children::InBlock>(reader) {
                Ok(children) => tt_call_macro!(::yew::$yew_name!(children)),
                Err(InvalidSyntax(e, span)) => tt::compile_error(e, span),
            }
        })*
    };
}

impl_proc_macro!(ah -> html, ah_nested -> html_nested);
