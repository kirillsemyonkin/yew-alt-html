use proc_macro::*;

use crate::tt;
use crate::tt_stream;
use crate::util::MatchOpt;

//
// tt:: utils
//

// Group

pub fn group(delimiter: Delimiter, items: impl IntoTokenStream) -> Group {
    Group::new(
        delimiter,
        items.into_token_stream(),
    )
}

pub fn brace(items: impl IntoTokenStream) -> Group { tt::group(Delimiter::Brace, items) }

// Ident

pub trait IntoIdent {
    fn into_ident(self) -> Ident;
}

impl IntoIdent for &str {
    fn into_ident(self) -> Ident { Ident::new(self, Span::mixed_site()) }
}

impl IntoIdent for Ident {
    fn into_ident(self) -> Ident { self }
}

pub fn ident(from: impl IntoIdent) -> Ident {
    from.into_ident()
}

macro_rules! tt_path {
    ($start_path:ident$(::$path_part:ident)*) => {
        $crate::tt::path(false, vec![stringify!($start_path)$(, stringify!($path_part))*])
    };
    ($(::$extern_path_part:ident)*) => {
        $crate::tt::path(true, vec![$(stringify!($extern_path_part)),*])
    };
}

pub fn path(r#extern: bool, parts: Vec<impl IntoIdent>) -> TokenStream {
    parts
        .into_iter()
        .enumerate()
        .flat_map(|(i, ident)| {
            let non_first_or_extern = i != 0 || r#extern;
            tt_stream![
                non_first_or_extern.then(|| tt::punct("::")),
                tt::ident(ident)
            ]
        })
        .collect()
}

// Punct

pub trait IntoPunct {
    type Ret;

    fn into_punct(self) -> Self::Ret;
}

impl IntoPunct for char {
    type Ret = Punct;

    fn into_punct(self) -> Self::Ret { Punct::new(self, Spacing::Alone) }
}

impl IntoPunct for &str {
    type Ret = Vec<Punct>;

    fn into_punct(self) -> Self::Ret {
        let last = self.len() - 1;
        self.chars()
            .enumerate()
            .map(|(i, c)| {
                Punct::new(
                    c,
                    match i {
                        i if i == last => Spacing::Alone,
                        _ => Spacing::Joint,
                    },
                )
            })
            .collect()
    }
}

impl IntoPunct for Punct {
    type Ret = Punct;

    fn into_punct(self) -> Self::Ret { self }
}

pub fn punct<I: IntoPunct>(from: I) -> I::Ret { from.into_punct() }

// Literal

pub trait IntoLiteral {
    fn into_literal(self) -> Literal;
}

impl IntoLiteral for String {
    fn into_literal(self) -> Literal { Literal::string(&self) }
}

macro_rules! impl_into_literal {
    ($($fn:ident($t:ty)),* $(,)?) => {
        $(impl IntoLiteral for $t {
            fn into_literal(self) -> Literal { Literal::$fn(self) }
        })*
    };
}

impl_into_literal!(
    string(&str),
    character(char),
    byte_string(&[u8]),
    u8_suffixed(u8),
    u16_suffixed(u16),
    u32_suffixed(u32),
    u64_suffixed(u64),
    u128_suffixed(u128),
    i8_suffixed(i8),
    i16_suffixed(i16),
    i32_suffixed(i32),
    i64_suffixed(i64),
    i128_suffixed(i128),
    f32_suffixed(f32),
    f64_suffixed(f64),
);

pub fn literal(text: impl IntoLiteral) -> Literal {
    text.into_literal()
        .into()
}

//
// IntoTokenStream
//

pub trait IntoTokenStream {
    fn into_token_stream(self) -> TokenStream;
}

impl IntoTokenStream for TokenTree {
    fn into_token_stream(self) -> TokenStream { self.into() }
}

impl IntoTokenStream for Group {
    fn into_token_stream(self) -> TokenStream { TokenTree::from(self).into() }
}

impl IntoTokenStream for Ident {
    fn into_token_stream(self) -> TokenStream { TokenTree::from(self).into() }
}

impl IntoTokenStream for Punct {
    fn into_token_stream(self) -> TokenStream { TokenTree::from(self).into() }
}

impl IntoTokenStream for Literal {
    fn into_token_stream(self) -> TokenStream { TokenTree::from(self).into() }
}

impl IntoTokenStream for TokenStream {
    fn into_token_stream(self) -> TokenStream { self }
}

impl<T: IntoTokenStream> IntoTokenStream for Option<T> {
    fn into_token_stream(self) -> TokenStream {
        self.map(T::into_token_stream)
            .unwrap_or_default()
    }
}

impl<T: IntoTokenStream> IntoTokenStream for Vec<T> {
    fn into_token_stream(self) -> TokenStream {
        self.into_iter()
            .map(T::into_token_stream)
            .collect()
    }
}

//
// tt_stream!
//

macro_rules! tt_stream {
    [$($tt:expr),* $(,)?] => {
        {
            use $crate::tt::IntoTokenStream;
            let vec: Vec<TokenStream> = vec![$($tt.into_token_stream()),*];
            vec.into_token_stream()
        }
    };
}

//
// call_macro
//

pub fn call_macro(name: impl IntoTokenStream, inner: impl IntoTokenStream) -> TokenStream {
    tt_stream![name, tt::punct('!'), tt::brace(inner),]
}

macro_rules! tt_call_macro {
    ( $start_path:ident$(::$path_part:ident)* ! ($($params:tt)*) ) => {
        $crate::tt::call_macro(
            $crate::tt_path!($start_path$(::$path_part)*),
            $crate::tt_stream![$($params)*],
        )
    };
    ( $(::$extern_path_part:ident)* ! ($($params:tt)*) ) => {
        $crate::tt::call_macro(
            $crate::tt_path!($(::$extern_path_part)*),
            $crate::tt_stream![$($params)*],
        )
    };
}

pub fn compile_error(message: &str) -> TokenStream {
    call_macro(
        tt_path!(compile_error),
        tt_stream![tt::literal(message)],
    )
}

//
// IdentExt
//

pub trait IdentExt {
    fn raw_string(self) -> String;
}

impl IdentExt for Ident {
    fn raw_string(self) -> String { (&self).raw_string() }
}

impl IdentExt for &Ident {
    fn raw_string(self) -> String {
        self.to_string()
            .trim_start_matches("r#")
            .to_string()
    }
}

//
// TokenStreamExt
//

pub trait TokenStreamExt {
    fn raw_string(self) -> String;

    fn span(&self) -> Span;
}

impl TokenStreamExt for TokenStream {
    fn raw_string(self) -> String {
        self.into_iter()
            .map(|tt| match tt {
                TokenTree::Ident(ident) => ident.raw_string(),
                TokenTree::Group(group) => match group.delimiter() {
                    Delimiter::Parenthesis => format!(
                        "({})",
                        group
                            .stream()
                            .raw_string()
                    ),
                    Delimiter::Brace => format!(
                        "{{{}}}",
                        group
                            .stream()
                            .raw_string()
                    ),
                    Delimiter::Bracket => format!(
                        "[{}]",
                        group
                            .stream()
                            .raw_string()
                    ),
                    Delimiter::None => group
                        .stream()
                        .raw_string(),
                },
                other => other.to_string(),
            })
            .collect::<String>()
    }

    fn span(&self) -> Span {
        Span::mixed_site() // TODO join all inner spans
    }
}

impl TokenStreamExt for &TokenStream {
    fn raw_string(self) -> String {
        self.clone()
            .raw_string()
    }

    fn span(&self) -> Span { (**self).span() }
}

//
// MatchOpt
//

macro_rules! impl_match_opt_tt {
    [$($type:ident),* $(,)?] => {
        $(impl MatchOpt<$type> for TokenTree {
            fn match_opt(self) -> Option<$type> {
                match self {
                    TokenTree::$type(v) => Some(v),
                    _ => None,
                }
            }
        })*
    };
}

impl_match_opt_tt![Group, Ident, Punct, Literal];
