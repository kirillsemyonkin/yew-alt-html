use proc_macro::*;

use crate::tt;
use crate::tt::IdentExt;
use crate::tt::IntoTokenStream;
use crate::tt::TokenStreamExt;
use crate::tt_call_macro;
use crate::tt_path;
use crate::tt_stream;

#[derive(Debug, Clone)]
pub enum Content {
    Tag(Tag),
    ForIn(ForIn),
    ForIter(TokenStream),
    IfOptLet(IfOptLet),
    Match(Match),
    Expression(TokenStream),
}

impl From<Content> for TokenStream {
    fn from(content: Content) -> Self {
        match content {
            Content::Tag(tag) => tt_stream![tag],
            Content::ForIn(for_in) => tt_stream![for_in],
            Content::ForIter(expr) => tt_stream![tt::brace(expr)],
            Content::IfOptLet(if_opt_let) => tt_stream![if_opt_let],
            Content::Match(r#match) => tt_stream![r#match],
            Content::Expression(expr) => tt_stream![tt::brace(expr)],
        }
    }
}

impl IntoTokenStream for Content {
    fn into_token_stream(self) -> TokenStream {
        self.into()
    }
}

//
// Match
//

#[derive(Debug, Clone)]
pub struct Match {
    match_keyword: Ident,
    value: TokenStream,
    cases: Vec<MatchCase>,
}

impl Match {
    pub fn new(match_keyword: Ident, value: TokenStream, cases: Vec<MatchCase>) -> Self {
        Self {
            match_keyword,
            value,
            cases,
        }
    }
}

impl From<Match> for TokenStream {
    fn from(
        Match {
            match_keyword,
            value,
            cases,
        }: Match,
    ) -> Self {
        tt_stream![tt::brace(tt_stream![
            match_keyword,
            value,
            tt::brace(cases)
        ])]
    }
}

impl IntoTokenStream for Match {
    fn into_token_stream(self) -> TokenStream {
        self.into()
    }
}

#[derive(Debug, Clone)]
pub enum MatchCase {
    Case {
        pattern: TokenStream,
        arrow: TokenStream,
        value: Content,
    },
    Comma(Punct),
}

impl From<MatchCase> for TokenStream {
    fn from(case: MatchCase) -> Self {
        match case {
            MatchCase::Case {
                pattern,
                arrow,
                value,
            } => tt_stream![pattern, arrow, tt_call_macro!(::yew::html!(value))],
            MatchCase::Comma(comma) => tt_stream![comma],
        }
    }
}

impl IntoTokenStream for MatchCase {
    fn into_token_stream(self) -> TokenStream {
        self.into()
    }
}

//
// IfOptLet
//

#[derive(Debug, Clone)]
pub struct IfOptLet {
    if_keyword: Ident,
    condition: TokenStream,
    if_true: Box<Content>,
    r#else: Option<(Ident, Box<Content>)>,
}

impl IfOptLet {
    pub fn new(
        if_keyword: Ident,
        condition: impl IntoTokenStream,
        if_true: Content,
        r#else: Option<(Ident, Box<Content>)>,
    ) -> Self {
        Self {
            if_keyword,
            condition: condition.into_token_stream(),
            if_true: Box::new(if_true),
            r#else,
        }
    }
}

impl From<IfOptLet> for TokenStream {
    fn from(
        IfOptLet {
            if_keyword,
            condition,
            if_true,
            r#else,
        }: IfOptLet,
    ) -> Self {
        let if_opt_let_start = tt_stream![if_keyword, condition, tt::brace((*if_true).clone())];

        let else_part = r#else
            .map(|(else_keyword, r#else)| tt_stream![else_keyword, tt::brace((*r#else).clone())])
            .unwrap_or_default();

        tt_stream![if_opt_let_start, else_part]
    }
}

impl IntoTokenStream for IfOptLet {
    fn into_token_stream(self) -> TokenStream {
        self.into()
    }
}

//
// ForIn
//

#[derive(Debug, Clone)]
pub struct ForIn {
    for_keyword: Ident,
    pattern: TokenStream,
    in_keyword: Ident,
    iter: TokenStream,
    body: Box<Content>,
}

impl ForIn {
    pub fn new(
        for_keyword: Ident,
        pattern: TokenStream,
        in_keyword: Ident,
        iter: TokenStream,
        body: Content,
    ) -> Self {
        Self {
            for_keyword,
            pattern,
            in_keyword,
            iter,
            body: Box::new(body),
        }
    }
}

impl From<ForIn> for TokenStream {
    fn from(
        ForIn {
            for_keyword,
            pattern,
            in_keyword,
            iter,
            body,
        }: ForIn,
    ) -> Self {
        tt_stream![tt::brace(tt_stream![
            for_keyword.clone(),
            tt::brace(tt_stream![
                // let mut _result = Vec::<::yew::html::Html>::new();
                tt::ident("let"),
                tt::ident("mut"),
                tt::ident("_result"),
                '=',
                tt::ident("Vec"),
                tt::punct("::"),
                '<',
                tt_path!(::yew::html::Html),
                '>',
                tt::punct("::"),
                tt::ident("new"),
                tt::parenthesis(()),
                ';',
                // for $pattern in $iter { ... }
                for_keyword,
                pattern,
                in_keyword,
                iter,
                tt::brace(tt_stream![
                    // _result.push(::yew::html! { body })
                    tt::ident("_result"),
                    '.',
                    tt::ident("push"),
                    tt::parenthesis(tt_call_macro!(::yew::html!(body))),
                ]),
                // _result
                tt::ident("_result"),
            ])
        ])]
    }
}

impl IntoTokenStream for ForIn {
    fn into_token_stream(self) -> TokenStream {
        self.into()
    }
}

//
// Tag
//

#[derive(Debug, Clone)]
pub struct Tag {
    description: TagOpen,
    children: Vec<Content>,
    void: bool,
}

impl Tag {
    pub fn new(description: TagOpen, children: Vec<Content>, void: bool) -> Self {
        Self {
            description,
            children,
            void,
        }
    }
}

impl From<Tag> for TokenStream {
    fn from(
        Tag {
            description,
            children,
            void,
        }: Tag,
    ) -> Self {
        match description {
            TagOpen::Named {
                name,
                generics,
                attributes,
            } => {
                if void {
                    // <...<...> ...=... />
                    return tt_stream!['<', name, generics, attributes, '/', '>'];
                }

                tt_stream![
                    // <...<...> ...=...>
                    '<',
                    name.clone(),
                    generics.clone(),
                    attributes,
                    '>',
                    // ...
                    children,
                    // </...<...>>
                    '<',
                    '/',
                    name.clone(),
                    generics.clone(),
                    '>',
                ]
            }
            TagOpen::Dashed { name, attributes } => {
                if void {
                    // <... ...=... />
                    return tt_stream!['<', name, attributes, '/', '>'];
                }

                tt_stream![
                    // <... ...=...>
                    '<',
                    name.clone(),
                    attributes,
                    '>',
                    // ...
                    children,
                    // </...>
                    '<',
                    '/',
                    name.clone(),
                    '>',
                ]
            }
            TagOpen::Dynamic {
                start,
                name,
                attributes,
            } => {
                let name = tt::brace(name);

                if void {
                    // <@{...} ...=... />
                    return tt_stream!['<', start, name, attributes, '/', '>'];
                }

                tt_stream![
                    // <@{...} ...=...>
                    '<',
                    start.clone(),
                    name,
                    attributes,
                    '>',
                    // ...
                    children,
                    // </@>
                    '<',
                    '/',
                    start.clone(),
                    '>',
                ]
            }
            TagOpen::Fragment => tt_stream!['<', '>', children, '<', '/', '>'],
        }
    }
}

impl IntoTokenStream for Tag {
    fn into_token_stream(self) -> TokenStream {
        self.into()
    }
}

// TagOpen

#[derive(Debug, Clone)]
pub enum TagOpen {
    Named {
        name: NamePath,
        generics: Generics,
        attributes: Attributes,
    },
    Dashed {
        name: TokenStream,
        attributes: Attributes,
    },
    Dynamic {
        start: Punct,
        name: TokenStream,
        attributes: Attributes,
    },
    Fragment,
}

impl TagOpen {
    pub fn span(&self) -> Span {
        // TODO span encompassing whole tag open
        match self {
            Self::Named { name, .. } => name.span(),
            Self::Dashed { name, .. } => name.span(),
            Self::Dynamic { start, .. } => start.span(),
            Self::Fragment => Span::mixed_site(), // TODO at least grab angle brackets smh
        }
    }

    pub fn close_match(&self, other: &TagClose) -> bool {
        match (self, other) {
            // (named, named): <_>...</_>
            (
                TagOpen::Named {
                    name: name1,
                    generics: generics1,
                    ..
                },
                TagClose::Named {
                    name: name2,
                    generics: generics2,
                    ..
                },
            ) => name1 == name2 && generics1 == generics2,
            // (dashed, dashed): <->...</->
            (TagOpen::Dashed { name: name1, .. }, TagClose::Dashed { name: name2, .. }) => {
                name1.to_string() == name2.to_string()
            }
            // (non-named, named): <@>...</_>, <>...</_>, <->...</_>
            (_, TagClose::Named { .. }) => false,
            // (non-dashed, dashed): <@>...</->, <>...</->, <_>...</->
            (_, TagClose::Dashed { .. }) => false,
            // (any, dynamic): <...>...</@>
            (_, TagClose::Dynamic { .. }) => true,
            // (any, fragment): <...>...</>
            (_, TagClose::Fragment) => true,
        }
    }
}

// TagClose

#[derive(Debug, Clone)]
pub enum TagClose {
    Named { name: NamePath, generics: Generics },
    Dashed { name: TokenStream },
    Dynamic { start: Punct },
    Fragment,
}

impl TagClose {
    pub fn span(&self) -> Span {
        // TODO span encompassing whole tag open
        match self {
            Self::Named { name, .. } => name.span(),
            Self::Dashed { name, .. } => name.span(),
            Self::Dynamic { start, .. } => start.span(),
            Self::Fragment => Span::mixed_site(), // TODO at least grab angle brackets smh
        }
    }
}

// Generics

#[derive(Debug, Clone)]
pub struct Generics {
    value: Option<TokenStream>,
    span: Span,
}

impl Generics {
    pub fn new(value: impl IntoTokenStream, span: Span) -> Self {
        Self {
            value: Some(value.into_token_stream()),
            span,
        }
    }

    pub fn empty(span: Span) -> Self {
        Self { value: None, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl PartialEq for Generics {
    fn eq(&self, other: &Self) -> bool {
        match (&self.value, &other.value) {
            (Some(a), Some(b)) => a.to_string() == b.to_string(), // my only thought unfortunately
            (Some(_), None) => true,
            (None, Some(_)) => false,
            (None, None) => true,
        }
    }
}

impl From<Generics> for TokenStream {
    fn from(generics: Generics) -> Self {
        generics
            .value
            .map(|generics| tt_stream!['<', generics, '>'])
            .unwrap_or_default()
    }
}

impl IntoTokenStream for Generics {
    fn into_token_stream(self) -> TokenStream {
        self.into()
    }
}

// Attributes

#[derive(Debug, Clone)]
pub enum Attribute {
    Full {
        name: TokenStream,
        equals: Punct,
        value: TokenStream,
        property: Option<Punct>,
    },
    Short {
        variable: Ident,
        property: Option<Punct>,
    },
    PropSpread {
        rest: Vec<Punct>,
        variable: Ident,
    },
}

impl From<Attribute> for TokenStream {
    fn from(attr: Attribute) -> Self {
        match attr {
            Attribute::Full {
                name,
                equals,
                value,
                property,
            } => tt_stream![property, name, equals, tt::brace(value)],
            Attribute::Short { variable, property } => tt_stream![property, tt::brace(variable)],
            Attribute::PropSpread { rest, variable } => tt_stream![rest, variable],
        }
    }
}

impl IntoTokenStream for Attribute {
    fn into_token_stream(self) -> TokenStream {
        self.into()
    }
}

impl Attribute {
    pub fn name_span(&self) -> Option<Span> {
        match self {
            Attribute::Full { name, .. } => Some(name.span()),
            Attribute::Short { variable, .. } => Some(variable.span()),
            Attribute::PropSpread { .. } => None,
        }
    }

    pub fn name_string(&self) -> Option<String> {
        match self {
            Attribute::Full { name, .. } => Some(name.raw_string()),
            Attribute::Short { variable, .. } => Some(variable.raw_string()),
            Attribute::PropSpread { .. } => None,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Attributes {
    values: Vec<Attribute>,
}

impl Attributes {
    pub fn new(values: Vec<Attribute>) -> Self {
        Self { values }
    }
}

impl From<Attributes> for TokenStream {
    fn from(attributes: Attributes) -> Self {
        attributes
            .values
            .into_iter()
            .map(IntoTokenStream::into_token_stream)
            .collect()
    }
}

impl IntoTokenStream for Attributes {
    fn into_token_stream(self) -> TokenStream {
        self.into()
    }
}

// NamePath

#[derive(Debug, Clone)]
pub struct NamePath(TokenStream);

impl NamePath {
    pub fn new(parts: impl IntoTokenStream) -> Self {
        Self(parts.into_token_stream())
    }

    fn span(&self) -> Span {
        self.0.span()
    }
}

impl PartialEq for NamePath {
    fn eq(&self, other: &Self) -> bool {
        TokenStreamExt::raw_string(&self.0) == TokenStreamExt::raw_string(&other.0)
    }
}

impl From<NamePath> for TokenStream {
    fn from(name: NamePath) -> Self {
        name.0
    }
}

impl IntoTokenStream for NamePath {
    fn into_token_stream(self) -> TokenStream {
        self.into()
    }
}
