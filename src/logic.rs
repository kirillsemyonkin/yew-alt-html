use std::collections::HashMap;
use std::vec;

use proc_macro::*;

use crate::content::Attribute;
use crate::content::Attributes;
use crate::content::Content;
use crate::content::Generics;
use crate::content::IfOptLet;
use crate::content::Match;
use crate::content::MatchCase;
use crate::content::NamePath;
use crate::content::Tag;
use crate::content::TagClose;
use crate::content::TagOpen;
use crate::reader::TokenReader;
use crate::tt::IdentExt;
use crate::tt::TokenStreamExt;
use crate::tt_stream;
use crate::util::FilterResultOrDefault;
use crate::util::ImplIntoExt;
use crate::util::MatchOpt;

#[derive(Debug, Copy, Clone, Default)]
pub struct DoesNotMatchPrerequisite;

#[derive(Debug, Copy, Clone)]
pub struct InvalidSyntax(pub &'static str, pub Span);

#[derive(Debug, Copy, Clone)]
pub enum ComplexError {
    DoesNotMatchPrerequisite,
    InvalidSyntax(InvalidSyntax),
}

impl From<DoesNotMatchPrerequisite> for ComplexError {
    fn from(DoesNotMatchPrerequisite: DoesNotMatchPrerequisite) -> Self {
        Self::DoesNotMatchPrerequisite
    }
}

impl From<InvalidSyntax> for ComplexError {
    fn from(e: InvalidSyntax) -> Self {
        Self::InvalidSyntax(e)
    }
}

impl From<(&'static str, Span)> for InvalidSyntax {
    fn from((message, span): (&'static str, Span)) -> Self {
        Self(message, span)
    }
}

impl From<(&'static str, Span)> for ComplexError {
    fn from(e: (&'static str, Span)) -> Self {
        Self::InvalidSyntax(e.into())
    }
}

// RequireDoesNotMatchPrerequisite

trait RequireOrDoesNotMatchPrerequisite {
    type Ret;

    fn require_or_does_not_match_prerequisite(self) -> Self::Ret;
}

impl RequireOrDoesNotMatchPrerequisite for DoesNotMatchPrerequisite {
    type Ret = DoesNotMatchPrerequisite;

    fn require_or_does_not_match_prerequisite(self) -> Self::Ret {
        self
    }
}

impl RequireOrDoesNotMatchPrerequisite for bool {
    type Ret = Result<(), DoesNotMatchPrerequisite>;

    fn require_or_does_not_match_prerequisite(self) -> Self::Ret {
        match self {
            true => Ok(()),
            false => Err(DoesNotMatchPrerequisite),
        }
    }
}

// RequireInvalidSyntax

trait RequireOrInvalidSyntax {
    type Ret;

    fn require_or_invalid_syntax(self, message: &'static str, span: Span) -> Self::Ret;
}

impl RequireOrInvalidSyntax for DoesNotMatchPrerequisite {
    type Ret = InvalidSyntax;

    fn require_or_invalid_syntax(self, message: &'static str, span: Span) -> Self::Ret {
        InvalidSyntax(message, span)
    }
}

impl RequireOrInvalidSyntax for ComplexError {
    type Ret = InvalidSyntax;

    fn require_or_invalid_syntax(self, message: &'static str, span: Span) -> Self::Ret {
        match self {
            ComplexError::DoesNotMatchPrerequisite => {
                DoesNotMatchPrerequisite.require_or_invalid_syntax(message, span)
            }
            ComplexError::InvalidSyntax(e) => e,
        }
    }
}

impl<T, E: RequireOrInvalidSyntax<Ret = InvalidSyntax>> RequireOrInvalidSyntax for Result<T, E> {
    type Ret = Result<T, InvalidSyntax>;

    fn require_or_invalid_syntax(self, message: &'static str, span: Span) -> Self::Ret {
        self.map_err(|e| e.require_or_invalid_syntax(message, span))
    }
}

impl RequireOrInvalidSyntax for bool {
    type Ret = Result<(), InvalidSyntax>;

    fn require_or_invalid_syntax(self, message: &'static str, span: Span) -> Self::Ret {
        match self {
            true => Ok(()),
            false => Err(InvalidSyntax(message, span)),
        }
    }
}

//
// TokenTree
//

pub fn read_token_tree_exact<TT: Clone>(
    reader: TokenReader,
    expected: impl FnOnce(TokenTree) -> Option<TT>,
) -> Result<TT, DoesNotMatchPrerequisite> {
    let ctx = reader.save();

    let value = ctx
        .next()
        .and_then(expected)
        .ok_or(DoesNotMatchPrerequisite)?;

    ctx.complete_with(value)
}

macro_rules! impl_read_tt {
    [$($fn_name:ident -> $type:ident),* $(,)?] => {
        $(pub fn $fn_name(reader: TokenReader) -> Result<$type, DoesNotMatchPrerequisite> {
            read_token_tree_exact(reader, MatchOpt::match_opt)
        })*
    };
}

impl_read_tt![
    read_group -> Group,
    read_ident -> Ident,
    read_punct -> Punct,
    read_literal -> Literal,
];

pub fn read_group_exact(
    reader: TokenReader,
    expected: Delimiter,
) -> Result<Group, DoesNotMatchPrerequisite> {
    let ctx = reader.save();

    let group =
        read_group(reader.clone()).filter_or_default(|group| group.delimiter() == expected)?;

    ctx.complete_with(group)
}

pub fn read_brace(reader: TokenReader) -> Result<Group, DoesNotMatchPrerequisite> {
    read_group_exact(reader, Delimiter::Brace)
}

pub fn read_ident_exact(
    reader: TokenReader,
    expected: &str,
) -> Result<Ident, DoesNotMatchPrerequisite> {
    let ctx = reader.save();

    let ident =
        read_ident(reader.clone()).filter_or_default(|ident| ident.raw_string() == expected)?;

    ctx.complete_with(ident)
}

pub trait ReadPunctExact {
    type Res;

    fn read_punct_exact(self, reader: TokenReader) -> Result<Self::Res, DoesNotMatchPrerequisite>;
}

impl ReadPunctExact for char {
    type Res = Punct;

    fn read_punct_exact(self, reader: TokenReader) -> Result<Self::Res, DoesNotMatchPrerequisite> {
        let ctx = reader.save();

        let result =
            read_punct(reader.clone()).filter_or_default(|punct| punct.as_char() == self)?;

        ctx.complete_with(result)
    }
}

impl ReadPunctExact for &'static str {
    type Res = Vec<Punct>;

    fn read_punct_exact(self, reader: TokenReader) -> Result<Self::Res, DoesNotMatchPrerequisite> {
        let ctx = reader.save();

        let mut results = Vec::new();
        for expected in self.chars() {
            results.push(read_punct_exact(reader.clone(), expected)?);
        }

        ctx.complete_with(results)
    }
}

pub fn read_punct_exact<I: ReadPunctExact>(
    reader: TokenReader,
    expected: I,
) -> Result<I::Res, DoesNotMatchPrerequisite> {
    expected.read_punct_exact(reader)
}

//
// Expressions
//

pub fn read_if_opt_let_expr(reader: TokenReader) -> Result<TokenStream, ComplexError> {
    let ctx = reader.save();

    let mut results = Vec::<TokenTree>::new();

    let if_keyword = read_ident_exact(reader.clone(), "if")?;
    let span = if_keyword.span();
    results.push_from(if_keyword);

    if let Ok(r#let) = read_ident_exact(reader.clone(), "let") {
        results.push_from(r#let);
    }

    results.append_from(
        read_expression(reader.clone(), vec![]).require_or_invalid_syntax(
            "Expected a condition for an if-optionally-let expression",
            span,
        )?,
    );
    results.push_from(read_brace(reader.clone()).require_or_invalid_syntax(
        "Expected an if-true braces value for an if-optionally-let expression",
        span,
    )?);

    'read_else: {
        let Ok(r#else) = read_ident_exact(reader.clone(), "else") else {
            break 'read_else;
        };

        results.push_from(r#else);

        match read_if_opt_let_expr(reader.clone()) {
            Ok(else_if) => {
                results.append_from(else_if);
                break 'read_else;
            }
            Err(ComplexError::DoesNotMatchPrerequisite) => {}
            Err(e) => return Err(e),
        }

        if let Ok(else_group) = read_brace(reader.clone()) {
            results.push_from(else_group);
            break 'read_else;
        }

        return Err((
            "Expected an if-false braces value or another if-optionally-let expression for an else-part of an if-optionally-let expression",
            span,
        ))?;
    }

    ctx.complete_with(tt_stream!(results))
}

pub fn read_match_expr(reader: TokenReader) -> Result<TokenStream, ComplexError> {
    let ctx = reader.save();

    let mut results = Vec::<TokenTree>::new();

    let match_keyword = read_ident_exact(reader.clone(), "match")?;
    let span = match_keyword.span();
    results.push_from(match_keyword);

    results.append_from(
        read_expression(reader.clone(), vec![])
            .require_or_invalid_syntax("Expected a value to match for a match expression", span)?,
    );
    results.push_from(
        read_brace(reader.clone())
            .require_or_invalid_syntax("Expected a matcher brace for a match expression", span)?,
    );

    ctx.complete_with(tt_stream![results])
}

pub fn read_expression_part(
    reader: TokenReader,
    early_quit_punct_sequences: Vec<&'static str>,
    last: Option<&TokenTree>,
) -> Result<TokenStream, ComplexError> {
    for early_quit_punct_sequence in early_quit_punct_sequences.clone() {
        let _ctx = reader.save(); // has to be _ctx for Drop to work at the end of the scope

        if let Err(DoesNotMatchPrerequisite) =
            read_punct_exact(reader.clone(), early_quit_punct_sequence)
        {
            continue;
        }

        return Err(DoesNotMatchPrerequisite)?;
    }

    {
        let ctx = reader.save();

        if let Ok(punct) = read_punct(reader.clone()) {
            return ctx.complete_with(tt_stream![punct]);
        }
    }

    {
        let ctx = reader.save();

        match read_if_opt_let_expr(reader.clone()) {
            Ok(expr) => {
                match last {
                    // allow `if ...` only after puncts (like `+` and such) or at beginning
                    Some(TokenTree::Punct(_)) | None => return ctx.complete_with(expr),
                    _ => {}
                }
            }
            Err(ComplexError::DoesNotMatchPrerequisite) => {}
            Err(e) => return Err(e),
        }
    }

    {
        let ctx = reader.save();

        match read_match_expr(reader.clone()) {
            Ok(expr) => {
                match last {
                    // allow `match ...` only after puncts (like `+` and such) or at beginning
                    Some(TokenTree::Punct(_)) | None => return ctx.complete_with(expr),
                    _ => {}
                }
            }
            Err(ComplexError::DoesNotMatchPrerequisite) => {}
            Err(e) => return Err(e),
        }
    }

    {
        let ctx = reader.save();

        if let Ok(literal) = read_literal(reader.clone()) {
            match last {
                // literal is fine after punct or nothing
                Some(TokenTree::Punct(_)) | None => return ctx.complete_with(tt_stream![literal]),
                _ => {}
            }
        }
    }

    {
        let ctx = reader.save();

        if let Ok(group) = read_group(reader.clone()) {
            match last {
                // do not put `{}` after another brace - this is handled by `match`/`if`
                Some(TokenTree::Group(_)) if group.delimiter() == Delimiter::Brace => {}
                // same for literal
                Some(TokenTree::Literal(_)) if group.delimiter() == Delimiter::Brace => {}
                // do not allow `{}` after ident
                Some(TokenTree::Ident(_)) if group.delimiter() == Delimiter::Brace => {}
                // group is fine in all other cases
                _ => return ctx.complete_with(tt_stream![group]),
            }
        }
    }

    {
        let ctx = reader.save();

        if let Ok(ident) = read_ident(reader.clone()) {
            match last {
                // ident is fine after punct or nothing
                Some(TokenTree::Punct(_)) | None => return ctx.complete_with(tt_stream!(ident)),
                _ => {}
            }
        }
    }

    Err(DoesNotMatchPrerequisite)?
}

pub fn read_expression(
    reader: TokenReader,
    early_quit_punct_sequences: Vec<&'static str>,
) -> Result<TokenStream, ComplexError> {
    let ctx = reader.save();

    let mut results = Vec::<TokenTree>::new();
    loop {
        match read_expression_part(
            reader.clone(),
            early_quit_punct_sequences.clone(),
            results.last(),
        ) {
            Ok(value) => results.append_from(value),
            Err(ComplexError::DoesNotMatchPrerequisite) => break,
            Err(e) => return Err(e),
        }
    }

    (!results.is_empty()).require_or_does_not_match_prerequisite()?;

    ctx.complete_with(tt_stream![results])
}

pub fn read_generics(reader: TokenReader) -> Result<Option<Generics>, InvalidSyntax> {
    let ctx = reader.save();

    let start = match read_punct_exact(reader.clone(), '<') {
        Ok(punct) => punct,
        Err(DoesNotMatchPrerequisite) => return Ok(None),
    };
    let span = start.span();

    // we have to read each one because otherwise we wouldn't be able to know where `>` actually represents a generic closing char
    let mut generics = Vec::<TokenTree>::new();
    loop {
        generics.append_from(
            match read_expression(
                reader.clone(),
                // generics end at generic closing
                vec![">"],
            ) {
                Ok(expr) => expr,
                Err(ComplexError::DoesNotMatchPrerequisite) => break,
                Err(ComplexError::InvalidSyntax(e)) => return Err(e),
            },
        );

        generics.push_from(match read_punct_exact(reader.clone(), ',') {
            Ok(expr) => expr,
            Err(DoesNotMatchPrerequisite) => break,
        });
    }

    read_punct_exact(reader.clone(), '>').require_or_invalid_syntax(
        "Expected ah! component generics closing character '>'",
        span,
    )?;
    let span = span; // TODO span from start to end

    (!generics.is_empty())
        .require_or_invalid_syntax("ah! tag generics should not be empty", span)?;

    ctx.complete_with(Some(Generics::new(generics, span)))
}

pub fn read_attribute_name(reader: TokenReader) -> Result<TokenStream, DoesNotMatchPrerequisite> {
    let ctx = reader.save();

    let mut parts = Vec::<TokenTree>::new();

    let start = read_ident(reader.clone())?;
    parts.push_from(start);

    while let Ok(dash) = read_punct_exact(reader.clone(), '-') {
        parts.push_from(dash);

        let part = read_ident(reader.clone())?;
        parts.push_from(part);
    }

    (!parts.is_empty()).require_or_does_not_match_prerequisite()?;

    ctx.complete_with(tt_stream![parts])
}

pub fn read_short_attribute(reader: TokenReader) -> Result<Attribute, ComplexError> {
    let ctx = reader.save();

    let property = read_punct_exact(reader.clone(), '~').ok();

    let group = read_brace(reader.clone())?;
    let span = group.span();
    let group_vec = group.stream().into_iter().collect::<Vec<_>>();

    (group_vec.len() == 1).require_or_invalid_syntax(
        "Shorthand for attributes is supposed to contain a single variable name",
        span,
    )?;

    let variable = read_ident(TokenReader::from(group.stream())).require_or_invalid_syntax(
        "Shorthand for attributes should contain a variable name",
        span,
    )?;

    ctx.complete_with(Attribute::Short { variable, property })
}

pub fn read_full_attribute(reader: TokenReader) -> Result<Attribute, ComplexError> {
    let ctx = reader.save();

    let property = read_punct_exact(reader.clone(), '~').ok();

    let name = read_attribute_name(reader.clone())?;
    let span = name.span();

    let equals = read_punct_exact(reader.clone(), '=')
        .require_or_invalid_syntax("Expected '=' after attribute name", span)?;

    let value = read_expression(
        reader.clone(),
        // attribute value expression ends at tag closings
        vec!["/>", ">"],
    )?;

    ctx.complete_with(Attribute::Full {
        name,
        equals,
        value,
        property,
    })
}

pub fn read_attributes(reader: TokenReader) -> Result<Attributes, InvalidSyntax> {
    let ctx = reader.save();

    let mut results = HashMap::<String, Attribute>::new();
    loop {
        match read_short_attribute(reader.clone()) {
            Ok(attribute) => {
                let attribute_name = attribute.name_string();

                (!results.contains_key(&attribute_name))
                    .require_or_invalid_syntax("Repeated attribute name", attribute.name_span())?;

                results.insert(attribute_name, attribute);
                continue;
            }
            Err(ComplexError::DoesNotMatchPrerequisite) => {}
            Err(ComplexError::InvalidSyntax(e)) => return Err(e),
        }

        match read_full_attribute(reader.clone()) {
            Ok(attribute) => {
                let attribute_name = attribute.name_string();

                (!results.contains_key(&attribute_name))
                    .require_or_invalid_syntax("Repeated attribute name", attribute.name_span())?;

                results.insert(attribute_name, attribute);
                continue;
            }
            Err(ComplexError::DoesNotMatchPrerequisite) => {}
            Err(ComplexError::InvalidSyntax(e)) => return Err(e),
        }

        break;
    }

    // empty results are ok

    ctx.complete_with(Attributes::new(results))
}

pub fn read_tag_name_dynamic(reader: TokenReader) -> Result<(Punct, TokenStream), ComplexError> {
    let ctx = reader.save();

    let start = read_punct_exact(reader.clone(), '@')?;
    let span = start.span();

    let name = read_expression(
        reader.clone(),
        // generic + tag closings
        vec!["<", "/>", ">"],
    )
    .require_or_invalid_syntax(
        "Expected expression after ah! '@' dynamic tag definition",
        span, // TODO .after()
    )?;

    ctx.complete_with((start, name))
}

pub fn read_tag_name_dashed(reader: TokenReader) -> Result<TokenStream, DoesNotMatchPrerequisite> {
    let ctx = reader.save();

    let mut parts = Vec::<TokenTree>::new();

    let start = read_ident(reader.clone())?;
    parts.push_from(start);

    while let Ok(dash) = read_punct_exact(reader.clone(), '-') {
        parts.push_from(dash);

        let part = read_ident(reader.clone())?;
        parts.push_from(part);
    }

    (parts.len() > 1).require_or_does_not_match_prerequisite()?;

    ctx.complete_with(tt_stream![parts])
}

pub fn read_name_path(reader: TokenReader) -> Result<NamePath, DoesNotMatchPrerequisite> {
    let ctx = reader.save();

    let mut result = Vec::<TokenTree>::new();
    if let Ok(r#extern) = read_punct_exact(reader.clone(), "::") {
        result.append_from(r#extern);
    }

    let mut any_names = false;
    while let Ok(name) = read_ident(reader.clone()) {
        result.push_from(name);
        any_names = true;

        match read_punct_exact(reader.clone(), "::") {
            Ok(separator) => result.append_from(separator),
            Err(DoesNotMatchPrerequisite) => break,
        }
    }

    any_names.require_or_does_not_match_prerequisite()?;

    ctx.complete_with(NamePath::new(result))
}

pub fn read_tag_open(reader: TokenReader) -> Result<(TagOpen, bool), ComplexError> {
    let ctx = reader.save();

    let start_punct = read_punct_exact(reader.clone(), '<')?;
    let span = start_punct.span();

    let result = 'read_name: {
        match read_tag_name_dynamic(reader.clone()) {
            Ok((start, name)) => {
                if let Some(generics) = read_generics(reader.clone())? {
                    return Err((
                        "Did not expect generics after non-path ah! tag name",
                        generics.span(),
                    ))?;
                }

                let attributes = read_attributes(reader.clone())?;

                break 'read_name TagOpen::Dynamic {
                    start,
                    name,
                    attributes,
                };
            }
            Err(ComplexError::DoesNotMatchPrerequisite) => {}
            Err(e) => return Err(e),
        }

        if let Ok(name) = read_tag_name_dashed(reader.clone()) {
            if let Some(generics) = read_generics(reader.clone())? {
                return Err((
                    "Did not expect generics after non-path ah! tag name",
                    generics.span(),
                ))?;
            }

            let attributes = read_attributes(reader.clone())?;

            break 'read_name TagOpen::Dashed { name, attributes };
        }

        if let Ok(name) = read_name_path(reader.clone()) {
            let generics = read_generics(reader.clone())?.unwrap_or_else(|| Generics::empty(span)); // TODO span after name
            let attributes = read_attributes(reader.clone())?;

            break 'read_name TagOpen::Named {
                name,
                generics,
                attributes,
            };
        }

        TagOpen::Fragment
    };

    let void = read_punct_exact(reader.clone(), '/').is_ok();

    read_punct_exact(reader.clone(), '>')
        .require_or_invalid_syntax("Expected ah! tag start closing character '>'", span)?;

    ctx.complete_with((result, void))
}

pub fn read_tag_close(reader: TokenReader, span: Span) -> Result<TagClose, InvalidSyntax> {
    let ctx = reader.save();

    let start_punct = read_punct_exact(reader.clone(), '<')
        .require_or_invalid_syntax("Expected ah! tag end opening character '<'", span)?;
    let span = start_punct.span();

    read_punct_exact(reader.clone(), '/')
        .require_or_invalid_syntax("Expected ah! tag end identifying character '/'", span)?;

    let result = 'read_name: {
        if let Ok(start) = read_punct_exact(reader.clone(), '@') {
            if let Some(generics) = read_generics(reader.clone())? {
                return Err((
                    "Did not expect generics after non-path ah! tag name",
                    generics.span(),
                ))?;
            }

            break 'read_name TagClose::Dynamic { start };
        }

        if let Ok(name) = read_tag_name_dashed(reader.clone()) {
            if let Some(generics) = read_generics(reader.clone())? {
                return Err((
                    "Did not expect generics after non-path ah! tag name",
                    generics.span(),
                ))?;
            }

            break 'read_name TagClose::Dashed { name };
        }

        if let Ok(name) = read_name_path(reader.clone()) {
            let generics = read_generics(reader.clone())?.unwrap_or_else(|| Generics::empty(span)); // TODO span after name

            break 'read_name TagClose::Named { name, generics };
        }

        TagClose::Fragment
    };

    read_punct_exact(reader.clone(), '>')
        .require_or_invalid_syntax("Expected ah! tag end closing character '>'", span)?;

    ctx.complete_with(result)
}

pub fn read_tag(reader: TokenReader) -> Result<Tag, ComplexError> {
    let ctx = reader.save();

    let (tag_open, void) = read_tag_open(reader.clone())?;
    let span = tag_open.span();

    let mut children = Vec::new();
    if !void {
        children = read_children::<read_children::InTag>(reader.clone())?;

        let tag_close = read_tag_close(reader.clone(), span)?;
        let span = tag_close.span();

        if !tag_open.close_match(&tag_close) {
            return Err(("Mismatched ah! tag start and tag end", span))?;
        }
    }

    ctx.complete_with(Tag::new(tag_open, children, void))
}

pub fn read_if_opt_let_content(reader: TokenReader) -> Result<IfOptLet, ComplexError> {
    let ctx = reader.save();

    let if_keyword = read_ident_exact(reader.clone(), "if")?;
    let span = if_keyword.span();

    let mut condition = Vec::<TokenTree>::new();
    if let Ok(r#let) = read_ident_exact(reader.clone(), "let") {
        condition.push_from(r#let);
    }

    condition.append_from(
        read_expression(reader.clone(), vec![]).require_or_invalid_syntax(
            "Expected a condition for an if-optionally-let content",
            span,
        )?,
    );

    let if_true = read_brace(reader.clone()).require_or_invalid_syntax(
        "Expected an if-true braces value for an if-optionally-let content",
        span,
    )?;
    let if_true = read_children::<read_children::InBlock>(if_true.stream())?;

    let mut r#else = None::<(Ident, Box<Content>)>;
    'read_else: {
        let else_keyword = match read_ident_exact(reader.clone(), "else") {
            Ok(else_keyword) => else_keyword,
            Err(DoesNotMatchPrerequisite) => break 'read_else,
        };

        match read_if_opt_let_content(reader.clone()) {
            Ok(else_if) => {
                r#else = Some((else_keyword, Box::new(Content::IfOptLet(else_if))));
                break 'read_else;
            }
            Err(ComplexError::DoesNotMatchPrerequisite) => {}
            Err(e) => return Err(e),
        };

        match read_brace(reader.clone()) {
            Ok(if_false) => {
                let if_false = read_children::<read_children::InBlock>(if_false.stream())?;
                r#else = Some((else_keyword, Box::new(if_false)));
                break 'read_else;
            }
            Err(DoesNotMatchPrerequisite) => {}
        }

        return Err((
            "Expected an if-false braces value or another if-optionally-let expression for an else-part of an if-optionally-let content",
            span,
        ))?;
    }

    ctx.complete_with(IfOptLet::new(if_keyword, condition, if_true, r#else))
}

pub fn read_for_content(
    reader: TokenReader,
    early_quit_punct_sequences: Vec<&'static str>,
) -> Result<TokenStream, ComplexError> {
    let ctx = reader.save();

    match read_brace(reader.clone()) {
        Ok(group) => {
            return read_for_content(TokenReader::from(group.stream()), vec![])
                .and_then(|value| ctx.complete_with(value))
        }
        Err(DoesNotMatchPrerequisite) => {}
    };

    let for_keyword = read_ident_exact(reader.clone(), "for")?;
    let span = for_keyword.span();

    let expr = read_expression(reader.clone(), early_quit_punct_sequences)
        .require_or_invalid_syntax("Expected an expression after a `for` keyword", span)?;

    ctx.complete_with(tt_stream![for_keyword, expr])
}

pub fn read_match_content_case_pattern(
    reader: impl Into<TokenReader>,
) -> Result<TokenStream, ComplexError> {
    let reader = reader.into();

    'read_path_group: {
        let ctx = reader.save();

        let name_path = match read_name_path(reader.clone()) {
            Ok(name_path) => name_path,
            Err(DoesNotMatchPrerequisite) => break 'read_path_group,
        };
        let group = match read_group(reader.clone()) {
            Ok(group) => Some(group),
            Err(DoesNotMatchPrerequisite) => None,
        };

        return ctx.complete_with(tt_stream![name_path, group]);
    }

    'read_group: {
        let ctx = reader.save();

        let group = match read_group(reader.clone()) {
            Ok(group) => group,
            Err(DoesNotMatchPrerequisite) => break 'read_group,
        };

        return ctx.complete_with(tt_stream![group]);
    }

    Err(ComplexError::DoesNotMatchPrerequisite)?
}

pub fn read_match_content_case_pattern_chain(
    reader: impl Into<TokenReader>,
) -> Result<TokenStream, ComplexError> {
    let reader = reader.into();
    let ctx = reader.save();

    let mut results = Vec::<TokenStream>::new();

    if let Ok(pre_or) = read_punct_exact(reader.clone(), '|') {
        results.push(tt_stream![pre_or]);
    }

    let mut any_patterns = false;
    loop {
        match read_match_content_case_pattern(reader.clone()) {
            Ok(case) => results.push(case),
            Err(ComplexError::DoesNotMatchPrerequisite) => break,
            Err(e) => return Err(e),
        }
        any_patterns = true;

        match read_punct_exact(reader.clone(), '|') {
            Ok(or) => results.push(tt_stream![or]),
            Err(DoesNotMatchPrerequisite) => break,
        }
    }

    any_patterns.require_or_does_not_match_prerequisite()?;

    if let Ok(guard_keyword) = read_ident_exact(reader.clone(), "if") {
        let span = guard_keyword.span();

        let guard = read_expression(reader.clone(), vec!["=>"]).require_or_invalid_syntax(
            "Expected expression after match guard `if` keyword",
            span,
        )?;

        results.push(tt_stream![guard_keyword, guard])
    }

    ctx.complete_with(tt_stream![results])
}

pub fn read_match_content_case_value(
    reader: impl Into<TokenReader>,
    span: Span,
) -> Result<(Content, bool), InvalidSyntax> {
    let reader = reader.into();
    let ctx = reader.save();

    match read_brace(reader.clone()) {
        Ok(group) => {
            return read_children::<read_children::InBlock>(group.stream())
                .and_then(|value| ctx.complete_with((value, true)))
        }
        Err(DoesNotMatchPrerequisite) => {}
    };

    let value = read_content::<read_content::InMatchCase>(reader.clone())
        .require_or_invalid_syntax("Expected content after the match case arrow", span)?;

    ctx.complete_with((value, false))
}

pub fn read_match_content_case(
    reader: impl Into<TokenReader>,
) -> Result<(MatchCase, bool), ComplexError> {
    let reader = reader.into();
    let ctx = reader.save();

    let pattern = read_match_content_case_pattern_chain(reader.clone())?;
    let span = pattern.span();

    let arrow = tt_stream![read_punct_exact(reader.clone(), "=>")
        .require_or_invalid_syntax("Expected match case arrow after match case pattern", span)?];
    let span = arrow.span();

    let (value, is_brace) = read_match_content_case_value(reader.clone(), span)?;

    ctx.complete_with((
        MatchCase::Case {
            pattern,
            arrow,
            value,
        },
        is_brace,
    ))
}

pub fn read_match_content_cases(
    reader: impl Into<TokenReader>,
) -> Result<Vec<MatchCase>, InvalidSyntax> {
    let reader = reader.into();
    let ctx = reader.save();

    let mut results = Vec::new();
    loop {
        let is_brace = match read_match_content_case(reader.clone()) {
            Ok((case, is_brace)) => {
                results.push(case);
                is_brace
            }
            Err(ComplexError::DoesNotMatchPrerequisite) => break,
            Err(ComplexError::InvalidSyntax(e)) => return Err(e),
        };

        match read_punct_exact(reader.clone(), ',') {
            Ok(comma) => results.push(MatchCase::Comma(comma)),
            Err(DoesNotMatchPrerequisite) => {
                if !is_brace {
                    break;
                }
            }
        }
    }

    if let Some(span) = reader.remaining_span() {
        return Err(("ah! match brace read dangling text", span))?;
    }

    ctx.complete_with(results)
}

pub fn read_match_content(reader: TokenReader) -> Result<Match, ComplexError> {
    let ctx = reader.save();

    match read_brace(reader.clone()) {
        Ok(group) => {
            return read_match_content(TokenReader::from(group.stream()))
                .and_then(|value| ctx.complete_with(value))
        }
        Err(DoesNotMatchPrerequisite) => {}
    };

    let match_keyword = read_ident_exact(reader.clone(), "match")?;
    let span = match_keyword.span();

    let value = read_expression(reader.clone(), vec![])
        .require_or_invalid_syntax("Expected a value expression after a `match` keyword", span)?;

    let group = read_brace(reader.clone()).require_or_invalid_syntax(
        "Expected a match cases brace after a match value expression",
        span,
    )?;

    let cases = read_match_content_cases(group.stream())?;

    ctx.complete_with(Match::new(match_keyword, value, cases))
}

//
// read_content
//

pub mod read_content {
    use super::*;

    pub trait Location: private::Sealed {
        fn early_quit_punct_sequences() -> Vec<&'static str>;

        fn early_quit_punct_sequences_expr() -> Vec<&'static str>;
    }

    pub struct InTag;

    impl Location for InTag {
        fn early_quit_punct_sequences() -> Vec<&'static str> {
            vec!["</"]
        }

        fn early_quit_punct_sequences_expr() -> Vec<&'static str> {
            vec!["</", "<"]
        }
    }

    pub struct InBlock;

    impl Location for InBlock {
        fn early_quit_punct_sequences() -> Vec<&'static str> {
            vec![]
        }

        fn early_quit_punct_sequences_expr() -> Vec<&'static str> {
            vec!["<"]
        }
    }

    pub struct InMatchCase;

    impl Location for InMatchCase {
        fn early_quit_punct_sequences() -> Vec<&'static str> {
            vec![","]
        }

        fn early_quit_punct_sequences_expr() -> Vec<&'static str> {
            vec![","]
        }
    }
}

pub fn read_content<Loc: read_content::Location>(
    reader: TokenReader,
) -> Result<Content, ComplexError> {
    let ctx = reader.save();

    'try_early_quit: for early_quit_punct_sequence in Loc::early_quit_punct_sequences() {
        let _ctx = reader.save(); // has to be _ctx for Drop to work at the end of the scope

        if let Err(DoesNotMatchPrerequisite) =
            read_punct_exact(reader.clone(), early_quit_punct_sequence)
        {
            continue 'try_early_quit;
        }

        return Err(DoesNotMatchPrerequisite)?;
    }

    match read_tag(reader.clone()) {
        Ok(tag) => return ctx.complete_with(Content::Tag(tag)),
        Err(ComplexError::DoesNotMatchPrerequisite) => {}
        Err(e) => return Err(e),
    }

    match read_for_content(reader.clone(), Loc::early_quit_punct_sequences_expr()) {
        Ok(r#for) => return ctx.complete_with(Content::For(r#for)),
        Err(ComplexError::DoesNotMatchPrerequisite) => {}
        Err(e) => return Err(e),
    }

    match read_if_opt_let_content(reader.clone()) {
        Ok(if_opt_let) => return ctx.complete_with(Content::IfOptLet(if_opt_let)),
        Err(ComplexError::DoesNotMatchPrerequisite) => {}
        Err(e) => return Err(e),
    }

    match read_match_content(reader.clone()) {
        Ok(r#match) => return ctx.complete_with(Content::Match(r#match)),
        Err(ComplexError::DoesNotMatchPrerequisite) => {}
        Err(e) => return Err(e),
    }

    match read_expression(reader.clone(), Loc::early_quit_punct_sequences_expr()) {
        Ok(expr) => return ctx.complete_with(Content::Expression(expr)),
        Err(ComplexError::DoesNotMatchPrerequisite) => {}
        Err(e) => return Err(e),
    }

    Err(DoesNotMatchPrerequisite)?
}

//
// read_children
//

pub mod read_children {
    use super::*;

    pub trait Location: private::Sealed {
        type Ret;
        type ReadContentLoc: read_content::Location;

        fn process_children(
            children: Vec<Content>,
            dangling_span: Option<Span>,
        ) -> Result<Self::Ret, InvalidSyntax>;
    }

    pub struct InTag;

    impl Location for InTag {
        type Ret = Vec<Content>;
        type ReadContentLoc = read_content::InTag;

        fn process_children(
            children: Vec<Content>,
            _dangling_span: Option<Span>,
        ) -> Result<Self::Ret, InvalidSyntax> {
            // empty children is ok
            Ok(children)
        }
    }

    pub struct InBlock;

    impl Location for InBlock {
        type Ret = Content;
        type ReadContentLoc = read_content::InBlock;

        fn process_children(
            children: Vec<Content>,
            dangling_span: Option<Span>,
        ) -> Result<Self::Ret, InvalidSyntax> {
            if let Some(span) = dangling_span {
                return Err(("ah! read dangling text", span))?;
            }

            let result = if children.len() != 1 {
                // auto-fragment at 0 or 2+ children inside braces {}
                Content::Tag(Tag::new(TagOpen::Fragment, children, false))
            } else {
                // grab first child if there's only one
                children.into_iter().next().unwrap()
            };

            Ok(result)
        }
    }
}

pub fn read_children<Loc: read_children::Location>(
    reader: impl Into<TokenReader>,
) -> Result<Loc::Ret, InvalidSyntax> {
    let reader = reader.into();
    let ctx = reader.save();

    let mut children = Vec::<Content>::new();
    loop {
        match read_content::<Loc::ReadContentLoc>(reader.clone()) {
            Ok(child) => children.push(child),
            Err(ComplexError::DoesNotMatchPrerequisite) => break,
            Err(ComplexError::InvalidSyntax(e)) => return Err(e),
        }
    }

    let result = Loc::process_children(children, reader.remaining_span())?;
    ctx.complete_with(result)
}

mod private {
    use super::*;

    pub trait Sealed {}

    impl Sealed for read_children::InTag {}
    impl Sealed for read_children::InBlock {}
    impl Sealed for read_content::InTag {}
    impl Sealed for read_content::InBlock {}
    impl Sealed for read_content::InMatchCase {}
}
