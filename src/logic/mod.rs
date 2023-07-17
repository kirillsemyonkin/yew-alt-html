pub mod errors;
mod expr;
mod pat;
mod tt;
mod path;

use std::collections::HashMap;
use std::vec;

use proc_macro::*;

use self::errors::*;
use self::tt::*;
use crate::content::Attribute;
use crate::content::Attributes;
use crate::content::Content;
use crate::content::Generics;
use crate::content::IfOptLet;
use crate::content::Match;
use crate::content::MatchCase;
use crate::content::Tag;
use crate::content::TagClose;
use crate::content::TagOpen;
use crate::reader::TokenReader;
use crate::tt::TokenStreamExt;
use crate::tt_stream;
use crate::util::ImplIntoExt;

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
            match expr::read(
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

    let value = expr::read(
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

    let name = expr::read(
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

        if let Ok(name) = path::read(reader.clone()) {
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

        if let Ok(name) = path::read(reader.clone()) {
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
        let span = r#let.span();

        condition.push_from(r#let);

        let pattern = pat::read(reader.clone(), vec!["="]).require_or_invalid_syntax(
            "Expected a pattern after if-optionally-let's `let` keyword",
            span,
        )?;
        let span = pattern.span();

        condition.append_from(pattern);

        let equals = read_punct_exact(reader.clone(), '=').require_or_invalid_syntax(
            "Expected an '=' assignment operator after if-optionally-let's let pattern",
            span,
        )?;

        condition.push_from(equals)
    }

    condition.append_from(
        expr::read(reader.clone(), vec![]).require_or_invalid_syntax(
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

    let expr = expr::read(reader.clone(), early_quit_punct_sequences)
        .require_or_invalid_syntax("Expected an expression after a `for` keyword", span)?;

    ctx.complete_with(tt_stream![for_keyword, expr])
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

    let pattern = pat::read(reader.clone(), vec!["=>"])?;
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

    let value = expr::read(reader.clone(), vec![])
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

    match expr::read(reader.clone(), Loc::early_quit_punct_sequences_expr()) {
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
