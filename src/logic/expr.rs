use proc_macro::*;

use crate::reader::TokenReader;
use crate::tt_stream;
use crate::util::ImplIntoExt;

use super::errors::*;
use super::tt::*;

pub fn read_if_opt_let(reader: TokenReader) -> Result<TokenStream, ComplexError> {
    let ctx = reader.save();

    let mut results = Vec::<TokenTree>::new();

    let if_keyword = read_ident_exact(reader.clone(), "if")?;
    let span = if_keyword.span();
    results.push_from(if_keyword);

    if let Ok(r#let) = read_ident_exact(reader.clone(), "let") {
        results.push_from(r#let);
    }

    results.append_from(read(reader.clone(), vec![]).require_or_invalid_syntax(
        "Expected a condition for an if-optionally-let expression",
        span,
    )?);
    results.push_from(read_brace(reader.clone()).require_or_invalid_syntax(
        "Expected an if-true braces value for an if-optionally-let expression",
        span,
    )?);

    'read_else: {
        let Ok(r#else) = read_ident_exact(reader.clone(), "else") else {
            break 'read_else;
        };

        results.push_from(r#else);

        match read_if_opt_let(reader.clone()) {
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

    ctx.complete_with(tt_stream![results])
}

fn read_match(reader: TokenReader) -> Result<TokenStream, ComplexError> {
    let ctx = reader.save();

    let mut results = Vec::<TokenTree>::new();

    let match_keyword = read_ident_exact(reader.clone(), "match")?;
    let span = match_keyword.span();
    results.push_from(match_keyword);

    results.append_from(
        read(reader.clone(), vec![])
            .require_or_invalid_syntax("Expected a value to match for a match expression", span)?,
    );
    results.push_from(
        read_brace(reader.clone())
            .require_or_invalid_syntax("Expected a matcher brace for a match expression", span)?,
    );

    ctx.complete_with(tt_stream![results])
}

fn read_part(
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

        match read_if_opt_let(reader.clone()) {
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

        match read_match(reader.clone()) {
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

pub fn read(
    reader: TokenReader,
    early_quit_punct_sequences: Vec<&'static str>,
) -> Result<TokenStream, ComplexError> {
    let ctx = reader.save();

    let mut results = Vec::<TokenTree>::new();
    loop {
        match read_part(
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
