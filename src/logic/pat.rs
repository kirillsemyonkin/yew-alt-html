use proc_macro::*;

use crate::reader::TokenReader;
use crate::tt_stream;

use super::errors::*;
use super::tt::*;

fn read_part(
    reader: impl Into<TokenReader>,
    early_quit_punct_sequences: Vec<&'static str>,
) -> Result<TokenTree, DoesNotMatchPrerequisite> {
    let reader = reader.into();
    let ctx = reader.save();

    for early_quit_punct_sequence in early_quit_punct_sequences.clone() {
        let _ctx = reader.save(); // has to be _ctx for Drop to work at the end of the scope

        if let Err(DoesNotMatchPrerequisite) =
            read_punct_exact(reader.clone(), early_quit_punct_sequence)
        {
            continue;
        }

        return Err(DoesNotMatchPrerequisite)?;
    }

    let tt = read_token_tree(reader.clone())?;

    ctx.complete_with(tt)
}

pub fn read(
    reader: impl Into<TokenReader>,
    early_quit_punct_sequences: Vec<&'static str>,
) -> Result<TokenStream, DoesNotMatchPrerequisite> {
    let reader = reader.into();
    let ctx = reader.save();

    let mut results = Vec::<TokenTree>::new();

    while let Ok(pattern_part) = read_part(reader.clone(), early_quit_punct_sequences.clone()) {
        results.push(pattern_part)
    }

    (!results.is_empty()).require_or_does_not_match_prerequisite()?;

    ctx.complete_with(tt_stream![results])
}
