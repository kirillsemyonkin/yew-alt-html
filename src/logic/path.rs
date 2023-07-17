use proc_macro::TokenTree;

use crate::content::NamePath;
use crate::reader::TokenReader;
use crate::util::ImplIntoExt;

use super::errors::*;
use super::tt::*;

pub fn read(reader: TokenReader) -> Result<NamePath, DoesNotMatchPrerequisite> {
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
