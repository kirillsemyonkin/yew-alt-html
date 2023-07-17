use proc_macro::*;

use crate::reader::TokenReader;
use crate::tt::IdentExt;
use crate::util::FilterResultOrDefault;
use crate::util::MatchOpt;

use super::errors::*;

pub fn read_token_tree(
    reader: impl Into<TokenReader>,
) -> Result<TokenTree, DoesNotMatchPrerequisite> {
    let reader = reader.into();
    let ctx = reader.save();

    let value = ctx.next().require_or_does_not_match_prerequisite()?;

    ctx.complete_with(value)
}

pub fn read_token_tree_exact<TT>(
    reader: TokenReader,
    expected: impl FnOnce(TokenTree) -> Option<TT>,
) -> Result<TT, DoesNotMatchPrerequisite> {
    let ctx = reader.save();

    let value = read_token_tree(reader.clone())?;
    let value = expected(value).require_or_does_not_match_prerequisite()?;

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
