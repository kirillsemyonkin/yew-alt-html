use proc_macro::Span;

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

pub trait RequireOrDoesNotMatchPrerequisite {
    type Ret;

    fn require_or_does_not_match_prerequisite(self) -> Self::Ret;
}

impl RequireOrDoesNotMatchPrerequisite for DoesNotMatchPrerequisite {
    type Ret = DoesNotMatchPrerequisite;

    fn require_or_does_not_match_prerequisite(self) -> Self::Ret {
        self
    }
}

impl<T> RequireOrDoesNotMatchPrerequisite for Option<T> {
    type Ret = Result<T, DoesNotMatchPrerequisite>;

    fn require_or_does_not_match_prerequisite(self) -> Self::Ret {
        self.ok_or(DoesNotMatchPrerequisite)
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

pub trait RequireOrInvalidSyntax {
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
