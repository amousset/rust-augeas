use augeas_sys::*;
use std::ffi::NulError;
use std::fmt;

#[derive(Clone, PartialEq, Debug)]
pub enum Error {
    Augeas(AugeasError),
    Parse(ParseError),
    Nul(NulError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Augeas(ref err) => err.fmt(f),
            Error::Nul(ref err) => err.fmt(f),
            Error::Parse(ref err) => err.fmt(f),
        }
    }
}

impl Error {
    pub fn is_code(&self, code: ErrorCode) -> bool {
        match self {
            Error::Augeas(err) => err.code == code,
            _ => false,
        }
    }
}

impl From<NulError> for Error {
    fn from(err: NulError) -> Error {
        Error::Nul(err)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct AugeasError {
    pub code: ErrorCode,
    pub message: Option<String>,
    pub minor_message: Option<String>,
    pub details: Option<String>,
}

impl AugeasError {
    pub fn new_no_mem(message: impl Into<String>) -> AugeasError {
        AugeasError {
            code: ErrorCode::NoMem,
            message: Some(message.into()),
            minor_message: None,
            details: None,
        }
    }
}

impl fmt::Display for AugeasError {
    // Write:
    //
    // ```
    //   augeas error:{code}:{message}
    //                {minor_message}
    //                {details}
    // ```
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = self.message.as_ref().map(String::as_ref).unwrap_or("");
        writeln!(f, "augeas error:{:?}:{}", self.code, message)?;

        if let Some(minor_message) = &self.minor_message {
            writeln!(f, "      {}", minor_message)?;
        }

        if let Some(details) = &self.details {
            writeln!(f, "      {}", details)?;
        }

        Ok(())
    }
}

#[repr(C)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ErrorCode {
    NoMem,
    Internal,
    PathExpr,
    NoMatch,
    ManyMatches,
    Syntax,
    NoLens,
    MultipleTransforms,
    NoSpan,
    MoveDescendant,
    CMDRun,
    BadArg,
    Label,
    CopyDescendant,
    Unknown,
}

impl ErrorCode {
    #[allow(non_upper_case_globals)]
    pub fn from_raw(code: aug_errcode_t) -> Option<ErrorCode> {
        Some(match code {
            aug_errcode_t_AUG_NOERROR => return None,
            aug_errcode_t_AUG_ENOMEM => ErrorCode::NoMem,
            aug_errcode_t_AUG_EINTERNAL => ErrorCode::Internal,
            aug_errcode_t_AUG_EPATHX => ErrorCode::PathExpr,
            aug_errcode_t_AUG_ENOMATCH => ErrorCode::NoMatch,
            aug_errcode_t_AUG_EMMATCH => ErrorCode::ManyMatches,
            aug_errcode_t_AUG_ESYNTAX => ErrorCode::Syntax,
            aug_errcode_t_AUG_ENOLENS => ErrorCode::NoLens,
            aug_errcode_t_AUG_EMXFM => ErrorCode::MultipleTransforms,
            aug_errcode_t_AUG_ENOSPAN => ErrorCode::NoSpan,
            aug_errcode_t_AUG_EMVDESC => ErrorCode::MoveDescendant,
            aug_errcode_t_AUG_ECMDRUN => ErrorCode::CMDRun,
            aug_errcode_t_AUG_EBADARG => ErrorCode::BadArg,
            aug_errcode_t_AUG_ELABEL => ErrorCode::Label,
            aug_errcode_t_AUG_ECPDESC => ErrorCode::CopyDescendant,
            _ => ErrorCode::Unknown,
        })
    }
}

impl From<ErrorCode> for Error {
    fn from(code: ErrorCode) -> Error {
        Error::Augeas(AugeasError {
            code,
            message: None,
            minor_message: None,
            details: None,
        })
    }
}

impl From<AugeasError> for Error {
    fn from(err: AugeasError) -> Error {
        Error::Augeas(err)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct ParseError {
    // There's a lot more information we can/should pull out of the
    // tree when parsing goes wrong.
    pub kind: String,
}

impl From<String> for Error {
    fn from(kind: String) -> Error {
        Error::Parse(ParseError { kind })
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error of kind {}", self.kind)
    }
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Error {
        Error::Parse(err)
    }
}