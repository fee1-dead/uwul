use std::fmt;

use ariadne::{Label, ReportKind, Source};

use crate::{GlobalCtxt, Context};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ErrorReported;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    lo: usize,
    hi: usize,
}

impl Span {
    pub const fn new(lo: usize, hi: usize) -> Self {
        Self { lo, hi }
    }

    pub const fn lo(&self) -> usize {
        self.lo
    }

    pub const fn hi(&self) -> usize {
        self.hi
    }

    pub fn to(self, other: Span) -> Span {
        Span::new(self.lo.min(other.lo), self.hi.max(other.hi))
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.lo..self.hi).fmt(f)
    }
}

impl ariadne::Span for Span {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        &()
    }

    fn start(&self) -> usize {
        self.lo
    }

    fn end(&self) -> usize {
        self.hi
    }
}

pub struct DiagnosticBuilder {
    builder: ariadne::ReportBuilder<Span>,
}

pub enum DiagnosticSeverity {
    Error,
}

impl From<DiagnosticSeverity> for ariadne::ReportKind {
    fn from(s: DiagnosticSeverity) -> Self {
        match s {
            DiagnosticSeverity::Error => ReportKind::Error,
        }
    }
}

impl DiagnosticBuilder {
    pub fn new(severity: DiagnosticSeverity, message: impl ToString, span: Span) -> Self {
        let builder = ariadne::Report::build(severity.into(), (), span.lo())
            .with_config(crate::ariadne_config())
            .with_message(message)
            .with_label(Label::new(span));
        Self { builder }
    }

    pub fn emit(self) -> ErrorReported {
        GlobalCtxt::with(|gcx| {
            let Some(file) = gcx.get_file(crate::FileId::main()) else { return };
            self.builder.finish().eprint(Source::from(file)).unwrap();
        });
        ErrorReported
    }
}

pub macro make_diag {
    (
        Error,
        $span:expr,
        $fmt:literal
        $(,
            $($arg:expr),*$(,)?
        )?
    ) => {
        $crate::errors::DiagnosticBuilder::new(
            $crate::errors::DiagnosticSeverity::Error,
            format!($fmt, $($($arg),*)?),
            $span,
        )
    }
}
