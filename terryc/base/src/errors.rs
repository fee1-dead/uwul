use std::fmt;

use ariadne::{Label, ReportKind, Source};

use crate::{GlobalCtxt, Context, FileId};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ErrorReported;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    lo: usize,
    hi: usize,
    file: FileId
}

impl Span {
    pub const fn new(lo: usize, hi: usize, file: FileId) -> Self {
        Self { lo, hi, file }
    }

    pub const fn lo(&self) -> usize {
        self.lo
    }

    pub const fn hi(&self) -> usize {
        self.hi
    }

    pub const fn file(&self) -> FileId {
        self.file
    }

    pub fn to(self, other: Span) -> Span {
        assert_eq!(self.file, other.file);
        Span::new(self.lo.min(other.lo), self.hi.max(other.hi), self.file)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.lo..self.hi).fmt(f)
    }
}

impl ariadne::Span for Span {
    type SourceId = FileId;

    fn source(&self) -> &Self::SourceId {
        &self.file
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
    main_span: Span,
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
        let builder = ariadne::Report::build(severity.into(), span.file(), span.lo())
            .with_config(crate::ariadne_config())
            .with_message(message)
            .with_label(Label::new(span));
        Self { builder, main_span: span }
    }

    pub fn emit(self) -> ErrorReported {
        GlobalCtxt::with(|gcx| {
            let id = self.main_span.file();
            let Some(file) = gcx.get_file(id) else { return };
            self.builder.finish().eprint((id, Source::from(file))).unwrap();
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
