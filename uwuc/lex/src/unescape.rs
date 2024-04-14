use std::borrow::Cow;
use std::iter;

use uwuc_base::errors::{DiagnosticBuilder, DiagnosticSeverity, ErrorReported};
use uwuc_base::Span;

pub fn unescape(s: &str, sp: Span) -> Result<Cow<'_, str>, ErrorReported> {
    if !s.contains('\\') {
        Ok(Cow::Borrowed(s))
    } else {
        let mut out = String::with_capacity(s.len());

        let mut chars = s.char_indices();
        while let Some((slash, c)) = chars.next() {
            if c == '\\' {
                let (nextidx, nextc) = chars.next().unwrap_or_else(|| unreachable!());
                match nextc {
                    'n' => out.push('\n'),
                    'r' => out.push('\r'),
                    't' => out.push('\t'),
                    '\\' => out.push('\\'),
                    '\'' => out.push('\''),
                    '"' => out.push('"'),
                    '0' => out.push('\0'),
                    'x' => match (chars.next(), chars.next()) {
                        (Some((_, c1)), Some((i2, c2))) => {
                            let string: String = iter::once(c1).chain(iter::once(c2)).collect();
                            let num = u8::from_str_radix(&string, 16);
                            let span = Span::new(sp.lo() + slash, sp.lo() + i2 + 1, sp.file());
                            match num {
                                Ok(num) if num < 0x80 => out.push(num as char),
                                Ok(_) => {
                                    DiagnosticBuilder::new(
                                        DiagnosticSeverity::Error,
                                        "ASCII escape literal must be in range of [0x00, 0x7F]",
                                        span,
                                    )
                                    .emit();
                                    return Err(ErrorReported);
                                }
                                _ => {
                                    DiagnosticBuilder::new(
                                        DiagnosticSeverity::Error,
                                        "ASCII escape literal must be a hexadecimal",
                                        span,
                                    )
                                    .emit();
                                    return Err(ErrorReported);
                                }
                            }
                        }
                        (Some((n1, _)), None) => {
                            DiagnosticBuilder::new(
                                DiagnosticSeverity::Error,
                                "ASCII escape literal must be followed by exactly two hexadecimal digits", 
                                Span::new(sp.lo() + slash, sp.lo() + n1, sp.file()),
                            ).emit();
                            return Err(ErrorReported);
                        }
                        _ => {
                            DiagnosticBuilder::new(
                                DiagnosticSeverity::Error,
                                "ASCII escape literal must be followed by exactly two hexadecimal digits",
                                Span::new(sp.lo() + slash, sp.lo() + nextidx, sp.file()),
                            ).emit();
                            return Err(ErrorReported);
                        }
                    },

                    _ => {
                        DiagnosticBuilder::new(
                            DiagnosticSeverity::Error,
                            "unknown escape sequence",
                            Span::new(sp.lo() + slash, sp.lo() + nextidx, sp.file()),
                        )
                        .emit();
                        return Err(ErrorReported);
                    }
                }
            } else {
                out.push(c);
            }
        }
        Ok(Cow::Owned(out))
    }
}
