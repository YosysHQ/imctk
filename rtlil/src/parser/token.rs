use flussab::{
    text::{self, LineReader},
    Parsed::{self, Fallthrough, Res},
};

use crate::parser::{error::ParseError, DesignSink};

#[cold]
pub fn unexpected(input: &mut LineReader, expected: &str) -> ParseError {
    let mut unexpected_bytes = vec![];

    if text::newline(input.reader(), 0) != 0 {
        return input.give_up(format!("expected {expected}, found end of line"));
    } else if input.reader.is_at_end() {
        return input.give_up(format!("expected {expected}, found end of file"));
    }

    while unexpected_bytes.len() < 60 {
        match input.reader.request_byte_at_offset(unexpected_bytes.len()) {
            Some(b'\n') | Some(b'\r') | Some(b'\t') | Some(b' ')
                if !unexpected_bytes.is_empty() =>
            {
                break
            }
            None => break,
            Some(byte) => unexpected_bytes.push(byte),
        }
    }

    input.give_up(format!(
        "expected {}, found {:?}",
        expected,
        String::from_utf8_lossy(&unexpected_bytes)
    ))
}

/// Returns true when the next character is one of `" \t\r\n#"` or we are at the EOF.
#[inline]
pub fn is_end_of_word(input: &mut LineReader, offset: usize) -> bool {
    matches!(
        input.reader.request_byte_at_offset(offset),
        Some(b' ' | b'\t' | b'\r' | b'\n' | b'#') | None
    )
}

/// Parses a single newline.
#[inline]
pub fn end_of_line(input: &mut LineReader) -> Parsed<(), ParseError> {
    if matches!(input.reader.request_byte(), Some(b'\n' | b'\r')) {
        input.reader.advance(1);
        input.line_at_offset(0);
        skip_multiline_whitespace(input);
        Res(Ok(()))
    } else {
        Fallthrough
    }
}

/// Parses a fixed sequence of bytes ending at an [end of a word][is_end_of_word].
#[inline]
pub fn word(input: &mut LineReader, fixed: &[u8]) -> Parsed<(), ParseError> {
    let offset = text::fixed(input.reader(), 0, fixed);
    if offset != 0 && is_end_of_word(input, offset) {
        let offset = text::tabs_or_spaces(input.reader(), offset);
        input.reader.advance(offset);
        Res(Ok(()))
    } else {
        Fallthrough
    }
}

/// Skips non-newline whitespace or a comment
#[inline]
pub fn skip_whitespace(input: &mut LineReader) {
    let mut skip = text::tabs_or_spaces(input.reader(), 0);
    if let Some(b'#') = input.reader.request_byte_at_offset(skip) {
        skip += 1;
        while !matches!(
            input.reader.request_byte_at_offset(skip),
            Some(b'\r' | b'\n') | None
        ) {
            skip += 1;
        }
    }
    input.reader.advance(skip);
}

#[inline]
pub fn skip_multiline_whitespace(input: &mut LineReader) {
    loop {
        skip_whitespace(input);
        if !matches!(input.reader.request_byte(), Some(b'\n' | b'\r')) {
            break;
        }
        input.reader.advance(1);
        input.line_at_offset(0);
    }
}

/// Parses an integer.
#[inline]
pub fn raw_int(input: &mut LineReader) -> Parsed<i32, String> {
    let (value, offset) = text::signed_ascii_digits_multi(input.reader(), 0);
    if offset != 0 && is_end_of_word(input, offset) {
        if let Some(value) = value {
            input.reader.advance(offset);
            skip_whitespace(input);
            Res(Ok(value))
        } else {
            Res(Err(std::str::from_utf8(&input.reader.buf()[..offset])
                .unwrap()
                .to_owned()))
        }
    } else {
        Fallthrough
    }
}

#[inline]
pub fn int(input: &mut LineReader) -> Parsed<i32, ParseError> {
    input.reader.set_mark();
    raw_int(input).map_err(|lit| {
        input.give_up_at(
            input.reader.mark(),
            format!("integer {} does not fit into a signed 32 bit ingeger", lit),
        )
    })
}

#[inline]
pub fn slice(input: &mut LineReader) -> Parsed<[u32; 2], ParseError> {
    if !matches!(input.reader.request_byte(), Some(b'[')) {
        return Fallthrough;
    }
    input
        .reader
        .set_mark_to_position(input.reader.position() + 1);
    let (value, offset): (Option<i32>, _) = text::ascii_digits_multi(input.reader(), 1);
    if offset == 1 {
        return Fallthrough;
    }

    if let Some(value) = value {
        let value = value as u32;
        match input.reader.request_byte_at_offset(offset) {
            Some(b':') => (),
            Some(b']') => {
                input.reader.advance(offset + 1);

                return Res(Ok([value, value]));
            }
            _ => return Fallthrough,
        }

        input
            .reader
            .set_mark_to_position(input.reader.position() + offset + 1);
        let (second_value, second_offset): (Option<i32>, _) =
            text::ascii_digits_multi(input.reader(), offset + 1);
        if second_offset == offset + 1 {
            return Fallthrough;
        }

        if let Some(second_value) = second_value {
            let second_value = second_value as u32;
            match input.reader.request_byte_at_offset(second_offset) {
                Some(b']') => (),
                _ => return Fallthrough,
            }

            input.reader.advance(second_offset + 1);

            Res(Ok([value, second_value]))
        } else {
            Res(Err(input.give_up_at(
                input.reader.mark(),
                format!(
                    "second offset {} does not fit into a signed 32 bit ingeger",
                    std::str::from_utf8(&input.reader.buf()[..offset]).unwrap()
                ),
            )))
        }
    } else {
        Res(Err(input.give_up_at(
            input.reader.mark(),
            format!(
                "offset {} does not fit into a signed 32 bit ingeger",
                std::str::from_utf8(&input.reader.buf()[..offset]).unwrap()
            ),
        )))
    }
}

#[inline]
pub fn identifier<Sink: DesignSink>(
    input: &mut LineReader,
    sink: &mut Sink,
) -> Parsed<Sink::Name, ParseError> {
    let public = match input.reader.request_byte() {
        Some(b'$') => false,
        Some(b'\\') => true,
        _ => return Fallthrough,
    };
    let mut offset = 1;
    while matches!(input.reader.request_byte_at_offset(offset), Some(b'!'..)) {
        offset += 1;
    }
    if offset == 1 {
        return Fallthrough;
    }
    let identifier = &input.reader.buf()[..offset];
    let result = if public {
        sink.public(identifier)
    } else {
        sink.private(identifier)
    };

    input.reader.advance(offset);

    Res(Ok(result))
}

#[inline]
pub fn string<Sink: DesignSink>(
    input: &mut LineReader,
    sink: &mut Sink,
) -> Parsed<Sink::Constant, ParseError> {
    match input.reader.request_byte() {
        Some(b'"') => (),
        _ => return Fallthrough,
    };

    input.reader.advance(1);

    let mut builder = None;

    let mut offset = 0;
    loop {
        match input.reader.request_byte_at_offset(offset) {
            None => {
                input.reader.advance(offset);
                return Res(Err(unexpected(input, "'\"'")));
            }
            Some(0) => {
                input.reader.advance(offset);
                return Res(Err(unexpected(input, "non-NUL byte")));
            }
            Some(b'"') => break,
            Some(b'\n') => {
                offset += 1;
                input.line_at_offset(offset);
            }

            Some(b'\\') => {
                let builder = builder.get_or_insert_with(|| sink.constant_str_begin());
                sink.constant_str_append(builder, &input.reader.buf()[..offset]);
                input.reader.advance(offset + 1);
                offset = 0;

                match input.reader.request_byte() {
                    None | Some(b'\n' | b'\r') => {
                        return Res(Err(unexpected(input, "escape sequence")))
                    }
                    Some(b'n') => {
                        input.reader.advance(1);
                        sink.constant_str_append(builder, b"\n");
                    }
                    Some(c @ b'0'..=b'7') => match input.reader.request_byte_at_offset(1) {
                        Some(b @ b'0'..=b'7') => match input.reader.request_byte_at_offset(1) {
                            Some(a @ b'0'..=b'7') => {
                                input.reader.advance(3);
                                sink.constant_str_append(
                                    builder,
                                    &[((c - b'0') << 6) | ((b - b'0') << 3) | (a - b'0')],
                                );
                            }
                            _ => {
                                input.reader.advance(2);

                                sink.constant_str_append(
                                    builder,
                                    &[((c - b'0') << 3) | (b - b'0')],
                                );
                            }
                        },
                        _ => {
                            input.reader.advance(1);
                            sink.constant_str_append(builder, &[c - b'0']);
                        }
                    },
                    Some(x) => {
                        input.reader.advance(1);
                        sink.constant_str_append(builder, [x].as_slice());
                    }
                }
            }
            _ => offset += 1,
        }
    }

    let data = if let Some(mut builder) = builder {
        sink.constant_str_append(&mut builder, &input.reader.buf()[..offset]);
        sink.constant_str_build(builder)
    } else {
        sink.constant_str(&input.reader.buf()[..offset])
    };

    input.reader.advance(offset + 1);
    Res(Ok(data))
}

#[inline]
pub fn bits<Sink: DesignSink>(
    input: &mut LineReader,
    sink: &mut Sink,
) -> Parsed<Sink::Constant, ParseError> {
    let (len, offset): (Option<i32>, _) = text::ascii_digits_multi(input.reader(), 0);
    if offset != 0 && matches!(input.reader.request_byte_at_offset(offset), Some(b'\'')) {
        if let Some(len) = len {
            input.reader.advance(offset + 1);

            let mut data_len = len as u32;
            for i in 0..len as usize {
                match input.reader.request_byte_at_offset(i) {
                    Some(b'0' | b'1' | b'x' | b'z' | b'm' | b'-') => {}
                    _ => {
                        if i >= 1 {
                            data_len = i as u32;
                            break;
                        }
                        input.reader.advance(i);
                        return Res(Err(unexpected(input, "valid bit")));
                    }
                }
            }
            let bits = sink.constant_bits(len as u32, unsafe {
                std::mem::transmute(&input.reader.buf()[..data_len as usize])
            });

            input.reader.advance(data_len as usize);
            Res(Ok(bits))
        } else {
            Res(Err(input.give_up_at(
                input.reader.mark(),
                format!(
                    "constant length {} does not fit into a signed 32 bit ingeger",
                    std::str::from_utf8(&input.reader.buf()[..offset]).unwrap()
                ),
            )))
        }
    } else {
        Fallthrough
    }
}
