use std::str::Chars;

#[derive(Debug)]
pub struct PeekableCharIndices<'a> {
    front: *const u8,
    iter: Chars<'a>,
    peeked: Option<(usize, char)>,
}

impl<'a> PeekableCharIndices<'a> {
    pub fn new(string: &'a str) -> PeekableCharIndices<'a> {
        PeekableCharIndices {
            front: string.as_ptr(),
            iter: string.chars(),
            peeked: None,
        }
    }

    pub fn next(&mut self) -> Option<(usize, char)> {
        let index = self.raw_offset();

        if let Some((index, ch)) = self.peeked.take() {
            Some((index, ch))
        } else if let Some(ch) = self.iter.next() {
            self.peeked = Some((index, ch));
            Some((index, ch))
        } else {
            None
        }
    }

    pub fn peek(&mut self) -> Option<(usize, char)> {
        if let Some((index, ch)) = self.peeked {
            Some((index, ch))
        } else {
            let result = self.next();
            self.peeked = result;
            result
        }
    }

    fn raw_offset(&self) -> usize {
        (self.iter.as_str().as_ptr() as usize) - (self.front as usize)
    }

    pub fn offset(&self) -> usize {
        if let Some((index, _)) = self.peeked {
            index
        } else {
            self.raw_offset()
        }
    }

    pub fn matches(&mut self, expected: char) -> bool {
        if let Some((_, found)) = self.peek() {
            if found == expected {
                self.peeked = None;
                true
            } else {
                false
            }
        } else {
            false
        }
    }
}
