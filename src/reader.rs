use std::cell::RefCell;
use std::rc::Rc;
use std::vec;

use proc_macro::*;

#[derive(Clone, Debug)]
pub struct TokenReader {
    state: Rc<RefCell<TokenReaderState>>,
}

pub struct TokenReaderState {
    stream: vec::IntoIter<TokenTree>,

    buffer: Vec<TokenTree>,
    context_stack: Vec<usize>,
    curr_buf_pos: usize,
}

impl std::fmt::Debug for TokenReaderState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TokenReaderState")
            .field("buffer", &self.buffer)
            .field(
                "context_stack",
                &self.context_stack,
            )
            .field(
                "curr_buf_pos",
                &self.curr_buf_pos,
            )
            .finish()
    }
}

impl From<TokenStream> for TokenReader {
    fn from(stream: TokenStream) -> Self {
        let data = Rc::new(RefCell::new(
            TokenReaderState {
                stream: stream
                    .into_iter()
                    .collect::<Vec<_>>()
                    .into_iter(),
                buffer: Vec::new(),
                context_stack: Vec::new(),
                curr_buf_pos: 0,
            },
        ));
        Self { state: data }
    }
}

impl TokenReader {
    pub fn save(&self) -> Context {
        let mut state = self
            .state
            .borrow_mut();
        let curr_buf_pos = state.curr_buf_pos;
        state
            .context_stack
            .push(curr_buf_pos);

        Context {
            reader: self,
            completed: false,
        }
    }

    fn restore(&self) {
        let mut state = self
            .state
            .borrow_mut();
        let last = state
            .context_stack
            .len()
            - 1;
        let offset = state
            .context_stack
            .remove(last);
        state.curr_buf_pos = offset;
    }

    fn accept(&self) {
        let mut state = self
            .state
            .borrow_mut();
        let last = state
            .context_stack
            .len()
            - 1;
        state
            .context_stack
            .remove(last);
        if state
            .context_stack
            .is_empty()
        {
            // probably not the fastest
            while state.curr_buf_pos > 0 {
                state.curr_buf_pos -= 1;
                state
                    .buffer
                    .remove(0);
            }
        }
    }
}

#[must_use]
pub struct Context<'a> {
    reader: &'a TokenReader,
    completed: bool,
}

impl<'a> Context<'a> {
    pub fn complete(mut self) { self.completed = true; }

    pub fn complete_with<T, E>(self, value: T) -> Result<T, E> {
        self.complete();
        Ok(value)
    }

    pub fn next(&self) -> Option<TokenTree> {
        let mut state = self
            .reader
            .state
            .borrow_mut();

        // if we are not at the end of the buffer, read from buffer
        if state.curr_buf_pos
            < state
                .buffer
                .len()
        {
            if !state
                .context_stack
                .is_empty()
            {
                let c = state.buffer[state.curr_buf_pos].clone();
                state.curr_buf_pos += 1;
                return Some(c);
            }

            return Some(
                state
                    .buffer
                    .remove(0),
            );
        }

        let c = state
            .stream
            .next()?;

        if !state
            .context_stack
            .is_empty()
        {
            state
                .buffer
                .push(c.clone());
            state.curr_buf_pos += 1;
        }

        Some(c)
    }
}

impl<'a> Drop for Context<'a> {
    fn drop(&mut self) {
        if self.completed {
            self.reader
                .accept();
        } else {
            self.reader
                .restore();
        }
    }
}
