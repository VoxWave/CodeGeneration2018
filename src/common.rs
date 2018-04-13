// MIT License
// Copyright (c) 2018 Victor Bankowski
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

use std::sync::mpsc::Receiver;
use std::collections::VecDeque;
use std::sync::mpsc::Sender;
use std::ops::Deref;

pub enum Direction {
    Left,
    Right,
    Up,
    Down,
}

///´Source´'s are sources for some type T. Taking from a source returns an optional.
/// While a ´Source´ has things it should return Some(T).
/// If the ´Source´ permanently runs out of things it should return None signaling to
/// the user of the source that they should move on to do other things.
pub trait Source<T> {
    fn take(&mut self) -> Option<T>;
}

impl<T> Source<T> for Vec<T> {
    fn take(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            Some(self.remove(0))
        }
    }
}

impl<T> Source<T> for VecDeque<T> {
    fn take(&mut self) -> Option<T> {
        self.pop_front()
    }
}

impl<T> Source<T> for Receiver<T> {
    fn take(&mut self) -> Option<T> {
        self.recv().ok()
    }
}

/// ´Sink´s are things take take in some type T. Generaly Sinks are used in tandem
/// with sources so that if something is put into a sink it should appear in a source somewhere.
pub trait Sink<T> {
    fn put(&mut self, thing: T);
}

impl<T> Sink<T> for Vec<T> {
    fn put(&mut self, thing: T) {
        self.push(thing);
    }
}

impl<T> Sink<T> for VecDeque<T> {
    fn put(&mut self, thing: T) {
        self.push_back(thing);
    }
}

impl<T> Sink<T> for Sender<T> {
    fn put(&mut self, thing: T) {
        self.send(thing).unwrap();
    }
}

pub struct State<M, Sy>(pub fn(&mut M, Sy) -> State<M, Sy>);
impl<M, Sy> Deref for State<M, Sy> {
    type Target = fn(&mut M, Sy) -> State<M, Sy>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn run_machine<M, Sy, I>(symbols: &mut I, mut machine: M, initial: State<M, Sy>)
where
    I: Source<Sy>
{
    let mut state = initial;
    while let Some(t) = symbols.take() {
        state = state(&mut machine, t);
    }
}