/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A thin wrapper around `ThreadPool` to make sure that we join all things
//! properly.

// From https://github.com/rust-lang/rust-analyzer/blob/7435b9e98c9280043605748c11a1f450669e04d6/crates/rust-analyzer/src/thread_pool.rs
use crossbeam_channel::Sender;

/// Thread stack size for server tasks, in bytes.
///
/// Mostly needed for eqWAlizer AST conversion.
/// Due to inefficient encoding of lists, the default stack size of 2MiB may
/// not be enough for some generated modules.
const THREAD_STACK_SIZE: usize = 10_000_000;

pub struct TaskPool<T> {
    sender: Sender<T>,
    inner: threadpool::ThreadPool,
}

#[allow(unused)]
impl<T> TaskPool<T> {
    pub fn new(sender: Sender<T>) -> TaskPool<T> {
        TaskPool {
            sender,
            inner: threadpool::Builder::new()
                .thread_stack_size(THREAD_STACK_SIZE)
                .build(),
        }
    }

    pub fn new_with_pool(sender: Sender<T>, pool: threadpool::ThreadPool) -> TaskPool<T> {
        TaskPool {
            sender,
            inner: pool,
        }
    }

    pub fn spawn<F>(&mut self, task: F)
    where
        F: FnOnce() -> T + Send + 'static,
        T: Send + 'static,
    {
        self.inner.execute({
            let sender = self.sender.clone();
            move || sender.send(task()).unwrap()
        })
    }

    pub fn spawn_with_sender<F>(&mut self, task: F)
    where
        F: FnOnce(Sender<T>) + Send + 'static,
        T: Send + 'static,
    {
        self.inner.execute({
            let sender = self.sender.clone();
            move || task(sender)
        })
    }

    pub fn len(&self) -> usize {
        self.inner.queued_count()
    }
}

impl<T> Drop for TaskPool<T> {
    fn drop(&mut self) {
        self.inner.join()
    }
}
