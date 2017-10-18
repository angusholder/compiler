#![allow(dead_code)]

use std::marker::PhantomData;
use std::ops::{ Index, IndexMut };

pub trait EntityRef {
    fn new(n: usize) -> Self;
    fn index(&self) -> usize;
}

macro_rules! impl_entity {
    ($T:ident) => {
        impl $crate::entity::EntityRef for $T {
            fn new(n: usize) -> Self {
                $T(n as u32)
            }

            fn index(&self) -> usize {
                self.0 as usize
            }
        }
    };
}



#[derive(Debug, Clone)]
pub struct PrimaryMap<K, V>
    where
        K: EntityRef,
{
    elems: Vec<V>,
    unused: PhantomData<K>,
}

impl<K, V> PrimaryMap<K, V>
    where
        K: EntityRef,
{
    /// Create a new empty map.
    pub fn new() -> Self {
        PrimaryMap {
            elems: Vec::new(),
            unused: PhantomData,
        }
    }

    /// Check if `k` is a valid key in the map.
    pub fn is_valid(&self, k: K) -> bool {
        k.index() < self.elems.len()
    }

    /// Get the element at `k` if it exists.
    pub fn get(&self, k: K) -> Option<&V> {
        self.elems.get(k.index())
    }

    /// Is this map completely empty?
    pub fn is_empty(&self) -> bool {
        self.elems.is_empty()
    }

    /// Get the total number of entity references created.
    pub fn len(&self) -> usize {
        self.elems.len()
    }

    /// Iterate over all the keys in this map.
    pub fn keys(&self) -> Keys<K> {
        Keys::new(self.elems.len())
    }

    /// Remove all entries from this map.
    pub fn clear(&mut self) {
        self.elems.clear()
    }

    /// Get the key that will be assigned to the next pushed value.
    pub fn next_key(&self) -> K {
        K::new(self.elems.len())
    }

    /// Append `v` to the mapping, assigning a new key which is returned.
    pub fn push(&mut self, v: V) -> K {
        let k = self.next_key();
        self.elems.push(v);
        k
    }
}

/// Immutable indexing into an `PrimaryMap`.
/// The indexed value must be in the map.
impl<K, V> Index<K> for PrimaryMap<K, V>
    where
        K: EntityRef,
{
    type Output = V;

    fn index(&self, k: K) -> &V {
        &self.elems[k.index()]
    }
}

/// Mutable indexing into an `PrimaryMap`.
impl<K, V> IndexMut<K> for PrimaryMap<K, V>
    where
        K: EntityRef,
{
    fn index_mut(&mut self, k: K) -> &mut V {
        &mut self.elems[k.index()]
    }
}



#[derive(Debug, Clone)]
pub struct EntityMap<K, V>
    where
        K: EntityRef,
        V: Clone,
{
    elems: Vec<V>,
    default: V,
    unused: PhantomData<K>,
}

/// Shared `EntityMap` implementation for all value types.
impl<K, V> EntityMap<K, V>
    where
        K: EntityRef,
        V: Clone,
{
    /// Create a new empty map.
    pub fn new() -> Self
        where
            V: Default,
    {
        EntityMap {
            elems: Vec::new(),
            default: Default::default(),
            unused: PhantomData,
        }
    }

    /// Create a new empty map with a specified default value.
    ///
    /// This constructor does not require V to implement Default.
    pub fn with_default(default: V) -> Self {
        EntityMap {
            elems: Vec::new(),
            default: default,
            unused: PhantomData,
        }
    }

    /// Get the element at `k` if it exists.
    pub fn get(&self, k: K) -> Option<&V> {
        self.elems.get(k.index())
    }

    /// Is this map completely empty?
    pub fn is_empty(&self) -> bool {
        self.elems.is_empty()
    }

    /// Remove all entries from this map.
    pub fn clear(&mut self) {
        self.elems.clear()
    }

    /// Iterate over all the keys in this map.
    pub fn keys(&self) -> Keys<K> {
        Keys::new(self.elems.len())
    }

    /// Resize the map to have `n` entries by adding default entries as needed.
    pub fn resize(&mut self, n: usize) {
        self.elems.resize(n, self.default.clone());
    }
}

/// Immutable indexing into an `EntityMap`.
///
/// All keys are permitted. Untouched entries have the default value.
impl<K, V> Index<K> for EntityMap<K, V>
    where
        K: EntityRef,
        V: Clone,
{
    type Output = V;

    fn index(&self, k: K) -> &V {
        self.get(k).unwrap_or(&self.default)
    }
}

/// Mutable indexing into an `EntityMap`.
///
/// The map grows as needed to accommodate new keys.
impl<K, V> IndexMut<K> for EntityMap<K, V>
    where
        K: EntityRef,
        V: Clone,
{
    fn index_mut(&mut self, k: K) -> &mut V {
        let i = k.index();
        if i >= self.elems.len() {
            self.resize(i + 1);
        }
        &mut self.elems[i]
    }
}

/// Iterate over all keys in order.
pub struct Keys<K: EntityRef> {
    pos: usize,
    rev_pos: usize,
    unused: PhantomData<K>,
}

impl<K: EntityRef> Keys<K> {
    /// Create a `Keys` iterator that visits `count` entities starting from 0.
    pub fn new(count: usize) -> Keys<K> {
        Keys {
            pos: 0,
            rev_pos: count,
            unused: PhantomData,
        }
    }
}

impl<K: EntityRef> Iterator for Keys<K> {
    type Item = K;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.rev_pos {
            let k = K::new(self.pos);
            self.pos += 1;
            Some(k)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.rev_pos - self.pos;
        (size, Some(size))
    }
}

impl<K: EntityRef> DoubleEndedIterator for Keys<K> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.rev_pos > self.pos {
            let k = K::new(self.rev_pos - 1);
            self.rev_pos -= 1;
            Some(k)
        } else {
            None
        }
    }
}

impl<K: EntityRef> ExactSizeIterator for Keys<K> {}