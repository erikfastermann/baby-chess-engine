use std::{fmt, ops};

use crate::position::position_to_index;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Bitset(u64);

pub const ZERO: Bitset = Bitset(0);
pub const ONES: Bitset = Bitset(u64::MAX);

pub const ROW_0: Bitset = Bitset(0xff);
pub const ROW_1: Bitset = Bitset(0xff << 8);
pub const ROW_6: Bitset = Bitset(0xff << 48);
pub const ROW_7: Bitset = Bitset(0xff << 56);

pub const COLUMN_0: Bitset = Bitset(1 | (1 << 8) | (1 << 16) | (1 << 24) | (1 << 32) | (1 << 40) | (1 << 48) | (1 << 56));
pub const COLUMN_7: Bitset = Bitset(COLUMN_0.0 << 7);

impl fmt::Binary for Bitset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl ops::BitOr for Bitset {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl ops::BitOrAssign for Bitset {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl ops::Not for Bitset {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl ops::BitAnd for Bitset {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl ops::Shl<u8> for Bitset {
    type Output = Self;

    fn shl(self, rhs: u8) -> Self::Output {
        debug_assert!(rhs < 64);
        Self(self.0 << rhs)
    }
}

impl ops::Shr<u8> for Bitset {
    type Output = Self;

    fn shr(self, rhs: u8) -> Self::Output {
        debug_assert!(rhs < 64);
        Self(self.0 >> rhs)
    }
}

impl fmt::Display for Bitset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board = [b'.'; 64];
        crate::fmt::fmt_pieces(&mut board, self.clone(), '#');
        crate::fmt::fmt_board(&board, f)
    }
}

impl fmt::Debug for Bitset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl Bitset {
    pub fn zero() -> Self {
        ZERO
    }

    pub fn ones() -> Self {
        ONES
    }

    pub fn from_index(index: u8) -> Self {
        Self::zero().with(index)
    }

    pub fn from_position(x: u8, y: u8) -> Self {
        Self::zero().with(position_to_index(x, y))
    }

    pub fn before(index: u8) -> Self {
        debug_assert!(index < 64);
        Self(u64::MAX.checked_shr((64-index).into()).unwrap_or(0))
    }

    pub fn after(index: u8) -> Self {
        debug_assert!(index < 64);
        Self(u64::MAX.checked_shl((index+1).into()).unwrap_or(0))
    }

    pub fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub fn count(self) -> i32 {
        self.0.count_ones() as i32
    }

    pub fn first_index(self) -> u8 {
        self.0.trailing_zeros() as u8
    }

    pub fn try_first_index(self) -> Option<u8> {
        let trailing_zeros = self.0.trailing_zeros() as u8;
        if trailing_zeros < 64 { Some(trailing_zeros) } else { None }
    }

    pub fn try_last_index(self) -> Option<u8> {
        63u8.checked_sub(self.0.leading_zeros() as u8)
    }

    pub fn has(self, index: u8) -> bool {
        debug_assert!(index < 64);
        (self.0 & (1 << index)) != 0
    }

    pub fn overlaps(self, other: Self) -> bool {
        (self & other) != Bitset::zero()
    }

    pub fn checked_clear(&mut self, index: u8) {
        assert!(self.has(index));
        self.clear(index);
    }

    pub fn clear(&mut self, index: u8) {
        *self = self.without(index);
    }

    pub fn without(self, index: u8) -> Self {
        debug_assert!(index < 64);
        Self(self.0 & !(1 << index))
    }

    pub fn checked_set(&mut self, index: u8) {
        assert!(!self.has(index));
        self.set(index);
    }

    pub fn set(&mut self, index: u8) {
        *self = self.with(index);
    }

    pub fn with(self, index: u8) -> Self {
        debug_assert!(index < 64);
        Self(self.0 | (1 << index))
    }

    pub fn mov(&mut self, from: u8, to: u8) {
        self.clear(from);
        self.set(to);
    }

    pub fn indices(self) -> Indices {
        Indices(self)
    }
}

pub struct Indices(Bitset);

impl Iterator for Indices {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_empty() {
            None
        } else {
            let index = self.0.first_index();
            self.0.clear(index);
            Some(index)
        }
    }
}
