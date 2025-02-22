use crate::plugin::*;
use crate::types::dynamic::Variant;
use crate::{def_package, ExclusiveRange, InclusiveRange, RhaiResultOf, INT};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    iter::{ExactSizeIterator, FusedIterator},
    ops::{Range, RangeInclusive},
};

#[cfg(not(feature = "unchecked"))]
use num_traits::{CheckedAdd as Add, CheckedSub as Sub};

#[cfg(feature = "unchecked")]
use std::ops::{Add, Sub};

// Range iterator with step
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
struct StepRange<T>(T, T, T)
where
    T: Variant + Copy + PartialOrd + Add<Output = T> + Sub<Output = T>;

impl<T> StepRange<T>
where
    T: Variant + Copy + PartialOrd + Add<Output = T> + Sub<Output = T>,
{
    pub fn new(from: T, to: T, step: T) -> RhaiResultOf<Self> {
        #[cfg(not(feature = "unchecked"))]
        if let Some(r) = from.checked_add(&step) {
            if r == from {
                return Err(crate::ERR::ErrorInFunctionCall(
                    "range".to_string(),
                    String::new(),
                    crate::ERR::ErrorArithmetic(
                        "step value cannot be zero".to_string(),
                        Position::NONE,
                    )
                    .into(),
                    Position::NONE,
                )
                .into());
            }
        }

        Ok(Self(from, to, step))
    }
}

impl<T> Iterator for StepRange<T>
where
    T: Variant + Copy + PartialOrd + Add<Output = T> + Sub<Output = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if self.0 == self.1 {
            None
        } else if self.0 < self.1 {
            #[cfg(not(feature = "unchecked"))]
            let diff1 = self.1.checked_sub(&self.0)?;
            #[cfg(feature = "unchecked")]
            let diff1 = self.1 - self.0;

            let v = self.0;

            #[cfg(not(feature = "unchecked"))]
            let n = self.0.checked_add(&self.2)?;
            #[cfg(feature = "unchecked")]
            let n = self.0 + self.2;

            #[cfg(not(feature = "unchecked"))]
            let diff2 = self.1.checked_sub(&n)?;
            #[cfg(feature = "unchecked")]
            let diff2 = self.1 - n;

            if diff2 >= diff1 {
                None
            } else {
                self.0 = if n >= self.1 { self.1 } else { n };
                Some(v)
            }
        } else {
            #[cfg(not(feature = "unchecked"))]
            let diff1 = self.0.checked_sub(&self.1)?;
            #[cfg(feature = "unchecked")]
            let diff1 = self.0 - self.1;

            let v = self.0;

            #[cfg(not(feature = "unchecked"))]
            let n = self.0.checked_add(&self.2)?;
            #[cfg(feature = "unchecked")]
            let n = self.0 + self.2;

            #[cfg(not(feature = "unchecked"))]
            let diff2 = n.checked_sub(&self.1)?;
            #[cfg(feature = "unchecked")]
            let diff2 = n - self.1;

            if diff2 >= diff1 {
                None
            } else {
                self.0 = if n <= self.1 { self.1 } else { n };
                Some(v)
            }
        }
    }
}

impl<T> FusedIterator for StepRange<T> where
    T: Variant + Copy + PartialOrd + Add<Output = T> + Sub<Output = T>
{
}

// Bit-field iterator with step
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
struct BitRange(INT, INT, usize);

const BITS: usize = std::mem::size_of::<INT>() * 8;

impl BitRange {
    pub fn new(value: INT, from: INT, len: INT) -> RhaiResultOf<Self> {
        let from = if from >= 0 {
            let offset = from as usize;

            #[cfg(not(feature = "unchecked"))]
            if offset >= BITS {
                return Err(crate::ERR::ErrorBitFieldBounds(BITS, from, Position::NONE).into());
            }
            offset
        } else {
            #[cfg(not(feature = "unchecked"))]
            if let Some(abs_from) = from.checked_abs() {
                if (abs_from as usize) > BITS {
                    return Err(crate::ERR::ErrorBitFieldBounds(BITS, from, Position::NONE).into());
                }
                BITS - (abs_from as usize)
            } else {
                return Err(crate::ERR::ErrorBitFieldBounds(BITS, from, Position::NONE).into());
            }

            #[cfg(feature = "unchecked")]
            {
                BITS - (from.abs() as usize)
            }
        };

        let len = if len < 0 {
            0
        } else if from + (len as usize) > BITS {
            BITS - from
        } else {
            len as usize
        };

        Ok(Self(value, 1 << from, len))
    }
}

impl Iterator for BitRange {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        if self.2 == 0 {
            None
        } else {
            let r = (self.0 & self.1) != 0;
            self.1 <<= 1;
            self.2 -= 1;
            Some(r)
        }
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.2, Some(self.2))
    }
}

impl FusedIterator for BitRange {}

impl ExactSizeIterator for BitRange {
    #[inline(always)]
    fn len(&self) -> usize {
        self.2
    }
}

// String iterator over characters
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
struct CharsStream(Vec<char>, usize);

impl CharsStream {
    pub fn new(string: &str, from: INT, len: INT) -> Self {
        if len <= 0 {
            return Self(Vec::new(), 0);
        }
        if from >= 0 {
            return Self(
                string
                    .chars()
                    .skip(from as usize)
                    .take(len as usize)
                    .collect(),
                0,
            );
        }
        #[cfg(not(feature = "unchecked"))]
        return if let Some(abs_from) = from.checked_abs() {
            let num_chars = string.chars().count();
            let offset = if num_chars < (abs_from as usize) {
                0
            } else {
                num_chars - (abs_from as usize)
            };
            Self(string.chars().skip(offset).take(len as usize).collect(), 0)
        } else {
            Self(string.chars().skip(0).take(len as usize).collect(), 0)
        };

        #[cfg(feature = "unchecked")]
        return Self(
            string
                .chars()
                .skip(from as usize)
                .take(len as usize)
                .collect(),
            0,
        );
    }
}

impl Iterator for CharsStream {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.1 >= self.0.len() {
            None
        } else {
            let ch = self.0[self.1];
            self.1 += 1;
            Some(ch)
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.0.len() - self.1;
        (remaining, Some(remaining))
    }
}

impl FusedIterator for CharsStream {}

impl ExactSizeIterator for CharsStream {
    #[inline]
    fn len(&self) -> usize {
        self.0.len() - self.1
    }
}

macro_rules! reg_range {
    ($lib:ident | $x:expr => $( $y:ty ),*) => {
        $(
            $lib.set_iterator::<Range<$y>>();
            let _hash = $lib.set_native_fn($x, |from: $y, to: $y| Ok(from..to));

            #[cfg(feature = "metadata")]
            $lib.update_fn_metadata(_hash, &[
                    concat!("from: ", stringify!($y)),
                    concat!("to: ", stringify!($y)),
                    concat!("Iterator<Item=", stringify!($y), ">")
            ]);

            $lib.set_iterator::<RangeInclusive<$y>>();
        )*
    };
    ($lib:ident | step $x:expr => $( $y:ty ),*) => {
        $(
            $lib.set_iterator::<StepRange<$y>>();
            let _hash = $lib.set_native_fn($x, |from: $y, to: $y, step: $y| StepRange::new(from, to, step));

            #[cfg(feature = "metadata")]
            $lib.update_fn_metadata(_hash, &[
                    concat!("from: ", stringify!($y)),
                    concat!("to: ", stringify!($y)),
                    concat!("step: ", stringify!($y)),
                    concat!("Iterator<Item=", stringify!($y), ">")
            ]);
        )*
    };
}

def_package! {
    /// Package of basic range iterators
    crate::BasicIteratorPackage => |lib| {
        lib.standard = true;

        reg_range!(lib | "range" => INT);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_range!(lib | "range" => i8, u8, i16, u16, i32, u32, i64, u64);

            #[cfg(not(target_arch = "wasm32"))]
            #[cfg(not(target_arch = "wasm64"))]
            reg_range!(lib | "range" => i128, u128);
        }

        reg_range!(lib | step "range" => INT);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_range!(lib | step "range" => i8, u8, i16, u16, i32, u32, i64, u64);

            #[cfg(not(target_arch = "wasm32"))]
            #[cfg(not(target_arch = "wasm64"))]
            reg_range!(lib | step "range" => i128, u128);
        }

        #[cfg(not(feature = "no_float"))]
        {
            use crate::FLOAT;

            #[derive(Debug, Clone, Copy, PartialEq)]
            struct StepFloatRange(FLOAT, FLOAT, FLOAT);

            impl StepFloatRange {
                pub fn new(from: FLOAT, to: FLOAT, step: FLOAT) -> RhaiResultOf<Self> {
                    #[cfg(not(feature = "unchecked"))]
                    if step == 0.0 {
                        return Err(crate::ERR::ErrorInFunctionCall("range".to_string(), "".to_string(),
                            crate::ERR::ErrorArithmetic("step value cannot be zero".to_string(), Position::NONE).into(),
                            Position::NONE,
                        ).into());
                    }

                    Ok(Self(from, to, step))
                }
            }

            impl Iterator for StepFloatRange {
                type Item = FLOAT;

                fn next(&mut self) -> Option<FLOAT> {
                    if self.0 == self.1 {
                        None
                    } else if self.0 < self.1 {
                        #[cfg(not(feature = "unchecked"))]
                        if self.2 < 0.0 {
                            return None;
                        }

                        let v = self.0;
                        let n = self.0 + self.2;

                        self.0 = if n >= self.1 { self.1 } else { n };
                        Some(v)
                    } else {
                        #[cfg(not(feature = "unchecked"))]
                        if self.2 > 0.0 {
                            return None;
                        }

                        let v = self.0;
                        let n = self.0 + self.2;

                        self.0 = if n <= self.1 { self.1 } else { n };
                        Some(v)
                    }
                }
            }

            impl FusedIterator for StepFloatRange {}

            lib.set_iterator::<StepFloatRange>();

            let _hash = lib.set_native_fn("range", StepFloatRange::new);
            #[cfg(feature = "metadata")]
            lib.update_fn_metadata(_hash, &["from: FLOAT", "to: FLOAT", "step: FLOAT", "Iterator<Item=FLOAT>"]);
        }

        #[cfg(feature = "decimal")]
        {
            use rust_decimal::Decimal;

            #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
            struct StepDecimalRange(Decimal, Decimal, Decimal);

            impl StepDecimalRange {
                pub fn new(from: Decimal, to: Decimal, step: Decimal) -> RhaiResultOf<Self> {
                    #[cfg(not(feature = "unchecked"))]
                    if step.is_zero() {
                        return Err(crate::ERR::ErrorInFunctionCall("range".to_string(), "".to_string(),
                            crate::ERR::ErrorArithmetic("step value cannot be zero".to_string(), Position::NONE).into(),
                            Position::NONE,
                        ).into());
                    }

                    Ok(Self(from, to, step))
                }
            }

            impl Iterator for StepDecimalRange {
                type Item = Decimal;

                fn next(&mut self) -> Option<Decimal> {
                    if self.0 == self.1 {
                        None
                    } else if self.0 < self.1 {
                        #[cfg(not(feature = "unchecked"))]
                        if self.2.is_sign_negative() {
                            return None;
                        }

                        let v = self.0;
                        let n = self.0 + self.2;

                        self.0 = if n >= self.1 { self.1 } else { n };
                        Some(v)
                    } else {
                        #[cfg(not(feature = "unchecked"))]
                        if self.2.is_sign_positive() {
                            return None;
                        }

                        let v = self.0;
                        let n = self.0 + self.2;

                        self.0 = if n <= self.1 { self.1 } else { n };
                        Some(v)
                    }
                }
            }

            impl FusedIterator for StepDecimalRange {}

            lib.set_iterator::<StepDecimalRange>();

            let _hash = lib.set_native_fn("range", StepDecimalRange::new);
            #[cfg(feature = "metadata")]
            lib.update_fn_metadata(_hash, &["from: Decimal", "to: Decimal", "step: Decimal", "Iterator<Item=Decimal>"]);
        }

        // Register string iterator
        lib.set_iterator::<CharsStream>();

        let _hash = lib.set_native_fn("chars", |string, range: ExclusiveRange| {
            let from = INT::max(range.start, 0);
            let to = INT::max(range.end, from);
            Ok(CharsStream::new(string, from, to - from))
        });
        #[cfg(feature = "metadata")]
        lib.update_fn_metadata(_hash, &["string: &str", "range: Range<INT>", "Iterator<Item=char>"]);

        let _hash = lib.set_native_fn("chars", |string, range: InclusiveRange| {
            let from = INT::max(*range.start(), 0);
            let to = INT::max(*range.end(), from - 1);
            Ok(CharsStream::new(string, from, to-from + 1))
        });
        #[cfg(feature = "metadata")]
        lib.update_fn_metadata(_hash, &["string: &str", "range: RangeInclusive<INT>", "Iterator<Item=char>"]);

        let _hash = lib.set_native_fn("chars", |string, from, len| Ok(CharsStream::new(string, from, len)));
        #[cfg(feature = "metadata")]
        lib.update_fn_metadata(_hash, &["string: &str", "from: INT", "len: INT", "Iterator<Item=char>"]);

        let _hash = lib.set_native_fn("chars", |string, from| Ok(CharsStream::new(string, from, INT::MAX)));
        #[cfg(feature = "metadata")]
        lib.update_fn_metadata(_hash, &["string: &str", "from: INT", "Iterator<Item=char>"]);

        let _hash = lib.set_native_fn("chars", |string| Ok(CharsStream::new(string, 0, INT::MAX)));
        #[cfg(feature = "metadata")]
        lib.update_fn_metadata(_hash, &["string: &str", "Iterator<Item=char>"]);

        #[cfg(not(feature = "no_object"))]
        {
            let _hash = lib.set_getter_fn("chars", |string: &mut ImmutableString| Ok(CharsStream::new(string, 0, INT::MAX)));
            #[cfg(feature = "metadata")]
            lib.update_fn_metadata(_hash, &["string: &mut ImmutableString", "Iterator<Item=char>"]);
        }

        // Register bit-field iterator
        lib.set_iterator::<BitRange>();

        let _hash = lib.set_native_fn("bits", |value, range: ExclusiveRange| {
            let from = INT::max(range.start, 0);
            let to = INT::max(range.end, from);
            BitRange::new(value, from, to - from)
        });
        #[cfg(feature = "metadata")]
        lib.update_fn_metadata(_hash, &["value: INT", "range: Range<INT>", "Iterator<Item=bool>"]);

        let _hash = lib.set_native_fn("bits", |value, range: InclusiveRange| {
            let from = INT::max(*range.start(), 0);
            let to = INT::max(*range.end(), from - 1);
            BitRange::new(value, from, to - from + 1)
        });
        #[cfg(feature = "metadata")]
        lib.update_fn_metadata(_hash, &["value: INT", "range: RangeInclusive<INT>", "Iterator<Item=bool>"]);

        let _hash = lib.set_native_fn("bits", BitRange::new);
        #[cfg(feature = "metadata")]
        lib.update_fn_metadata(_hash, &["value: INT", "from: INT", "len: INT", "Iterator<Item=bool>"]);

        let _hash = lib.set_native_fn("bits", |value, from| BitRange::new(value, from, INT::MAX));
        #[cfg(feature = "metadata")]
        lib.update_fn_metadata(_hash, &["value: INT", "from: INT", "Iterator<Item=bool>"]);

        let _hash = lib.set_native_fn("bits", |value| BitRange::new(value, 0, INT::MAX) );
        #[cfg(feature = "metadata")]
        lib.update_fn_metadata(_hash, &["value: INT", "Iterator<Item=bool>"]);

        #[cfg(not(feature = "no_object"))]
        {
            let _hash = lib.set_getter_fn("bits", |value: &mut INT| BitRange::new(*value, 0, INT::MAX) );
            #[cfg(feature = "metadata")]
            lib.update_fn_metadata(_hash, &["value: &mut INT", "range: Range<INT>", "Iterator<Item=bool>"]);
        }

        combine_with_exported_module!(lib, "range", range_functions);
    }
}

#[export_module]
mod range_functions {
    #[rhai_fn(get = "start", name = "start", pure)]
    pub fn start(range: &mut ExclusiveRange) -> INT {
        range.start
    }
    #[rhai_fn(get = "end", name = "end", pure)]
    pub fn end(range: &mut ExclusiveRange) -> INT {
        range.end
    }
    #[rhai_fn(get = "is_inclusive", name = "is_inclusive", pure)]
    pub fn is_inclusive(range: &mut ExclusiveRange) -> bool {
        let _range = range;
        false
    }
    #[rhai_fn(get = "is_exclusive", name = "is_exclusive", pure)]
    pub fn is_exclusive(range: &mut ExclusiveRange) -> bool {
        let _range = range;
        true
    }
    #[rhai_fn(get = "start", name = "start", pure)]
    pub fn start_inclusive(range: &mut InclusiveRange) -> INT {
        *range.start()
    }
    #[rhai_fn(get = "end", name = "end", pure)]
    pub fn end_inclusive(range: &mut InclusiveRange) -> INT {
        *range.end()
    }
    #[rhai_fn(get = "is_inclusive", name = "is_inclusive", pure)]
    pub fn is_inclusive_inclusive(range: &mut InclusiveRange) -> bool {
        let _range = range;
        true
    }
    #[rhai_fn(get = "is_exclusive", name = "is_exclusive", pure)]
    pub fn is_exclusive_inclusive(range: &mut InclusiveRange) -> bool {
        let _range = range;
        false
    }
}
