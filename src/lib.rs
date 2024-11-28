//! Allows to compute on big numbers. No negative numbers support. Provides only some
//! basic mathematical functions.

#![no_std]

extern crate alloc;
type RawRow = Vec<u8>;

/// `PlacesRow` represents row of decimal places starting at ones (`0` index).
#[derive(Clone, PartialEq, Debug)]
pub struct PlacesRow {
    row: RawRow,
}

use core::ops::Deref;
impl Deref for PlacesRow {
    type Target = [u8];

    /// View into internal storage.
    fn deref(&self) -> &[u8] {
        self.row.as_slice()
    }
}

impl PlacesRow {
    /// Strong ctor for usage with prebuilded raw places row.
    ///
    /// Only ones are allowed in `row`.
    /// Places in `row` have to be ordered from ones over tens, hundreds, … to highest place;
    /// from 0-index to last-index.
    ///
    /// Leading zeros are truncated. Does not change capacity.
    ///
    /// Returns `PlacesRow` or index where place > `9` was
    /// encountered. `None` for 0-len `row`.
    pub fn new_from_vec(mut row: Vec<u8>) -> Result<Self, Option<usize>> {
        if row.len() == 0 {
            return Err(None);
        }

        let row_len = len_without_leading_raw(&row, 0, 1);

        for ix in 0..row_len {
            if row[ix] > 9 {
                return Err(Some(ix));
            }
        }

        row.truncate(row_len);
        Ok(PlacesRow { row })
    }

    /// Handy ctor for usage with _classic_ primitive numeric data type.
    pub fn new_from_num(mut num: u128) -> Self {
        let mut row = Vec::new();
        loop {
            let d = num % 10;
            row.push(d as u8);
            num = num / 10;

            if num == 0 {
                break;
            }
        }

        PlacesRow { row }
    }

    /// Handy ctor for usage with long numbers.
    ///
    /// Only digits are allowed in `s`. Leading zeros are ommitted.
    ///
    /// Returns `PlacesRow` or index in `s` where uncovertable `char` was
    /// encountered. `None` for empty string.
    pub fn new_from_str(mut s: &str) -> Result<Self, Option<usize>> {
        let s_len_orig = s.len();
        if s_len_orig == 0 {
            return Err(None);
        }

        s = s.trim_start_matches('0');
        let s_len = s.len();

        let row = if s_len == 0 {
            PlacesRow::nought_raw()
        } else {
            let mut row = Vec::new();
            row.reserve_exact(s_len);

            let mut err_inx = s_len_orig;
            for (c, sc) in s.chars().rev().zip(row.spare_capacity_mut()) {
                err_inx -= 1;
                if c.is_ascii_digit() {
                    let d = c.to_digit(10).unwrap();
                    sc.write(d as u8);
                } else {
                    return Err(Some(err_inx));
                }
            }

            unsafe { row.set_len(s_len) }
            row
        };

        Ok(PlacesRow { row })
    }

    /// Returns `String` representation.
    pub fn to_number(&self) -> String {
        let row = &self.row;
        let len = row.len();
        let mut number = String::new();
        number.reserve_exact(len);
        for i in row.iter().rev() {
            let digit = match i {
                0 => '0',
                1 => '1',
                2 => '2',
                3 => '3',
                4 => '4',
                5 => '5',
                6 => '6',
                7 => '7',
                8 => '8',
                9 => '9',
                _ => panic!("Only ones supported."),
            };

            number.push(digit);
        }

        number
    }

    fn unity_raw() -> RawRow {
        vec![1; 1]
    }

    fn nought_raw() -> RawRow {
        vec![0; 1]
    }

    fn is_unity_raw(row: &RawRow) -> bool {
        Self::is_one_raw(row, 1)
    }

    fn is_nought_raw(row: &RawRow) -> bool {
        Self::is_one_raw(row, 0)
    }

    fn is_one_raw(row: &RawRow, one: u8) -> bool {
        row.len() == 1 && row[0] == one
    }

    /// `true` if and only if `PlacesRow` is _unity_ value.
    pub fn is_unity(&self) -> bool {
        Self::is_unity_raw(&self.row)
    }

    /// `true` if and only if `PlacesRow` is _nought_ value.
    pub fn is_nought(&self) -> bool {
        Self::is_nought_raw(&self.row)
    }

    /// Returns unity `PlacesRow`.
    pub fn unity() -> PlacesRow {
        PlacesRow {
            row: Self::unity_raw(),
        }
    }

    /// Returns nought `PlacesRow`.
    pub fn nought() -> PlacesRow {
        PlacesRow {
            row: Self::nought_raw(),
        }
    }

    #[deprecated(since = "2.2.0", note = "Pick `fn nought` instead.")]
    /// Returns zero `PlacesRow`.
    pub fn zero() -> PlacesRow {
        Self::nought()
    }
}

fn shrink_to_fit_raw(row: &mut RawRow) {
    truncate_leading_raw(row, 0, 1);
    row.shrink_to_fit();
}

fn truncate_leading_raw(row: &mut RawRow, lead: u8, upto: usize) {
    let new_len = len_without_leading_raw(row, lead, upto);
    row.truncate(new_len);
}

fn len_without_leading_raw(row: &RawRow, lead: u8, upto: usize) -> usize {
    let mut row_len = row.len();
    for ix in (upto..row_len).rev() {
        if lead == row[ix] {
            row_len -= 1;
        } else {
            break;
        }
    }

    row_len
}

impl alloc::string::ToString for PlacesRow {
    /// Returns `String` representation.
    fn to_string(&self) -> String {
        self.to_number()
    }
}

use core::convert::From;
impl From<u128> for PlacesRow {
    /// Converts `value` into `PlacesRow`.
    fn from(value: u128) -> Self {
        Self::new_from_num(value)
    }
}

/// Relation enumeration.
#[derive(Debug, PartialEq)]
pub enum Rel {
    Greater,
    Equal,
    Lesser,
}

/// Checks relation of `num` to `comparand`.
///
/// Returns `Rel` relation.
pub fn rel(num: &PlacesRow, comparand: &PlacesRow) -> Rel {
    let r1 = &num.row;
    let r2 = &comparand.row;

    // ⟺ no leading zeros
    // num.len() > comparand.len() ⇒ num > comparand
    // num.len() < comparand.len() ⇒ num < comparand
    // num.len() = comparand.len() ⇒ num ⪒ comparand
    let r1_len = r1.len();
    let r2_len = r2.len();

    return if r1_len > r2_len {
        Rel::Greater
    } else if r1_len == r2_len {
        for inx in (0..r2_len).rev() {
            if r1[inx] > r2[inx] {
                return Rel::Greater;
            } else if r1[inx] < r2[inx] {
                return Rel::Lesser;
            }
        }

        Rel::Equal
    } else {
        Rel::Lesser
    };
}

use alloc::{string::String, vec, vec::Vec};

/// Computes `addend1` and `addend2` sum.
///
/// Returns `PlacesRow` with result.
pub fn add(addend1: &PlacesRow, addend2: &PlacesRow) -> PlacesRow {
    let r1 = &addend1.row;
    let r2 = &addend2.row;

    match add_shorcut(r1, r2) {
        Some(row) => return PlacesRow { row },
        _ => {}
    }

    let (addend, augend) = if r1.len() > r2.len() {
        (r1, r2)
    } else {
        (r2, r1)
    };

    // avoids repetetive reallocations
    // +1 stands for contigent new place
    let mut sum = Vec::new();
    sum.reserve_exact(addend.len() + 1);

    #[cfg(test)]
    let sum_ptr = sum.as_ptr();

    addition(addend, Some(augend), &mut sum, 0);

    #[cfg(test)]
    assert!(sum_ptr == sum.as_ptr());

    PlacesRow { row: sum }
}

fn add_shorcut(r1: &RawRow, r2: &RawRow) -> Option<RawRow> {
    if PlacesRow::is_nought_raw(r1) {
        Some(r2.clone())
    } else if PlacesRow::is_nought_raw(r2) {
        Some(r1.clone())
    } else {
        None
    }
}

/// Computes `minuend` and `subtrahend` difference.
///
/// Returns difference `PlacesRow` if `minuend` ≥ `subtrahend`, `None` otherwise.
pub fn sub(minuend: &PlacesRow, subtrahend: &PlacesRow) -> Option<PlacesRow> {
    let rel = rel(minuend, subtrahend);

    if rel == Rel::Lesser {
        None
    } else if rel == Rel::Equal {
        Some(PlacesRow::nought())
    } else {
        let min_row = &minuend.row;
        let sub_row = &subtrahend.row;
        let diff = subtraction(&min_row, &sub_row, false).0;
        Some(PlacesRow { row: diff })
    }
}

/// Computes `factor1` and `factor2` product.
///
/// Returns `PlacesRow` with result.
pub fn mul(factor1: &PlacesRow, factor2: &PlacesRow) -> PlacesRow {
    mulmul(&factor1.row, &factor2.row, 1)
}

/// Computes power `pow` of `base`.
///
/// Potentially CPU, memory intesive.
///
/// Returns `PlacesRow` with result.
pub fn pow(base: &PlacesRow, pow: u16) -> PlacesRow {
    let row = &base.row;
    if pow == 0 {
        return PlacesRow { row: vec![1] };
    } else if pow == 1 {
        return PlacesRow { row: row.clone() };
    }

    mulmul(row, row, pow - 1)
}

/// Computes `dividend` and `divisor` ratio and remainder.
///
/// Returns tuple with `PlacesRow` ratio and `PlacesRow` remainder in order or `None` when `divisor` is nought.
pub fn divrem(dividend: &PlacesRow, divisor: &PlacesRow) -> Option<(PlacesRow, PlacesRow)> {
    if divisor.is_nought() {
        return None;
    }

    let rel = rel(dividend, divisor);

    let res = if rel == Rel::Lesser {
        (PlacesRow::nought(), dividend.clone())
    } else if rel == Rel::Equal {
        (PlacesRow::unity(), PlacesRow::nought())
    } else {
        let remratio = subtraction(&dividend.row, &divisor.row, true);
        (PlacesRow { row: remratio.1 }, PlacesRow { row: remratio.0 })
    };

    Some(res)
}

/// Combined method allows to compute multiplication and power using shared code.
///
/// Space for effecient power computation?
///   🡺 Inspect log₂ power speed up.
fn mulmul(row1: &RawRow, row2: &RawRow, times: u16) -> PlacesRow {
    let (mpler, mut mcand) = (row1, row2.clone());

    let mpler_len = mpler.len();

    // intermediate product of `mcand` and `mpler`
    let mut i_product = Vec::with_capacity(0);
    // intermediate sum of intermediate products
    let mut i_sum = Vec::with_capacity(0);

    let mut cntr = 0;
    loop {
        let mcand_len = mcand.len();

        // avoids repetetive reallocations
        // +1 stands for contigent new place
        i_product.reserve(mcand_len + 1);
        // avoids repetetive reallocations
        // places count of product cannot
        // be greater than sum of places of operands
        i_sum.reserve(mcand_len + mpler_len);

        #[cfg(test)]
        let i_product_ptr = i_product.as_ptr();

        #[cfg(test)]
        let i_sum_ptr = i_sum.as_ptr();

        for offset in 0..mpler_len {
            product(mpler[offset], &mcand, &mut i_product);
            addition(&i_product, None, &mut i_sum, offset);
            i_product.clear();
        }

        #[cfg(test)]
        assert!(i_product_ptr == i_product.as_ptr());

        #[cfg(test)]
        assert!(i_sum_ptr == i_sum.as_ptr());

        cntr += 1;
        if cntr == times {
            mcand = i_sum;
            break;
        }

        mcand.clear();
        let swap = mcand;
        mcand = i_sum;
        i_sum = swap;
    }

    // useless when both of factors cannot be nought
    shrink_to_fit_raw(&mut mcand);
    PlacesRow { row: mcand }
}

/// Computes product of `mpler` and `mcand`.
fn product(mpler: u8, mcand: &RawRow, product: &mut RawRow) {
    let mut takeover = 0;

    // runs in vain for `mpler` = 0
    //   🡺 inspect possibilities
    for &num in mcand {
        // each `prod` can be immediately added to intermediate sum
        //   🡺 inspect this option
        let prod = mpler * num;
        let prod = ones(prod, &mut takeover);
        product.push(prod);
    }

    if takeover != 0 {
        product.push(takeover);
    }
}

/// Adds `addend_1` to `sum` or adds `addend_1` and `addend_2` sum into `sum`.
///
/// Precise expectations must be upkept when adding 2 addends: sum is assumed to be empty, `addend_1` to be longer or equal of numbers and offset to be `0`.
fn addition(addend_1: &RawRow, addend_2: Option<&RawRow>, sum: &mut RawRow, offset: usize) {
    let addend_1_len = addend_1.len();

    let (addend_2_ptr, addend_2_len) = if let Some(addend) = addend_2 {
        (addend.as_ptr(), addend.len())
    } else {
        (sum.as_ptr(), sum.len())
    };

    let mut takeover = 0;
    let mut addend_1_inx = 0;
    let mut addend_2_inx = offset;

    loop {
        let addend_1_available = addend_1_inx < addend_1_len;
        if !addend_1_available && takeover == 0 {
            break;
        }

        let addend_1_num = if addend_1_available {
            addend_1[addend_1_inx]
        } else {
            0
        };

        let addend_2_num = if addend_2_inx < addend_2_len {
            unsafe { addend_2_ptr.offset(addend_2_inx as isize).read() }
        } else {
            0
        };

        let add = ones(addend_2_num + addend_1_num, &mut takeover);
        if let Some(refer) = sum.get_mut(addend_2_inx) {
            *refer = add;
        } else {
            sum.push(add);
        }

        addend_1_inx += 1;
        addend_2_inx += 1;
    }
}

/// For difference computation applies precondition minuend ≥ subtrahend.
/// Returns difference/remainder and ration in order.
fn subtraction(minuend: &RawRow, subtrahend: &RawRow, remainder: bool) -> (RawRow, RawRow) {
    let mut diffrem_populated = false;

    let minuend_len = minuend.len();
    let subtrahend_len = subtrahend.len();

    let mut diffrem = vec![0; minuend_len];
    let diffrem_ptr = diffrem.as_ptr();
    let mut minuend_ptr = minuend.as_ptr();

    let mut ratio = PlacesRow::nought_raw();
    let one = vec![1; 1];
    let mut takeover;
    let mut inx;
    loop {
        takeover = 0;
        inx = 0;

        while inx < minuend_len {
            let s_num = if inx < subtrahend_len {
                subtrahend[inx]
            } else if takeover == 0 && diffrem_populated {
                break;
            } else {
                0
            };

            let mut m_num = unsafe { minuend_ptr.offset(inx as isize).read() };

            let total_s = s_num + takeover;
            takeover = if m_num < total_s {
                m_num += 10;
                1
            } else {
                0
            };

            diffrem[inx] = m_num - total_s;
            inx += 1;
        }

        // existing remainder implies _minuend_ exhaustion
        // thus remainder is one turn more than is correct
        if takeover == 1 {
            inx = 0;
            takeover = 0;
            while inx < subtrahend_len {
                let correction = diffrem[inx] + subtrahend[inx];
                diffrem[inx] = ones(correction, &mut takeover);
                inx += 1;
            }

            truncate_leading_raw(&mut diffrem, 9, inx);
            break;
        }

        addition(&one, None, &mut ratio, 0);

        if !remainder {
            break;
        }

        if !diffrem_populated {
            minuend_ptr = diffrem_ptr;
            diffrem_populated = true;
        }
    }

    shrink_to_fit_raw(&mut diffrem);
    (diffrem, ratio)
}

/// Supports algorithimical decimal row computations.
/// Solve problem as ones to ones addition.
/// Takes current size of place `num`, adds takeover
/// `takeover_ref` to it, returns ones of summation
/// and sets up `takeover_ref` with tens of summation.
fn ones(num: u8, takeover_ref: &mut u8) -> u8 {
    let mut takeover_val = *takeover_ref;
    let total = num + takeover_val;

    takeover_val = total / 10;
    *takeover_ref = takeover_val;

    total - takeover_val * 10
}

#[cfg(test)]
mod tests_of_units {

    mod placesrow {
        use crate::PlacesRow as Row;
        use alloc::string::ToString;

        mod new_from_vec {
            use crate::PlacesRow as Row;
            use alloc::vec;

            #[test]
            fn basic_test() {
                let row = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
                let proof = row.clone();
                let row = Row::new_from_vec(row);

                assert!(row.is_ok());
                assert_eq!(proof, row.unwrap().row);
            }

            #[test]
            fn zero_len_test() {
                let row = Row::new_from_vec(vec![0; 0]);
                assert!(row.is_err());
                assert_eq!(None, row.err().unwrap());
            }

            #[test]
            fn unsupported_num_len_index_test() {
                let row = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 0];
                let row = Row::new_from_vec(row);

                assert!(row.is_err());
                assert_eq!(Some(10), row.err().unwrap());
            }

            #[test]
            fn unsupported_num_0_index_test() {
                let row = vec![10, 0, 0, 0];
                let row = Row::new_from_vec(row);

                assert!(row.is_err());
                assert_eq!(Some(0), row.err().unwrap());
            }

            #[test]
            fn leading_zeros_trim_test() {
                let row = Row::new_from_vec(vec![1, 2, 0, 0]);
                assert_eq!(&[1, 2], &*row.unwrap().row);
            }

            #[test]
            fn zero_reduction_test() {
                let row = Row::new_from_vec(vec![0, 0, 0, 0]);
                assert_eq!(&[0], &*row.unwrap().row);
            }
        }

        #[test]
        fn new_from_num_test() {
            let row = Row::new_from_num(000_1234567890);
            assert_eq!(&[0, 9, 8, 7, 6, 5, 4, 3, 2, 1], &*row);
        }

        mod new_from_str {
            use crate::PlacesRow as Row;

            #[test]
            fn zero_len_test() {
                let row = Row::new_from_str("");
                assert!(row.is_err());
                assert_eq!(None, row.err().unwrap());
            }

            #[test]
            fn leading_zeros_trim_test() {
                let row = Row::new_from_str("0021");
                assert!(row.is_ok());
                assert_eq!(&[1, 2], &*row.unwrap().row);
            }

            #[test]
            fn zeros_reduction_test() {
                let row = Row::new_from_str("0000");
                assert!(row.is_ok());
                assert_eq!(&[0], &*row.unwrap().row);
            }

            #[test]
            fn nondigit_str_test() {
                let row = Row::new_from_str("0012w123");
                assert!(row.is_err());
                let inx = row.err().unwrap();
                assert!(inx.is_some());
                assert_eq!(4, inx.unwrap());
            }

            #[test]
            fn basic_test() {
                let row = Row::new_from_str("1234567890");
                assert!(row.is_ok());
                assert_eq!(&[0, 9, 8, 7, 6, 5, 4, 3, 2, 1], &*row.unwrap().row);
            }
        }

        mod to_number {
            use crate::PlacesRow as Row;
            use alloc::vec;

            #[test]
            fn basic_test() {
                let row = Row::new_from_vec(vec![0, 9, 8, 7, 6, 5, 4, 3, 2, 1]).unwrap();
                assert_eq!("1234567890", row.to_number().as_str());
            }

            #[test]
            #[should_panic(expected = "Only ones supported.")]
            fn only_ones_supported_test() {
                let row = Row { row: vec![10] };
                _ = row.to_number();
            }
        }

        use crate::RawRow;

        fn ac_unity() -> RawRow {
            [1].to_vec()
        }

        fn ac_nought() -> RawRow {
            [0].to_vec()
        }

        #[test]
        fn unity_raw() {
            assert_eq!(ac_unity(), &*Row::unity_raw());
        }

        #[test]
        fn nought_raw() {
            assert_eq!(ac_nought(), &*Row::nought_raw());
        }

        #[test]
        fn is_unity_raw() {
            assert_eq!(true, Row::is_unity_raw(&ac_unity()));
        }

        #[test]
        fn is_nought_raw() {
            assert_eq!(true, Row::is_nought_raw(&ac_nought()));
        }

        #[test]
        fn is_one_raw() {
            let test = [3].to_vec();
            assert_eq!(true, Row::is_one_raw(&test, 3));
        }

        #[test]
        fn is_unity() {
            let test = Row { row: ac_unity() };
            assert_eq!(true, test.is_unity());
        }

        #[test]
        fn is_nought() {
            let test = Row { row: ac_nought() };
            assert_eq!(true, test.is_nought());
        }

        #[test]
        fn unity() {
            let proof = Row { row: ac_unity() };
            assert_eq!(proof, Row::unity());
        }

        #[test]
        fn nought() {
            let proof = Row { row: ac_nought() };
            assert_eq!(proof, Row::nought());
        }

        #[test]
        #[allow(deprecated)]
        fn zero_test() {
            assert_eq!(&[0], &*Row::zero());
        }

        #[test]
        fn to_string_test() {
            let row = Row::new_from_num(1);
            assert_eq!("1", row.to_string());
        }

        #[test]
        fn from_test() {
            let row: Row = From::from(123);
            assert_eq!(&[3, 2, 1], &*row);
        }
    }

    mod shrink_to_fit_raw {
        use crate::shrink_to_fit_raw;
        use alloc::vec::Vec;

        #[test]
        fn zero_truncation_test() {
            let cap = 100;
            let mut row = Vec::with_capacity(cap);
            row.push(1);
            row.push(2);
            row.push(0);

            shrink_to_fit_raw(&mut row);
            assert_eq!(&[1, 2], &*row);
            assert!(row.capacity() < cap);
        }

        #[test]
        fn first_preservation_test() {
            let cap = 100;
            let mut row = Vec::with_capacity(cap);
            row.push(0);
            row.push(0);
            row.push(0);

            shrink_to_fit_raw(&mut row);
            assert_eq!(&[0], &*row);
            assert!(row.capacity() < cap);
        }
    }

    mod truncate_leading_raw {
        use crate::truncate_leading_raw;
        use alloc::vec;

        #[test]
        fn basic_test() {
            let mut row = vec![7, 7, 7, 7, 7];
            truncate_leading_raw(&mut row, 7, 3);
            assert_eq!(vec![7, 7, 7,], row);
        }
    }

    mod len_without_leading_raw {
        use crate::len_without_leading_raw;
        use alloc::vec;

        #[test]
        fn counting_test() {
            let count = len_without_leading_raw(&vec![1, 2, 5, 5, 5], 5, 0);
            assert_eq!(2, count);
        }

        #[test]
        fn preservation_test() {
            let count = len_without_leading_raw(&vec![5, 5, 5, 5], 5, 1);
            assert_eq!(1, count);
        }

        #[test]
        fn no_leading_test() {
            let count = len_without_leading_raw(&vec![5, 5, 5, 0], 5, 0);
            assert_eq!(4, count);
        }

        #[test]
        fn upto_equal_len_test() {
            let count = len_without_leading_raw(&vec![5, 5, 5], 5, 3);
            assert_eq!(3, count);
        }
    }

    // Relational comparison.
    mod rel {

        use crate::{rel, PlacesRow as Row, Rel};

        #[test]
        fn longer_test() {
            let num = Row::new_from_num(11);
            let comparand = Row::new_from_num(9);

            assert_eq!(Rel::Greater, rel(&num, &comparand));
        }

        #[test]
        fn shorter_test() {
            let num = Row::new_from_num(9);
            let comparand = Row::new_from_num(10);

            assert_eq!(Rel::Lesser, rel(&num, &comparand));
        }

        #[test]
        fn greater_test() {
            let num_num = 1234567899;
            let cpd_num = 1234567890;

            let num = Row::new_from_num(num_num);
            let comparand = Row::new_from_num(cpd_num);

            assert_eq!(Rel::Greater, rel(&num, &comparand));
        }

        #[test]
        fn equal_test() {
            let num = Row::new_from_num(1234567890);
            assert_eq!(Rel::Equal, rel(&num, &num));
        }

        #[test]
        fn lesser_test() {
            let num_num = 1234567890;
            let cpd_num = 1234567899;

            let num = Row::new_from_num(num_num);
            let comparand = Row::new_from_num(cpd_num);

            assert_eq!(Rel::Lesser, rel(&num, &comparand));
        }
    }

    // Addition.
    mod add {
        use crate::{add, PlacesRow as Row};

        #[test]
        fn basic_test() {
            let row1 = Row::new_from_num(4);
            let row2 = Row::new_from_num(5);

            let sum = add(&row1, &row2);
            assert_eq!(&[9], &*sum.row);
        }

        #[test]
        fn left_num_longer_test() {
            let row1 = Row::new_from_num(10_000);
            let row2 = Row::new_from_num(5);

            let sum = add(&row1, &row2);
            assert_eq!(Row::new_from_num(10_005), sum);
        }

        #[test]
        fn right_num_longer_test2() {
            let row1 = Row::new_from_num(5);
            let row2 = Row::new_from_num(10_000);

            let sum = add(&row1, &row2);
            assert_eq!(Row::new_from_num(10_005), sum);
        }

        #[test]
        fn advanced_test() {
            let row = Row::new_from_str("680564733841876926926749214863536422910").unwrap();

            let sum = add(&row, &row);
            assert_eq!("1361129467683753853853498429727072845820", sum.to_number());
        }

        #[test]
        fn addend1_is_nought() {
            let addend1 = Row::nought();
            let addend2 = Row::new_from_num(4321);
            let sum = add(&addend1, &addend2);
            assert_eq!(addend2, sum);
        }

        #[test]
        fn addend2_is_nought() {
            let addend1 = Row::new_from_num(4321);
            let addend2 = Row::nought();
            let sum = add(&addend1, &addend2);
            assert_eq!(addend1, sum);
        }
    }

    mod add_shorcut {
        use crate::{add_shorcut, PlacesRow as Row};

        #[test]
        fn none_is_nought() {
            assert_eq!(None, add_shorcut(&Row::unity_raw(), &Row::unity_raw()));
        }

        use alloc::vec;
        #[test]
        fn r1_is_nought() {
            let r1 = Row::nought_raw();
            let r2 = vec![1, 2, 3, 4];
            let res = add_shorcut(&r1, &r2);
            assert_eq!(Some(r2.clone()), res);
            assert_ne!(r2.as_ptr(), res.unwrap().as_ptr());
        }

        #[test]
        fn r2_is_nought() {
            let r1 = vec![1, 2, 3, 4];
            let r2 = Row::nought_raw();
            let res = add_shorcut(&r1, &r2);
            assert_eq!(Some(r1.clone()), res);
            assert_ne!(r1.as_ptr(), res.unwrap().as_ptr());
        }
    }

    /// Subtraction.
    mod sub {
        use crate::{sub, PlacesRow as Row};

        #[test]
        fn lesser_minuend_test() {
            let minuend = Row::new_from_num(4);
            let subtrahend = Row::new_from_num(5);

            assert!(sub(&minuend, &subtrahend).is_none());
        }

        #[test]
        fn universal_test() {
            for triplet in [(99, 11, 88), (133, 133, 0), (90, 19, 71), (700, 699, 1)] {
                let minuend = Row::new_from_num(triplet.0);
                let subtrahend = Row::new_from_num(triplet.1);

                let proof = Row::new_from_num(triplet.2);
                let diff = sub(&minuend, &subtrahend);
                assert!(diff.is_some());

                assert_eq!(proof, diff.unwrap());
            }
        }
    }

    /// Multiplication.
    mod mul {
        use crate::{mul, PlacesRow as Row};

        #[test]
        fn basic_test() {
            let row1 = Row::new_from_num(2);
            let row2 = Row::new_from_num(3);
            let prod = mul(&row1, &row2);
            assert_eq!(&[6], &*prod);
        }

        #[test]
        fn row1_nought_test() {
            let row1 = Row::nought();
            let row2 = Row::new_from_num(123456789_10111213);
            let prod = mul(&row1, &row2);
            let row = &prod.row;
            assert_eq!(&[0], row.as_slice());
            assert!(row.capacity() < row2.len());
        }

        #[test]
        fn row2_nought_test() {
            let row1 = Row::new_from_num(123456789_10111213);
            let row2 = Row::nought();
            let prod = mul(&row1, &row2);
            let row = &prod.row;
            assert_eq!(&[0], row.as_slice());
            assert!(row.capacity() < row1.len());
        }

        #[test]
        fn zero_nums_test() {
            let row1 = Row::new_from_num(0);
            let row2 = Row::new_from_num(0);
            let prod = mul(&row1, &row2);
            assert_eq!(&[0], &*prod);
        }

        #[test]
        fn advanced_test() {
            let row = Row::new_from_num(u128::MAX);
            let prod = mul(&row, &row);
            let proof =
                "115792089237316195423570985008687907852589419931798687112530834793049593217025";
            assert_eq!(proof, prod.to_number());
        }
    }

    /// For base ≥ 0 and exponent ≥ 0 power can be viewed as nothing more
    /// than repetetive multiplication with number in question.
    /// 0º=1, 0¹=1×0, 0²=1×0×0, 0³=1×0×0×0, …
    /// 1º=1, 1¹=1×1, 1²=1×1×1, 1³=1×1×1×1, …
    /// 2º=1, 2¹=1×2, 2²=1×2×2, 2³=1×2×2×2, …
    ///                   ⋮                   
    mod pow {
        use crate::{pow, PlacesRow as Row};

        #[test]
        fn basic_test() {
            let row = Row::new_from_num(2);
            assert_eq!(&[4], &*pow(&row, 2));
        }

        #[test]
        fn advanced_test2() {
            let proof = Row::new_from_str("88817841970012523233890533447265625").unwrap();
            let row = Row::new_from_num(25);
            assert_eq!(proof, pow(&row, 25));
        }

        #[test]
        fn advanced_test3() {
            let proof = Row::new_from_str(
                "949279437109690919948053832937215463733689853138782229364504479870922851876864",
            )
            .unwrap();

            let row = Row::new_from_num(998);
            assert_eq!(proof, pow(&row, 26));
        }

        #[test]
        fn advanced_test4() {
            let proof = Row::new_from_str(
                "926336713898529563388567880069503262826159877325124512315660672063305037119488",
            )
            .unwrap();

            let row = Row::new_from_num(2);
            assert_eq!(proof, pow(&row, 259));
        }

        #[test]
        #[cfg(feature = "ext-tests")]
        // readme sample
        fn advanced_test5() {
            let row = Row::new_from_num(u128::MAX);
            let pow = pow(&row, 500);
            let number = pow.to_number();

            assert!(number.starts_with("8312324609993336522"));
            assert_eq!(19266, number.len());
        }

        #[test]
        fn zero_power_test() {
            let row = Row::new_from_num(0);
            let pow = pow(&row, 0);
            assert_eq!(&[1], &*pow);
        }

        #[test]
        fn one_power_test() {
            let row = Row::new_from_num(3030);
            let pow = pow(&row, 1);
            assert_eq!(&[0, 3, 0, 3], &*pow);
        }

        #[test]
        fn power_of_zero_test() {
            let row = Row::new_from_num(0);
            let pow = pow(&row, 1000);
            assert_eq!(&[0], &*pow);
        }

        #[test]
        fn power_of_one_test() {
            let row = Row::new_from_num(1);
            let pow = pow(&row, u16::MAX);
            assert_eq!(&[1], &*pow);
        }
    }

    /// Division with remainder.
    mod divrem {
        use crate::{divrem, PlacesRow as Row};

        #[test]
        fn zero_divisor_test() {
            let dividend = Row::new_from_num(1);
            let divisor = Row::new_from_num(0);

            let ratrem = divrem(&dividend, &divisor);
            assert!(ratrem.is_none());
        }

        #[test]
        fn lesser_dividend_test() {
            let dividend = Row::new_from_num(998);
            let divisor = Row::new_from_num(999);

            let ratrem = divrem(&dividend, &divisor);
            assert!(ratrem.is_some());

            let ratrem = ratrem.unwrap();

            assert_eq!(Row::nought(), ratrem.0);
            assert_eq!(dividend, ratrem.1);
        }

        #[test]
        fn universal_test() {
            for quadruplet in [
                (0, 100, 0, 0),
                (99, 11, 9, 0),
                (133, 133, 1, 0),
                (90, 19, 4, 14),
                (700, 699, 1, 1),
            ] {
                let dividend = Row::new_from_num(quadruplet.0);
                let divisor = Row::new_from_num(quadruplet.1);

                let ratio = Row::new_from_num(quadruplet.2);
                let remainder = Row::new_from_num(quadruplet.3);
                let ratrem = divrem(&dividend, &divisor);

                assert!(ratrem.is_some());
                let ratrem = ratrem.unwrap();

                assert_eq!(ratio, ratrem.0);
                assert_eq!(remainder, ratrem.1);
            }
        }
    }

    /// Long multiplication fact notes:
    /// - When multiplying ones, maximum product is 81=9×9.
    /// - Thus maximum tens product is 8=⌊81÷10⌋.
    /// - Since 8+81=89 all results fit into 8=⌊89÷10⌋ tens.
    mod product {
        use crate::product as product_fn;
        use alloc::vec;
        use alloc::vec::Vec;

        #[test]
        fn basic_test() {
            let mcand = vec![3, 2, 1];
            let mpler = 3;
            let mut product = Vec::new();

            product_fn(mpler, &mcand, &mut product);

            assert_eq!(vec![9, 6, 3], product);
        }

        #[test]
        fn takeover_test() {
            let mcand = vec![9, 9, 9, 9, 9];
            let mpler = 9;
            let mut product = Vec::new();

            product_fn(mpler, &mcand, &mut product);

            assert_eq!(vec![1, 9, 9, 9, 9, 8], product);
        }
    }

    /// Column addition fact notes:
    /// - When adding ones, maximum sum is 18=9+9.
    /// - Thus maximum tens sum is 1=⌊18÷10⌋.
    /// - Since 18+1=19 any value fits into 1=⌊19÷10⌋ ten.
    mod addition {

        mod one_addend {
            use crate::addition;
            use alloc::vec;

            #[test]
            fn basic_test() {
                let ad1 = vec![4, 3, 2, 5];
                let mut sum = vec![1, 2, 3];

                addition(&ad1, None, &mut sum, 0);

                assert_eq!(vec![5, 5, 5, 5], sum);
            }

            #[test]
            fn takover_test() {
                let ad1 = vec![9];
                let mut sum = vec![9, 9, 9, 9, 9];

                addition(&ad1, None, &mut sum, 0);

                assert_eq!(vec![8, 0, 0, 0, 0, 1], sum);
            }

            #[test]
            fn longer_addition_test() {
                let ad1 = vec![8, 9, 9, 9, 9];
                let mut sum = vec![1, 1];

                addition(&ad1, None, &mut sum, 0);

                assert_eq!(vec![9, 0, 0, 0, 0, 1], sum);
            }

            #[test]
            fn offset_test() {
                let ad1 = vec![9, 9, 9, 9];
                let mut sum = vec![1, 1, 7, 8];

                addition(&ad1, None, &mut sum, 2);

                assert_eq!(vec![1, 1, 6, 8, 0, 0, 1], sum);
            }
        }

        mod two_addends {
            use crate::addition;
            use alloc::vec;
            use alloc::vec::Vec;

            #[test]
            fn basic_test() {
                let ad1 = vec![1, 1, 2, 4, 9];
                let ad2 = vec![8, 8, 7, 5];
                let mut sum = Vec::new();

                addition(&ad1, Some(&ad2), &mut sum, 0);

                assert_eq!(vec![9, 9, 9, 9, 9], sum);
            }

            #[test]
            fn takover_test() {
                let ad1 = vec![9];
                let ad2 = vec![9];
                let mut sum = Vec::new();

                addition(&ad1, Some(&ad2), &mut sum, 0);

                assert_eq!(vec![8, 1], sum);
            }

            #[test]
            fn longer_addition_test() {
                let ad1 = vec![8, 8, 9, 9, 9];
                let ad2 = vec![1, 1];
                let mut sum = Vec::new();

                addition(&ad1, Some(&ad2), &mut sum, 0);

                assert_eq!(vec![9, 9, 9, 9, 9], sum);
            }
        }
    }

    /// Column subtraction fact notes:
    /// - Subtrahend always must be lower or equal to minuend.
    /// - Minimum difference is 0=a-a, maximum 9=9-0=(9+a)-a, a ∈ [0;9].
    /// - Maximum subtrahend is 10=9+1(takeover).
    mod subtraction {

        mod subtracting {
            use crate::{subtraction, PlacesRow as Row};
            use alloc::vec;

            #[test]
            fn basic_test() {
                let diffratio = subtraction(&vec![9, 9], &vec![0, 1], false);
                assert_eq!(&[9, 8], &*diffratio.0);
                assert_eq!(&[1], &*diffratio.1);
            }

            #[test]
            // minuend must be "copied" to difference if subtrahend is
            // exhausted
            fn minuend_copy_test() {
                let diffratio = subtraction(&vec![7, 7, 7], &vec![1], false);
                assert_eq!(&[6, 7, 7], &*diffratio.0);
                assert_eq!(&[1], &*diffratio.1);
            }

            #[test]
            fn advanced_test() {
                let minuend =
                    Row::new_from_str("6577102745386680762814942322444851025767571854389858533375")
                        .unwrap();
                let subtrahend =
                    Row::new_from_str("6296101835386680762814942322444851025767571854389858533376")
                        .unwrap();
                let proof =
                    Row::new_from_str("281000909999999999999999999999999999999999999999999999999")
                        .unwrap();

                let diffratio = subtraction(&minuend.row, &subtrahend.row, false);
                assert_eq!(proof.row, diffratio.0);
                assert_eq!(&[1], &*diffratio.1);
            }

            #[test]
            /// tests takeover ∈ [0,1] carry on
            fn takeover_test() {
                let diffratio = subtraction(&vec![8, 2, 2, 0, 1], &vec![9, 2, 1, 1], false);
                assert_eq!(&[9, 9, 0, 9], &*diffratio.0);
                assert_eq!(&[1], &*diffratio.1);
            }

            #[test]
            fn zero_truncation_test() {
                let diffratio = subtraction(&vec![9, 9, 9], &vec![8, 9, 9], false);
                let diff = diffratio.0;
                assert_eq!(&[1], &*diff);
                assert_eq!(&[1], &*diffratio.1);
                let diffcap = diff.capacity();
                assert!(1 == diffcap || diffcap < 3);
            }

            // because it can be
            // [1,0,9] - [2,0,9] = [9,9,9]
            // [9,9,9] + [2,0,9] = [1,0,9]
            // top place 9 must be preserved
            #[test]
            fn top_place_9_preservation_test() {
                let minuend = &vec![1, 0, 9];
                let subtrahend = vec![2, 0, 9];
                let diffratio = subtraction(minuend, &subtrahend, false);
                assert_eq!(minuend, &*diffratio.0);
                assert_eq!(&[0], &*diffratio.1);
            }
        }

        mod remainder {
            use crate::{subtraction, PlacesRow as Row};
            use alloc::vec;

            #[test]
            fn basic_test() {
                let remratio = subtraction(&vec![3, 3], &vec![1, 1], true);
                assert_eq!(&[0], &*remratio.0);
                assert_eq!(&[3], &*remratio.1);
            }

            #[test]
            // minuend must be "copied" to remainder if subtrahend is
            // exhausted
            fn minuend_copy_test() {
                let remratio = subtraction(&vec![7, 7, 7], &vec![1], true);
                assert_eq!(&[0], &*remratio.0);
                assert_eq!(&[7, 7, 7], &*remratio.1);
            }

            #[test]
            fn remainder_test() {
                let remratio = subtraction(&vec![9], &vec![7], true);
                assert_eq!(&[2], &*remratio.0);
                assert_eq!(&[1], &*remratio.1);
            }

            #[test]
            fn takeover_test() {
                let remratio = subtraction(&vec![9, 0, 9], &vec![9], true);
                assert_eq!(&[0], &*remratio.0);
                assert_eq!(&[1, 0, 1], &*remratio.1);
            }

            #[test]
            // after invalid subtraction on remainder, places hold numbers resulting
            // from borrowing and subtracting
            // e.g. [2,0,0,0,0]-[7,7,3]=[5,2,6,9,9]:
            // - after remainder restoration [2,0,0,9,9],
            // - after `9`s truncation [2,0,0],
            // - after `0`s truncation [2]
            fn overrun_clearing_test() {
                let remratio = subtraction(&vec![2, 0, 0, 7, 7], &vec![7, 7], true);
                let remainder = remratio.0;
                assert_ne!(vec![5, 2, 9, 9, 9], remainder);
                assert_ne!(vec![2, 0, 9, 9, 9], remainder);
                assert_ne!(vec![2, 0], remainder);
                assert_eq!(vec![2], remainder);
                let remcap = remainder.capacity();
                assert!(1 == remcap || remcap < 5);
                assert_eq!(&[0, 0, 0, 1], &*remratio.1);
            }

            #[test]
            fn advanced_test() {
                let minuend = Row::new_from_num(627710173);
                let remainder = Row::new_from_num(130);
                let ratio = Row::new_from_num(1955483);

                let remratio = subtraction(&minuend.row, &vec![1, 2, 3], true);
                assert_eq!(&*remainder, &*remratio.0);
                assert_eq!(&*ratio, &*remratio.1);
            }

            #[test]
            fn advanced_test2() {
                let minuend = Row::new_from_num(627710173);
                let subtrahend = Row::new_from_num(3552741);
                let remainder = Row::new_from_num(2427757);
                let ratio = Row::new_from_num(176);

                let remratio = subtraction(&minuend.row, &subtrahend.row, true);
                assert_eq!(&*remainder, &*remratio.0);
                assert_eq!(&*ratio, &*remratio.1);
            }

            #[test]
            fn advanced_test3() {
                let minuend = Row::new_from_num(242775712);
                let subtrahend = Row::new_from_num(33333);
                let remainder = Row::new_from_num(11473);
                let ratio = Row::new_from_num(7283);

                let remratio = subtraction(&minuend.row, &subtrahend.row, true);
                assert_eq!(&*remainder, &*remratio.0);
                assert_eq!(&*ratio, &*remratio.1);
            }

            // because it can be
            // [1,0,9] - [2,0,9] = [9,9,9]
            // [9,9,9] + [2,0,9] = [1,0,9]
            // top place 9 must be preserved
            // 5411 = 5 ·902 +901
            #[test]
            fn top_place_9_preservation_test() {
                let minuend = vec![1, 1, 4, 5];
                let subtrahend = vec![2, 0, 9];
                let remratio = subtraction(&minuend, &subtrahend, true);
                assert_eq!(&[1, 0, 9], &*remratio.0);
                assert_eq!(&[5], &*remratio.1);
            }

            #[test]
            fn readme_sample_test() {
                use crate::{divrem, PlacesRow};

                let dividend =
                    PlacesRow::new_from_str("3402823669209384634633746074317682114565556668744123")
                        .unwrap();
                let divisor =
                    PlacesRow::new_from_str("14034568236692093846346337460345176821145655563453")
                        .unwrap();
                let ratio = "242";
                let remainder = "6458155929897923817932408914149323848308022388497";

                let ratrem = divrem(&dividend, &divisor).unwrap();

                assert_eq!(ratio, ratrem.0.to_number());
                assert_eq!(remainder, ratrem.1.to_number());
            }
        }
    }

    /// Supporting method. Desinged to split ones from tens. Supports any range of tens.
    mod ones {
        use crate::ones;

        #[test]
        fn basic_test() {
            let num = 9;
            let mut takeover = 0;

            assert_eq!(9, ones(num, &mut takeover));
            assert_eq!(0, takeover);
        }

        #[test]
        fn split_test() {
            let num = 9;
            let mut takeover = 3;

            assert_eq!(2, ones(num, &mut takeover));
            assert_eq!(1, takeover);
        }

        #[test]
        fn maximum_test() {
            let num = 246;
            let mut takeover = 9;

            assert_eq!(5, ones(num, &mut takeover));
            assert_eq!(25, takeover);
        }
    }
}

// cargo test --features ext-tests --release
