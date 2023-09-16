// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::bitfield;

pub trait Specifier {
    const BITS: usize;
    type BackType;

    fn from_u64(v: u64) -> Self::BackType;
    fn into_u64(v: Self::BackType) -> u64;
}

pub fn read_bits<const N: usize>(data: &[u8; N], bit_offset: usize, bit_size: usize) -> u64 {
    assert!(bit_size <= 64);
    assert!(bit_offset + bit_size < data.len() * 8 * N);

    let start_byte_index = bit_offset / 8;
    let start_bit_offset = bit_offset % 8;
    let end_byte_index = (bit_offset + bit_size) / 8;
    let end_bit_offset = (bit_offset + bit_size) % 8;

    if start_byte_index == end_byte_index {
        return ((data[start_byte_index] & (!0 >> start_bit_offset)) >> (8 - end_bit_offset))
            as u64;
    }

    let mut r: u64 = (data[start_byte_index] & (!0 >> start_bit_offset)) as u64;
    for i in start_byte_index + 1..end_byte_index {
        r = (r << 8) | data[i] as u64
    }
    if end_bit_offset > 0 {
        r = (r << end_bit_offset) | (data[end_byte_index] >> (8 - end_bit_offset)) as u64;
    }
    r
}

pub fn write_bits<const N: usize>(data: &mut [u8; N], bit_offset: usize, bit_size: usize, v: u64) {
    assert!(bit_size <= 64);
    assert!(bit_offset + bit_size < data.len() * 8 * N);
    assert!(v & (!0 << bit_size) == 0);

    let start_byte_index = bit_offset / 8;
    let start_bit_offset = bit_offset % 8;
    let end_byte_index = (bit_offset + bit_size) / 8;
    let end_bit_offset = (bit_offset + bit_size) % 8;

    if start_byte_index == end_byte_index {
        let mask: u8 = !(!0 >> start_bit_offset) | (!0 >> end_bit_offset);
        data[start_byte_index] =
            (data[start_byte_index] & mask) | (v << (8 - end_bit_offset)) as u8;
        return;
    }

    data[start_byte_index] = (data[start_byte_index] & !(!0 >> start_bit_offset))
        | (v >> (bit_size + start_bit_offset - 8)) as u8;
    for i in start_byte_index + 1..end_byte_index {
        data[i] = (v >> (bit_size + start_bit_offset - 8 - 8 * (i - start_byte_index))) as u8;
    }
    if end_bit_offset > 0 {
        data[end_byte_index] =
            (data[end_byte_index] & (!0 >> end_bit_offset)) | (v << (8 - end_bit_offset)) as u8;
    }
}

bitfield_impl::gen_b_types!();

#[cfg(test)]
mod tests {
    use rand::Rng;

    use crate::{read_bits, write_bits};

    #[test]
    fn test_read_write_bits() {
        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            let mut data = [0; 8];

            let bit_offset: usize = rng.gen_range(0..64);
            let bit_size: usize = rng.gen_range(1..=(64 - bit_offset));
            let max_v: u64 = !0 >> (64 - bit_size);
            let v: u64 = rng.gen_range(0..=max_v);

            assert_eq!(0, read_bits(&data, bit_offset, bit_size),);
            if bit_offset > 0 {
                assert_eq!(0, read_bits(&data, 0, bit_offset))
            }
            if bit_offset + bit_size < 64 {
                assert_eq!(
                    0,
                    read_bits(&data, bit_offset + bit_size, 64 - bit_offset - bit_size)
                )
            }
            write_bits(&mut data, bit_offset, bit_size, v);
            assert_eq!(
                v,
                read_bits(&data, bit_offset, bit_size),
                "after write {}: data={:#X?} bit_offset={} bit_size={}",
                v,
                &data,
                bit_offset,
                bit_size
            );
            if bit_offset > 0 {
                assert_eq!(0, read_bits(&data, 0, bit_offset))
            }
            if bit_offset + bit_size < 64 {
                assert_eq!(
                    0,
                    read_bits(&data, bit_offset + bit_size, 64 - bit_offset - bit_size)
                )
            }
        }
    }
}
