pub type Word = u64;
pub type WordVec = wide::u64x4;

const VEC_LANES: usize = WordVec::LANES as usize;
const WORD_BITS: usize = Word::BITS as usize;
const VEC_BITS: usize = WordVec::BITS as usize;

#[derive(Clone)]
pub struct BitMatrix {
    words: Vec<WordVec>,
    columns: usize,
    rows: usize,
    stride: usize,
}

impl BitMatrix {
    pub fn new(columns: usize) -> Self {
        Self {
            words: vec![],
            columns,
            rows: 0,
            stride: columns.div_ceil(VEC_BITS),
        }
    }

    pub fn zeroed(rows: usize, columns: usize) -> Self {
        let stride = columns.div_ceil(VEC_BITS);
        Self {
            words: vec![WordVec::ZERO; rows * stride],
            columns,
            rows,
            stride,
        }
    }

    pub fn rows(&self) -> usize {
        self.rows
    }

    pub fn columns(&self) -> usize {
        self.columns
    }

    pub fn push_zero_row(&mut self) {
        let old_size = self.rows * self.stride;
        self.rows += 1;
        self.words.truncate(old_size);
        self.words.resize(old_size + self.stride, WordVec::ZERO);
    }

    pub fn truncate_rows(&mut self, rows: usize) {
        self.rows = self.rows.min(rows);
        self.words.truncate(self.rows * self.stride);
    }

    pub fn bit(&self, row: usize, col: usize) -> bool {
        assert!(row < self.rows);
        assert!(col < self.columns);

        self.words[row * self.stride + col / VEC_BITS].as_array_ref()[(col % VEC_BITS) / WORD_BITS]
            & (1 << (col % WORD_BITS))
            != 0
    }

    pub fn set_bit(&mut self, row: usize, col: usize, bit: bool) {
        let word = &mut self.words[row * self.stride + col / VEC_BITS].as_array_mut()
            [(col % VEC_BITS) / WORD_BITS];
        let mask = 1 << (col % WORD_BITS);
        if bit {
            *word |= mask;
        } else {
            *word &= !mask;
        }
    }

    pub fn packed_row(&self, row: usize) -> &[WordVec] {
        assert!(row < self.rows);
        &self.words[row * self.stride..][..self.stride]
    }

    pub fn packed_row_mut(&mut self, row: usize) -> &mut [WordVec] {
        &mut self.words[row * self.stride..][..self.stride]
    }

    pub fn copy_row(&mut self, src: usize, dest: usize) {
        self.words.copy_within(
            src * self.stride..(src + 1) * self.stride,
            dest * self.stride,
        )
    }

    pub fn transposed(&self) -> Self {
        let mut tmp = Self::new(self.rows);
        self.transpose_into(&mut tmp);
        tmp
    }

    pub fn transpose_into(&self, target: &mut Self) {
        #![allow(clippy::needless_range_loop)]

        target.columns = self.rows;
        target.rows = self.columns;
        target.stride = target.columns.div_ceil(VEC_BITS);
        target.words.clear();
        target
            .words
            .resize(target.rows * target.stride, Default::default());

        let mut tmp = [WordVec::default(); WORD_BITS];

        for row_word_chunk in 0..self.rows / WORD_BITS {
            for col_vec_chunk in 0..self.stride {
                for row_in_chunk in 0..WORD_BITS {
                    tmp[row_in_chunk] = self.words
                        [col_vec_chunk + (row_word_chunk * WORD_BITS + row_in_chunk) * self.stride];
                }

                transpose64(&mut tmp);

                for lane in 0..VEC_LANES {
                    for pos_in_chunk in 0..WORD_BITS {
                        let word = tmp[pos_in_chunk].as_array_ref()[lane];

                        let dst_row = pos_in_chunk + WORD_BITS * (col_vec_chunk * VEC_LANES + lane);
                        let dst_word_col = row_word_chunk;

                        if dst_row < target.rows {
                            target.words[dst_row * target.stride + dst_word_col / VEC_LANES]
                                .as_array_mut()[dst_word_col % VEC_LANES] = word;
                        }
                    }
                }
            }
        }

        if self.rows % WORD_BITS != 0 {
            let row_word_chunk = self.rows / WORD_BITS;
            let partial_row_word_chunk_len = self.rows % WORD_BITS;

            for col_vec_chunk in 0..self.stride {
                for row_in_chunk in 0..partial_row_word_chunk_len {
                    tmp[row_in_chunk] = self.words
                        [col_vec_chunk + (row_word_chunk * WORD_BITS + row_in_chunk) * self.stride];
                }
                for row_in_chunk in partial_row_word_chunk_len..WORD_BITS {
                    tmp[row_in_chunk] = Default::default();
                }

                transpose64(&mut tmp);

                for lane in 0..VEC_LANES {
                    for pos_in_chunk in 0..WORD_BITS {
                        let word = tmp[pos_in_chunk].as_array_ref()[lane];

                        let dst_row = pos_in_chunk + WORD_BITS * (col_vec_chunk * VEC_LANES + lane);
                        let dst_word_col = row_word_chunk;

                        if dst_row < target.rows {
                            target.words[dst_row * target.stride + dst_word_col / VEC_LANES]
                                .as_array_mut()[dst_word_col % VEC_LANES] = word;
                        }
                    }
                }
            }
        }
    }
}

fn transpose64(data: &mut [WordVec; 64]) {
    #![allow(clippy::identity_op)]
    let i64 = 0;
    for i32 in i64 * 2..(i64 + 1) * 2 {
        for i16 in i32 * 2..(i32 + 1) * 2 {
            for i8 in i16 * 2..(i16 + 1) * 2 {
                for i4 in i8 * 2..(i8 + 1) * 2 {
                    for i2 in i4 * 2..(i4 + 1) * 2 {
                        let a = data[2 * i2 + 0];
                        let b = data[2 * i2 + 1];
                        let low_mask_word = 0x55555555_55555555;
                        let low_mask = WordVec::splat(low_mask_word);
                        let high_mask = WordVec::splat(!low_mask_word);
                        let a_low = a & low_mask;
                        let a_high = a & high_mask;
                        let b_low = b & low_mask;
                        let b_high = b & high_mask;

                        data[2 * i2 + 0] = (b_low << 1) | a_low;
                        data[2 * i2 + 1] = (a_high >> 1) | b_high;
                    }

                    for off in 0..2 {
                        let a = data[4 * i4 + off + 0];
                        let b = data[4 * i4 + off + 2];

                        let low_mask_word = 0x33333333_33333333;
                        let low_mask = WordVec::splat(low_mask_word);
                        let high_mask = WordVec::splat(!low_mask_word);
                        let a_low = a & low_mask;
                        let a_high = a & high_mask;
                        let b_low = b & low_mask;
                        let b_high = b & high_mask;

                        data[4 * i4 + off + 0] = (b_low << 2) | a_low;
                        data[4 * i4 + off + 2] = (a_high >> 2) | b_high;
                    }
                }

                for off in 0..4 {
                    let a = data[8 * i8 + off + 0];
                    let b = data[8 * i8 + off + 4];

                    let low_mask_word = 0x0f0f0f0f_0f0f0f0f;
                    let low_mask = WordVec::splat(low_mask_word);
                    let high_mask = WordVec::splat(!low_mask_word);
                    let a_low = a & low_mask;
                    let a_high = a & high_mask;
                    let b_low = b & low_mask;
                    let b_high = b & high_mask;

                    data[8 * i8 + off + 0] = (b_low << 4) | a_low;
                    data[8 * i8 + off + 4] = (a_high >> 4) | b_high;
                }
            }

            for off in 0..8 {
                let a = data[16 * i16 + off + 0];
                let b = data[16 * i16 + off + 8];

                let low_mask_word = 0x00ff00ff_00ff00ff;
                let low_mask = WordVec::splat(low_mask_word);
                let high_mask = WordVec::splat(!low_mask_word);
                let a_low = a & low_mask;
                let a_high = a & high_mask;
                let b_low = b & low_mask;
                let b_high = b & high_mask;

                data[16 * i16 + off + 0] = (b_low << 8) | a_low;
                data[16 * i16 + off + 8] = (a_high >> 8) | b_high;
            }
        }

        for off in 0..16 {
            let a = data[32 * i32 + off + 0];
            let b = data[32 * i32 + off + 16];

            let low_mask_word = 0x0000ffff_0000ffff;
            let low_mask = WordVec::splat(low_mask_word);
            let high_mask = WordVec::splat(!low_mask_word);
            let a_low = a & low_mask;
            let a_high = a & high_mask;
            let b_low = b & low_mask;
            let b_high = b & high_mask;

            data[32 * i32 + off + 0] = (b_low << 16) | a_low;
            data[32 * i32 + off + 16] = (a_high >> 16) | b_high;
        }
    }

    for off in 0..32 {
        let a = data[64 * i64 + off + 0];
        let b = data[64 * i64 + off + 32];

        let low_mask_word = 0x00000000_ffffffff;
        let low_mask = WordVec::splat(low_mask_word);
        let high_mask = WordVec::splat(!low_mask_word);
        let a_low = a & low_mask;
        let a_high = a & high_mask;
        let b_low = b & low_mask;
        let b_high = b & high_mask;

        data[64 * i64 + off + 0] = (b_low << 32) | a_low;
        data[64 * i64 + off + 32] = (a_high >> 32) | b_high;
    }
}

#[cfg(test)]
mod tests {
    use rand::{Rng, SeedableRng};

    use super::*;

    #[test]
    fn test_bitmatrix_transpose() {
        let mut rng = rand::rngs::SmallRng::seed_from_u64(1);
        for (cols, rows) in [(701, 521), (4571, 25 * 256)] {
            let mut m = BitMatrix::new(cols);
            let mut m2 = m.clone();

            for _ in 0..rows {
                m.push_zero_row();
            }

            for i in 0..rows {
                for j in 0..cols {
                    if rng.gen() {
                        m.set_bit(i, j, true);
                    }
                }
            }

            m.transpose_into(&mut m2);
            let mut m3 = m2.clone();
            m2.transpose_into(&mut m3);

            for i in 0..rows {
                for j in 0..cols {
                    assert_eq!(m.bit(i, j), m3.bit(i, j));
                    assert_eq!(m.bit(i, j), m2.bit(j, i), "{i} {j}");
                }
            }
        }
    }
}
