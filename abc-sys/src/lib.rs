#![allow(unexpected_cfgs)] // for coverage_nightly
#![cfg_attr(coverage_nightly, feature(coverage_attribute))]

#[allow(non_snake_case)]
#[allow(non_camel_case_types)]
#[allow(non_upper_case_globals)]
#[allow(clippy::all)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod bindings {
    use libc::FILE;
    include!("generated/bindings.rs");
}

pub use bindings::*;

#[cfg(test)]
mod tests {
    #[test]
    fn test_gia_man() {
        use super::bindings as abc;
        unsafe {
            let gia = abc::Gia_ManStart(100);

            let ci_1 = abc::Gia_ManAppendCi(gia);
            let ci_2 = abc::Gia_ManAppendCi(gia);

            let obj_2 = abc::Gia_ManAppendAnd(gia, ci_1, ci_2);

            abc::Gia_ManAppendCo(gia, obj_2);

            abc::Gia_ManStop(gia);
        }
    }

    #[test]
    fn test_fraig_y() {
        use super::bindings as abc;
        unsafe {
            let mut gia = abc::Gia_ManStart(100);

            let ci_1 = abc::Gia_ManAppendCi(gia);
            let ci_2 = abc::Gia_ManAppendCi(gia);

            let obj_2 = abc::Gia_ManAppendAnd(gia, ci_1, ci_2);

            abc::Gia_ManAppendCo(gia, obj_2);

            let mut params: abc::Cec_ParFra_t = std::mem::zeroed();
            abc::Cec4_ManSetParams(&mut params);
            params.fVerbose = 0;

            let new_gia = abc::Cec5_ManSimulateTest(gia, &mut params, 1, 600, 1, 500);
            abc::Gia_ManStop(gia);
            gia = new_gia;

            abc::Gia_ManStop(gia);
        }
    }
}
