/// Manually constructed representation of `*mut dyn Trait` pointers.
///
/// This is used as workaround for the lack of a stable pointer metadata API and should be replaced
/// by the use of such an API as soon as it becomes stable.
///
/// I'm assuming that some form of such an API will be stabilized before the representation of such
/// pointers ever changes. Just in case that assumption turns out to be incorrect, this includes
/// compile time checks that verify this struct against the actual pointer layout.
#[repr(C)]
pub struct WidePtr {
    pub ptr: *mut u8,
    pub vtable: *const u8,
}

/// This is a compile time check to ensure that `WidePtr` matches the layout of a `*mut dyn Trait`
/// pointer.
const _: () = {
    let u32_ptr = 0usize as *mut u32;
    let u32_dyn_ptr = u32_ptr as *mut dyn std::fmt::Debug;
    let char_ptr = 0usize as *mut char;
    let char_dyn_ptr = char_ptr as *mut dyn std::fmt::Debug;

    // SAFETY: If there is a mismatch in size, the transmute will fail to compile. With the given
    // size, there is just enough space for the underlying pointer and a single vtable pointer, with
    // two possible field orderings. If the field ordering doesn't match, on current rustc versions,
    // the comparison will fail because compile time integer vs pointer comparisons are not allowed.
    // In a potential future rustc versions that allows compile time null pointer checks, at least
    // one of the asserts will fail as it can't be the case that the `dyn Debug` vtables for both
    // `u32` and `char` are null pointers.
    unsafe {
        let wide_ptr: WidePtr = std::mem::transmute(u32_dyn_ptr);
        assert!(std::mem::transmute::<*mut u8, usize>(wide_ptr.ptr) == 0);

        let wide_ptr: WidePtr = std::mem::transmute(char_dyn_ptr);
        assert!(std::mem::transmute::<*mut u8, usize>(wide_ptr.ptr) == 0);
    }
};
