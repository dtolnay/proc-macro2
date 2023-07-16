pub(crate) fn usize_to_u32(u: usize) -> Option<u32> {
    u32::try_from(u).ok()
}
