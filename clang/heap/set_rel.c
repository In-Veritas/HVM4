// heap_set with release ordering, for writing locations that may be shared with other threads
fn void heap_set_rel(u32 loc, Term val) {
  __atomic_store_n(&HEAP[loc], val, __ATOMIC_RELEASE);
}
