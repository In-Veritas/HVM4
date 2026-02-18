fn u64 heap_alloc_thread(u32 tid) {
  u32 threads = thread_get_count();
  if (tid >= threads) {
    return 0;
  }

  u64 words   = HEAP_CAP;
  u64 bank_sz = words / threads;
  u64 start   = (u64)tid * bank_sz;
  if (tid == 0 && start == 0) {
    start = 1;
  }

  u64 next = HEAP_NEXT_AT(tid);
  if (next <= start) {
    return 0;
  }
  return next - start;
}
