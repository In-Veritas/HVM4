fn u64 heap_alloc_total(void) {
  u64 sum = 0;
  u32 threads = thread_get_count();
  for (u32 i = 0; i < threads; i++) {
    sum += heap_alloc_thread(i);
  }
  return sum;
}
