fn Term heap_take(u32 loc) {
  if (__builtin_expect(THREAD_COUNT == 1, 1)) {
    Term term = HEAP[loc];
    if (__builtin_expect(term != 0, 1)) {
      return term;
    }
    do {
      cpu_relax();
      term = HEAP[loc];
    } while (term == 0);
    return term;
  }
  for (;;) {
    Term prev = __atomic_exchange_n(&HEAP[loc], 0, __ATOMIC_RELAXED);
    if (__builtin_expect(prev != 0, 1)) {
      return prev;
    }
    cpu_relax();
  }
}
