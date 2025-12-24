#define HEAP_CHUNK_WORDS (1ULL << 24)

static inline void heap_refill(HeapBank *bank, u64 need) {
  u64 chunk = HEAP_CHUNK_WORDS;
  if (need > chunk) {
    chunk = need;
  }
  u64 base = __atomic_fetch_add(&HEAP_NEXT, chunk, __ATOMIC_RELAXED);
  u64 end = base + chunk;
  if (end > HEAP_CAP || end < base) {
    fprintf(stderr,
            "Out of heap memory in thread bank %u (need %llu words)\n",
            wnf_tid(), need);
    exit(1);
  }
  bank->start = base;
  bank->end   = end;
  bank->next  = base;
}

fn u64 heap_alloc(u64 size) {
  HeapBank *bank = &HEAP_BANKS[wnf_tid()];
  u64 at = bank->next;
  u64 next = at + size;
  if (next > bank->end || next < at) {
    heap_refill(bank, size);
    at = bank->next;
    next = at + size;
  }
  bank->next = next;
  return at;
}
