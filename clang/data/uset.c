// data/uset.c - concurrent bitset for non-zero u64 keys.
//
// Context
// - Used by parallel normalization to track visited heap locations.
// - One bit per heap location.

typedef struct {
  _Atomic u64 *words;
  u64 word_count;
} Uset;

// Number of 64-bit words needed to represent all heap locations.
fn u64 uset_words_for_heap(void) {
  return (HEAP_CAP + 63ull) >> 6;
}

// Initialize bitset storage.
fn void uset_init(Uset *set) {
  u64 words = uset_words_for_heap();
  set->words = (_Atomic u64 *)calloc((size_t)words, sizeof(u64));
  if (!set->words) {
    fprintf(stderr, "uset: allocation failed\n");
    exit(1);
  }
  set->word_count = words;
}

// Release storage and reset state.
fn void uset_free(Uset *set) {
  if (set->words) {
    free((void *)set->words);
  }
  *set = (Uset){0};
}

// Insert key if missing; returns 1 if inserted, 0 if already present.
fn u8 uset_add(Uset *set, u64 key) {
  u64 word_idx = key >> 6;
  u64 bit_mask = 1ull << (key & 63u);
  u64 prev = atomic_fetch_or_explicit(&set->words[word_idx], bit_mask, memory_order_relaxed);
  return (prev & bit_mask) == 0;
}
