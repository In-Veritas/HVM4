// data/uset.c - open-addressing set for non-zero u32 keys.
//
// Context
// - Used by parallel normalization to track visited heap locations cheaply.
// - Optimized for insert/contains; there is no deletion.
//
// Design
// - Linear probing over a power-of-two table.
// - Key 0 is reserved as the empty slot sentinel.
// - Hash is multiplicative; index is masked by (cap - 1).
// - Grows by doubling when load exceeds 50%.
//
// Notes
// - Not thread-safe; callers must synchronize externally.
// - cap_hint is rounded up to a power of two (minimum 2).
// - On allocation failure, the process exits with an error.
// - Because 0 is reserved, 0 is always reported as "not present".

// Uset stores the open-addressing table and its load metadata.
typedef struct {
  u32 *table;
  u32 cap;
  u32 mask;
  u32 len;
  u32 thresh;
} Uset;

// Multiplicative hash for u32 keys (unmasked).
fn u32 uset_hash(u32 key) {
  return key * 2654435761u;
}

// Round up to next power of two (minimum 2) for mask arithmetic.
fn u32 uset_next_pow2(u32 x) {
  if (x < 2u) {
    return 2u;
  }
  x--;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return x + 1;
}

// Initialize the set with a capacity >= cap_hint.
fn void uset_init(Uset *set, u32 cap_hint) {
  u32 cap = uset_next_pow2(cap_hint);
  set->table = (u32 *)calloc(cap, sizeof(u32));
  if (!set->table) {
    fprintf(stderr, "uset: allocation failed\n");
    exit(1);
  }
  set->cap    = cap;
  set->thresh = cap >> 1;
  set->mask   = cap - 1;
  set->len    = 0;
}

// Release the backing table and reset the set state.
fn void uset_free(Uset *set) {
  if (set->table) {
    free(set->table);
  }
  *set = (Uset){0};
}

// Resize to new_cap and reinsert all keys.
fn void uset_rehash(Uset *set, u32 new_cap) {
  if (new_cap < set->cap) {
    fprintf(stderr, "uset: allocation failed\n");
    exit(1);
  }
  u32 *old = set->table;
  u32 old_cap = set->cap;

  set->table = (u32 *)calloc(new_cap, sizeof(u32));
  if (!set->table) {
    fprintf(stderr, "uset: allocation failed\n");
    exit(1);
  }
  set->cap    = new_cap;
  set->thresh = new_cap >> 1;
  set->mask   = new_cap - 1;
  set->len    = 0;

  for (u32 i = 0; i < old_cap; i++) {
    u32 key = old[i];
    if (key == 0) {
      continue;
    }
    u32 idx = uset_hash(key) & set->mask;
    while (set->table[idx] != 0) {
      idx = (idx + 1) & set->mask;
    }
    set->table[idx] = key;
    set->len++;
  }

  free(old);
}

// Check whether key is present (0 is never present).
fn u8 uset_has(Uset *set, u32 key) {
  if (key == 0) {
    return 0;
  }
  u32 idx = uset_hash(key) & set->mask;
  for (u32 i = 0; i < set->cap; i++) {
    u32 cur = set->table[idx];
    if (cur == 0) {
      return 0;
    }
    if (cur == key) {
      return 1;
    }
    idx = (idx + 1) & set->mask;
  }
  return 0;
}

// Insert key if missing; returns 1 if inserted, 0 if already present.
fn u8 uset_add(Uset *set, u32 key) {
  if (key == 0) {
    return 0;
  }

  if (set->len >= set->thresh) {
    uset_rehash(set, set->cap << 1);
  }                                                                       

  u32 *table = set->table;
  u32 mask   = set->mask;
  u32 idx    = uset_hash(key) & mask;

  while (1) {
    u32 cur = table[idx];
    if (cur == key) {
      return 0;
    }
    if (cur == 0) {
      table[idx] = key;
      set->len++;
      return 1;
    }
    idx = (idx + 1) & mask;
  }
}
