fn void heap_recompute(void) {
  HEAP_NEXT = 1;
  for (u32 t = 0; t < MAX_THREADS; t++) {
    HEAP_BANKS[t].start = 0;
    HEAP_BANKS[t].end   = 0;
    HEAP_BANKS[t].next  = 0;
  }
}
