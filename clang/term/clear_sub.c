fn Term term_clear_sub(Term t) {
  return t & ~(((u64)SUB_MASK) << SUB_SHIFT);
}
