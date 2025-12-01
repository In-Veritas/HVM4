fn Term term_mark_sub(Term t) {
  return t | ((u64)1 << SUB_SHIFT);
}
