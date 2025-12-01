fn u32 term_val(Term t) {
  return (t >> VAL_SHIFT) & VAL_MASK;
}
