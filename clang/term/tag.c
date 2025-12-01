fn u8 term_tag(Term t) {
  return (t >> TAG_SHIFT) & TAG_MASK;
}
