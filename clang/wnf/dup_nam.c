// ! X &L = name
// ------------ DUP-NAM
// X₀ ← name
// X₁ ← name
fn Term wnf_dup_nam(u32 lab, u64 loc, u8 side, Term nam) {
  ITRS_INC("DUP-NAM");
  heap_subst_var_dup(loc, nam);
  return nam;
}
