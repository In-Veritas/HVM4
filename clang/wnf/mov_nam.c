// % X = name
// ---------- MOV-NAM
// X ← name
fn Term wnf_mov_nam(u32 loc, Term nam) {
  ITRS++;
  heap_subst_var(loc, nam);
  return nam;
}
