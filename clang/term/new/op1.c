// Op1(opr, x, y): binary operation with x already reduced, strict on y
// Layout: HEAP[loc+0] = x (NUM), HEAP[loc+1] = y
// EXT field = operation code (OP_ADD, OP_MUL, etc.)
fn Term term_new_op1(u32 opr, Term x, Term y) {
  u32 loc = heap_alloc(2);
  HEAP[loc + 0] = x;
  HEAP[loc + 1] = y;
  return term_new(0, OP1, opr, loc);
}
