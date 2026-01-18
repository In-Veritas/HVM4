#define WNF_PRIM_ARGS_STACK_MAX 16

fn Term wnf_app_pri(Term pri, Term *stack, u32 *s_pos, u32 base) {
  u32 prim_id = term_ext(pri);
  Term (*fun)(Term *args) = prim_fun(prim_id);
  u32 arity = prim_arity(prim_id);

  if (fun == NULL || arity == 0) {
    char *prim_name = table_get(prim_id);
    if (prim_name != NULL) {
      fprintf(stderr, "RUNTIME_ERROR: unknown primitive '%%%s'\n", prim_name);
    } else {
      fprintf(stderr, "RUNTIME_ERROR: unknown primitive id %u\n", prim_id);
    }
    exit(1);
  }

  if ((*s_pos) - base < arity) {
    char *prim_name = table_get(prim_id);
    if (prim_name != NULL) {
      fprintf(stderr, "RUNTIME_ERROR: primitive '%%%s' called with wrong arity\n", prim_name);
    } else {
      fprintf(stderr, "RUNTIME_ERROR: primitive id %u called with wrong arity\n", prim_id);
    }
    exit(1);
  }

  Term  args_stack[WNF_PRIM_ARGS_STACK_MAX];
  Term *args = args_stack;
  if (arity > WNF_PRIM_ARGS_STACK_MAX) {
    args = malloc(arity * sizeof(Term));
    if (args == NULL) {
      fprintf(stderr, "RUNTIME_ERROR: primitive args allocation failed\n");
      exit(1);
    }
  }

  for (u32 i = 0; i < arity; i++) {
    Term frame = stack[--(*s_pos)];
    if (term_tag(frame) != APP) {
      char *prim_name = table_get(prim_id);
      if (prim_name != NULL) {
        fprintf(stderr, "RUNTIME_ERROR: primitive '%%%s' called with wrong arity\n", prim_name);
      } else {
        fprintf(stderr, "RUNTIME_ERROR: primitive id %u called with wrong arity\n", prim_id);
      }
      exit(1);
    }
    u32  app_loc = term_val(frame);
    Term arg     = heap_read(app_loc + 1);
    args[i] = arg;
  }

  Term res = fun(args);
  if (args != args_stack) {
    free(args);
  }
  return res;
}
