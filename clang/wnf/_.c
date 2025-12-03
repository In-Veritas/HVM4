__attribute__((hot)) fn Term wnf(Term term) {
  u32 base = S_POS;
  Term next = term;
  Term whnf;

  enter: {
    if (__builtin_expect(DEBUG, 0)) {
      printf("wnf_enter: ");
      print_term(next);
      printf("\n");
    }

    switch (term_tag(next)) {
      case VAR: {
        u32 loc = term_val(next);
        if (term_sub(HEAP[loc])) {
          next = term_unmark(HEAP[loc]);
          goto enter;
        }
        whnf = next;
        goto apply;
      }

      case CO0:
      case CO1: {
        u32 loc = term_val(next);
        if (term_sub(HEAP[loc])) {
          next = term_unmark(HEAP[loc]);
          goto enter;
        }
        Term dup_val = HEAP[loc];
        STACK[S_POS++] = next;
        next = dup_val;
        goto enter;
      }

      case APP: {
        u32  loc = term_val(next);
        Term fun = HEAP[loc];
        STACK[S_POS++] = next;
        next = fun;
        goto enter;
      }

      case DUP: {
        u32  loc  = term_val(next);
        Term body = HEAP[loc + 1];
        next = body;
        goto enter;
      }

      case REF: {
        u32 nam = term_ext(next);
        if (BOOK[nam] != 0) {
          u64 alo_loc = heap_alloc(1);
          HEAP[alo_loc] = (u64)BOOK[nam];
          next = term_new(0, ALO, 0, alo_loc);
          goto enter;
        }
        whnf = next;
        goto apply;
      }

      case ALO: {
        u32  alo_loc = term_val(next);
        u64  pair    = HEAP[alo_loc];
        u32  tm_loc  = (u32)(pair & 0xFFFFFFFF);
        u32  ls_loc  = (u32)(pair >> 32);
        Term book    = HEAP[tm_loc];

        switch (term_tag(book)) {
          case VAR: {
            next = wnf_alo_var(ls_loc, term_val(book));
            goto enter;
          }
          case CO0:
          case CO1: {
            next = wnf_alo_cop(ls_loc, term_val(book), term_ext(book), term_tag(book) == CO0 ? 0 : 1);
            goto enter;
          }
          case NAM: {
            next = wnf_alo_nam(term_ext(book));
            goto enter;
          }
          case DRY: {
            next = wnf_alo_dry(ls_loc, term_val(book));
            goto enter;
          }
          case LAM: {
            next = wnf_alo_lam(ls_loc, term_val(book));
            goto enter;
          }
          case APP:
          case SUP:
          case MAT:
          case SWI:
          case USE:
          case C00 ... C16:
          case OP2:
          case OP1:
          case DYS:
          case DYD: {
            next = wnf_alo_node(ls_loc, term_val(book), term_tag(book), term_ext(book), term_arity(book));
            goto enter;
          }
          case DUP: {
            next = wnf_alo_dup(ls_loc, term_val(book), term_ext(book));
            goto enter;
          }
          case NUM: {
            next = term_new_num(term_val(book));
            goto enter;
          }
          case REF:
          case ERA: {
            next = book;
            goto enter;
          }
        }
      }

      case OP2: {
        u32  loc = term_val(next);
        Term x   = HEAP[loc + 0];
        STACK[S_POS++] = next;
        next = x;
        goto enter;
      }

      case OP1: {
        u32  loc = term_val(next);
        Term y   = HEAP[loc + 1];
        STACK[S_POS++] = next;
        next = y;
        goto enter;
      }

      case DYS: {
        u32  loc = term_val(next);
        Term lab = HEAP[loc + 0];
        STACK[S_POS++] = next;
        next = lab;
        goto enter;
      }

      case DYD: {
        u32  loc = term_val(next);
        Term lab = HEAP[loc + 0];
        STACK[S_POS++] = next;
        next = lab;
        goto enter;
      }

      case NAM:
      case DRY:
      case ERA:
      case SUP:
      case LAM:
      case NUM:
      case MAT:
      case SWI:
      case USE:
      case C00 ... C16: {
        whnf = next;
        goto apply;
      }

      default: {
        whnf = next;
        goto apply;
      }
    }
  }

  apply: {
    if (__builtin_expect(DEBUG, 0)) {
      printf("wnf_apply: ");
      print_term(whnf);
      printf("\n");
    }

    while (S_POS > base) {
      Term frame = STACK[--S_POS];

      switch (term_tag(frame)) {
        case APP: {
          u32  loc = term_val(frame);
          Term arg = HEAP[loc + 1];

          switch (term_tag(whnf)) {
            case ERA: {
              whnf = wnf_app_era();
              continue;
            }
            case NAM: {
              whnf = wnf_app_nam(whnf, arg);
              continue;
            }
            case DRY: {
              whnf = wnf_app_dry(whnf, arg);
              continue;
            }
            case LAM: {
              next = wnf_app_lam(whnf, arg);
              goto enter;
            }
            case SUP: {
              next = wnf_app_sup(frame, whnf);
              goto enter;
            }
            case MAT: {
              STACK[S_POS++] = whnf;
              next = arg;
              goto enter;
            }
            case SWI: {
              STACK[S_POS++] = whnf;
              next = arg;
              goto enter;
            }
            case USE: {
              STACK[S_POS++] = whnf;
              next = arg;
              goto enter;
            }
            default: {
              whnf = term_new_app(whnf, arg);
              continue;
            }
          }
        }

        case MAT: {
          Term mat = frame;
          switch (term_tag(whnf)) {
            case ERA: {
              whnf = wnf_app_era();
              continue;
            }
            case SUP: {
              next = wnf_app_mat_sup(mat, whnf);
              goto enter;
            }
            case C00 ... C16: {
              next = wnf_app_mat_ctr(mat, whnf);
              goto enter;
            }
            default: {
              whnf = term_new_app(mat, whnf);
              continue;
            }
          }
        }

        case SWI: {
          switch (term_tag(whnf)) {
            case ERA: {
              whnf = wnf_app_swi_era();
              continue;
            }
            case NUM: {
              next = wnf_app_swi_num(frame, whnf);
              goto enter;
            }
            case SUP: {
              next = wnf_app_swi_sup(frame, whnf);
              goto enter;
            }
            default: {
              whnf = term_new_app(frame, whnf);
              continue;
            }
          }
        }

        case USE: {
          switch (term_tag(whnf)) {
            case ERA: {
              whnf = wnf_use_era();
              continue;
            }
            case SUP: {
              next = wnf_use_sup(frame, whnf);
              goto enter;
            }
            default: {
              next = wnf_use_val(frame, whnf);
              goto enter;
            }
          }
        }

        case CO0:
        case CO1: {
          u8  side = (term_tag(frame) == CO0) ? 0 : 1;
          u32 loc  = term_val(frame);
          u32 lab  = term_ext(frame);

          switch (term_tag(whnf)) {
            case NAM: {
              whnf = wnf_dup_nam(lab, loc, side, whnf);
              continue;
            }
            case DRY: {
              next = wnf_dup_dry(lab, loc, side, whnf);
              goto enter;
            }
            case LAM: {
              next = wnf_dup_lam(lab, loc, side, whnf);
              goto enter;
            }
            case SUP: {
              next = wnf_dup_sup(lab, loc, side, whnf);
              goto enter;
            }
            case ERA:
            case NUM: {
              whnf = wnf_dup_node(lab, loc, side, whnf);
              continue;
            }
            case MAT:
            case SWI:
            case USE:
            case OP2:
            case OP1:
            case DYS:
            case DYD:
            case C00 ... C16: {
              next = wnf_dup_node(lab, loc, side, whnf);
              goto enter;
            }
            default: {
              u64 new_loc   = heap_alloc(1);
              HEAP[new_loc] = whnf;
              heap_subst_var(loc, term_new(0, side == 0 ? CO1 : CO0, lab, new_loc));
              whnf          = term_new(0, side == 0 ? CO0 : CO1, lab, new_loc);
              continue;
            }
          }
        }

        case OP2: {
          u32  opr = term_ext(frame);
          u32  loc = term_val(frame);
          Term y   = HEAP[loc + 1];

          switch (term_tag(whnf)) {
            case ERA: {
              whnf = wnf_op2_era();
              continue;
            }
            case NUM: {
              next = wnf_op2_num(opr, whnf, y);
              goto enter;
            }
            case SUP: {
              next = wnf_op2_sup(opr, whnf, y);
              goto enter;
            }
            default: {
              whnf = term_new_op2(opr, whnf, y);
              continue;
            }
          }
        }

        case OP1: {
          u32  opr = term_ext(frame);
          u32  loc = term_val(frame);
          Term x   = HEAP[loc + 0];

          switch (term_tag(whnf)) {
            case ERA: {
              whnf = wnf_op1_era();
              continue;
            }
            case NUM: {
              whnf = wnf_op1_num(opr, x, whnf);
              continue;
            }
            case SUP: {
              next = wnf_op1_sup(opr, x, whnf);
              goto enter;
            }
            default: {
              whnf = term_new_op1(opr, x, whnf);
              continue;
            }
          }
        }

        case DYS: {
          u32  loc = term_val(frame);
          Term a   = HEAP[loc + 1];
          Term b   = HEAP[loc + 2];

          switch (term_tag(whnf)) {
            case ERA: {
              whnf = wnf_dsu_era();
              continue;
            }
            case NUM: {
              next = wnf_dsu_num(whnf, a, b);
              goto enter;
            }
            case SUP: {
              next = wnf_dsu_sup(whnf, a, b);
              goto enter;
            }
            default: {
              whnf = term_new_dys(whnf, a, b);
              continue;
            }
          }
        }

        case DYD: {
          u32  loc = term_val(frame);
          Term val = HEAP[loc + 1];
          Term bod = HEAP[loc + 2];

          switch (term_tag(whnf)) {
            case ERA: {
              whnf = wnf_ddu_era();
              continue;
            }
            case NUM: {
              next = wnf_ddu_num(whnf, val, bod);
              goto enter;
            }
            case SUP: {
              next = wnf_ddu_sup(whnf, val, bod);
              goto enter;
            }
            default: {
              whnf = term_new_dyd(whnf, val, bod);
              continue;
            }
          }
        }

        default: {
          continue;
        }
      }
    }
  }

  return whnf;
}
