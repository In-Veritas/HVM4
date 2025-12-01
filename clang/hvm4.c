// HVM4 Runtime Implementation in C
// =================================
//
// This file implements the HVM4, an Interaction Calculus runtime, ported from
// Haskell. It includes parsing, stringification, and a stack-based weak normal
// form (WNF) evaluator with all interaction rules.
//
// Term Pointer Layout (64-bit)
// ----------------------------
// | SUB  (1 bit)   ::= marks heap slot as containing a substitution
// | TAG  (7 bits)  ::= constructor variant (APP, LAM, SUP, etc.)
// | EXT  (24 bits) ::= dup label, ctr name, or ref name
// | VAL  (32 bits) ::= heap address or unboxed value
//
// Tag Encoding
// ------------
// - CO0/CO1: Two tags for Cop (copy) nodes, representing sides 0 and 1
// - C00...C16: Constructor tags encode arity directly (C00+n for n fields)
// - Names (variable, constructor, reference) use 6-char base64 strings encoded
//   as 24-bit integers fitting in the EXT field
//
// Memory Model (No Separate Maps)
// -------------------------------
// Unlike the Haskell version which uses IntMaps for 'dups' and 'subs', this
// implementation stores everything directly on the heap:
//
// - DUP nodes: Stored inline on heap. CO0/CO1 point to a dup node holding the
//   duplicated expression (label stored in CO0/CO1's EXT field).
//
// - Substitutions: Stored where the lambda's body, or duplicator expression,
//   was. When app_lam fires, the argument replaces the lambda body slot. The
//   SUB bit distinguishes actual terms from substitutions, allowing VAR, CO0
//   and CO1 to detect whether their target is a binding node or a subst.
//
// Book vs Runtime Term Representation
// -----------------------------------
// Book terms (parsed definitions) use de Bruijn indices and are immutable:
//   - VAR: ext = 0         ; val = bru_index
//   - CO_: ext = dup_label ; val = bru_index
//   - LAM: ext = bru_depth ; val = body_location
//   - DUP: ext = dup_label ; val = expr_location
//
// Runtime terms (after ALO allocation) use heap locations:
//   - VAR : ext = 0         ; val = binding_lam_body_location
//   - CO_ : ext = dup_label ; val = binding_dup_expr_location
//   - LAM : ext = 0         ; val = expr_location
//   - DUP : ext = 0         ; val = expr_location
//
// ALO (Allocation) Nodes
// ----------------------
// ALO terms reference immutable book entries and lazily convert them to
// runtime terms. Each ALO stores a pair (bind_list, book_term_loc) packed
// into a single 64-bit heap word:
//   - Low 32 bits: book term location
//   - High 32 bits: bind list head (linked list of binder locations)
//
// The bind list maps de Bruijn levels to runtime heap locations of binding
// LAM/DUP nodes. When an ALO interaction occurs, one layer of the book term
// is extracted and converted to a runtime term.
//
// Stack-Based WNF Evaluator
// -------------------------
//   To avoid stack overflow, WNF uses an explicit stack with two phases:
//
//   REDUCE phase: Push eliminators onto stack and descend into their targets
//     - APP: push frame, enter function
//     - MAT: push frame, enter scrutinee (after MAT reaches head position)
//     - CO0/CO1: push frame, enter dup'd expression
//
//   APPLY phase: Pop frames and dispatch interactions based on WHNF result
//
//   DUP and ALO don't push stack frames since they immediately trigger their
//   respective interactions without requiring sub-WNF results first.
//
// Internal-Only Constructs
// ------------------------
// These nodes are internal and not parseable:
// - ALO: lazy alloc
// Stuck terms use special CTR names:
// - #VAR{#name{}}: stuck variable (name encoded as 0-arity CTR)
// - #APP{f,x}: stuck application
//
// Collapse Function
// -----------------
// Extracts superpositions (SUP) to the top level. For each term type:
// 1. Collapse subterms recursively
// 2. Build a template: nested lambdas that reconstruct the term
// 3. Call inject(template, collapsed_subterms)
// This will move the term to inside SUP layers, 'collapsing' it.
//
// Key: VARs in templates must point to their binding lambda's body location.
// For LAM, the inner lambda MUST reuse lam_loc so existing VARs stay bound.
//
// Style Guide
// -----------
// Abide to the guidelines below strictly!
//
// > NEVER write single-line ifs, loops, statements, functions.
//
//   Don't:
//     if { ... }
//     while { ... }
//     u32 foo(x) { ... }
//     foo(); bar();
//
//   Do:
//     if {
//       ...
//     }
//     while {
//       ...
//     }
//     u32 foo(x) {
//       ...
//     }
//     foo();
//     bar();
//
// > ALWAYS use switch for Term pattern matching.
//
//   Don't:
//     if (tag == FOO) {
//       ...
//     } else if (tag == BAR) {
//       ...
//     } ...
//
//   Do:
//     switch (tag) {
//       case FOO: {
//         ...
//       }
//       case BAR: {
//         ...
//       }
//     }
//
// > Aggressively abstract common patterns (DRY).
//
//   When a pattern is repeated in multiple places:
//
//   Don't:
//     fn Term <many_fns>(...) {
//       ...
//       if (side == 0) {
//         subst_var(loc, res1);
//         return res0;
//       } else {
//         subst_var(loc, res0);
//         return res1;
//       }
//    }
//
//   Do:
//     fn Term subst_cop(u8 side, u32 loc, Term r0, Term r1) {
//       subst_var(loc, side == 0 ? r1 : r0);
//       return side == 0 ? r0 : r1;
//     }
//     fn Term <many_fns>(...) {
//       ...
//       return subst_cop(side, loc, res0, res1);
//     }
//
//   In general, spend some time reasoning about opportunities to apply the DRY
//   principle, extracting common patterns out to reduce code size. We greatly
//   appreciate simplicity brought by good abstractions!
//
// > Align columns whenever reasonable; adjust names as needed.
//
//   Don't:
//
//   Term abc = foo;
//   u32 x = 123;
//   Term the_amazing_cat = bar;
//
//   Do:
//
//   Term abc = foo;
//   u32  x   = 123;
//   Term cat = bar;
//
//   Don't:
//
//   foo[x] = 123;
//   foo[x+1] = 456;
//
//   Do:
//
//   foo[x+0] = 123;
//   foo[x+1] = 456;
//
// > Separate sessions with markdown-inspired headers.
//
//   Don't:
//
//   ---------------------------------
//   File Session
//   ---------------------------------
//
//   Do:
//
//   File Session
//   ============
//
//   File Sub-Session
//   ----------------
//
//   ### File Sub-Sub-Session

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

// Term
// ====

#include "term/_.c"
#include "term/new.c"
#include "term/sub.c"
#include "term/tag.c"
#include "term/ext.c"
#include "term/val.c"
#include "term/arity.c"
#include "term/mark_sub.c"
#include "term/clear_sub.c"

// Heap
// ====

#include "heap/_.c"
#include "heap/alloc.c"

// Term Constructors
// =================

#include "term/make_at.c"
#include "term/make.c"
#include "term/var.c"
#include "term/ref.c"
#include "term/era.c"
#include "term/co0.c"
#include "term/co1.c"
#include "term/lam_at.c"
#include "term/lam.c"
#include "term/app_at.c"
#include "term/app.c"
#include "term/sup_at.c"
#include "term/sup.c"
#include "term/dup_at.c"
#include "term/dup.c"
#include "term/mat_at.c"
#include "term/mat.c"
#include "term/ctr_at.c"
#include "term/ctr.c"
#include "term/num.c"
#include "term/clone_at.c"
#include "term/clone.c"
#include "term/clone_many.c"

// Heap Substitution
// =================

#include "heap/subst_var.c"
#include "heap/subst_cop.c"

// Book
// ====

#include "book/_.c"

// Letter
// ======

#include "letter/alphabet.c"
#include "letter/to_b64.c"
#include "letter/is_name_char.c"

// System
// ======

#include "sys/error.c"
#include "sys/path_join.c"
#include "sys/file_read.c"

// Print
// =====

#include "print/_.c"
#include "print/str_putc.c"
#include "print/str_puts.c"
#include "print/str_name.c"
#include "print/str_uint.c"
#include "print/is_app.c"
#include "print/str_term_go.c"
#include "print/term.c"
#include "print/term_buf_init.c"
#include "print/term_buf_free.c"
#include "print/term_to_str.c"

// Parse
// =====

#include "parse/_.c"
#include "parse/error.c"
#include "parse/at_end.c"
#include "parse/peek_at.c"
#include "parse/peek.c"
#include "parse/advance.c"
#include "parse/starts_with.c"
#include "parse/match.c"
#include "parse/is_space.c"
#include "parse/skip_comment.c"
#include "parse/skip.c"
#include "parse/consume.c"
#include "parse/bind_push.c"
#include "parse/bind_pop.c"
#include "parse/bind_lookup.c"
#include "parse/name.c"
#include "parse/term.c"

// WNF
// ===

#include "wnf/_.c"
#include "wnf/app_era.c"
#include "wnf/app_ctr.c"
#include "wnf/app_lam.c"
#include "wnf/app_sup.c"
#include "wnf/app_mat_sup.c"
#include "wnf/app_mat_ctr.c"
#include "wnf/dup_lam.c"
#include "wnf/dup_sup.c"
#include "wnf/dup_node.c"
#include "wnf/alo_var.c"
#include "wnf/alo_cop.c"
#include "wnf/alo_lam.c"
#include "wnf/alo_dup.c"
#include "wnf/alo_node.c"
#include "wnf/wnf.c"

// SNF
// ===

#include "snf/_.c"

// Collapse
// ========

#include "collapse/inject.c"
#include "collapse/_.c"
