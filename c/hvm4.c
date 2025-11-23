//./../README.md//
//./../haskell/hvm4.hs//
//./../haskell/main.hs//
// 
// Above, you can see:
// - README.md: HVM4 spec
// - HVM4.hs: HVM4 in Haskell
//
// Your goal is to port HVM4 from Haskell to C, including:
// - a stringifier for HVM4 terms, fully compatible with the Haskell version
// - a parser for HVM4 terms, fully compatible with the Haskell version
// - a stack-based wnf function for HVM4 terms (important: don't use recursion on wnf)
// - all interactions: app_lam, app_sup, dup_lam, etc.
//
// the bit layout of the Term pointer must be:
// - 1 bit  : sub (is this heap slot a substitution entry?)
// - 7 bit  : tag (the constructor variant: APP, LAM, etc.)
// - 24 bit : lab (the CO0/CO1/DUP label, or the CTR name)
// - 32 bit : val (the node address on the heap, or unboxed value)
//
// for Cop, use two tags: CO0 (meaning COP 0) and CO1 (meaning COP 1)
// 
// notes:
//
// - on Ctr, we store the constructor name on the Lab field, and we store the
// arity on the TAG field itself, using CTR+0, CTR+1, CTR+2, etc., up to 16
// 
// - variable names use 6 letters strings in the base64 alphabet (thus, 24-bit,
// which fits in the Lab field of a Term). same for constructor names. note that
// var names are used just for parsing; they're removed entirely from the
// runtime (i.e., post bruijn()). the LAB field of a LAM/VAR is 0, the LAB field
// of a CO0/CO1/DUP is the dup label. the val field of a VAR/CO0/CO1's points to
// the binding LAM/DUP node on the heap.
// 
// - we do NOT include a 'dups' map or a 'subs' map. instead, we just store dup
// nodes directly on the heap (so, for example, CO0 and CO1 point to a "dup
// node", which holds just 1 slot, the dup'd expression (the label is stored on
// CO0/CO1). similarly, we store subsitutions directly on the heap: when we do
// an app_lam interaction, we store the substitution where the lam's body; when
// we do a dup interaction, we store the substitution where the dup'd expr was;
// to distinguish substitutions and actual terms (otherwise a VAR/CO0/CO1
// wouldn't be able to tell whether the term it is pointing to is its binding
// LAM/DUP node, or a substitution that took place), we reserve a bit on the
// Term pointer, the SUB bit, for that)
// 
// - Alo terms have 2 fields: the allocated Term, and the bind map, which maps
// bruijn levels to the index of the binding LAM or DUP. so, for example, if
// bind_map[3] = 123, that means that the VAR with bruijn level 3 is related to
// a LAM, whose body is stored on index 123. we store bind_map directly on the
// heap, compacting two 32-bit locations per 64-bit HEAP word. that means that
// ALO+0 has arity 1 (the static book Term), ALO+2 has arity 2 (the static book
// Term, plus a word for the 2 bind_map entried), ALO+4 has arity 3, and so on.
// 
// - we do de bruijn conversion before storing on Book, so, there will never be
// a naming conflict (stored book terms are always sanitized with fresh vars).
// 
// - a clarification about the match syntax: when parsing `λ{#A:x; #B:y; z}`,
// the parser must construct a nested chain of `Mat` nodes on the heap (e.g.,
// `Mat A x (Mat B y z)`), and the final term (last default case), if absent, is
// filled with just Era. see the Haskell parser for a reference
// ex: Mat parses λ{#A:x;#B:y;#C:z} as λ{#A:x;λ{#B:y;λ{#C:z;&{}}}}
//     Mat parses λ{#A:x;#B:y;#C:z;d} as λ{#A:x;λ{#B:y;λ{#C:z;d}}}
//     (and so on)
// 
// - to avoid recursion on the WNF, we just use a stack of 64-bit frames. see how
// it is done on harness.c. note that, here, we need 3 stack frames:
// than just App:
// - FApp ::= (*f x)
// - FMat ::= (λ{#A:h;m} *x)
// - FCo0 ::= ! F &L = *x (entered from Co0)
// - FCo1 ::= ! F &L = *x (entered from Co1)
// where * stands for a term we must wnf before continuing. to implement that,
// we store the stack using 4 globals: Term* STACK_BUF, u8* STACK_TAG. u32
// STACK_LEN, STACK_POS. the STACK_BUF buffer holds the non-strict Terms of that
// stack (on FMat, for example, that would be 'h', 'm'; on FApp, that's just
// 'x'). the STACK_TAG buffer holds the tag of stack items (FApp, FMat, etc.).
// the STACK_LEN stores the length fo the STACK_BUF, and STACK_POS the length of
// STACK_TAG.
// 
// - we split wnf in two phases:
// - REDUCE: matches eliminators (like App), pushes to the stack
// - UNWIND: dispatches interactions (like app_lam) and rebuilds
// note that in eliminators like Dup and Alo, no stack frame is needed, since
// they immediatelly trigger their respective interactiosn without needing to
// wnf anything.
// 
// - when matching on term tags, cover the CTR/ALO cases with 'case' expressions,
// do not use a default to cover them.
// 
// - ctrs have a max len of 16 fields
// - alos have a max of 32 binders on the bind_map
// - alos aren't parsed (see the Haskell parser - make it equivalent)
// 
// - we store entries on the book by their 6-letter, 24-bit names, as parsed.
// equivalently, the REF pointer stores the name in the 24-bit lab field.
// 
// - regarding Alo evaluation: remember that Book entries are Terms. note that
// book terms are immutable and can't interact with anything, or be copied. an
// Alo Term points to a Book Term as an immutable pointer. when an Alo
// interaction takes place, it extracts a layer of the immutable Book Term,
// converting it into a proper runtime term, and adding the binder to the subst
// map if it is a Lam/Dup, and generating new Alo's that point to the fields of
// the original Book Term (without mutating it).
// 
// 
// remember to use max addressable cap for all stacks. assume OS will handle.

// HVM4.c
// -------

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef u64 Term;

#define VAR  0 
#define REF  1 
#define NAM  2
#define ERA  3
#define CO0  4
#define CO1  5
#define LAM  6
#define APP  7
#define SUP  8
#define DRY  9
#define DUP 10
#define MAT 11
#define CT0 12
#define CT1 13
#define CT2 14
#define CT3 15
#define CT4 16
#define CT5 17
#define CT6 18
#define CT7 19
#define CT8 20
#define CT9 21
#define CTA 22
#define CTB 23
#define CTC 24
#define CTD 25
#define CTE 26
#define CTF 27
#define CTG 28
#define AL0 29
#define AL1 30
#define AL2 31
#define AL3 32
#define AL4 33
#define AL5 34
#define AL6 35
#define AL7 36
#define AL8 37
#define AL9 38
#define ALA 39
#define ALB 40
#define ALC 41
#define ALD 42
#define ALE 43
#define ALF 44
#define ALG 45

// Bit layout helpers
// ==================

#define SUB_BITS 1
#define TAG_BITS 7
#define LAB_BITS 24
#define VAL_BITS 32

#define SUB_SHIFT 63
#define TAG_SHIFT 56
#define LAB_SHIFT 32
#define VAL_SHIFT 0

#define SUB_MASK 0x1
#define TAG_MASK 0x7F
#define LAB_MASK 0xFFFFFF
#define VAL_MASK 0xFFFFFFFF

// Capacities
// ==========

#define HEAP_CAP  (1ULL << 32)
#define BOOK_CAP  (1ULL << 24)
#define STACK_CAP (1ULL << 24)

// Globals
// =======

static Term *BOOK;
static Term *HEAP;

static u64 HEAP_LEN = 1;

static Term *STACK_BUF;
static u8   *STACK_TAG;
static u32   STACK_LEN = 0;
static u32   STACK_POS = 0;

// System helpers
// ==============

static void error(const char *msg) {
  fprintf(stderr, "ERROR: %s\n", msg);
  exit(1);
}

// Term helpers
// ============

static inline Term new_term(u8 sub, u8 tag, u32 lab, u32 val) {
  return ((u64)sub << SUB_SHIFT)
       | ((u64)(tag & TAG_MASK) << TAG_SHIFT)
       | ((u64)(lab & LAB_MASK) << LAB_SHIFT)
       | ((u64)(val & VAL_MASK));
}

static inline u8 sub_of(Term t) {
  return (t >> SUB_SHIFT) & SUB_MASK;
}

static inline u8 tag_of(Term t) {
  return (t >> TAG_SHIFT) & TAG_MASK;
}

static inline u32 lab_of(Term t) {
  return (t >> LAB_SHIFT) & LAB_MASK;
}

static inline u32 val_of(Term t) {
  return (t >> VAL_SHIFT) & VAL_MASK;
}

static inline u64 heap_alloc(u64 size) {
  if (HEAP_LEN + size >= HEAP_CAP) {
    error("HEAP_OVERFLOW\n");
  }
  u64 at = HEAP_LEN;
  HEAP_LEN += size;
  return at;
}

// Constructors
// ============

static inline Term Var(u32 name) {
  return new_term(0, VAR, 0, name);
}

static inline Term Ref(u32 name) {
  return new_term(0, REF, name, 0);
}

static inline Term Nam(u32 name) {
  return new_term(0, NAM, 0, name);
}

static inline Term Era() {
  return new_term(0, ERA, 0, 0);
}

static inline Term Co0(u8 side, u32 lab, u32 name) {
  return new_term(0, side == 0 ? CO0 : CO1, lab, name);
}

static inline Term App(Term fun, Term arg) {
  u64 loc = heap_alloc(2);
  HEAP[loc+0] = fun;
  HEAP[loc+1] = arg;
  return new_term(0, APP, 0, loc);
}

static inline Term Lam(u32 name, Term body) {
  u64 loc = heap_alloc(1);
  HEAP[loc+0] = body;
  return new_term(0, LAM, name, loc);
}

static inline Term Sup(u32 lab, Term tm0, Term tm1) {
  u64 loc = heap_alloc(2);
  HEAP[loc+0] = tm0;
  HEAP[loc+1] = tm1;
  return new_term(0, SUP, lab, loc);
}

static inline Term Dry(Term tm0, Term tm1) {
  u64 loc = heap_alloc(2);
  HEAP[loc+0] = tm0;
  HEAP[loc+1] = tm1;
  return new_term(0, DRY, 0, loc);
}

static inline Term Dup(u32 lab, Term val, Term body) {
  u64 loc = heap_alloc(2);
  HEAP[loc+0] = val;
  HEAP[loc+1] = body;
  return new_term(0, DUP, lab, loc);
}

static inline Term Mat(u32 name, Term val, Term next) {
  u64 loc = heap_alloc(2);
  HEAP[loc+0] = val;
  HEAP[loc+1] = next;
  return new_term(0, MAT, name, loc);
}

static inline Term Ctr(u32 name, u32 arity, Term *args) {
  u64 loc = heap_alloc(arity);
  for (u32 i = 0; i < arity; i++) {
    HEAP[loc+i] = args[i];
  }
  return new_term(0, CT0 + arity, name, loc);
}

static inline Term Alo(u32 size, u32 *binds, Term term) {
  u32 words = (size + 1) / 2;
  u64 loc = heap_alloc(1 + words);
  HEAP[loc+0] = term;
  for (u32 i = 0; i < words; i++) {
    u64 b0 = binds[i*2+0];
    u64 b1 = (i*2+1 < size) ? binds[i*2+1] : 0;
    HEAP[loc+1+i] = b0 | (b1 << 32);
  }
  return new_term(0, AL0 + words, 0, loc);
}

// Names
// =====

static const char *alphabet
  = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$";

// TODO: char_to_b64
static int char_to_b64(char c) {
  if (c == '_') return 0;
  if (c >= 'a' && c <= 'z') return 1 + (c - 'a');
  if (c >= 'A' && c <= 'Z') return 27 + (c - 'A');
  if (c >= '0' && c <= '9') return 53 + (c - '0');
  if (c == '$') return 63;
  return -1;
}

static int is_name_start(char c) {
  return char_to_b64(c) >= 0;
}

static int is_name_char(char c) {
  return char_to_b64(c) >= 0;
}

// Stringifier
// ===========

static void print_name(u32 n) {
  if (n < 64) {
    putchar(alphabet[n]);
  } else {
    print_name(n / 64);
    putchar(alphabet[n % 64]);
  }
}

static void print_term(Term term) {
  switch (tag_of(term)) {
    case VAR: {
      print_name(val_of(term));
      break;
    }
    case REF: {
      printf("@");
      print_name(lab_of(term));
      break;
    }
    case NAM: {
      printf("^");
      print_name(val_of(term));
      break;
    }
    case ERA: {
      printf("&{}");
      break;
    }
    case CO0: {
      print_name(val_of(term));
      printf("₀");
      break;
    }
    case CO1: {
      print_name(val_of(term));
      printf("₁");
      break;
    }
    case LAM: {
      u32 loc = val_of(term);
      printf("λ");
      print_name(lab_of(term));
      printf(".");
      print_term(HEAP[loc]);
      break;
    }
    case APP:
    case DRY: {
      // Collect application spine
      Term spine[256];
      u32 len = 0;
      Term curr = term;
      
      while ((tag_of(curr) == APP || tag_of(curr) == DRY) && len < 256) {
        u32 loc = val_of(curr);
        spine[len++] = HEAP[loc+1];
        curr = HEAP[loc];
      }
      
      // Print function
      if (tag_of(curr) == LAM) {
        printf("(");
        print_term(curr);
        printf(")");
      } else {
        print_term(curr);
      }
      
      // Print arguments
      printf("(");
      for (u32 i = 0; i < len; i++) {
        if (i > 0) printf(",");
        print_term(spine[len - 1 - i]);
      }
      printf(")");
      break;
    }
    case SUP: {
      u32 loc = val_of(term);
      printf("&");
      print_name(lab_of(term));
      printf("{");
      print_term(HEAP[loc]);
      printf(",");
      print_term(HEAP[loc+1]);
      printf("}");
      break;
    }
    case DUP: {
      u32 loc = val_of(term);
      printf("!_&");
      print_name(lab_of(term));
      printf("=");
      print_term(HEAP[loc]);
      printf(";");
      print_term(HEAP[loc+1]);
      break;
    }
    case MAT: {
      u32 loc = val_of(term);
      printf("λ{#");
      print_name(lab_of(term));
      printf(":");
      print_term(HEAP[loc]);
      printf(";");
      print_term(HEAP[loc+1]);
      printf("}");
      break;
    }
    case CT0: case CT1: case CT2: case CT3:
    case CT4: case CT5: case CT6: case CT7:
    case CT8: case CT9: case CTA: case CTB:
    case CTC: case CTD: case CTE: case CTF:
    case CTG: {
      u32 arity = tag_of(term) - CT0;
      u32 loc = val_of(term);
      printf("#");
      print_name(lab_of(term));
      printf("{");
      for (u32 i = 0; i < arity; i++) {
        if (i > 0) printf(",");
        print_term(HEAP[loc+i]);
      }
      printf("}");
      break;
    }
    case AL0: case AL1: case AL2: case AL3:
    case AL4: case AL5: case AL6: case AL7:
    case AL8: case AL9: case ALA: case ALB:
    case ALC: case ALD: case ALE: case ALF:
    case ALG: {
      u32 words = tag_of(term) - AL0;
      u32 size = words * 2;
      u32 loc = val_of(term);
      printf("@{");
      for (u32 i = 0; i < size; i++) {
        if (i > 0) printf(",");
        u32 idx = i / 2;
        u64 pair = HEAP[loc+1+idx];
        u32 bind = (i % 2 == 0) ? (pair & 0xFFFFFFFF) : (pair >> 32);
        if (bind != 0 || i < size - 1) {
          print_name(bind);
        }
      }
      printf("}");
      print_term(HEAP[loc]);
      break;
    }
  }
}

// Parser
// ======

typedef struct {
  char *file;
  char *src;
  u32   pos;
  u32   len;
  u32   line;
  u32   col;
} State;

static char *SEEN_FILES[1024];
static u32   SEEN_COUNT = 0;

static void parse_error(State *s, const char *expected, char detected) {
  fprintf(stderr, "\033[1;31mPARSE_ERROR\033[0m (%s:%d:%d)\n", s->file, s->line, s->col);
  fprintf(stderr, "- expected: %s\n", expected);
  if (detected == 0) {
    fprintf(stderr, "- detected: EOF\n");
  } else {
    fprintf(stderr, "- detected: '%c'\n", detected);
  }
  exit(1);
}

static void parse_def(State *s);

static char peek(State *s) {
  if (s->pos < s->len) {
    return s->src[s->pos];
  } else {
    return 0;
  }
}

static void next(State *s) {
  if (s->pos < s->len) {
    if (s->src[s->pos] == '\n') {
      s->line++;
      s->col = 1;
    } else {
      s->col++;
    }
    s->pos++;
  }
}

static int match(State *s, const char *str) {
  u32 pos = s->pos;
  const char *pat = str;
  while (*pat) {
    if (pos >= s->len || s->src[pos] != *pat) return 0;
    pos++;
    pat++;
  }
  s->pos = pos;
  s->col += (pat - str); 
  return 1;
}

static void skip(State *s) {
  while (1) {
    char c = peek(s);
    if (isspace(c)) {
      next(s);
      continue;
    }
    if (match(s, "//")) {
      while (peek(s) && peek(s) != '\n') {
        next(s);
      }
      continue;
    }
    break;
  }
}

static void consume(State *s, const char *str) {
  skip(s);
  if (!match(s, str)) {
    parse_error(s, str, peek(s));
  }
}

static u32 parse_name(State *s) {
  skip(s);
  char c = peek(s);
  if (!is_name_start(c)) {
    parse_error(s, "name", c);
  }
  u32 k = 0;
  while (is_name_char(peek(s))) {
    c = peek(s);
    k = ((k << 6) + char_to_b64(c)) & LAB_MASK; // cap to 24 bits to stay inside name space
    next(s);
  }
  skip(s);
  return k;
}

static Term parse_term(State *s);
static Term parse_lam(State *s);
static Term parse_dup(State *s);
static Term parse_sup(State *s);
static Term parse_ctr(State *s);
static Term parse_ref(State *s);
static Term parse_par(State *s);
static Term parse_var(State *s);
static Term parse_app(Term f, State *s);

static Term parse_mat_body(State *s) {
  skip(s);
  char c = peek(s);
  if (c == '}') {
    consume(s, "}");
    return Era();
  }
  if (c == '#') {
    consume(s, "#");
    u32 name = parse_name(s);
    consume(s, ":");
    Term val = parse_term(s);
    skip(s);
    match(s, ";");
    Term nxt = parse_mat_body(s);
    return Mat(name, val, nxt);
  }
  Term val = parse_term(s);
  consume(s, "}");
  return val;
}

static Term parse_lam(State *s) {
  skip(s);
  if (peek(s) == '{') {
    consume(s, "{");
    return parse_mat_body(s);
  } else {
    u32 name = parse_name(s);
    consume(s, ".");
    Term body = parse_term(s);
    return Lam(name, body);
  }
}

static Term parse_dup(State *s) {
  u32 name = parse_name(s);
  consume(s, "&");
  u32 lab = parse_name(s);
  consume(s, "=");
  Term val = parse_term(s);
  consume(s, ";");
  Term body = parse_term(s);
  return Dup(lab, val, body);
}

static Term parse_sup(State *s) {
  skip(s);
  if (peek(s) == '{') {
    consume(s, "{");
    consume(s, "}");
    return Era();
  } else {
    u32 lab = parse_name(s);
    consume(s, "{");
    Term tm0 = parse_term(s);
    consume(s, ",");
    Term tm1 = parse_term(s);
    consume(s, "}");
    return Sup(lab, tm0, tm1);
  }
}

static Term parse_ctr(State *s) {
  u32 name = parse_name(s);
  consume(s, "{");
  Term args[16];
  u32 cnt = 0;
  skip(s);
  if (peek(s) != '}') {
    while (1) {
      args[cnt++] = parse_term(s);
      skip(s);
      if (peek(s) == ',') {
        consume(s, ",");
        skip(s);
      } else {
        break;
      }
    }
  }
  consume(s, "}");
  return Ctr(name, cnt, args);
}

static Term parse_ref(State *s) {
  skip(s);
  if (peek(s) == '{') {
    parse_error(s, "reference name", peek(s));
  }
  u32 name = parse_name(s);
  return Ref(name);
}

static Term parse_par(State *s) {
  Term term = parse_term(s);
  consume(s, ")");
  return term;
}

static Term parse_var(State *s) {
  skip(s);
  u32 name = parse_name(s);
  skip(s);
  if (match(s, "₀")) {
    return new_term(0, CO0, 0, name);
  } else if (match(s, "₁")) {
    return new_term(0, CO1, 0, name);
  } else {
    return Var(name);
  }
}

static Term parse_app(Term f, State *s) {
  if (peek(s) != '(') {
    return f;
  }

  next(s);
  skip(s);

  if (peek(s) == ')') {
    next(s);
    return parse_app(f, s);
  }

  while (1) {
    Term arg = parse_term(s);
    f = App(f, arg);
    skip(s);
    char c = peek(s);
    if (c == ',') {
      next(s);
      skip(s);
      continue;
    }
    if (c == ')') {
      next(s);
      break;
    }
    parse_error(s, "comma or ')'", c);
  }

  return parse_app(f, s);
}

static Term parse_term(State *s) {
  skip(s);
  Term t;
  if (match(s, "λ")) {
    t = parse_lam(s);
  } else if (match(s, "!")) {
    t = parse_dup(s);
  } else if (match(s, "&")) {
    t = parse_sup(s);
  } else if (match(s, "#")) {
    t = parse_ctr(s);
  } else if (match(s, "@")) {
    t = parse_ref(s);
  } else if (match(s, "(")) {
    t = parse_par(s);
  } else {
    t = parse_var(s);
  }
  return parse_app(t, s);
}

static void parse_def(State *s);

static void do_include(State *s, char *file) {
  for (int i = 0; i < SEEN_COUNT; i++) {
    if (strcmp(SEEN_FILES[i], file) == 0) return;
  }
  SEEN_FILES[SEEN_COUNT++] = strdup(file);

  FILE *fp = fopen(file, "rb");
  if (!fp) {
    fprintf(stderr, "Error: could not open file '%s'\n", file);
    exit(1);
  }
  fseek(fp, 0, SEEK_END);
  u32 len = ftell(fp);
  fseek(fp, 0, SEEK_SET);
  char *src = malloc(len + 1);
  if (!src) error("OOM");
  fread(src, 1, len, fp);
  src[len] = 0;
  fclose(fp);

  State new_s = { .file = file, .src = src, .pos = 0, .len = len, .line = 1, .col = 1 };
  parse_def(&new_s);
  free(src);
}

static void parse_def(State *s) {
  skip(s);
  if (s->pos >= s->len) {
    return;
  }
  if (match(s, "#include")) {
    consume(s, "\"");
    u32 ini = s->pos;
    while (peek(s) != '"' && peek(s) != 0) {
      next(s);
    }
    u32 end = s->pos;
    consume(s, "\"");
    char *f_name = malloc(end - ini + 1);
    memcpy(f_name, s->src + ini, end - ini);
    f_name[end - ini] = 0;
    do_include(s, f_name);
    free(f_name);
    parse_def(s);
  } else if (match(s, "@")) {
    u32 name = parse_name(s) & LAB_MASK;
    consume(s, "=");
    Term val = parse_term(s);
    BOOK[name] = val;
    parse_def(s);
  } else {
    parse_error(s, "definition or #include", peek(s));
  }
}

// De Bruijn
// =========

static u32 BRUIJN_ENV[1 << 24];

static Term bruijn_go(Term term, u32 depth) {
  u8 tag = tag_of(term);
  u32 lab = lab_of(term);
  u32 val = val_of(term);

  switch (tag) {
    case VAR:
    case CO0:
    case CO1: {
      u32 name = val;
      u32 bound_at = BRUIJN_ENV[name];
      if (bound_at != 0) {
        return new_term(0, tag, lab, depth - bound_at);
      }
      return term;
    }
    case LAM: {
      u32 name = lab;
      u32 prev = BRUIJN_ENV[name];
      BRUIJN_ENV[name] = depth + 1;
      Term body = HEAP[val];
      Term new_body = bruijn_go(body, depth + 1);
      BRUIJN_ENV[name] = prev;
      return Lam(0, new_body);
    }
    case APP: {
      Term fun = HEAP[val+0];
      Term arg = HEAP[val+1];
      Term new_fun = bruijn_go(fun, depth);
      Term new_arg = bruijn_go(arg, depth);
      return App(new_fun, new_arg);
    }
    case SUP: {
      Term tm0 = HEAP[val+0];
      Term tm1 = HEAP[val+1];
      Term new_tm0 = bruijn_go(tm0, depth);
      Term new_tm1 = bruijn_go(tm1, depth);
      return Sup(lab, new_tm0, new_tm1);
    }
    case DRY: {
      Term tm0 = HEAP[val+0];
      Term tm1 = HEAP[val+1];
      Term new_tm0 = bruijn_go(tm0, depth);
      Term new_tm1 = bruijn_go(tm1, depth);
      return Dry(new_tm0, new_tm1);
    }
    case DUP: {
      u32 name = lab;
      Term val_tm = HEAP[val+0];
      Term bod_tm = HEAP[val+1];
      Term new_val = bruijn_go(val_tm, depth);
      u32 prev = BRUIJN_ENV[name];
      BRUIJN_ENV[name] = depth + 1;
      Term new_bod = bruijn_go(bod_tm, depth + 1);
      BRUIJN_ENV[name] = prev;
      return Dup(lab, new_val, new_bod);
    }
    case MAT: {
      Term val_tm = HEAP[val+0];
      Term nxt_tm = HEAP[val+1];
      Term new_val = bruijn_go(val_tm, depth);
      Term new_nxt = bruijn_go(nxt_tm, depth);
      return Mat(lab, new_val, new_nxt);
    }
    case CT0: case CT1: case CT2: case CT3:
    case CT4: case CT5: case CT6: case CT7:
    case CT8: case CT9: case CTA: case CTB:
    case CTC: case CTD: case CTE: case CTF:
    case CTG: {
      u32 arity = tag - CT0;
      Term args[16];
      for (u32 i = 0; i < arity; i++) {
        Term arg = HEAP[val+i];
        args[i] = bruijn_go(arg, depth);
      }
      return Ctr(lab, arity, args);
    }
    case AL0: case AL1: case AL2: case AL3:
    case AL4: case AL5: case AL6: case AL7:
    case AL8: case AL9: case ALA: case ALB:
    case ALC: case ALD: case ALE: case ALF:
    case ALG: {
      u32 words = tag - AL0;
      u32 size = words * 2;
      u32 binds[32];
      for (u32 i = 0; i < words; i++) {
        u64 pair = HEAP[val+1+i];
        binds[i*2+0] = (u32)(pair & 0xFFFFFFFF);
        binds[i*2+1] = (u32)(pair >> 32);
      }
      Term term_tm = HEAP[val+0];
      Term new_term = bruijn_go(term_tm, depth);
      return Alo(size, binds, new_term);
    }
    default: {
      return term;
    }
  }
}

static Term bruijn(Term term) {
  return bruijn_go(term, 0);
}

// Cloning
// =======

static void clone(u32 lab, Term val, Term *out0, Term *out1) {
  u64 loc = heap_alloc(1);
  HEAP[loc] = val;
  *out0 = new_term(0, CO0, lab, loc);
  *out1 = new_term(0, CO1, lab, loc);
}

static void clone_list(u32 lab, u32 count, Term *args, Term *out0, Term *out1) {
  for (u32 i = 0; i < count; i++) {
    clone(lab, args[i], &out0[i], &out1[i]);
  }
}

// now, complete the file, implementing wnf:

// WNF
// ===

#define TAG_APP 0
#define TAG_MAT 1
#define TAG_CO0 2
#define TAG_CO1 3

// (first attempt omitted)
// 
// seems like there was some confusion w.r.t dup_lam rule. let me show you exactly what happens.
// the interaction is:
// 
// ! F &L = λx.f
// -------------
// F₀ ← λx0. G₀
// F₁ ← λx1. G₁
// x  ← &L{x0,x1}
// ! G &L = f
// 
// so, what does this mean?
// first, we create a dup to clone the function:
// ! G &L = f
// to do so, we just alloc 1 word and store 'f' on it. yet, since the old slot:
// ! F &L = λx.f
// would now be unused, we can actually skip that allocation, and just store f
// where λx.f was, as an optimization.
// then, we alloc two slots, one for each new lambda, λx0.G₀, and λx1.G₁.
// we then alloc two more slots to create the sup node (&L{x0,x1}).
// then, we substitute:
// - x by the sup node
// - the twin CO_ by the twin's lambda (ex: λx1.G₁ if we accessed via N₀)
// finally, we return this lambda (ex: λx0.G₀ if we accessed via N₀)
// 
// here's how it is done on a slightly different implementation of this runtime:
//static inline Term wnf_dup_lam(Term dup, Term lam_tm) {
//  Val  dup_loc  = term_val(dup);
//  WNF_ITRS[DUP_LAM]++;
//  u32  side     = (term_tag(dup) == DP1) ? 1u : 0u;
//  Lab  L        = term_lab(dup);
//  Val  lam_nod  = term_val(lam_tm);
//  Val  body_loc = lam_nod + 0;
//  Nick v0       = (Nick)(term_lab(lam_tm));
//  Nick v1       = (Nick)(term_lab(lam_tm));
//  Dups fd       = new_dps(L, take_at(body_loc));
//  Term lm0      = new_lam(v0, fd.dp0);
//  Term lm1      = new_lam(v1, fd.dp1);
//  Term su0      = new_sup(L, new_var(v0, term_loc(lm0)), new_var(v1, term_loc(lm1)));
//  set_at(body_loc, as_sub(su0));
//  if (side == 0) { set_at(dup_loc, as_sub(lm1)); return lm0; }
//  else           { set_at(dup_loc, as_sub(lm0)); return lm1; }
//}
//
// now, let's try again. write below the complete wnf unction:

// WNF
// ===

#define TAG_APP 0
#define TAG_MAT 1
#define TAG_CO0 2
#define TAG_CO1 3

Term wnf(Term term) {
  while (1) {
    Term tag = tag_of(term);
    Term lab = lab_of(term);
    Term val = val_of(term);

    // 1. Dereference / Normalization
    if (tag == REF) {
      u64 loc = heap_alloc(1);
      HEAP[loc] = BOOK[lab];
      term = new_term(0, AL0, 0, loc);
      continue;
    }

    if (tag == CO0 || tag == CO1) {
      u64 loc = val;
      Term node = HEAP[loc];
      if (sub_of(node)) {
        term = node & ~((u64)SUB_MASK << SUB_SHIFT);
        continue;
      }
      if (STACK_POS >= STACK_CAP) error("STACK_OVERFLOW");
      STACK_TAG[STACK_POS++] = (tag == CO0) ? TAG_CO0 : TAG_CO1;
      if (STACK_LEN >= STACK_CAP) error("STACK_OVERFLOW");
      STACK_BUF[STACK_LEN++] = term;
      term = node;
      continue;
    }

    // 2. Reductions
    if (tag == APP) {
      u64 loc = val;
      if (STACK_POS >= STACK_CAP) error("STACK_OVERFLOW");
      STACK_TAG[STACK_POS++] = TAG_APP;
      if (STACK_LEN >= STACK_CAP) error("STACK_OVERFLOW");
      STACK_BUF[STACK_LEN++] = HEAP[loc+1];
      term = HEAP[loc+0];
      continue;
    }

    if (tag >= AL0 && tag <= ALG) {
      u32 words = tag - AL0;
      u32 size = words * 2;
      u64 loc = val;
      Term inner = HEAP[loc];
      u8 i_tag = tag_of(inner);
      u32 i_lab = lab_of(inner);
      u32 i_val = val_of(inner);

      // Extract binds
      u32 binds[32];
      for (u32 i = 0; i < words; i++) {
        u64 pair = HEAP[loc+1+i];
        binds[i*2+0] = (u32)(pair & 0xFFFFFFFF);
        binds[i*2+1] = (u32)(pair >> 32);
      }

      switch (i_tag) {
        case VAR: {
          term = Var(binds[i_val]);
          break;
        }
        case CO0: {
          term = Co0(0, i_lab, binds[i_val]);
          break;
        }
        case CO1: {
          term = Co0(1, i_lab, binds[i_val]);
          break;
        }
        case REF: {
          term = inner;
          break;
        }
        case NAM: {
          term = inner;
          break;
        }
        case ERA: {
          term = inner;
          break;
        }
        case APP: {
          Term f = HEAP[i_val+0];
          Term x = HEAP[i_val+1];
          term = App(Alo(size, binds, f), Alo(size, binds, x));
          break;
        }
        case LAM: {
          Term body = HEAP[i_val];
          u64 new_loc = heap_alloc(1);
          binds[size] = new_loc;
          Term alo_body = Alo(size+1, binds, body);
          HEAP[new_loc] = alo_body;
          term = new_term(0, LAM, i_lab, new_loc);
          break;
        }
        case SUP: {
          Term a = HEAP[i_val+0];
          Term b = HEAP[i_val+1];
          term = Sup(i_lab, Alo(size, binds, a), Alo(size, binds, b));
          break;
        }
        case DUP: {
          Term v = HEAP[i_val+0];
          Term b = HEAP[i_val+1];
          Term alo_v = Alo(size, binds, v);
          u64 new_loc = heap_alloc(2); // DUP node
          binds[size] = new_loc; // DUP binds to the node
          Term alo_b = Alo(size+1, binds, b);
          HEAP[new_loc+0] = alo_v;
          HEAP[new_loc+1] = alo_b;
          term = new_term(0, DUP, i_lab, new_loc);
          break;
        }
        case MAT: {
          Term h = HEAP[i_val+0];
          Term m = HEAP[i_val+1];
          term = Mat(i_lab, Alo(size, binds, h), Alo(size, binds, m));
          break;
        }
        case DRY: {
          Term f = HEAP[i_val+0];
          Term x = HEAP[i_val+1];
          term = Dry(Alo(size, binds, f), Alo(size, binds, x));
          break;
        }
        case CT0 ... CTG: {
          u32 arity = i_tag - CT0;
          Term args[16];
          for (u32 i = 0; i < arity; i++) {
            args[i] = Alo(size, binds, HEAP[i_val+i]);
          }
          term = Ctr(i_lab, arity, args);
          break;
        }
        default: {
          // Should not happen (Alo inside Alo, etc)
          term = inner;
          break;
        }
      }
      continue;
    }

    // 3. Unwind
    if (STACK_POS == 0) return term;

    u8 frame = STACK_TAG[--STACK_POS];

    if (frame == TAG_APP) {
      Term arg = STACK_BUF[--STACK_LEN];
      switch (tag) {
        case LAM: {
          u64 loc = val;
          Term body = HEAP[loc];
          HEAP[loc] = arg | ((u64)1 << SUB_SHIFT);
          term = body;
          break;
        }
        case SUP: {
          Term a0, a1;
          clone(lab, arg, &a0, &a1);
          u64 loc = val;
          term = Sup(lab, App(HEAP[loc+0], a0), App(HEAP[loc+1], a1));
          break;
        }
        case ERA: {
          term = Era();
          break;
        }
        case NAM: {
          term = Dry(term, arg);
          break;
        }
        case DRY: {
          term = Dry(term, arg);
          break;
        }
        case MAT: {
          if (STACK_POS >= STACK_CAP) error("STACK_OVERFLOW");
          STACK_TAG[STACK_POS++] = TAG_MAT;
          if (STACK_LEN >= STACK_CAP) error("STACK_OVERFLOW");
          STACK_BUF[STACK_LEN++] = term;
          term = arg;
          break;
        }
        default: {
          term = App(term, arg);
          break;
        }
      }
      continue;
    }

    if (frame == TAG_MAT) {
      Term mat = STACK_BUF[--STACK_LEN];
      if (tag >= CT0 && tag <= CTG) {
        if (lab_of(mat) == lab) {
          u64 mat_loc = val_of(mat);
          Term body = HEAP[mat_loc+0];
          u64 ctr_loc = val;
          u32 arity = tag - CT0;
          for (u32 i=0; i<arity; i++) {
            body = App(body, HEAP[ctr_loc+i]);
          }
          term = body;
        } else {
          u64 mat_loc = val_of(mat);
          term = App(HEAP[mat_loc+1], term);
        }
      } else if (tag == SUP) {
        Term m0, m1;
        clone(lab, mat, &m0, &m1);
        u64 loc = val;
        term = Sup(lab, App(m0, HEAP[loc+0]), App(m1, HEAP[loc+1]));
      } else if (tag == ERA) {
        term = Era();
      } else {
        term = App(mat, term);
      }
      continue;
    }

    if (frame == TAG_CO0 || frame == TAG_CO1) {
      Term co = STACK_BUF[--STACK_LEN];
      u64 dup_loc = val_of(co);
      u32 dup_lab = lab_of(co);
      u32 side = (frame == TAG_CO1);
      
      HEAP[dup_loc] = term;

      switch (tag) {
        case ERA: {
          HEAP[dup_loc] = Era() | ((u64)1 << SUB_SHIFT);
          term = Era();
          break;
        }
        case SUP: {
          if (dup_lab == lab) {
            u64 loc = val;
            if (side == 0) {
              HEAP[dup_loc] = HEAP[loc+1] | ((u64)1 << SUB_SHIFT);
              term = HEAP[loc+0];
            } else {
              HEAP[dup_loc] = HEAP[loc+0] | ((u64)1 << SUB_SHIFT);
              term = HEAP[loc+1];
            }
          } else {
            u64 loc = val;
            Term a0, a1, b0, b1;
            clone(dup_lab, HEAP[loc+0], &a0, &a1);
            clone(dup_lab, HEAP[loc+1], &b0, &b1);
            Term x0 = Sup(lab, a0, b0);
            Term x1 = Sup(lab, a1, b1);
            if (side == 0) {
              HEAP[dup_loc] = x1 | ((u64)1 << SUB_SHIFT);
              term = x0;
            } else {
              HEAP[dup_loc] = x0 | ((u64)1 << SUB_SHIFT);
              term = x1;
            }
          }
          break;
        }
        case LAM: {
          u64 lam_loc = val;
          Term f = HEAP[lam_loc];
          u64 G_loc = heap_alloc(1);
          HEAP[G_loc] = f;
          Term G0 = Co0(dup_lab, G_loc);
          Term G1 = Co1(dup_lab, G_loc);
          Term F0 = Lam(lab, G0);
          Term F1 = Lam(lab, G1);
          Term x0 = Var(val_of(F0));
          Term x1 = Var(val_of(F1));
          Term sup_x = Sup(dup_lab, x0, x1);
          HEAP[lam_loc] = sup_x | ((u64)1 << SUB_SHIFT);
          if (side == 0) {
            HEAP[dup_loc] = F1 | ((u64)1 << SUB_SHIFT);
            term = F0;
          } else {
            HEAP[dup_loc] = F0 | ((u64)1 << SUB_SHIFT);
            term = F1;
          }
          break;
        }
        case NAM: {
          HEAP[dup_loc] = term | ((u64)1 << SUB_SHIFT);
          break;
        }
        case DRY: {
          u64 loc = val;
          Term f0, f1, x0, x1;
          clone(dup_lab, HEAP[loc+0], &f0, &f1);
          clone(dup_lab, HEAP[loc+1], &x0, &x1);
          Term r0 = Dry(f0, x0);
          Term r1 = Dry(f1, x1);
          if (side == 0) {
            HEAP[dup_loc] = r1 | ((u64)1 << SUB_SHIFT);
            term = r0;
          } else {
            HEAP[dup_loc] = r0 | ((u64)1 << SUB_SHIFT);
            term = r1;
          }
          break;
        }
        case CT0 ... CTG: {
          u32 arity = tag - CT0;
          u64 loc = val;
          Term args0[16], args1[16];
          for (u32 i=0; i<arity; i++) {
            clone(dup_lab, HEAP[loc+i], &args0[i], &args1[i]);
          }
          Term c0 = Ctr(lab, arity, args0);
          Term c1 = Ctr(lab, arity, args1);
          if (side == 0) {
            HEAP[dup_loc] = c1 | ((u64)1 << SUB_SHIFT);
            term = c0;
          } else {
            HEAP[dup_loc] = c0 | ((u64)1 << SUB_SHIFT);
            term = c1;
          }
          break;
        }
        case MAT: {
          u64 loc = val;
          Term h0, h1, m0, m1;
          clone(dup_lab, HEAP[loc+0], &h0, &h1);
          clone(dup_lab, HEAP[loc+1], &m0, &m1);
          Term r0 = Mat(lab, h0, m0);
          Term r1 = Mat(lab, h1, m1);
          if (side == 0) {
            HEAP[dup_loc] = r1 | ((u64)1 << SUB_SHIFT);
            term = r0;
          } else {
            HEAP[dup_loc] = r0 | ((u64)1 << SUB_SHIFT);
            term = r1;
          }
          break;
        }
        default: {
          term = (side == 0) ? Co0(dup_lab, dup_loc) : Co1(dup_lab, dup_loc);
          break;
        }
      }
      continue;
    }
  }
}

// Collapse
// ========

static Term collapse(Term term) {
  term = wnf(term);
  u8 tag = tag_of(term);
  u32 lab = lab_of(term);
  u32 val = val_of(term);

  switch (tag) {
    case ERA: {
      return Era();
    }
    case SUP: {
      u64 loc = val;
      Term t0 = collapse(HEAP[loc+0]);
      Term t1 = collapse(HEAP[loc+1]);
      return Sup(lab, t0, t1);
    }
    case LAM: {
      u64 loc = val;
      Term body = collapse(HEAP[loc]);
      return Lam(lab, body);
    }
    case APP: {
      u64 loc = val;
      Term fun = collapse(HEAP[loc+0]);
      Term arg = collapse(HEAP[loc+1]);
      return App(fun, arg);
    }
    case CT0 ... CTG: {
      u32 arity = tag - CT0;
      u64 loc = val;
      Term args[16];
      for (u32 i=0; i<arity; i++) {
        args[i] = collapse(HEAP[loc+i]);
      }
      return Ctr(lab, arity, args);
    }
    case MAT: {
      u64 loc = val;
      Term h = collapse(HEAP[loc+0]);
      Term m = collapse(HEAP[loc+1]);
      return Mat(lab, h, m);
    }
    case DRY: {
      u64 loc = val;
      Term f = collapse(HEAP[loc+0]);
      Term x = collapse(HEAP[loc+1]);
      return Dry(f, x);
    }
    default: {
      return term;
    }
  }
}

// Main
// ====

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "Usage: ./hvm4 <file.hvm>\n");
    exit(1);
  }

  BOOK = malloc(BOOK_CAP * sizeof(Term));
  HEAP = malloc(HEAP_CAP * sizeof(Term));
  STACK_BUF = malloc(STACK_CAP * sizeof(Term));
  STACK_TAG = malloc(STACK_CAP * sizeof(u8));

  State s = { .file = argv[1], .src = NULL, .pos = 0, .len = 0, .line = 1, .col = 1 };
  do_include(&s, argv[1]);

  Term main_term = Ref(parse_name(&(State){.src="main", .len=4, .pos=0}));
  Term norm = collapse(main_term);
  
  print_term(norm);
  printf("\n");

  return 0;
}
