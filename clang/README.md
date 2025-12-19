# HVM4 Runtime Implementation in C

This file implements the HVM4, an Interaction Calculus runtime, ported from
Haskell. It includes parsing, stringification, and a stack-based weak normal
form (WNF) evaluator with all interaction rules.

## Term Pointer Layout (64-bit)

| SUB  (1 bit)   ::= marks heap slot as containing a substitution
| TAG  (7 bits)  ::= constructor variant (APP, LAM, SUP, etc.)
| EXT  (24 bits) ::= dup label, ctr name, or ref name
| VAL  (32 bits) ::= heap address or unboxed value

## Tag Encoding

- CO0/CO1: Two tags for Cop (copy) nodes, representing sides 0 and 1
- C00...C16: Constructor tags encode arity directly (C00+n for n fields)
- Names (variable, constructor, reference) use 6-char base64 strings encoded
  as 24-bit integers fitting in the EXT field

## Memory Model (No Separate Maps)

Unlike the Haskell version which uses IntMaps for 'dups' and 'subs', this
implementation stores everything directly on the heap:

- DUP nodes: Stored inline on heap. CO0/CO1 point to a dup node holding the
  duplicated expression (label stored in CO0/CO1's EXT field).

- Substitutions: Stored where the lambda's body, or duplicator expression,
  was. When app_lam fires, the argument replaces the lambda body slot. The
  SUB bit distinguishes actual terms from substitutions, allowing VAR, CO0
  and CO1 to detect whether their target is a binding node or a subst.

## Book vs Runtime Term Representation

Book terms (parsed definitions) use de Bruijn indices and are immutable:
  - VAR: ext = 0         ; val = bru_index
  - CO_: ext = dup_label ; val = bru_index
  - LAM: ext = bru_depth + 1 ; val = body_location
  - DUP: ext = dup_label ; val = expr_location

Runtime terms (after ALO allocation) use heap locations:
  - VAR : ext = 0         ; val = binding_lam_body_location
  - CO_ : ext = dup_label ; val = binding_dup_expr_location
  - LAM : ext = 0         ; val = body_location
  - DUP : ext = 0         ; val = expr_location

## ALO (Allocation) Nodes

ALO terms reference immutable book entries and lazily convert them to
runtime terms. Each ALO stores a pair (bind_list, book_term_loc) packed
into a single 64-bit heap word:
  - Low 32 bits: book term location
  - High 32 bits: bind list head (linked list of binder locations)

The bind list maps de Bruijn levels to runtime heap locations of binding
LAM/DUP nodes. When an ALO interaction occurs, one layer of the book term
is extracted and converted to a runtime term.

## Pretty Printer (print/)

The printer has two modes that correspond to the two term representations:

- **Runtime mode** (default): terms are linked by heap locations. The printer
  assigns a globally unique name to each lambda body location, and prints every
  VAR that points to that location using the same name. This avoids lexical
  renaming issues, so unscoped variables still print with their binders.
- **Quoted mode** (used for ALO): terms are immutable book terms with de Bruijn
  indices. Lambdas are printed using depth-based names, and VAR/CO0/CO1 try to
  use the ALO substitution list to show concrete runtime values when available.

Key rules the printer must obey:

- **Substitution chasing**: VAR/CO0/CO1 can point to heap slots with the SUB bit
  set. In that case the printer must unmark and print the substituted term
  instead of the variable itself. This applies both to the main term and to the
  ALO substitution list.
- **Floating DUPs**: dups live in the heap and may be referenced only by CO0/CO1.
  The printer records a dup when it first sees any member of its family and
  prints all discovered dups after the main term as `!A&L=val;`. Printing a dup
  can discover more dups, so this continues until the list is exhausted.
- **Names**: lambda names are lower-case (a, b, ..., aa, ab, ...), dup names are
  upper-case (A, B, ..., AA, AB, ...). These names are global, keyed by heap
  location, not by lexical scope. The printer uses fixed-size name tables
  limited to 65536 entries for lambdas and dups.

The printer entry points are:

- `print_term`: runtime mode (global naming + floating dups).
- `print_term_quoted`: same entry point as `print_term`, kept for clarity when
  printing quoted SNF output (see below).

## SNF Quoted Mode

`snf(term, depth, quoted)` normalizes to strong normal form. In quoted mode,
lambda-bound variables are substituted with NAMs (the previous behavior), and
LAM nodes are returned with `ext = de_bruijn_depth + 1`. This makes quoted
lambdas unambiguous (runtime lambdas always have `ext = 0`), and lets the
printer align LAM/NAM names using `ext`. Collapse uses quoted mode to preserve
the original output format, while non-collapsed runs use `quoted = 0`, printing
the interaction-calculus form with global names.

## Stack-Based WNF Evaluator

To avoid stack overflow, WNF uses an explicit stack with two phases:

REDUCE phase: Push eliminators onto stack and descend into their targets
  - APP: push frame, enter function
  - MAT: push frame, enter scrutinee (after MAT reaches head position)
  - CO0/CO1: push frame, enter dup'd expression

APPLY phase: Pop frames and dispatch interactions based on WHNF result

DUP and ALO don't push stack frames since they immediately trigger their
respective interactions without requiring sub-WNF results first.

## Internal-Only Constructs

These nodes are internal and not parseable:
- ALO: lazy alloc
- NAM: stuck variable (^name)
- DRY: stuck application (^(f x))

## Collapse Function

Extracts superpositions (SUP) to the top level. For each term type:
1. Collapse subterms recursively
2. Build a template: nested lambdas that reconstruct the term
3. Call inject(template, collapsed_subterms)
This will move the term to inside SUP layers, 'collapsing' it.

Key: VARs in templates must point to their binding lambda's body location.
For LAM, the inner lambda MUST reuse lam_loc so existing VARs stay bound.
