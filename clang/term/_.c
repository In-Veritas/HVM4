// Types
// =====

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef u64 Term;

typedef struct {
  Term k0;
  Term k1;
} Copy;

// Function Definition Macro
// =========================

#define fn static inline

// Tags
// ====

#define REF  0
#define ALO  1
#define ERA  2
#define CO0  3
#define CO1  4
#define VAR  5
#define LAM  6
#define APP  7
#define SUP  8
#define DUP  9
#define MAT 10
#define C00 11
#define C01 12
#define C02 13
#define C03 14
#define C04 15
#define C05 16
#define C06 17
#define C07 18
#define C08 19
#define C09 20
#define C10 21
#define C11 22
#define C12 23
#define C13 24
#define C14 25
#define C15 26
#define C16 27
#define NUM 28

// Special constructor names for stuck terms
// =========================================

#define _VAR_ 198380  // name_to_int("VAR")
#define _APP_ 113322  // name_to_int("APP")

// Bit Layout
// ==========

#define SUB_BITS 1
#define TAG_BITS 7
#define EXT_BITS 24
#define VAL_BITS 32

#define SUB_SHIFT 63
#define TAG_SHIFT 56
#define EXT_SHIFT 32
#define VAL_SHIFT 0

#define SUB_MASK 0x1
#define TAG_MASK 0x7F
#define EXT_MASK 0xFFFFFF
#define VAL_MASK 0xFFFFFFFF
