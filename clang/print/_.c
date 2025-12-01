// Stringifier Modes
// =================

#define STR_LOG 0
#define STR_BUF 1

// Stringifier Globals
// ===================

static u8    STR_MODE     = STR_LOG;
static char *TERM_BUF     = NULL;
static u32   TERM_BUF_POS = 0;
static u32   TERM_BUF_CAP = 0;
