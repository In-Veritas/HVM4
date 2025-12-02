// Table Globals
// ==============
// Bidirectional mapping between unique IDs and function names.
// - TABLE[id] points to the name string for that id
// - TABLE_LEN is the next available unique ID
// - For name→id lookup, we do a linear scan (simple, good enough for now)

static char **TABLE;
static u32    TABLE_LEN = 0;
