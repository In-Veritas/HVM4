// pqueue.c - sliding 3-bucket priority queue (top/mid/bot)
//
// Overview:
// - Maintains 3 FIFO buffers:
//   - top: highest priority value observed (numerically largest 'pri')
//   - mid: exactly one below top (top - 1)
//   - bot: all priorities <= (top - 2)
// - On push:
//   - if p <= top-2 -> enqueue on bot
//   - if p == top-1 -> enqueue on mid
//   - if p == top   -> enqueue on top
//   - if p > top    -> slide window up until top == p, then enqueue
// - On pop: try bot, then mid (LIFO), then top (lowest numeric priority first)
// - Stores actual 'pri' per item for correct propagation on push

#define PQUEUE_BUFSIZE_LOG2 23u
#define PQUEUE_BUFSIZE      (1u << PQUEUE_BUFSIZE_LOG2)
#define PQUEUE_INDEX_MASK   (PQUEUE_BUFSIZE - 1u)

typedef struct {
  u8  pri;  // 0..63 - lower is better (explored first)
  u32 loc;  // heap location
} PQItem;

typedef struct {
  u32     head;  // dequeue cursor (mod PQUEUE_BUFSIZE)
  u32     size;  // current length
  PQItem *data;  // ring storage (FIFO)
} PQBuf;

typedef struct {
  PQBuf top;      // highest priority value group
  PQBuf mid;      // (top - 1)
  PQBuf bot;      // (<= top - 2)
  u8    max_pri;  // highest priority value observed
  u8    has_pri;  // whether max_pri has been initialized
} PQueue;

fn void pqbuf_init(PQBuf *b) {
  b->head = 0;
  b->size = 0;
  b->data = (PQItem*)malloc((size_t)PQUEUE_BUFSIZE * sizeof(PQItem));
  if (!b->data) {
    fprintf(stderr, "pqueue: out of memory\n");
    exit(1);
  }
}

fn void pqbuf_free(PQBuf *b) {
  if (b->data) {
    free(b->data);
    b->data = NULL;
  }
  b->head = 0;
  b->size = 0;
}

fn void pqbuf_clear(PQBuf *b) {
  b->head = 0;
  b->size = 0;
}

fn void pqbuf_push(PQBuf *b, PQItem it) {
  if (b->size == PQUEUE_BUFSIZE) {
    fprintf(stderr, "pqueue: buffer overflow\n");
    exit(1);
  }
  u32 tail = (b->head + b->size) & PQUEUE_INDEX_MASK;
  b->data[tail] = it;
  b->size++;
}

fn u8 pqbuf_pop(PQBuf *b, PQItem *out) {
  if (b->size == 0) return 0;
  *out = b->data[b->head & PQUEUE_INDEX_MASK];
  b->head = (b->head + 1) & PQUEUE_INDEX_MASK;
  b->size--;
  return 1;
}

// Pop from tail (LIFO) - used for mid bucket
fn u8 pqbuf_pop_back(PQBuf *b, PQItem *out) {
  if (b->size == 0) return 0;
  u32 idx = (b->head + b->size - 1) & PQUEUE_INDEX_MASK;
  *out = b->data[idx];
  b->size--;
  return 1;
}

// Copy ring buffer elements
fn void pqbuf_ring_copy(PQItem *dst_data, u32 dpos, const PQItem *src_data, u32 spos, u32 len) {
  while (len > 0) {
    u32 drem = PQUEUE_BUFSIZE - (dpos & PQUEUE_INDEX_MASK);
    u32 srem = PQUEUE_BUFSIZE - (spos & PQUEUE_INDEX_MASK);
    u32 chunk = len;
    if (chunk > drem) chunk = drem;
    if (chunk > srem) chunk = srem;
    memcpy(&dst_data[dpos & PQUEUE_INDEX_MASK],
           &src_data[spos & PQUEUE_INDEX_MASK],
           chunk * sizeof(PQItem));
    dpos += chunk;
    spos += chunk;
    len -= chunk;
  }
}

// Append all elements from src to dst (FIFO order), empties src
fn void pqbuf_append_all(PQBuf *dst, PQBuf *src) {
  if (src->size == 0) return;
  if (dst->size + src->size > PQUEUE_BUFSIZE) {
    fprintf(stderr, "pqueue: overflow in append\n");
    exit(1);
  }
  u32 dst_tail = dst->head + dst->size;
  pqbuf_ring_copy(dst->data, dst_tail, src->data, src->head, src->size);
  dst->size += src->size;
  pqbuf_clear(src);
}

// Prepend all elements from src to front of dst, empties src
fn void pqbuf_prepend_all(PQBuf *dst, PQBuf *src) {
  if (src->size == 0) return;
  if (dst->size + src->size > PQUEUE_BUFSIZE) {
    fprintf(stderr, "pqueue: overflow in prepend\n");
    exit(1);
  }
  u32 new_head = (dst->head - src->size) & PQUEUE_INDEX_MASK;
  pqbuf_ring_copy(dst->data, new_head, src->data, src->head, src->size);
  dst->head = new_head;
  dst->size += src->size;
  pqbuf_clear(src);
}

fn void pqueue_init(PQueue *q) {
  pqbuf_init(&q->top);
  pqbuf_init(&q->mid);
  pqbuf_init(&q->bot);
  q->max_pri = 0;
  q->has_pri = 0;
}

fn void pqueue_free(PQueue *q) {
  pqbuf_free(&q->top);
  pqbuf_free(&q->mid);
  pqbuf_free(&q->bot);
  q->has_pri = 0;
}

// Slide the top/mid/bot window up by 1 (max_pri := max_pri + 1)
fn void pqueue_slide_up(PQueue *q) {
  PQBuf old_top = q->top;
  PQBuf old_mid = q->mid;
  PQBuf old_bot = q->bot;

  if (old_bot.size <= old_mid.size) {
    // Prepend bot into mid: [bot][mid]
    pqbuf_prepend_all(&old_mid, &old_bot);
    q->top = old_bot;  // empty, becomes new top
    q->mid = old_top;
    q->bot = old_mid;
  } else {
    // Append mid into bot: [bot][mid]
    pqbuf_append_all(&old_bot, &old_mid);
    q->top = old_mid;  // empty, becomes new top
    q->mid = old_top;
    q->bot = old_bot;
  }
  q->max_pri = q->max_pri + 1;
}

fn void pqueue_push(PQueue *q, PQItem it) {
  u8 p = it.pri & 63;  // clamp to 0..63

  if (!q->has_pri) {
    q->max_pri = p;
    q->has_pri = 1;
    pqbuf_push(&q->top, it);
    return;
  }

  int d = (int)p - (int)q->max_pri;
  if (d <= -2) {
    pqbuf_push(&q->bot, it);
  } else if (d == -1) {
    pqbuf_push(&q->mid, it);
  } else if (d == 0) {
    pqbuf_push(&q->top, it);
  } else {
    // d > 0: slide up until max_pri == p
    for (int i = 0; i < d; i++) {
      pqueue_slide_up(q);
    }
    pqbuf_push(&q->top, it);
  }
}

fn u8 pqueue_pop(PQueue *q, PQItem *out) {
  // Pop order: bot (FIFO), mid (LIFO), top (FIFO)
  // This ensures lower numeric priority is processed first
  if (q->bot.size > 0) return pqbuf_pop(&q->bot, out);
  if (q->mid.size > 0) return pqbuf_pop_back(&q->mid, out);
  if (q->top.size > 0) return pqbuf_pop(&q->top, out);
  return 0;
}

fn u8 pqueue_is_empty(PQueue *q) {
  return (q->top.size | q->mid.size | q->bot.size) == 0;
}
