#pragma once

#include <stdint.h>
#include <stdbool.h>

typedef struct
{
  uint32_t l;           /* length of blocks */
  uint8_t* buf_k;       /* buffer at offset k */
  uint32_t buf_k_len;
  uint8_t* buf_l;       /* buffer at offset (k+l) */
  uint32_t buf_l_len;
  uint32_t a;
  uint32_t b;
} rollinghash_ctxt_t;

rollinghash_ctxt_t* rolling_init(uint8_t *bytestring, uint32_t len, uint32_t l);
bool rolling_roll(rollinghash_ctxt_t *ctxt);
bool rolling_append(rollinghash_ctxt_t *ctxt, uint8_t *bytestring, uint32_t len);
void rolling_finalize(rollinghash_ctxt_t *ctxt);

uint32_t rolling_hash32(rollinghash_ctxt_t *ctxt);
