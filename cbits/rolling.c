#include "rolling.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#define M (1 << 16)

#define X_k   (ctxt->buf_k[0])
#define X_lp1 (ctxt->buf_l[0])

rollinghash_ctxt_t* rolling_init(uint8_t *bytestring, uint32_t len, uint32_t l)
{
  rollinghash_ctxt_t *ctxt = malloc(sizeof(rollinghash_ctxt_t));

  // Set blocklength
  ctxt->l = l;

  // set buffers
  ctxt->buf_k     = bytestring;
  ctxt->buf_k_len = len;
  ctxt->buf_l     = bytestring + l;
  ctxt->buf_l_len = len - l;

  // Calculate a & b
  uint64_t a = 0, b = 0;
  if (l < len)
  {
    uint32_t i;
    for (i = 0; i <= l; i++)
    {
      a += (uint64_t) ctxt->buf_k[i];
      b += (l-i+1) * (uint64_t) ctxt->buf_k[i];
    }
  }
  ctxt->a = (uint32_t) (a % M);
  ctxt->b = (uint32_t) (b % M);

  return ctxt;
}

bool rolling_roll(rollinghash_ctxt_t *ctxt)
{
  const uint32_t l = ctxt->l;
  uint64_t a = ctxt->a, b = ctxt->b;

  if (buf_k_len > 0 && buf_l_len > 0)
  {
    a = a - X_k + X_lp1;
    b = b - (l+1) * X_k + a;

    ctxt->a = (uint32_t) (a % M);
    ctxt->b = (uint32_t) (b % M);

    // set new buffer positions
    ctxt->buf_k++;
    ctxt->buf_k_len--;

    ctxt->buf_l++;
    ctxt->buf_l_len--;

    return true;
  }
  else if (buf_k_len == 0 && buf_l_len > 0)
  {
    ctxt->buf_k_len = 
  }
  else return false;
}

bool rolling_append(rollinghash_ctxt_t *ctxt, uint8_t *bytestring, uint32_t len)
{
  if (ctxt->buf_l_len == 0)
  {
    ctxt->buf_l     = bytestring;
    ctxt->buf_l_len = len;
    return true;
  }
  else return false;
}

uint32_t rolling_hash32(rollinghash_ctxt_t *ctxt)
{
  return ((uint64_t) ctxt->a) + (((uint64_t) ctxt->b) << 16);
}

void rolling_finalize(rollinghash_ctxt_t *ctxt)
{
  free(ctxt);
}
