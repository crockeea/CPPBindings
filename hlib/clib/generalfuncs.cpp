#include "tensorTypes.h"

hDim_t ipow(hDim_t base, hShort_t exp)
{
	hDim_t result = 1;
  while (exp) {
    if (exp & 1) {
      result *= base;
    }
    exp >>= 1;
    base *= base;
  }
  return result;
}

//for square transforms
template <typename ring> void tensorFuserPrime (ring* y, hShort_t tupSize, void (*f) (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
  hDim_t lts = totm;
  hDim_t rts = 1;
  hShort_t i;

  for (i = 0; i < sizeOfPE; ++i) {
    PrimeExponent pe = peArr[i];
    hDim_t ipow_pe = ipow(pe.prime, (pe.exponent-1));
    hDim_t dim = (pe.prime-1) * ipow_pe;  // the totient of pe
    lts /= dim;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      (*f) (y+tupIdx, tupSize, lts*ipow_pe, rts, pe.prime);
    }
    rts *= dim;
  }
}

template void tensorFuserPrime (hInt_t* y, hShort_t tupSize, void (*f) (hInt_t* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t pe), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);
template void tensorFuserPrime (double* y, hShort_t tupSize, void (*f) (double* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t pe), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);
