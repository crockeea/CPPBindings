
#ifndef TENSORTYPES_H_
#define TENSORTYPES_H_

#include <stdbool.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define ASSERT(EXP) { \
	if (!(EXP)) { \
		fprintf (stderr, "Assertion in file '%s' line %d : " #EXP "  is false\n", __FILE__, __LINE__); \
		exit(-1); \
	} \
}

typedef int64_t hInt_t ;
typedef int32_t hDim_t ;
typedef int16_t hShort_t ;
typedef int8_t hByte_t ;

typedef struct
{
	hDim_t prime;
	hShort_t exponent;
} PrimeExponent;

// calculates base ** exp
hDim_t ipow(hDim_t base, hShort_t exp);

template <typename ring> void tensorFuserPrime (ring* y, hShort_t tupSize, void (*f) (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);

#endif /* TENSORTYPES_H_ */
