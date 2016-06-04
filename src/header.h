
#ifndef TENSORTYPES_H_
#define TENSORTYPES_H_

#include <inttypes.h>

typedef int64_t hInt_t ;

#ifdef __cplusplus
class Zq
{
public:
  hInt_t x;
  static hInt_t q; // defined in zq.cpp

  Zq& operator=(const hInt_t& c)
  {
    this->x = c % q;
    return *this;
  }
};
#endif /* __cplusplus */
#endif /* TENSORTYPES_H_ */
