#include "tensorTypes.h"

template <typename T> T addCAny (T y)
{
  return y + 1;
}

template hInt_t addCAny (hInt_t y);
template double addCAny (double y);
