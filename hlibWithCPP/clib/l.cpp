#include "tensorTypes.h"

extern "C" hInt_t addCInt(hInt_t x) {
  return addCAny(x);
}

extern "C" double addCDouble(double x) {
  return addCAny(x);
}
