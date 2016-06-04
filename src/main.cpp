#include "header.h"
#include <iostream>

extern "C" void helloC(hInt_t x, hInt_t q)
{
  Zq::q = q;
  Zq y;
  y = x;
  std::cout << "Hello from C world! " << x << " = " << y.x << " mod " << q << "\n";
}
