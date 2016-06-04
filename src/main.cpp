#include "header.h"
#include "stdio.h"
#include <iostream>

extern "C" void helloC(hInt_t x, hInt_t q)
{
  Zq::q = q;
  Zq y;
  y = x;
  printf("Hello from C world! %" PRId64 " = %" PRId64 " mod %" PRId64 "\n", x, y.x, q);
  //std::cout << "Hello from C world! " << x << " = " << y.x << " mod " << q << "\n";
}
