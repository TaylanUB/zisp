#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

union test {
  double d;
  uint64_t u;
};

int main(int argc, char** argv) {

  volatile uint64_t mask;
  volatile uint64_t min;
  volatile uint64_t max;

  volatile double d1;
  volatile double d2;

  volatile uint64_t pd1;
  volatile uint64_t pd2;


  // 0 .. 2^51

  //       SE__________QP__
  mask = 0b1111111111110111111111111111111111111111111111111111111111111111;
  min  = 0b1111111111110111111111111111111111111111111111111111111111111111;
  max  = 0b1111111111110000000000000000000000000000000000000000000000000001;

  memcpy(&d1, &min, 8);
  memcpy(&d2, &max, 8);

  printf("%lf\n", d1);
  printf("%lf\n", d2);

  memcpy(&pd1, &d1, 8);
  memcpy(&pd2, &d2, 8);
  pd1 ^= mask;
  pd2 ^= mask;

  printf("%ld\n", pd1);
  printf("%ld\n", pd2);

  printf("\n");

  // -2^51 + 1 .. -1

  //       SE__________QP__
  min  = 0b1111111111111000000000000000000000000000000000000000000000000001;
  max  = 0b1111111111111111111111111111111111111111111111111111111111111111;

  memcpy(&d1, &min, 8);
  memcpy(&d2, &max, 8);

  printf("%lf\n", d1);
  printf("%lf\n", d2);

  memcpy(&pd1, &d1, 8);
  memcpy(&pd2, &d2, 8);

  printf("%ld\n", pd1);
  printf("%ld\n", pd2);

  printf("\n");

  return 0;

  // -2^50 + 1 .. -1

  //       SE__________QFP__
  mask = 0b1111111111111100000000000000000000000000000000000000000000000000;
  min  = 0b0000000000000000000000000000000000000000000000000000000000000001;
  max  = 0b0000000000000011111111111111111111111111111111111111111111111111;

  printf("%ld\n", (int64_t) (min | mask));
  printf("%ld\n", (int64_t) (max | mask));


  // 0 .. 2^50

  //       SE__________QFP__
  mask = 0b0000000000000011111111111111111111111111111111111111111111111111;
  min  = 0b0000000000000100000000000000000000000000000000000000000000000000;
  max  = 0b0000000000000111111111111111111111111111111111111111111111111111;

  printf("%ld\n", (int64_t) (min & mask));
  printf("%ld\n", (int64_t) (max & mask));




  /* volatile union test x; */
  /* volatile double f1; */
  /* volatile double f2; */

  /* f1 = 0.0; */
  /* f2 = 0.0; */
  /* x.d = f1 / f2; */
  /* printf(" 0/0: %lx\n", x.u); */

  /* f1 = -0.0; */
  /* f2 = +0.0; */
  /* x.d = f1 / f2; */
  /* printf("-0/0: %lx\n", x.u); */

  /* x.d = sqrt(-1); */
  /* printf("sqrt(-1): %lx\n", x.u); */


  /* double nan_value = fabs(sqrt(-1.0));  // Standard way to generate NaN */

  /* uint64_t nan_bits; */
  /* memcpy(&nan_bits, &nan_value, sizeof(nan_bits)); */

  /* printf("NaN in hex: 0x%016lx\n", nan_bits); */
    
  /* return 0; */
}
