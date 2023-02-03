/*
if D = 0 then error(DivisionByZeroException) end
Q := 0                  -- Initialize quotient and remainder to zero
R := 0                     
for i := n - 1 .. 0 do  -- Where n is number of bits in N
  R := R << 1           -- Left-shift R by 1 bit
  R(0) := N(i)          -- Set the least-significant bit of R equal to bit i of the numerator
  if R >= D then
    R := R - D
    Q(i) := 1
  end
end */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static int long_division(uint32_t n, uint32_t d, uint32_t *quo, uint32_t *rem) {
	assert(quo);
	assert(rem);
	*quo = 0;
	*rem = 0;
	if (d == 0)
		return -1;
	uint32_t q = 0, r = 0;
	for (int i = 31; i >= 0; i--) {
		r <<= 1;
		r |= !!(n & (1ul << i));
		if (r >= d) {
			r -= d;
			q |= (1ul << i);
		}
	}
	*quo = q;
	*rem = r;
	return 0;
}

int main(int argc, char **argv) {
	if (argc != 3)
		return 1;
	unsigned long op = atol(argv[1]);
	unsigned long di = atol(argv[2]);
	uint32_t quo = 0, rem = 0;
	if (long_division(op, di, &quo, &rem) < 0)
		return 2;
	const int r = fprintf(stdout, "%lu / %lu = %lu rem: %lu\n", op, di, (unsigned long)quo, (unsigned long)rem);
	return r < 0 ? 3 : 0;
}
