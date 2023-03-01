/* Author: Richard James Howe
 * Email:  howe.r.j.89@gmail.com 
 * Using primitives available in SUBLEQ to perform bitwise operations */
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define N   (16)
#define TST (9999)

typedef int16_t uword_t;
typedef int16_t  word_t;

static uword_t zlt(uword_t z) {
    return ((word_t)z) < 0 ? 0xFFFFu : 0;
}

static uword_t add(uword_t a, uword_t b) {
    return a - (uword_t)((uword_t)0 - b);
}

static uword_t lshift1(uword_t a) {
    return add(a, a);
}

static uword_t b_or(uword_t a, uword_t b) {
    uword_t r = 0;
    for (size_t i = 0; i < N; i++) {
        r = lshift1(r);
        if ((uword_t)(zlt(a) + zlt(b)) != (uword_t)0u)
            r++;
        a = lshift1(a);
        b = lshift1(b);
    }
    return r;
}

static uword_t b_xor(uword_t a, uword_t b) {
    uword_t r = 0;
    for (size_t i = 0; i < N; i++) {
        r = lshift1(r);
        if ((uword_t)(zlt(a) + zlt(b)) == (uword_t)0xFFFFu)
            r++;
        a = lshift1(a);
        b = lshift1(b);
    }
    return r;
}

static uword_t b_and(uword_t a, uword_t b) {
    uword_t r = 0;
    for (size_t i = 0; i < N; i++) {
        r = lshift1(r);
        if ((uword_t)(zlt(a) + zlt(b)) == (uword_t)0xFFFEu)
            r++;
        a = lshift1(a);
        b = lshift1(b);
    }
    return r;
}

static uword_t rnd(void) {
    return rand();
}

int main(void) {
    int pass_or = 1, pass_xor = 1, pass_and = 1;
    for (long i = 0; i < TST; i++) {
        uword_t a = rnd(), b = rnd();
        uword_t rn = a | b;
        uword_t rb = b_or(a, b);
        if (rb != rn) {
            printf("or fail %x %x -- expected %x got %x\n", a, b, rn, rb);
            pass_or = 0;
        }
    }
    printf("or %s\n", pass_or ? "pass" : "FAIL");
    for (long i = 0; i < TST; i++) {
        uword_t a = rnd(), b = rnd();
        uword_t rn = a ^ b;
        uword_t rb = b_xor(a, b);
        if (rb != rn) {
            printf("xor fail %x %x -- expected %x got %x\n", a, b, rn, rb);
            pass_xor = 0;
        }
    }
    printf("xor %s\n",  pass_xor ? "pass" : "FAIL");
    for (long i = 0; i < TST; i++) {
        uword_t a = rnd(), b = rnd();
        uword_t rn = a & b;
        uword_t rb = b_and(a, b);
        if (rb != rn) {
            printf("and fail %x %x -- expected %x got %x\n", a, b, rn, rb);
            pass_and = 0;
        }
    }
    printf("and %s\n",  pass_and ? "pass" : "FAIL");
    printf("done %s\n", pass_or && pass_xor && pass_and ? "pass" : "FAIL");
    return 0;
}

