#ifndef C_H
#define C_H

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>

#ifndef ELINE
#define ELINE (-__LINE__)
#endif

#ifndef MIN
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#endif

#ifndef MAX
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#endif

typedef uint16_t u16;

int mode(const char *opt);
int load(u16 *m, u16 *prog, int file_count, char **files);
int save(u16 *m, size_t mlen, char *file);
int stats(FILE *o, const u16 prog, const u16 oprog);

#endif
