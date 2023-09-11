/* From <https://github.com/lmcilroy/lzp>
 * Also see
 * <https://cs.stackexchange.com/questions/134277/what-is-the-simplest-algorithm-to-compress-a-string>
 * */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define putchar		putchar_unlocked
#define getchar		getchar_unlocked
#define fwrite		fwrite_unlocked
#define fread		fread_unlocked

#define HASH_ORDER	16
#define HASH_SIZE	(1 << HASH_ORDER)
#define HASH(h, x)	(h = (h << 4) ^ x)

unsigned char table[HASH_SIZE];

void
encode(void)
{
	unsigned char buf[9];
	unsigned short hash;
	int mask, i, j, c;

	buf[0] = 'L';
	buf[1] = 'Z';
	buf[2] = 'P';
	buf[3] = '1';
	i = fwrite(buf, 1, 4, stdout);
	if (i != 4) {
		fprintf(stderr, "fwrite failed\n");
		exit(1);
	}

	hash = 0;
	for (;;) {
		j = 1;
		mask = 0;
		for (i = 0; i < 8; i++) {
			c = getchar();
			if (c == EOF)
				break;
			if (c == table[hash]) {
				mask |= 1 << i;
			} else {
				table[hash] = c;
				buf[j++] = c;
			}
			HASH(hash, c);
		}
		if (i > 0) {
			buf[0] = mask;
			i = fwrite(buf, 1, j, stdout);
			if (i != j) {
				fprintf(stderr, "fwrite failed\n");
				exit(1);
			}
		}
		if (c == EOF)
			break;
	}
}

void
decode(void)
{
	unsigned char buf[8];
	unsigned short hash;
	int mask, i, j, c;

	i = fread(buf, 1, 4, stdin);
	if (i != 4) {
		fprintf(stderr, "fread failed\n");
		exit(1);
	}

	if (buf[0] != 'L' || buf[1] != 'Z' ||
	    buf[2] != 'P' || buf[3] != '1') {
		fprintf(stderr, "not compressed stream\n");
		exit(1);
	}

	hash = 0;
	for (;;) {
		j = 0;
		mask = getchar();
		if (mask == EOF)
			return;
		for (i = 0; i < 8; i++) {
			if ((mask & (1 << i)) != 0) {
				c = table[hash];
			} else {
				c = getchar();
				if (c == EOF)
					break;
				table[hash] = c;
			}
			buf[j++] = c;
			HASH(hash, c);
		}
		if (j > 0) {
			i = fwrite(buf, 1, j, stdout);
			if (i != j) {
				fprintf(stderr, "fwrite failed\n");
				exit(1);
			}
		}
	}
}

void
usage(void)
{
	fprintf(stderr, "lzp <-c|-d>\n");
	exit(1);
}

int
main(int argc, char **argv)
{
	if (argc != 2)
		usage();

	if (strcmp(argv[1], "-c") == 0)
		encode();
	else if (strcmp(argv[1], "-d") == 0)
		decode();
	else
		usage();
}

/* Version from
 * https://cs.stackexchange.com/questions/134277/what-is-the-simplest-algorithm-to-compress-a-string */
#if 0
#include <stdio.h>

#define HASH_SIZE   (1 << 16)
#define HASH(h, x)  (h = (h << 4) ^ x)

void encode(unsigned char *input, unsigned int inputlen, unsigned char *output, unsigned int *outlen) {
    unsigned char buf[9], table[HASH_SIZE] = {0};
    unsigned short hash = 0;
    int mask, i, j, c, inpos = 0, outpos = 0;
    
    for (;;) {
        j = 1;
        mask = 0;
        for (i = 0; i < 8; i++) {
            if (inpos == inputlen) break;
            c = input[inpos++];
            if (c == table[hash]) {
                mask |= 1 << i;
            } else {
                table[hash] = c;
                buf[j++] = c;
            }
            HASH(hash, c);
        }
        if (i > 0) {
            buf[0] = mask;
            for (i=0;i<j;i++) { output[outpos++] = buf[i]; } // one-liner copy function
        }
        if (inpos == inputlen) break;
    }
    *outlen = outpos;
}

void decode(unsigned char *input, unsigned int inputlen, unsigned char *output, unsigned int *outlen) {
    unsigned char buf[8], table[HASH_SIZE] = {0};
    unsigned short hash = 0;
    int mask, i, j, c, inpos = 0, outpos = 0;
    
    for (;;) {
        j = 0;
        if (inpos == inputlen) break;
        mask = input[inpos++];
        for (i = 0; i < 8; i++) {
            if ((mask & (1 << i)) != 0) {
                c = table[hash];
            } else {
                if (inpos == inputlen) break;
                c = input[inpos++];
                table[hash] = c;
            }
            buf[j++] = c;
            HASH(hash, c);
        }
        if (j > 0) {
            for (i=0;i<j;i++) { output[outpos++] = buf[i]; } // one-liner copy function
        }
    }
    *outlen = outpos;
}

int main(){
    unsigned int strlen = 82, outlen;
    unsigned char str[1024] = "Fuzzy Wuzzy was a bear. Fuzzy Wuzzy had no hair. Fuzzy Wuzzy wasn't fuzzy, was he?";
    unsigned char out[1024];
    
    printf("original length: %d\n", strlen);
    for (int i=0;i<strlen;i++) { printf("%c", str[i]); }
    printf("\n\n");
    
    encode(str, strlen, out, &outlen);
    
    printf("encoded length: %d\n", outlen);
    for (int i=0;i<outlen;i++) { printf("%d ", out[i]); }
    printf("\n\n");
    
    decode(out, outlen, str, &strlen);
    
    printf("decoded length: %d\n", strlen);
    for (int i=0;i<strlen;i++) { printf("%c", str[i]); }
    printf("\n\n");
}
#endif
